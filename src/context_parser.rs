//! A yet another shell script parser. In contrast to [`parser::parse`]
//! this returns an syntax array (rather than syntax tree) which makes it easy to
//! implement context-aware stuffs such as completion and syntax highlighting.
use std::collections::VecDeque;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BlockType {
    If,
    ParamExpand,
    CmdSubst,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum KeywordType {
    If,
    Fi,
    Then,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum QuoteType {
    Double,
    Single,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum CommandSepType {
    /// `\n'
    Newline,
    /// `;'
    Semi,
    /// `&'
    SingleAnd,
    /// `&&'
    DoubleAnd,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Span {
    Literal(String),
    Space(String),
    Argv0(String),
    Keyword(KeywordType),
    /// `$var`
    Param(String),
    /// `;`, `&&`, or `||`
    CommandSep(CommandSepType),

    // `${...}`
    ParamExpandStart,
    ParamExpandEnd,
    Name(String),
    Op(String),

    // `$(...)`
    CmdSubstStart,
    CmdSubstEnd,

    // `"'
    QuoteStart(QuoteType),
    QuoteEnd(QuoteType),
}

fn is_word_separator(span: &Span) -> bool {
    match span {
        Span::Space(_) | Span::CommandSep(_) | Span::CmdSubstStart => true,
        _ => false,
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
enum State {
    EnvOrArgv0,
    Word,
    Eof,
    ParamName,
    ParamOp,
}

fn is_whitespace(ch: char) -> bool {
    " \t".contains(ch)
}

fn is_varname_char(ch: char) -> bool {
    "@*?!$_".contains(ch) || ch.is_ascii_alphanumeric()
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct InputContext {
    // The input string.
    pub input: String,
    // The cursor position.
    pub cursor: usize,
    // Words of a command where the cursor is in (`$COMP_WORDS`).
    pub words: Vec<String>,
    // The fragments of the input. Primarily used for syntax highlighting.
    pub spans: Vec<Span>,
    // The context of the input. It is not empty if input is incomplete. For
    // example, parsing "if true" returns nested=[BlockType::If] because it
    // does not contain `fi`.
    pub nested: Vec<BlockType>,
    // The range of the current *literal-like* part over the cursor in `input`.
    // Primarily used by completion to extract and replace the current word
    // string. It is `None` if the cursor is not at a literal-like `Span` such
    // as `Span::Param`, CmdSubstStart, etc.
    pub current_literal: Option<std::ops::Range<usize>>,
    // The index of the word where the cursor is at in `words` (`$COMP_CWORD`).
    pub current_word: usize,
    // The index of the span where the cursor is at in `words`.
    pub current_span: Option<usize>,
}

struct ContextParser {
    state: State,
    input: String,
    cursor: usize,
    index: usize,
    in_quote: Option<QuoteType>,
    nested: VecDeque<BlockType>,
    nested_param_expands: usize,
    nested_cmd_substs: usize,
}

impl ContextParser {
    fn new(input: &str, cursor: usize) -> ContextParser {
        ContextParser {
            state: State::EnvOrArgv0,
            input: input.to_owned(),
            cursor,
            nested: VecDeque::new(),
            index: 0,
            in_quote: None,
            nested_cmd_substs: 0,
            nested_param_expands: 0,
        }
    }

    fn enter_block(&mut self, block: BlockType) {
        self.nested.push_back(block);
    }

    fn leave_block(&mut self, leave_from: BlockType) {
        for (i, block) in self.nested.iter().rev().enumerate() {
            if *block == leave_from {
                self.nested.remove(self.nested.len() - i - 1);
                return;
            }
        }
    }

    fn consume_span(&mut self) -> (State, Span) {
        let s: String = self.input.chars().skip(self.index).collect();

        // Command separator.
        if s.starts_with('\n') {
            self.index += 1;
            return (State::EnvOrArgv0, Span::CommandSep(CommandSepType::Newline));
        }

        // Command separator.
        if s.starts_with(';') {
            self.index += 1;
            return (State::EnvOrArgv0, Span::CommandSep(CommandSepType::Semi));
        }

        // Command separator.
        if s.starts_with("&&") {
            self.index += 2;
            return (State::EnvOrArgv0, Span::CommandSep(CommandSepType::DoubleAnd));
        }

        // Command separator.
        if s.starts_with('&') {
            self.index += 1;
            return (State::EnvOrArgv0, Span::CommandSep(CommandSepType::SingleAnd));
        }

        // Whitespaces.
        if let Some(ch) = s.chars().nth(0) {
            if is_whitespace(ch) {
                let mut sep = String::new();
                for ch in s.chars().take_while(|ch| is_whitespace(*ch)) {
                    sep.push(ch);
                    self.index += 1;
                }

                // Keep old state.
                return (self.state, Span::Space(sep));
            }
        }

        // The end of a parameter expansion.
        if s.starts_with('}') && self.nested_param_expands > 0 {
            self.leave_block(BlockType::ParamExpand);
            self.nested_param_expands -= 1;
            self.index += 1;
            return (State::Word, Span::ParamExpandEnd);
        }

        // The end of a command subtitution.
        if s.starts_with(')') && self.nested_cmd_substs > 0 {
            self.leave_block(BlockType::CmdSubst);
            self.nested_cmd_substs -= 1;
            self.index += 1;
            return (State::Word, Span::CmdSubstEnd);
        }

        // The beginning of a parameter expansion.
        if s.starts_with("${#") {
            self.enter_block(BlockType::ParamExpand);
            self.nested_param_expands += 1;
            self.index += 3;
           return (State::ParamName, Span::ParamExpandStart);
        }

        // The beginning of a parameter expansion.
        if s.starts_with("${") {
            self.enter_block(BlockType::ParamExpand);
            self.nested_param_expands += 1;
            self.index += 2;
           return (State::ParamName, Span::ParamExpandStart);
        }

        // The beginning of a command subtitution.
        if s.starts_with("$(") {
            self.enter_block(BlockType::CmdSubst);
            self.nested_cmd_substs += 1;
            self.index += 2;
           return (State::EnvOrArgv0, Span::CmdSubstStart);
        }

        // A parameter expansion without braces.
        if s.starts_with('$') {
            let name: String = s.chars().skip(1).take_while(|ch| is_varname_char(*ch)).collect();
            self.index += 1 + name.len();
           return (State::Word, Span::Param(name));
        }

        // A parameter name.
        if self.state == State::ParamName {
            let name: String = s.chars().take_while(|ch| is_varname_char(*ch)).collect();
            self.index += name.len();
            return (State::ParamOp, Span::Name(name));
        }

        // A parameter expansion operator.
        if self.state == State::ParamOp {
            for op in &[":=", "=", ":-", "-", "//", "/"] {
                if s.starts_with(op) {
                    self.index += op.len();
                    return (State::Word, Span::Op((*op).to_owned()));
                }
            }

            // Unknown param op. Recognize as a word.
        }

        // Quoted strings.
        match s.chars().nth(0) {
            Some(ch @ '"') | Some(ch @ '\'') =>  {
                let quote_type = if ch == '"' {
                    QuoteType::Double
                } else {
                    QuoteType::Single
                };

                match self.in_quote {
                    Some(current) if current == quote_type => {
                        self.in_quote = None;
                        self.index += 1;
                        return (State::Word, Span::QuoteEnd(quote_type));
                    }
                    // Treat as a literal character in the quoted string.
                    Some(_) => (),
                    None => {
                        self.in_quote = Some(quote_type);
                        self.index += 1;
                        return (State::Word, Span::QuoteStart(quote_type));
                    }
                }

            }
            _ => (),
        }

        // Literal, etc.
        let mut buf = String::new();
        let mut iter = self.input.chars().skip(self.index).peekable();
        loop {
            let current = iter.next();
            let next = iter.peek().cloned();

            match (current, next) {
                (Some('\\'), Some(escaped)) => {
                    // Escaped word separators.

                    // Skip `escaped` char.
                    iter.next();
                    self.index += 2;

                    buf.push('\\');
                    buf.push(escaped);
                }
                (Some(ch), _) if " \t\r\n".contains(ch) && self.in_quote.is_none() => {
                    break;
                }
                (Some(ch), _) if "'\";$".contains(ch) => {
                    break;
                }
                (Some('}'), _) if self.nested_param_expands > 0 => {
                    break;
                }
                (Some(')'), _) if self.nested_cmd_substs > 0 => {
                    break;
                }
                (Some(ch), _) => {
                    buf.push(ch);
                    self.index += 1;
                }
                (None, _) => {
                    break;
                }
            }
        }

        match buf.as_str() {
            "" => (State::Eof, Span::Literal("".to_owned())),
            "if" => {
                self.enter_block(BlockType::If);
                (State::EnvOrArgv0, Span::Keyword(KeywordType::If))
            }
            "fi" => {
                self.leave_block(BlockType::If);
                (State::EnvOrArgv0, Span::Keyword(KeywordType::Fi))
            }
            "then" => (State::EnvOrArgv0, Span::Keyword(KeywordType::Then)),
            _ => {
                match self.state {
                    State::EnvOrArgv0 => (State::Word, Span::Argv0(buf)),
                    _ => (State::Word, Span::Literal(buf)),
                }
            }
        }
    }

    pub fn parse(mut self) -> InputContext {
        let mut words = Vec::new();
        let mut spans = Vec::new();
        let mut current_word = String::new();
        let mut current_word_index = 0;
        let mut current_literal = None;
        let mut current_span = None;
        let mut freeze_words = false;

        loop {
            self.state = match self.state {
                State::Eof => {
                    break;
                }
                _ => {
                    let prev_index = self.index;
                    let (new_state, span) = self.consume_span();

                    if current_literal.is_none() {
                        match &span {
                            Span::Literal(lit) | Span::Argv0(lit) if !lit.is_empty() => {
                                if prev_index <= self.cursor && self.cursor <= self.index {
                                    if !freeze_words {
                                        current_word_index = words.len();
                                    }

                                    let end = self.index.saturating_sub(1);
                                    current_literal = Some(prev_index..end);
                                    current_span = Some(spans.len());
                                }
                            }
                            _ => (),
                        }
                    }

                    if !freeze_words {
                        if new_state != State::Eof {
                            if is_word_separator(&span) {
                                words.push(current_word);
                                current_word = String::new();
                            } else {
                                current_word += &self.input[prev_index..self.index];
                            }
                        }

                        if new_state == State::EnvOrArgv0 {
                            if self.cursor < self.index {
                                // The current `words` is where the cursor is in. Stop
                                // updating `words` and `current_word_index`.
                                freeze_words = true;
                            } else {
                                words = Vec::new();
                            }
                        }
                    }

                    if new_state != State::Eof {
                        spans.push(span);
                    }

                    new_state
                }
            };
        }

        if !current_word.is_empty() {
            words.push(current_word);
        }

        if words.is_empty() {
            words.push("".to_owned());
        }

        if let Some(Span::Space(_)) = spans.last() {
            if self.cursor == self.input.len() {
                words.push("".to_owned());
                current_word_index = words.len() - 1;
            }
        }

        InputContext {
            words,
            current_word: current_word_index,
            spans,
            nested: self.nested.clone().into_iter().collect(),
            current_literal,
            current_span,
            input: self.input,
            cursor: self.cursor,
        }
    }
}

pub fn parse(input: &str, cursor: usize) -> InputContext {
    let parser = ContextParser::new(input, cursor);
    parser.parse()
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::{assert_eq};

    #[test]
    fn short() {
        let input = "".to_owned();
        let cursor = 0;
        assert_eq!(
            parse(&input, cursor),
            InputContext {
                spans: vec![],
                nested: vec![],
                current_literal: None,
                input,
                cursor,
                words: vec![
                    "".to_owned()
                ],
                current_word: 0,
                current_span: None,
            }
        );

        let input = "ls   /tmp /usr /var".to_owned();
        let cursor = 7; /* after `t` */
        assert_eq!(
            parse(&input, cursor),
            InputContext {
                spans: vec![
                    Span::Argv0("ls".to_owned()),
                    Span::Space("   ".to_owned()),
                    Span::Literal("/tmp".to_owned()),
                    Span::Space(" ".to_owned()),
                    Span::Literal("/usr".to_owned()),
                    Span::Space(" ".to_owned()),
                    Span::Literal("/var".to_owned())
                ],
                input,
                cursor,
                nested: vec![],
                current_literal: Some(5..8),
                words: vec![
                    "ls".to_owned(),
                    "/tmp".to_owned(),
                    "/usr".to_owned(),
                    "/var".to_owned(),
                ],
                current_word: 1,
                current_span: Some(2),
            }
        );

        let input = "git co ".to_owned();
        let cursor = 7; /* after `o` */
        assert_eq!(
            parse(&input, cursor),
            InputContext {
                spans: vec![
                    Span::Argv0("git".to_owned()),
                    Span::Space(" ".to_owned()),
                    Span::Literal("co".to_owned()),
                    Span::Space(" ".to_owned()),
                ],
                input,
                cursor,
                nested: vec![],
                current_literal: None,
                words: vec![
                    "git".to_owned(),
                    "co".to_owned(),
                    "".to_owned(),
                ],
                current_word: 2,
                current_span: None,
            }
        );

        let input = "echo \"Hello \\\"\"$world\\\"".to_owned();
        let cursor = 2;
        assert_eq!(
            parse(&input, cursor),
            InputContext {
                spans: vec![
                    Span::Argv0("echo".to_owned()),
                    Span::Space(" ".to_owned()),
                    Span::QuoteStart(QuoteType::Double),
                    Span::Literal("Hello \\\"".to_owned()),
                    Span::QuoteEnd(QuoteType::Double),
                    Span::Param("world".to_owned()),
                    Span::Literal("\\\"".to_owned())
                ],
                input,
                cursor,
                nested: vec![],
                current_literal: Some(0..3),
                words: vec![
                    "echo".to_owned(),
                    "\"Hello \\\"\"$world\\\"".to_owned(),
                ],
                current_word: 0,
                current_span: Some(0),
            }
        );

        let input = "echo Hello ${var:=$(echo Wor)ld}".to_owned();
        let cursor = 12; /* first $ */
        assert_eq!(
            parse(&input, cursor),
            InputContext {
                spans: vec![
                    Span::Argv0("echo".to_owned()),
                    Span::Space(" ".to_owned()),
                    Span::Literal("Hello".to_owned()),
                    Span::Space(" ".to_owned()),
                    Span::ParamExpandStart,
                    Span::Name("var".to_owned()),
                    Span::Op(":=".to_owned()),
                    Span::CmdSubstStart,
                    Span::Argv0("echo".to_owned()),
                    Span::Space(" ".to_owned()),
                    Span::Literal("Wor".to_owned()),
                    Span::CmdSubstEnd,
                    Span::Literal("ld".to_owned()),
                    Span::ParamExpandEnd,
                ],
                input,
                cursor,
                nested: vec![],
                current_literal: None,
                words: vec![
                    "echo".to_owned(),
                    "Hello".to_owned(),
                    "${var:=".to_owned(),
                ],
                current_word: 0,
                current_span: None
            }
        );
    }

    #[test]
    fn statements() {
        let input = "if yes; then echo say yes; fi".to_owned();
        let cursor = 17; /* after `o' */
        assert_eq!(
            parse(&input, cursor),
            InputContext {
                spans: vec![
                    Span::Keyword(KeywordType::If),
                    Span::Space(" ".to_owned()),
                    Span::Argv0("yes".to_owned()),
                    Span::CommandSep(CommandSepType::Semi),
                    Span::Space(" ".to_owned()),
                    Span::Keyword(KeywordType::Then),
                    Span::Space(" ".to_owned()),
                    Span::Argv0("echo".to_owned()),
                    Span::Space(" ".to_owned()),
                    Span::Literal("say".to_owned()),
                    Span::Space(" ".to_owned()),
                    Span::Literal("yes".to_owned()),
                    Span::CommandSep(CommandSepType::Semi),
                    Span::Space(" ".to_owned()),
                    Span::Keyword(KeywordType::Fi),
                ],
                input,
                cursor,
                nested: vec![],
                current_literal: Some(13..16),
                words: vec![
                    "echo".to_owned(),
                    "say".to_owned(),
                    "yes".to_owned(),
                ],
                current_word: 0,
                current_span: Some(7),
            }
        );
    }

    #[test]
    fn incomplete() {
        let input = "echo Hello ${var:=$(echo ".to_owned();
        let cursor = 25; /* at the end */
        assert_eq!(
            parse(&input, cursor),
            InputContext {
                spans: vec![
                    Span::Argv0("echo".to_owned()),
                    Span::Space(" ".to_owned()),
                    Span::Literal("Hello".to_owned()),
                    Span::Space(" ".to_owned()),
                    Span::ParamExpandStart,
                    Span::Name("var".to_owned()),
                    Span::Op(":=".to_owned()),
                    Span::CmdSubstStart,
                    Span::Argv0("echo".to_owned()),
                    Span::Space(" ".to_owned()),
                ],
                input,
                cursor,
                nested: vec![BlockType::ParamExpand, BlockType::CmdSubst],
                current_literal: None,
                words: vec![
                    "echo".to_owned(),
                    "".to_owned()
                ],
                current_word: 1,
                current_span: None,
            }
        );

        let input = "if yes; then echo say yes".to_owned();
        let cursor = 6 /* after `s' */;
        assert_eq!(
            parse(&input, cursor),
            InputContext {
                spans: vec![
                    Span::Keyword(KeywordType::If),
                    Span::Space(" ".to_owned()),
                    Span::Argv0("yes".to_owned()),
                    Span::CommandSep(CommandSepType::Semi),
                    Span::Space(" ".to_owned()),
                    Span::Keyword(KeywordType::Then),
                    Span::Space(" ".to_owned()),
                    Span::Argv0("echo".to_owned()),
                    Span::Space(" ".to_owned()),
                    Span::Literal("say".to_owned()),
                    Span::Space(" ".to_owned()),
                    Span::Literal("yes".to_owned())
                ],
                input,
                cursor,
                nested: vec![BlockType::If],
                current_literal: Some(3..5),
                words: vec![
                    "yes".to_owned(),
                ],
                current_word: 0,
                current_span: Some(2),
            }
        );
    }
}

#[cfg(test)]
mod benchmarks {
    extern crate test;
    use test::Bencher;
    use super::*;

    #[bench]
    fn simple_oneliner_parsring_bench(b: &mut Bencher) {
        b.iter(|| {
            parse("git reset --", 0);
        })
    }

    #[bench]
    fn complex_oneliner_parsring_bench(b: &mut Bencher) {
        b.iter(|| {
            parse("ls -avh $(echo hello) \"string ${ls:=bar $(cowsay) } boo\" yay", 0);
        })
    }
}
