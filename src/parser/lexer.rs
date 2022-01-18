use std::collections::VecDeque;

use thiserror::Error;

use crate::ast::*;
use crate::highlight::{HighlightKind, HighlightSpan};

const STDIN_FD: i32 = 0;
const STDOUT_FD: i32 = 1;

struct HighlighterContext {
    char_offset_start: usize,
}

struct InputReader {
    input: Box<dyn Iterator<Item = char>>,
    char_offset: usize,
    push_back_stack: Vec<char>,
}

impl InputReader {
    pub fn new(input: impl Iterator<Item = char> + 'static) -> InputReader {
        InputReader {
            input: Box::new(input),
            char_offset: 0,
            push_back_stack: Vec::new(),
        }
    }

    pub fn char_offset(&self) -> usize {
        self.char_offset
    }

    /// Pops a character from the input stream without consuming the character,
    /// that is, the same character will be returned next time `pop` or `peek`
    /// is called.
    pub fn peek(&mut self) -> Option<char> {
        if let Some(c) = self.push_back_stack.last() {
            Some(*c)
        } else {
            let c = self.input.next()?;
            self.push_back_stack.push(c);
            Some(c)
        }
    }

    /// Consumes a character from the input stream.
    pub fn consume(&mut self) -> Option<char> {
        let ret = if let Some(c) = self.push_back_stack.pop() {
            Some(c)
        } else {
            self.input.next()
        };

        if ret.is_some() {
            self.char_offset += 1;
        }

        ret
    }

    /// Pushes a character back to the input stream. The character will be
    /// returned next time `pop` or `peek` is called.
    pub fn unconsume(&mut self, c: char) {
        self.push_back_stack.push(c);
        self.char_offset -= 1;
    }
}

struct SpansBuilder {
    spans: Vec<Span>,
    plain_text: String,
}

impl SpansBuilder {
    pub fn new() -> Self {
        Self {
            spans: Vec::new(),
            plain_text: String::new(),
        }
    }

    pub fn into_vec(self) -> Vec<Span> {
        let mut spans = self.spans;
        if !self.plain_text.is_empty() {
            spans.push(Span::Plain(self.plain_text));
        }

        spans
    }

    pub fn push_char(&mut self, c: char) {
        self.plain_text.push(c);
    }

    pub fn push_str(&mut self, s: &str) {
        self.plain_text.push_str(s);
    }

    pub fn push_span(&mut self, span: Span) {
        if !self.plain_text.is_empty() {
            self.spans.push(Span::Plain(self.plain_text.clone()));
            self.plain_text = String::new();
        }

        self.spans.push(span);
    }
}

enum HereDocMarker {
    /// `<< EOS`
    Normal(String),
    /// `<< "EOS"`
    Plain(String),
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum UnclosedBrace {
    /// A variable expansion `${foo`.
    Variable,
    /// A brace expansion `{1,2,3`.
    Brace,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum UnclosedParen {
    /// A command substitution `$(`.
    Command,
    /// A Arithmetic expansion `$((`.
    Arith,
    /// A parenthesis within an arithmetic expansion `$(( (1 +`.
    ArithExpr,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Context {
    /// A command substitution (e.g. `\`foo\``).
    BackTick,
    /// A command substitution (e.g. `$(foo)`).
    Command,
    /// A arithmetic expansion (e.g. `$(( n + 1 ))`).
    Arith,
    /// A variable substitution (e.g. `${foo}`).
    BraceParam,
    /// Double quoted string (e.g. `"foo"`).
    DoubleQuote,
    /// Single quoted string (e.g. `'foo'`).
    SingleQuote,
}

#[derive(Error, Debug, PartialEq)]
pub enum LexerError {
    /// Indicates the lexer has already returned an error. If you see this,
    /// it's a bug because you should not call `lexer.next()` after an error.
    #[error("Lexer is already halted in an error state (BUG).")]
    Halted,
    /// Indicates the lexer has reached the end of the input.
    #[error("Reached the end of the input.")]
    Eof,
    #[error("No matching right parenthesis `)'.")]
    NoMatchingRightParen,
    #[error("No matching right brace `}}'.")]
    NoMatchingRightBrace,
    #[error("No closing backtick '`'.")]
    NoMatchingClosingBackTick,
    #[error("Expected here document marker (e.g. \"EOF\" in \"cat << EOF\").")]
    ExpectedHereDocMarker,
    #[error("Unclosed here document.")]
    UnclosedHereDoc,
    #[error("Unclosed brace expansion.")]
    UnclosedBraceExp,
    #[error("Unclosed parameter expansion.")]
    UnclosedParamExp,
    #[error("Unclosed wildcard character set.")]
    UnclosedAnyCharInPattern,
    #[error("Inconsistent parenthesis (perhaps some `)' needs to be added?).")]
    InconsistentParenthesis,
    #[error("Invalid word found in an arithmetic expansion.")]
    InvalidTokenInArith,
    #[error("Invalid variable name `{0}'.")]
    InvalidVariableName(String),
}

/// A lexer.
///
/// This is NOT await-ready: the iterator should block the thread until the next
/// character gets available. This is because the lexer in async seemed to be
/// unnecessarily complicated.
pub struct Lexer {
    input: InputReader,
    reached_to_eof: bool,
    halted: bool,
    in_backtick: bool,
    argv0_mode: bool,
    unclosed_braces: Vec<UnclosedBrace>,
    unclosed_parens: Vec<UnclosedParen>,
    unclosed_context_stack: Vec<Context>,
    highlight_spans: Vec<HighlightSpan>,
    heredocs: Vec<HereDoc>,
    next_heredoc_index: usize,
    unclosed_heredoc_markers: VecDeque<HereDocMarker>,
}

impl Lexer {
    pub fn new<I>(input: I) -> Lexer
    where
        I: Iterator<Item = char> + 'static,
    {
        Lexer {
            reached_to_eof: false,
            halted: false,
            input: InputReader::new(input),
            in_backtick: false,
            argv0_mode: false,
            unclosed_braces: Vec::new(),
            unclosed_parens: Vec::new(),
            unclosed_context_stack: Vec::new(),
            highlight_spans: Vec::new(),
            heredocs: Vec::new(),
            next_heredoc_index: 0,
            unclosed_heredoc_markers: VecDeque::new(),
        }
    }

    pub fn set_argv0_mode(&mut self, enable: bool) {
        self.argv0_mode = enable;
    }

    pub fn highlight_spans(&self) -> &[HighlightSpan] {
        &self.highlight_spans
    }

    pub fn heredoc(&self, index: usize) -> &HereDoc {
        &self.heredocs[index]
    }

    pub fn heredocs(&self) -> &[HereDoc] {
        &self.heredocs
    }

    /// Returns the next token.
    fn next_token(&mut self) -> Result<Token, LexerError> {
        if self.halted {
            return Err(LexerError::Halted);
        }

        match self.do_next_token() {
            Ok(token) => Ok(token),
            Err(LexerError::Eof) => {
                self.reached_to_eof = true;
                Err(LexerError::Eof)
            }
            Err(err) => {
                self.halted = true;
                Err(err)
            }
        }
    }

    fn do_next_token(&mut self) -> Result<Token, LexerError> {
        self.skip_whitespaces();

        // Read digits. It sounds a bit weird, but we need to handle the case
        // a redirection with the fd number.
        let digits = self.consume_digits();

        // First we check if it's a redirection.
        let first = self.input.consume();
        let second = self.input.peek();
        let n = if digits.is_empty() {
            None
        } else {
            Some(digits.parse::<i32>().unwrap())
        };
        let token = match (first, second) {
            (Some('>'), Some('&')) => {
                // Skip '&'.
                self.input.consume();
                let dst = self.consume_digits();
                if dst.is_empty() {
                    // ">&" is at EOF. Handle it as plain text.
                    Token::Word(plain_word(format!("{}>&", digits)))
                } else {
                    Token::Redirection(Redirection {
                        op: RedirOp::Output(n.unwrap_or(STDOUT_FD)),
                        // SAFETY: dst is guaranteed to be a valid integer.
                        rhs: RedirRhs::Fd(dst.parse().unwrap()),
                    })
                }
            }
            (Some('<'), Some('&')) => {
                // Skip '&'.
                self.input.consume();
                let src = self.consume_digits();
                if src.is_empty() {
                    // "<&" is at EOF. Handle it as plain text.
                    Token::Word(plain_word(format!("{}<&", digits)))
                } else {
                    Token::Redirection(Redirection {
                        op: RedirOp::Input(n.unwrap_or(STDIN_FD)),
                        // SAFETY: dst is guaranteed to be a valid integer.
                        rhs: RedirRhs::Fd(src.parse().unwrap()),
                    })
                }
            }
            (Some('&'), Some('>')) => {
                // Skip '>'.
                self.input.consume();
                let (op_str, op) = if let Some('>') = self.input.peek() {
                    // `&>>`
                    ("&>>", RedirOp::AppendStdoutAndStderr)
                } else {
                    // `&>`
                    ("&>", RedirOp::OutputStdoutAndStderr)
                };

                let word = self.visit_word_skipping_whitespaces()?;
                if !digits.is_empty() || word.spans().is_empty() {
                    // "&>" is at EOF. Handle it as plain text.
                    Token::Word(plain_word(format!("{}{}", digits, op_str)))
                } else {
                    Token::Redirection(Redirection {
                        op,
                        rhs: RedirRhs::File(word),
                    })
                }
            }
            (Some('>'), Some('>')) => {
                // Skip the second '>'.
                self.input.consume();

                let word = self.visit_word_skipping_whitespaces()?;
                if word.spans().is_empty() {
                    // ">>" is at EOF. Handle it as plain text.
                    Token::Word(plain_word(format!("{}>>", digits)))
                } else {
                    Token::Redirection(Redirection {
                        op: RedirOp::Append(n.unwrap_or(STDOUT_FD)),
                        rhs: RedirRhs::File(word),
                    })
                }
            }
            (Some('<'), Some('<')) => {
                // Skip the second '<'.
                self.input.consume();

                let heredoc = self.visit_heredoc_marker()?;
                Token::Redirection(Redirection {
                    op: RedirOp::Input(n.unwrap_or(STDIN_FD)),
                    rhs: RedirRhs::HereDoc(heredoc),
                })
            }
            (Some('<'), Some(c)) if c != '(' => {
                let word = self.visit_word_skipping_whitespaces()?;
                if word.spans().is_empty() {
                    // "<" is at EOF. Handle it as plain text.
                    Token::Word(plain_word(format!("{}<", digits)))
                } else {
                    Token::Redirection(Redirection {
                        op: RedirOp::Input(n.unwrap_or(STDIN_FD)),
                        rhs: RedirRhs::File(word),
                    })
                }
            }
            (Some('>'), Some(c)) if c != '(' => {
                let word = self.visit_word_skipping_whitespaces()?;
                if word.spans().is_empty() {
                    // ">" is at EOF. Handle it as plain text.
                    Token::Word(plain_word(format!("{}>", digits)))
                } else {
                    Token::Redirection(Redirection {
                        op: RedirOp::Output(n.unwrap_or(STDOUT_FD)),
                        rhs: RedirRhs::File(word),
                    })
                }
            }
            (Some('<'), None) => {
                // Handle it as a plain text.
                Token::Word(Word(vec![Span::Plain("<".to_owned())]))
            }
            (Some('>'), None) => {
                // Handle it as a plain text.
                Token::Word(Word(vec![Span::Plain(">".to_owned())]))
            }
            _ => {
                // Not a redirection. Go back before the digits we've read above.
                if let Some(c) = first {
                    self.input.unconsume(c);
                }
                for c in digits.chars().rev() {
                    self.input.unconsume(c);
                }

                let first = self.input.consume().ok_or(LexerError::Eof)?;
                let second = self.input.peek();
                match (first, second) {
                    ('|', Some('|')) => {
                        self.input.consume();
                        Token::DoubleOr
                    }
                    ('&', Some('&')) => {
                        self.input.consume();
                        Token::DoubleAnd
                    }
                    (';', Some(';')) => {
                        self.input.consume();
                        Token::DoubleSemi
                    }
                    ('|', _) => Token::Or,
                    ('&', _) => Token::And,
                    (';', _) => Token::Semi,
                    ('(', _) if !self.in_arith() => Token::LeftParen,
                    (')', _) if !self.in_arith_paren() => Token::RightParen,
                    // Handling curly braces is a bit tricky. In a shell script,
                    // they're used in:
                    //
                    // - A command grouping (argv0_mode).
                    // - A brace expansion in non-argv0 words (!argv0_mode).
                    //   It's handled in `visit_word()`.
                    // - A parameter expansion (unclosed_braces). Note that
                    //   the beginning of it `{` is handled in `visit_word()`.
                    ('{', _) if self.argv0_mode => Token::LeftBrace,
                    ('}', _)
                        if self.argv0_mode
                            || matches!(
                                self.unclosed_braces.last(),
                                Some(UnclosedBrace::Variable)
                            ) =>
                    {
                        Token::RightBrace
                    }
                    // The end of A command substitution.
                    ('`', _) if self.in_backtick => Token::ClosingBackTick,
                    // Comment.
                    ('#', _) => {
                        // Skip until the end of the line or EOF.
                        loop {
                            // If the comment is in the last line and there's no newline
                            // at EOF, return None from the `?` operator.
                            if self.input.consume().ok_or(LexerError::Eof)? == '\n' {
                                break Token::Newline;
                            }
                        }
                    }
                    ('\n', _) => {
                        if !self.unclosed_heredoc_markers.is_empty() {
                            self.visit_heredoc_body()?;
                        }

                        Token::Newline
                    }
                    _ if self.argv0_mode => {
                        self.input.unconsume(first);
                        self.visit_argv0()?
                    }
                    _ => {
                        self.input.unconsume(first);
                        Token::Word(self.visit_word()?)
                    }
                }
            }
        };

        Ok(token)
    }

    fn visit_argv0(&mut self) -> Result<Token, LexerError> {
        const ARGV0_KEYWORDS: &[(&str, Token)] = &[
            ("if", Token::If),
            ("while", Token::While),
            ("for", Token::For),
            ("function", Token::Function),
            ("break", Token::Break),
            ("continue", Token::Continue),
            ("return", Token::Return),
        ];

        let hctx = self.enter_highlight(0);
        // Check if it's a assignment first.
        if let Some(assignment) = self.visit_assignment()? {
            return Ok(assignment);
        }

        // It's not an assignment. It must be a keyword or a command.
        let word = self.visit_word()?;

        // Check if it's a keyword.
        if word.spans().len() == 1 {
            if let Span::Plain(ref value) = word.spans()[0] {
                if let Some((_, token)) = ARGV0_KEYWORDS.iter().find(|(k, _)| k == value) {
                    self.leave_highlight(hctx, HighlightKind::Keyword, 0);
                    return Ok(token.clone());
                }
            }
        }

        // A function or command name.
        self.leave_highlight(hctx, HighlightKind::Argv0, 0);
        Ok(Token::Argv0(word))
    }

    fn visit_assignment(&mut self) -> Result<Option<Token>, LexerError> {
        // An assignment looks like /^[a-zA-Z_]+=/.
        let mut name = String::new();
        while let Some(c) = self.input.consume() {
            if !is_identifier_char(c) {
                if !name.is_empty() && c == '=' {
                    return Ok(Some(Token::Assignment(Assignment {
                        name,
                        rhs: AssignRhs::String(self.visit_word()?),
                    })));
                }

                // Push c to uncosume later.
                name.push(c);
                break;
            }

            name.push(c);
        }

        // It's not an assignment. Go back to the beginning of the word.
        for c in name.chars().rev() {
            self.input.unconsume(c);
        }

        Ok(None)
    }

    fn visit_tilde_exp(&mut self) -> Result<Span, String> {
        let is_valid_char_after_tilde_exp = |c: char| {
            matches!(
                c,
                // A path separator.
                '/' |
                // A word terminator.
                ' ' | '\t' | '\n' | '|' | '&' | ';' | '#' | ')' | '<' | '>'
            )
        };

        let mut plain = String::new();
        if Some('~') == self.input.peek() {
            self.input.consume();
            plain.push('~');

            match self.input.peek() {
                Some(c) if is_valid_username_char(c) => {
                    // `~seiya`, `~seiya/`, or `~seiya/foo`.
                    while let Some(c) = self.input.consume() {
                        if !is_valid_username_char(c) {
                            self.input.unconsume(c);
                            break;
                        }

                        plain.push(c);
                    }

                    let name = plain[1..].to_owned();
                    return Ok(Span::Tilde(Tilde::HomeOf(name)));
                }
                Some(c) if is_valid_char_after_tilde_exp(c) => {
                    // `~`, `~/`, or `~/foo`.
                    return Ok(Span::Tilde(Tilde::Home));
                }
                None => {
                    // `~`.
                    return Ok(Span::Tilde(Tilde::Home));
                }
                _ => {
                    return Err(plain);
                }
            }
        }

        Err(plain)
    }

    fn in_arith(&self) -> bool {
        matches!(
            self.unclosed_parens.last(),
            Some(UnclosedParen::Arith | UnclosedParen::ArithExpr)
        )
    }

    fn in_arith_paren(&self) -> bool {
        matches!(self.unclosed_parens.last(), Some(UnclosedParen::ArithExpr))
    }

    fn visit_word_skipping_whitespaces(&mut self) -> Result<Word, LexerError> {
        self.skip_whitespaces();
        self.visit_word()
    }

    fn visit_word(&mut self) -> Result<Word, LexerError> {
        let mut spans = SpansBuilder::new();

        // Check if the word starts with a tilde expansion.
        match self.visit_tilde_exp() {
            Ok(span) => {
                spans.push_span(span);
            }
            Err(s) => spans.push_str(&s),
        };

        let mut in_double_quotes = false;
        let mut in_single_quotes = false;
        let mut quote_hctx = None;
        while let Some(c) = self.input.consume() {
            let next_c = self.input.peek();
            match c {
                '<' | '>' if next_c == Some('(') && !in_double_quotes && !in_single_quotes => {
                    self.input.unconsume(c);
                    spans.push_span(self.visit_process_sub()?);
                }
                '(' if self.in_arith() && !in_double_quotes && !in_single_quotes => {
                    spans.push_char(c);
                    self.enter_paren(UnclosedParen::ArithExpr);
                }
                ')' if self.in_arith_paren() && !in_double_quotes && !in_single_quotes => {
                    spans.push_char(c);
                    self.leave_paren(UnclosedParen::ArithExpr);
                }
                ' ' | '\t'
                    if matches!(self.unclosed_braces.last(), Some(UnclosedBrace::Variable)) =>
                {
                    spans.push_char(c);
                }
                ' ' | '\t' | '\n' | '|' | '&' | ';' | '#' | ')' | '<' | '>'
                    if !in_double_quotes && !in_single_quotes =>
                {
                    self.input.unconsume(c);
                    break;
                }
                '}' if matches!(self.unclosed_braces.last(), Some(UnclosedBrace::Variable))
                    && !in_double_quotes
                    && !in_single_quotes =>
                {
                    // Return as Token::RightBrace in the next next_token() call.
                    self.input.unconsume(c);
                    break;
                }
                '}' | ','
                    if matches!(self.unclosed_braces.last(), Some(UnclosedBrace::Brace))
                        && !in_double_quotes
                        && !in_single_quotes =>
                {
                    self.input.unconsume(c);
                    break;
                }
                '"' if in_double_quotes && !in_single_quotes => {
                    in_double_quotes = false;
                    self.leave_context(Context::DoubleQuote);
                    self.leave_highlight(
                        quote_hctx.take().unwrap(),
                        HighlightKind::QuotedString,
                        0,
                    );
                }
                '"' if !in_single_quotes => {
                    in_double_quotes = true;
                    self.enter_context(Context::DoubleQuote);
                    quote_hctx = Some(self.enter_highlight(1 /* len('"') */));
                }
                '\'' if in_single_quotes && !in_double_quotes => {
                    in_single_quotes = false;
                    self.leave_context(Context::SingleQuote);
                    self.leave_highlight(
                        quote_hctx.take().unwrap(),
                        HighlightKind::QuotedString,
                        0,
                    );
                }
                '\'' if !in_double_quotes => {
                    in_single_quotes = true;
                    self.enter_context(Context::SingleQuote);
                    quote_hctx = Some(self.enter_highlight(1 /* len('"') */));
                }
                '{' if !in_double_quotes && !in_single_quotes => {
                    spans.push_span(Span::Brace(self.visit_brace_exp()?));
                }
                '*' if !in_double_quotes && !in_single_quotes && !self.in_arith() => {
                    spans.push_span(Span::AnyString);
                }
                '?' if !in_double_quotes && !in_single_quotes && !self.in_arith() => {
                    spans.push_span(Span::AnyChar);
                }
                '[' if !in_double_quotes && !in_single_quotes && !self.in_arith() => {
                    // Extract the pattern.
                    let mut pattern = String::new();
                    loop {
                        let c = self
                            .input
                            .consume()
                            .ok_or(LexerError::UnclosedAnyCharInPattern)?;

                        match (c, self.input.peek()) {
                            ('\\', Some(']')) => {
                                // Skip the escaped ']'.
                                self.input.consume();
                                pattern.push(']');
                            }
                            (']', _) => {
                                break;
                            }
                            _ => {
                                pattern.push(c);
                            }
                        }
                    }

                    spans.push_span(Span::AnyCharIn(pattern));
                }
                '`' if self.in_backtick => {
                    // Unconsume to return Token::ClosingBackTick.
                    self.input.unconsume(c);
                    break;
                }
                '`' if !in_single_quotes => {
                    spans.push_span(self.visit_backtick_exp()?);
                }
                '$' if !in_single_quotes => {
                    spans.push_span(self.visit_variable_exp(in_double_quotes)?);
                }
                '\\' if next_c == Some('\n') => {
                    // Skip the newline character.
                    self.input.consume();
                }
                // Escaped character.
                '\\' => {
                    let hctx = self.enter_highlight(1 /* len("\\") */);

                    let ch = self.input.consume().unwrap_or('\\' /* backslash at EOF */);
                    spans.push_char(ch);

                    self.leave_highlight(hctx, HighlightKind::EscSeq, 0);
                }
                _ => {
                    spans.push_char(c);
                }
            }
        }

        Ok(Word(spans.into_vec()))
    }

    /// Visits a variable expansion (after `$`).
    fn visit_variable_exp(&mut self, quoted: bool) -> Result<Span, LexerError> {
        let c = self.input.consume();
        let next = self.input.peek();
        let span = match c {
            // `$(( n + 1 ))`
            Some('(') if next == Some('(') => {
                // Skip the second '('.
                self.input.consume();
                self.visit_arith_exp()?
            }
            // `$(echo foo)`
            Some('(') => {
                self.add_highlight(HighlightKind::CommandSymbol, 1 /* len('`') */);
                let inner_htx = self.enter_highlight(0);
                self.enter_paren(UnclosedParen::Command);

                let tokens = self.consume_tokens_until(
                    Context::Command,
                    Token::RightParen,
                    LexerError::NoMatchingRightParen,
                )?;

                self.leave_highlight(
                    inner_htx,
                    HighlightKind::Plain,
                    1, /* not to include the right parenthesis */
                );
                self.add_highlight(HighlightKind::CommandSymbol, 1 /* len(')') */);

                self.leave_paren(UnclosedParen::Command);
                Span::Command(tokens)
            }
            // `${#foo}`
            Some('{') if next == Some('#') => {
                let hctx = self.enter_highlight(2 /* len("${") */);
                // Skip '#'.
                self.input.consume();
                self.enter_context(Context::BraceParam);

                // Read the name.
                let mut name = String::new();
                let last_c;
                loop {
                    let c = self.input.consume().ok_or(LexerError::UnclosedParamExp)?;
                    if !is_identifier_char(c) {
                        last_c = c;
                        break;
                    }
                    name.push(c);
                }

                if last_c != '}' {
                    name.push(last_c);
                    return Err(LexerError::InvalidVariableName(name));
                }

                self.leave_context(Context::BraceParam);
                self.leave_highlight(hctx, HighlightKind::Variable { name: name.clone() }, 0);
                Span::Variable {
                    name,
                    expansion: VarExpansion::Length,
                    quoted,
                }
            }
            // `${foo}`
            Some('{') => {
                let hctx = self.enter_highlight(2 /* len("${") */);
                self.enter_context(Context::BraceParam);

                // Read the name.
                let mut name = String::new();
                let last_c;
                loop {
                    let c = self.input.consume().ok_or(LexerError::UnclosedParamExp)?;
                    if !is_identifier_char(c) {
                        last_c = c;
                        break;
                    }
                    name.push(c);
                }

                self.enter_brace(UnclosedBrace::Variable);
                let expansion = match (last_c, self.input.peek()) {
                    ('}', _) => VarExpansion::GetOrEmpty,
                    (':', Some('-')) => VarExpansion::GetOrDefault(self.visit_word()?),
                    ('-', _) => VarExpansion::GetNullableOrDefault(self.visit_word()?),
                    (':', Some('=')) => VarExpansion::GetOrDefaultAndAssign(self.visit_word()?),
                    ('=', _) => VarExpansion::GetNullableOrDefaultAndAssign(self.visit_word()?),
                    (':', Some('?')) => VarExpansion::GetOrExit(self.visit_word()?),
                    ('?', _) => VarExpansion::GetNullableOrExit(self.visit_word()?),
                    (':', Some('+')) => VarExpansion::GetWordIfSet(self.visit_word()?),
                    ('+', None) => VarExpansion::GetWordIfSetOrNull(self.visit_word()?),
                    _ => {
                        name.push(last_c);
                        return Err(LexerError::InvalidVariableName(name));
                    }
                };
                self.leave_brace(UnclosedBrace::Variable);

                self.leave_context(Context::BraceParam);
                self.leave_highlight(hctx, HighlightKind::Variable { name: name.clone() }, 0);
                Span::Variable {
                    name,
                    expansion,
                    quoted,
                }
            }
            // `$foo`
            Some(c) if is_identifier_char(c) => {
                let hctx = self.enter_highlight(2 /* len("$" + c) */);

                let mut name = String::new();
                name.push(c);
                while let Some(c) = self.input.consume() {
                    if !is_identifier_char(c) {
                        self.input.unconsume(c);
                        break;
                    }
                    name.push(c);
                }

                self.leave_highlight(hctx, HighlightKind::Variable { name: name.clone() }, 0);
                Span::Variable {
                    name,
                    expansion: VarExpansion::GetOrEmpty,
                    quoted,
                }
            }
            // Not a variable expansion. Handle it as a plain `$`.
            Some(c) => {
                self.input.unconsume(c);
                Span::Plain("$".to_owned())
            }
            None => {
                // `$` at EOF. Treat it as a plain `$`.
                Span::Plain("$".to_owned())
            }
        };

        Ok(span)
    }

    /// Visits an arithmetic expression (after `$((`).
    fn visit_arith_exp(&mut self) -> Result<Span, LexerError> {
        self.enter_paren(UnclosedParen::Arith);

        let tokens = self.consume_tokens_until(
            Context::Arith,
            Token::RightParen,
            LexerError::NoMatchingRightParen,
        )?;

        // A arithmetic expression should be terminated by `))`.
        if self.input.consume() != Some(')') {
            return Err(LexerError::InconsistentParenthesis);
        }

        let mut merged_spans = Vec::new();
        for token in tokens {
            match token {
                Token::Word(Word(spans)) => {
                    merged_spans.extend(spans);
                }
                Token::LeftParen => {
                    merged_spans.push(Span::Plain("(".to_owned()));
                }
                Token::RightParen => {
                    merged_spans.push(Span::Plain(")".to_owned()));
                }
                _ => {
                    return Err(LexerError::InvalidTokenInArith);
                }
            }
        }

        self.enter_paren(UnclosedParen::Arith);
        Ok(Span::Arith(Word(merged_spans)))
    }

    /// Visits a backtick substitution (after `\``).
    fn visit_backtick_exp(&mut self) -> Result<Span, LexerError> {
        self.add_highlight(HighlightKind::CommandSymbol, 1 /* len('`') */);
        let inner_htx = self.enter_highlight(0);

        self.in_backtick = true;
        let tokens = self.consume_tokens_until(
            Context::BackTick,
            Token::ClosingBackTick,
            LexerError::NoMatchingClosingBackTick,
        )?;
        self.in_backtick = false;

        self.leave_highlight(
            inner_htx,
            HighlightKind::Plain,
            1, /* not to include the right backtick */
        );
        self.add_highlight(HighlightKind::CommandSymbol, 1 /* len('`') */);

        Ok(Span::Command(tokens))
    }

    /// Visits a process substitution (the reader must be at `>` or `<`).
    fn visit_process_sub(&mut self) -> Result<Span, LexerError> {
        let ctor = match self.input.consume() {
            Some('>') => Span::ProcessWritable,
            Some('<') => Span::ProcessReadable,
            _ => unreachable!(),
        };

        // Skip '('.
        self.input.consume();

        self.add_highlight(HighlightKind::CommandSymbol, 1 /* len('`') */);
        let inner_htx = self.enter_highlight(0);

        let tokens = self.consume_tokens_until(
            Context::Command,
            Token::RightParen,
            LexerError::NoMatchingRightParen,
        )?;

        self.leave_highlight(
            inner_htx,
            HighlightKind::Plain,
            1, /* not to include the right parenthesis */
        );
        self.add_highlight(HighlightKind::CommandSymbol, 1 /* len(')') */);

        Ok(ctor(tokens))
    }

    /// Visits a here document makrer. `self.input` should be positioned at the
    /// first character next to the here document marker `<<`.
    ///
    /// Returns a heredoc index.
    fn visit_heredoc_marker(&mut self) -> Result<usize, LexerError> {
        self.skip_whitespaces();

        let marker = match self.input.peek() {
            // << "EOS"
            Some(quote) if quote == '"' || quote == '\'' => {
                // Skip the left-side '"' or '\'' (what quote holds).
                self.input.consume();

                // Read the marker string.
                let mut s = String::new();
                while let Some(c) = self.input.consume() {
                    if c == quote {
                        break;
                    }
                    s.push(c);
                }

                HereDocMarker::Plain(s)
            }
            // << EOS
            Some(c) if is_valid_marker_char(c) => {
                // Read the marker string.
                let mut s = String::new();
                while let Some(c) = self.input.consume() {
                    if !is_valid_marker_char(c) {
                        self.input.unconsume(c);
                        break;
                    }
                    s.push(c);
                }

                HereDocMarker::Normal(s)
            }
            _ => {
                return Err(LexerError::ExpectedHereDocMarker);
            }
        };

        self.unclosed_heredoc_markers.push_back(marker);

        let index = self.next_heredoc_index;
        self.next_heredoc_index += 1;
        Ok(index)
    }

    fn visit_heredoc_body(&mut self) -> Result<(), LexerError> {
        while let Some(marker) = self.unclosed_heredoc_markers.pop_front() {
            let heredoc = match marker {
                HereDocMarker::Normal(eos) => {
                    let mut plain = String::new();
                    let mut lines = Vec::new();
                    let mut current_line = Vec::new();
                    loop {
                        let c = match self.input.consume() {
                            Some(c) => c,
                            None => return Err(LexerError::UnclosedHereDoc),
                        };

                        match c {
                            '\n' => {
                                if current_line.is_empty() && plain == eos {
                                    break;
                                }

                                if !plain.is_empty() {
                                    current_line.push(Span::Plain(plain));
                                    plain = String::new();
                                }

                                lines.push(current_line);
                                current_line = Vec::new();
                            }
                            '`' => {
                                if !plain.is_empty() {
                                    current_line.push(Span::Plain(plain));
                                    plain = String::new();
                                }

                                current_line.push(self.visit_backtick_exp()?);
                            }
                            '$' => {
                                if !plain.is_empty() {
                                    current_line.push(Span::Plain(plain));
                                    plain = String::new();
                                }

                                current_line.push(self.visit_variable_exp(false)?);
                            }

                            _ => {
                                plain.push(c);
                            }
                        }
                    }

                    HereDoc(lines)
                }
                HereDocMarker::Plain(eos) => {
                    let mut plain = String::new();
                    let mut lines = Vec::new();
                    let mut current_line = Vec::new();
                    loop {
                        let c = match self.input.consume() {
                            Some(c) => c,
                            None => return Err(LexerError::UnclosedHereDoc),
                        };

                        match c {
                            '\n' => {
                                if current_line.is_empty() && plain == eos {
                                    break;
                                }

                                if !plain.is_empty() {
                                    current_line.push(Span::Plain(plain));
                                    plain = String::new();
                                }

                                lines.push(current_line);
                                current_line = Vec::new();
                            }
                            _ => {
                                plain.push(c);
                            }
                        }
                    }

                    HereDoc(lines)
                }
            };

            self.heredocs.push(heredoc);
        }

        Ok(())
    }

    /// Visits a brace expansion (after `{`).
    fn visit_brace_exp(&mut self) -> Result<BraceExpansion, LexerError> {
        let mut list = Vec::new();
        loop {
            match self.input.consume() {
                None => {
                    return Err(LexerError::UnclosedBraceExp);
                }
                Some('}') => {
                    break;
                }
                Some(',') => {
                    continue;
                }
                // A nested brace expansion.
                Some('{') => {
                    self.input.consume();
                    list.push(self.visit_brace_exp()?);
                }
                Some(c) => {
                    // BraceExpansion::Word or BraceExpansion::Sequence.
                    self.input.unconsume(c);

                    // First, try parsing as a sequence and then fall back to a word
                    // if failed.
                    match self.visit_sequence_brace_exp() {
                        Some(exp) => {
                            list.push(exp);
                        }
                        None => {
                            self.enter_brace(UnclosedBrace::Brace);
                            let word = self.visit_word()?;
                            self.leave_brace(UnclosedBrace::Brace);
                            list.push(BraceExpansion::Word(word));
                        }
                    }
                }
            }
        }

        Ok(BraceExpansion::List(list))
    }

    /// Visits a sequence brace expansion like `{0..10}` (after `{`).
    ///
    /// We only support (positive) integer sequences.
    fn visit_sequence_brace_exp(&mut self) -> Option<BraceExpansion> {
        let mut start = String::new();
        while let Some(c) = self.input.consume() {
            if !c.is_ascii_digit() {
                self.input.unconsume(c);
                break;
            }

            start.push(c);
        }

        if start.is_empty() {
            // It's not a sequence.
            return None;
        }

        // After an integer, ".." should come next if it's a sequence.
        let abort = match (self.input.consume(), self.input.consume()) {
            (Some('.'), Some('.')) => false,
            (Some(c1), Some(c2)) => {
                self.input.unconsume(c2);
                self.input.unconsume(c1);
                true
            }
            (Some(c1), None) => {
                self.input.unconsume(c1);
                true
            }
            (None, None) => true,
            (None, Some(_)) => {
                unreachable!()
            }
        };

        if abort {
            // It's not a sequence. Revert the reader state.
            for c in start.chars().rev() {
                self.input.unconsume(c);
            }
            return None;
        }

        let mut end = String::new();
        while let Some(c) = self.input.consume() {
            if !c.is_ascii_digit() {
                self.input.unconsume(c);
                break;
            }

            end.push(c);
        }

        let next_ch = self.input.peek();
        if (next_ch != Some('}') && next_ch != Some(',')) || end.is_empty() {
            // It's not a sequence. Revert the reader state.
            for c in end.chars().rev() {
                self.input.unconsume(c);
            }
            self.input.unconsume('.');
            self.input.unconsume('.');
            for c in start.chars().rev() {
                self.input.unconsume(c);
            }
            return None;
        }

        // SAFETY: `start` and `end` are guaranteed to be valid integers
        //         by `is_ascii_digit`.
        Some(BraceExpansion::Sequence(Sequence::Integer {
            start: start.parse().unwrap(),
            end: end.parse().unwrap(),
            num_digits: start.chars().count(),
        }))
    }

    /// Skip whitespace characters.
    fn skip_whitespaces(&mut self) {
        while let Some(c) = self.input.consume() {
            match c {
                ' ' | '\t' => {
                    continue;
                }
                '\\' if self.input.peek() == Some('\n') => {
                    self.input.unconsume(c);
                    break;
                }
                _ => {
                    self.input.unconsume(c);
                    break;
                }
            }
        }
    }

    fn consume_tokens_until(
        &mut self,
        context: Context,
        end_marker: Token,
        err_on_eof: LexerError,
    ) -> Result<Vec<Token>, LexerError> {
        let mut tokens = Vec::new();
        self.enter_context(context);

        loop {
            let token = match self.next_token() {
                Ok(token) => token,
                Err(LexerError::Eof) => return Err(err_on_eof),
                Err(err) => return Err(err),
            };

            if token == end_marker {
                break;
            }

            tokens.push(token);
        }

        self.leave_context(context);
        Ok(tokens)
    }

    fn consume_digits(&mut self) -> String {
        let mut digits = String::new();
        while let Some(c) = self.input.consume() {
            if !c.is_ascii_digit() {
                self.input.unconsume(c);
                break;
            }

            digits.push(c);
        }

        digits
    }

    fn enter_context(&mut self, context: Context) {
        self.unclosed_context_stack.push(context);
    }

    fn leave_context(&mut self, context: Context) {
        let last_context = self.unclosed_context_stack.pop().unwrap();
        debug_assert_eq!(last_context, context);
    }

    fn enter_brace(&mut self, context: UnclosedBrace) {
        self.unclosed_braces.push(context);
    }

    fn leave_brace(&mut self, context: UnclosedBrace) {
        let last_context = self.unclosed_braces.pop().unwrap();
        debug_assert_eq!(last_context, context);
    }

    fn enter_paren(&mut self, context: UnclosedParen) {
        self.unclosed_parens.push(context);
    }

    fn leave_paren(&mut self, context: UnclosedParen) {
        let last_context = self.unclosed_parens.pop().unwrap();
        debug_assert_eq!(last_context, context);
    }

    fn enter_highlight(&mut self, diff: usize) -> HighlighterContext {
        HighlighterContext {
            char_offset_start: self.input.char_offset() - diff,
        }
    }

    fn leave_highlight(&mut self, hctx: HighlighterContext, kind: HighlightKind, diff: usize) {
        self.highlight_spans.push(HighlightSpan {
            kind,
            char_range: hctx.char_offset_start..(self.input.char_offset() - diff),
        });
    }

    /// Highlights last `len` characters.
    fn add_highlight(&mut self, kind: HighlightKind, len: usize) {
        self.highlight_spans.push(HighlightSpan {
            kind,
            char_range: (self.input.char_offset() - len)..self.input.char_offset(),
        });
    }
}

impl Iterator for Lexer {
    type Item = Result<Token, LexerError>;

    fn next(&mut self) -> Option<Result<Token, LexerError>> {
        if self.reached_to_eof {
            return None;
        }

        match self.next_token() {
            Ok(token) => Some(Ok(token)),
            Err(LexerError::Eof) => None,
            Err(err) => Some(Err(err)),
        }
    }
}

fn plain_word<T: Into<String>>(s: T) -> Word {
    Word::new(vec![Span::Plain(s.into())])
}

fn is_identifier_char(c: char) -> bool {
    matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_')
}

fn is_valid_marker_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

fn is_valid_username_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn input_reader() {
        let mut input = InputReader::new("abc".chars());
        assert_eq!(input.consume(), Some('a'));
        assert_eq!(input.peek(), Some('b'));
        assert_eq!(input.consume(), Some('b'));
        input.unconsume('X');
        input.unconsume('Y');
        assert_eq!(input.consume(), Some('Y'));
        assert_eq!(input.consume(), Some('X'));
        assert_eq!(input.consume(), Some('c'));
        assert_eq!(input.consume(), None);
        input.unconsume('Z');
        assert_eq!(input.consume(), Some('Z'));
        assert_eq!(input.consume(), None);
    }

    #[test]
    fn input_reader_regression_1() {
        let mut input = InputReader::new("a".chars());
        assert_eq!(input.peek(), Some('a'));
        assert_eq!(input.peek(), Some('a'));
        assert_eq!(input.peek(), Some('a'));
        assert_eq!(input.consume(), Some('a'));
        assert_eq!(input.consume(), None);
    }
}
