use std::{collections::VecDeque, os::unix::prelude::RawFd};

use thiserror::Error;

use crate::highlight::{HighlightKind, HighlightSpan};

/// A fragment of a word.
#[derive(Clone, Debug, PartialEq)]
pub enum Span {
    /// A plain text.
    Plain(String),
    /// A variable substitution, e.g. `$foo`.
    Variable { name: String },
    /// A command substitution, e.g. `$(echo "hi")`.
    Command(Vec<Token>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Word(Vec<Span>);

impl Word {
    pub fn new(spans: Vec<Span>) -> Word {
        Word(spans)
    }

    pub fn spans(&self) -> &[Span] {
        &self.0
    }

    /// Returns true if the word is a single plain text.
    ///
    /// It's useful for checking if a word is a keyword.
    pub fn equals(&self, text: &str) -> bool {
        self.0.len() == 1 && matches!(self.0[0], Span::Plain(ref s) if s == text)
    }
}

/// Contains heredoc body. The outer Vec represents lines and
/// `Vec<Word>` represents the contents of a line.
#[derive(Debug, PartialEq, Clone)]
pub struct HereDoc(Vec<Vec<Span>>);

impl HereDoc {
    pub fn new(lines: Vec<Vec<Span>>) -> HereDoc {
        HereDoc(lines)
    }

    pub fn lines(&self) -> &[Vec<Span>] {
        &self.0
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum RedirectionKind {
    /// `cat < foo.txt` or here document.
    Input,
    /// `cat > foo.txt`
    Output,
    /// `cat >> foo.txt`
    Append,
}

enum HereDocMarker {
    /// `<< EOS`
    Normal(String),
    /// `<< "EOS"`
    Plain(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum RedirectionTarget {
    /// `foo.log` in `> foo.log`.
    File(Word),
    /// `1` in `2>&1`.
    Fd(RawFd),
    /// A here document. Contains the index for [`Lexer::heredoc`].
    HereDoc(usize),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Redirection {
    pub kind: RedirectionKind,
    pub target: RedirectionTarget,
    pub fd: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    /// A newline (`\n`).
    Newline,
    /// `|`
    Or,
    /// `&`.
    And,
    /// `;`
    Semi,
    /// `&&`
    DoubleAnd,
    /// `||`
    DoubleOr,
    /// `;;`
    DoubleSemi,
    /// `(`
    LeftParen,
    /// `)`
    RightParen,
    /// `{`
    LeftBrace,
    /// `}`
    RightBrace,
    /// `\``
    ClosingBackTick,
    /// A redirection like `echo > foo.log`.
    Redirection(Redirection),
    /// A word.
    Word(Word),
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
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Context {
    /// A command substitution (e.g. `\`foo\``).
    BackTick,
    /// A command substitution (e.g. `$(foo)`).
    Command,
    /// A variable substitution (e.g. `${foo}`).
    BraceParam,
    /// Double quoted string (e.g. `"foo"`).
    DoubleQuote,
    /// Single quoted string (e.g. `'foo'`).
    SingleQuote,
}

struct HighlighterContext {
    char_offset_start: usize,
}

struct InputReader<I: Iterator<Item = char>> {
    input: I,
    char_offset: usize,
    push_back_stack: Vec<char>,
}

impl<I: Iterator<Item = char>> InputReader<I> {
    pub fn new(input: I) -> InputReader<I> {
        InputReader {
            input,
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

/// A lexer.
///
/// This is NOT await-ready: the iterator should block the thread until the next
/// character gets available. This is because the lexer in async seemed to be
/// unnecessarily complicated.
pub struct Lexer<I: Iterator<Item = char>> {
    input: InputReader<I>,
    halted: bool,
    in_backtick: bool,
    argv0_mode: bool,
    brace_param_level: usize,
    unclosed_context_stack: Vec<Context>,
    highlight_spans: Vec<HighlightSpan>,
    heredocs: Vec<HereDoc>,
    next_heredoc_index: usize,
    unclosed_heredoc_markers: VecDeque<HereDocMarker>,
}

impl<I: Iterator<Item = char>> Lexer<I> {
    pub fn new(input: I) -> Lexer<I> {
        Lexer {
            halted: false,
            input: InputReader::new(input),
            in_backtick: false,
            argv0_mode: false,
            brace_param_level: 0,
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

        let ret = self.do_next_token();
        if ret.is_err() {
            self.halted = true;
        }

        ret
    }

    fn do_next_token(&mut self) -> Result<Token, LexerError> {
        self.skip_whitespaces();

        // Read digits. It sounds a bit weird, but we need to handle the case
        // a redirection with the fd number.
        let mut digits = String::new();
        while let Some(c) = self.input.consume() {
            if !c.is_ascii_digit() {
                self.input.unconsume(c);
                break;
            }

            digits.push(c);
        }

        // First we check if it's a redirection.
        let first = self.input.consume();
        let second = self.input.peek();
        let n = if digits.is_empty() {
            None
        } else {
            Some(digits.parse::<usize>().unwrap())
        };
        let token = match (first, second) {
            (Some('>'), Some('>')) => {
                let word = self.visit_word()?;
                Token::Redirection(Redirection {
                    kind: RedirectionKind::Append,
                    target: RedirectionTarget::File(word),
                    fd: n.unwrap_or(1 /* stdout */),
                })
            }
            (Some('<'), Some('<')) => {
                // Skip the second '<'.
                self.input.consume();

                let heredoc = self.visit_heredoc_marker()?;
                Token::Redirection(Redirection {
                    kind: RedirectionKind::Input,
                    target: RedirectionTarget::HereDoc(heredoc),
                    fd: n.unwrap_or(0 /* stdin */),
                })
            }
            (Some('<'), _) => {
                let word = self.visit_word()?;
                Token::Redirection(Redirection {
                    kind: RedirectionKind::Input,
                    target: RedirectionTarget::File(word),
                    fd: n.unwrap_or(0 /* stdin */),
                })
            }
            (Some('>'), _) => {
                let word = self.visit_word()?;
                Token::Redirection(Redirection {
                    kind: RedirectionKind::Output,
                    target: RedirectionTarget::File(word),
                    fd: n.unwrap_or(1 /* stdout */),
                })
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
                    ('|', Some('|')) => Token::DoubleOr,
                    ('|', _) => Token::Or,
                    ('&', Some('&')) => Token::DoubleAnd,
                    ('&', _) => Token::And,
                    (';', Some(';')) => Token::DoubleSemi,
                    (';', _) => Token::Semi,
                    ('(', _) => Token::LeftParen,
                    (')', _) => Token::RightParen,
                    ('{', _) if self.argv0_mode => Token::LeftBrace,
                    ('}', _) if self.argv0_mode || self.brace_param_level > 0 => Token::RightBrace,
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
                    _ => {
                        self.input.unconsume(first);
                        Token::Word(self.visit_word()?)
                    }
                }
            }
        };

        Ok(token)
    }

    fn visit_word(&mut self) -> Result<Word, LexerError> {
        let mut spans = Vec::new();
        let mut plain = String::new();
        let mut in_double_quotes = false;
        let mut in_single_quotes = false;
        let mut quote_hctx = None;
        while let Some(c) = self.input.consume() {
            match c {
                ' ' | '\t' | '\n' | '|' | '&' | ';' | '#' | ')' | '<' | '>'
                    if !in_double_quotes && !in_single_quotes =>
                {
                    self.input.unconsume(c);
                    break;
                }
                '}' if self.brace_param_level > 0 && !in_double_quotes && !in_single_quotes => {
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
                '`' if self.in_backtick => {
                    if !plain.is_empty() {
                        spans.push(Span::Plain(plain));
                        plain = String::new();
                    }

                    // Unconsume to return Token::ClosingBackTick.
                    self.input.unconsume(c);
                    break;
                }
                // The beginning of a command substitution.
                '`' if !in_single_quotes => {
                    if !plain.is_empty() {
                        spans.push(Span::Plain(plain));
                        plain = String::new();
                    }

                    spans.push(self.visit_backtick_exp()?);
                }
                '$' if !in_single_quotes => {
                    if !plain.is_empty() {
                        spans.push(Span::Plain(plain));
                        plain = String::new();
                    }

                    spans.push(self.visit_variable_exp()?);
                }
                // Escaped character.
                '\\' => {
                    let hctx = self.enter_highlight(1 /* len("\\") */);

                    let ch = self.input.consume().unwrap_or('\\' /* backslash at EOF */);
                    plain.push(ch);

                    self.leave_highlight(hctx, HighlightKind::EscSeq, 0);
                }
                _ => {
                    plain.push(c);
                }
            }
        }

        if !plain.is_empty() {
            spans.push(Span::Plain(plain));
        }

        Ok(Word(spans))
    }

    /// Parses a variable expansion (after `$`).
    fn visit_variable_exp(&mut self) -> Result<Span, LexerError> {
        let span = match self.input.consume() {
            // `$(echo foo)`
            Some('(') => {
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

                Span::Command(tokens)
            }
            Some('{') => {
                let hctx = self.enter_highlight(2 /* len("${") */);

                // Read its name.
                let mut name = String::new();
                self.enter_context(Context::BraceParam);
                while let Some(c) = self.input.consume() {
                    if !is_identifier_char(c) {
                        self.input.unconsume(c);
                        break;
                    }
                    name.push(c);
                }

                // TODO:
                self.brace_param_level += 1;
                let _tokens = self.consume_tokens_until(
                    Context::BraceParam,
                    Token::RightBrace,
                    LexerError::NoMatchingRightBrace,
                )?;
                self.brace_param_level -= 1;

                self.leave_highlight(hctx, HighlightKind::Variable { name: name.clone() }, 0);
                Span::Variable { name }
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
                Span::Variable { name }
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

    /// Parses a backtick substitution (after `\``).
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

                                current_line.push(self.visit_variable_exp()?);
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

    /// Skip whitespace characters.
    fn skip_whitespaces(&mut self) {
        while let Some(c) = self.input.consume() {
            if !matches!(c, ' ' | '\t') {
                self.input.unconsume(c);
                break;
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

    fn enter_context(&mut self, context: Context) {
        self.unclosed_context_stack.push(context);
    }

    fn leave_context(&mut self, context: Context) {
        debug_assert_eq!(self.unclosed_context_stack.pop().unwrap(), context);
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

impl<I: Iterator<Item = char>> Iterator for Lexer<I> {
    type Item = Result<Token, LexerError>;

    fn next(&mut self) -> Option<Result<Token, LexerError>> {
        match self.next_token() {
            Ok(token) => Some(Ok(token)),
            Err(LexerError::Eof) => None,
            Err(err) => Some(Err(err)),
        }
    }
}

fn is_identifier_char(c: char) -> bool {
    matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_')
}

fn is_valid_marker_char(c: char) -> bool {
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
