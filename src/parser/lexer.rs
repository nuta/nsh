use std::ops::Range;

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
    pub fn spans(&self) -> &[Span] {
        &self.0
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Redirection {
    /// `cat < foo.txt` or here document.
    Input,
    /// `cat > foo.txt`.
    Output,
    /// `cat >> foo.txt`.
    Append,
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
    /// `)`
    RightParen,
    /// `}`
    RightBrace,
    /// `\``
    ClosingBackTick,
    /// `echo > foo.log` or `echo >> foo.log`.
    Redirection(Redirection),
    /// A word.
    Word(Word),
}

#[derive(Debug, PartialEq)]
pub enum LexerError {
    /// Indicates the lexer has already returned an error. If you see this,
    /// it's a bug because you should not call `lexer.next()` after an error.
    Halted,
    /// Indicates the lexer has reached the end of the input.
    Eof,
    NoMatchingRightParen,
    NoMatchingRightBrace,
    NoMatchingClosingBackTick,
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

#[derive(Clone, Debug, PartialEq)]
pub enum HighlightKind {
    /// A variable substitution, e.g. `$foo`.
    Variable,
    /// A quoted string (e.g. `"foo"` and `'foo'`).
    QuotedString,
    /// An escaped sequence (e.g. `\"`).
    EscapedSeq,
    /// A command (e.g. "ls" in "ls /var").
    Argv0,
    /// A keyword.
    Keyword,
}

#[derive(Clone, Debug, PartialEq)]
pub struct HighlightSpan {
    pub kind: HighlightKind,
    pub char_range: Range<usize>,
}

struct HighlighterContext {
    kind: HighlightKind,
    char_offset_start: usize,
}

/// A lexer.
///
/// This is NOT await-ready: the iterator should block the thread until the next
/// character gets available. This is because the lexer in async seemed to be
/// unnecessarily complicated.
pub struct Lexer<I: Iterator<Item = char>> {
    halted: bool,
    input: I,
    char_offset: usize,
    push_back_stack: Vec<char>,
    in_backtick: bool,
    unclosed_context_stack: Vec<Context>,
    highlight_spans: Vec<HighlightSpan>,
}

impl<I: Iterator<Item = char>> Lexer<I> {
    pub fn new(input: I) -> Lexer<I> {
        Lexer {
            halted: false,
            input,
            char_offset: 0,
            push_back_stack: Vec::new(),
            in_backtick: false,
            unclosed_context_stack: Vec::new(),
            highlight_spans: Vec::new(),
        }
    }

    pub fn highlight_spans(&self) -> &[HighlightSpan] {
        &self.highlight_spans
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
        // Skip whitespace characters.
        loop {
            let c = self.consume_next_char().ok_or(LexerError::Eof)?;
            if !matches!(c, ' ' | '\t') {
                self.unconsume_char(c);
                break;
            }
        }

        let first = self.consume_next_char().ok_or(LexerError::Eof)?;
        let second = self.peek_next_char();
        let token = match (first, second) {
            ('\n', _) => Token::Newline,
            ('|', Some('|')) => Token::DoubleOr,
            ('|', _) => Token::Or,
            ('&', Some('&')) => Token::DoubleAnd,
            ('&', _) => Token::And,
            (';', Some(';')) => Token::DoubleSemi,
            (';', _) => Token::Semi,
            (')', _) => Token::RightParen,
            ('}', _) => Token::RightBrace,
            // ('<', '<') => Token::LeftBracket,
            // ('>', '>') => Token::LeftBracket,
            // ('<', _) => Token::LeftBracket,
            // ('>', _) => Token::RightBracket,
            // The end of A command substitution.
            ('`', _) if self.in_backtick => Token::ClosingBackTick,
            // Comment.
            ('#', _) => {
                // Skip until the end of the line or EOF.
                loop {
                    // If the comment is in the last line and there's no newline
                    // at EOF, return None from the `?` operator.
                    if self.consume_next_char().ok_or(LexerError::Eof)? == '\n' {
                        break Token::Newline;
                    }
                }
            }
            _ => self.parse_word(first)?,
        };

        Ok(token)
    }

    fn parse_word(&mut self, first_char: char) -> Result<Token, LexerError> {
        let mut c = first_char;
        let mut spans = Vec::new();
        let mut plain = String::new();
        let mut in_double_quotes = false;
        let mut in_single_quotes = false;
        loop {
            match c {
                ' ' | '\t' | '\n' | '|' | '&' | ';' | '#' | '}' | ')' | '<' | '>'
                    if !in_double_quotes && !in_single_quotes =>
                {
                    self.unconsume_char(c);
                    break;
                }
                '"' if in_double_quotes && !in_single_quotes => {
                    in_double_quotes = false;
                    self.leave_context(Context::DoubleQuote);
                }
                '"' if !in_single_quotes => {
                    in_double_quotes = true;
                    self.enter_context(Context::DoubleQuote);
                }
                '\'' if in_single_quotes && !in_double_quotes => {
                    in_single_quotes = false;
                    self.leave_context(Context::SingleQuote);
                }
                '\'' if !in_double_quotes => {
                    in_single_quotes = true;
                    self.enter_context(Context::SingleQuote);
                }
                '`' if self.in_backtick => {
                    // Unconsume to return Token::ClosingBackTick.
                    self.unconsume_char(c);
                    break;
                }
                // The beginning of A command substitution.
                '`' if !in_single_quotes => {
                    if !plain.is_empty() {
                        spans.push(Span::Plain(plain));
                        plain = String::new();
                    }

                    self.in_backtick = true;
                    let tokens = self.consume_tokens_until(
                        Context::BackTick,
                        Token::ClosingBackTick,
                        LexerError::NoMatchingClosingBackTick,
                    )?;
                    self.in_backtick = false;
                    spans.push(Span::Command(tokens));
                }
                '$' if !in_single_quotes => {
                    if !plain.is_empty() {
                        spans.push(Span::Plain(plain));
                        plain = String::new();
                    }

                    spans.push(self.parse_variable_exp()?);
                }
                // Escaped character.
                '\\' => {
                    plain.push(
                        self.consume_next_char()
                            .unwrap_or('\\' /* backslash at EOF */),
                    );
                }
                _ => {
                    plain.push(c);
                }
            }

            c = match self.consume_next_char() {
                Some(c) => c,
                None => break,
            };
        }

        if !plain.is_empty() {
            spans.push(Span::Plain(plain));
        }

        Ok(Token::Word(Word(spans)))
    }

    /// Parses a variable expansion (after `$`).
    fn parse_variable_exp(&mut self) -> Result<Span, LexerError> {
        let span = match self.consume_next_char() {
            // `$(echo foo)`
            Some('(') => {
                let tokens = self.consume_tokens_until(
                    Context::Command,
                    Token::RightParen,
                    LexerError::NoMatchingRightParen,
                )?;
                Span::Command(tokens)
            }
            Some('{') => {
                let hctx = self.enter_highlight(HighlightKind::Variable, 2 /* len("${") */);

                // Read its name.
                let mut name = String::new();
                self.enter_context(Context::BraceParam);
                while let Some(c) = self.consume_next_char() {
                    if !is_identifier_char(c) {
                        self.unconsume_char(c);
                        break;
                    }
                    name.push(c);
                }

                // TODO:
                let _tokens = self.consume_tokens_until(
                    Context::BraceParam,
                    Token::RightBrace,
                    LexerError::NoMatchingRightBrace,
                )?;

                self.leave_highlight(hctx);
                Span::Variable { name }
            }
            // `$foo`
            Some(c) if is_identifier_char(c) => {
                let hctx = self.enter_highlight(HighlightKind::Variable, 2 /* len("$" + c) */);

                let mut name = String::new();
                name.push(c);
                while let Some(c) = self.consume_next_char() {
                    if !is_identifier_char(c) {
                        self.unconsume_char(c);
                        break;
                    }
                    name.push(c);
                }

                self.leave_highlight(hctx);
                Span::Variable { name }
            }
            // Not a variable expansion. Handle it as a plain `$`.
            Some(c) => {
                self.unconsume_char(c);
                Span::Plain("$".to_owned())
            }
            None => {
                // `$` at EOF. Treat it as a plain `$`.
                Span::Plain("$".to_owned())
            }
        };

        Ok(span)
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

    /// Pops a character from the input stream without consuming the character,
    /// that is, the same character will be returned next time `pop` or `peek`
    /// is called.
    fn peek_next_char(&mut self) -> Option<char> {
        if let Some(c) = self.push_back_stack.pop() {
            Some(c)
        } else {
            let c = self.input.next()?;
            self.push_back_stack.push(c);
            Some(c)
        }
    }

    /// Consumes a character from the input stream.
    fn consume_next_char(&mut self) -> Option<char> {
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
    fn unconsume_char(&mut self, c: char) {
        self.push_back_stack.push(c);
        self.char_offset -= 1;
    }

    fn enter_context(&mut self, context: Context) {
        self.unclosed_context_stack.push(context);
    }

    fn leave_context(&mut self, context: Context) {
        debug_assert_eq!(self.unclosed_context_stack.pop().unwrap(), context);
    }

    fn enter_highlight(&mut self, kind: HighlightKind, diff: usize) -> HighlighterContext {
        HighlighterContext {
            kind,
            char_offset_start: self.char_offset - diff,
        }
    }

    fn leave_highlight(&mut self, hctx: HighlighterContext) {
        self.highlight_spans.push(HighlightSpan {
            kind: hctx.kind,
            char_range: hctx.char_offset_start..self.char_offset,
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

#[cfg(test)]
mod tests {
    use super::*;

    fn string(s: &str) -> String {
        s.to_owned()
    }

    fn plain_span(s: &str) -> Span {
        Span::Plain(string(s))
    }

    fn word(spans: Vec<Span>) -> Token {
        Token::Word(Word(spans))
    }

    fn single_plain_word(s: &str) -> Token {
        word(vec![plain_span(s)])
    }

    fn lex(input: &str) -> Result<Vec<Token>, LexerError> {
        let mut lexer = Lexer::new(input.chars());
        let mut tokens = Vec::new();
        loop {
            match lexer.next() {
                Some(Ok(token)) => tokens.push(token),
                Some(Err(err)) => return Err(err),
                None => break,
            }
        }
        Ok(tokens)
    }

    #[test]
    fn simple_command() {
        let input = "echo hello";
        assert_eq!(
            lex(input),
            Ok(vec![single_plain_word("echo"), single_plain_word("hello")])
        );

        let input = "echo | cat|grep foo";
        assert_eq!(
            lex(input),
            Ok(vec![
                single_plain_word("echo"),
                Token::Or,
                single_plain_word("cat"),
                Token::Or,
                single_plain_word("grep"),
                single_plain_word("foo")
            ])
        );
    }

    #[test]
    fn command_substituion() {
        let input = "echo $(ls /)";
        assert_eq!(
            lex(input),
            Ok(vec![
                single_plain_word("echo"),
                word(vec![Span::Command(vec![
                    single_plain_word("ls"),
                    single_plain_word("/")
                ])])
            ])
        );

        let input = "echo $(grep $(ls /foo*) bar)";
        assert_eq!(
            lex(input),
            Ok(vec![
                single_plain_word("echo"),
                word(vec![Span::Command(vec![
                    single_plain_word("grep"),
                    word(vec![Span::Command(vec![
                        single_plain_word("ls"),
                        single_plain_word("/foo*"),
                    ])]),
                    single_plain_word("bar"),
                ])])
            ])
        );
    }

    #[test]
    fn variable_expansion() {
        let input = "echo a${b}c";
        assert_eq!(
            lex(input),
            Ok(vec![
                single_plain_word("echo"),
                word(vec![
                    plain_span("a"),
                    Span::Variable { name: string("b") },
                    plain_span("c"),
                ])
            ])
        );
    }

    #[test]
    fn double_quotes() {
        let input = "echo \"a b c\"";
        assert_eq!(
            lex(input),
            Ok(vec![single_plain_word("echo"), single_plain_word("a b c"),])
        );

        let input = "echo \"a\\\"b\"";
        assert_eq!(
            lex(input),
            Ok(vec![single_plain_word("echo"), single_plain_word("a\"b"),])
        );

        let input = "echo X\"a b c\"X";
        assert_eq!(
            lex(input),
            Ok(vec![
                single_plain_word("echo"),
                single_plain_word("Xa b cX"),
            ])
        );

        let input = "echo \"a $b c\"";
        assert_eq!(
            lex(input),
            Ok(vec![
                single_plain_word("echo"),
                word(vec![
                    plain_span("a "),
                    Span::Variable { name: string("b") },
                    plain_span(" c"),
                ])
            ])
        );

        let input = "echo \"a b\" c \"d e\"";
        assert_eq!(
            lex(input),
            Ok(vec![
                single_plain_word("echo"),
                single_plain_word("a b"),
                single_plain_word("c"),
                single_plain_word("d e"),
            ])
        );
    }

    #[test]
    fn single_quotes() {
        let input = "echo 'a b c'";
        assert_eq!(
            lex(input),
            Ok(vec![single_plain_word("echo"), single_plain_word("a b c"),])
        );

        let input = "echo 'a\\'b'";
        assert_eq!(
            lex(input),
            Ok(vec![single_plain_word("echo"), single_plain_word("a'b"),])
        );

        let input = "echo X'a b c'X";
        assert_eq!(
            lex(input),
            Ok(vec![
                single_plain_word("echo"),
                single_plain_word("Xa b cX"),
            ])
        );

        let input = "echo 'a $b c'";
        assert_eq!(
            lex(input),
            Ok(vec![single_plain_word("echo"), single_plain_word("a $b c"),])
        );

        let input = "echo 'a b' c 'd e'";
        assert_eq!(
            lex(input),
            Ok(vec![
                single_plain_word("echo"),
                single_plain_word("a b"),
                single_plain_word("c"),
                single_plain_word("d e"),
            ])
        );
    }

    #[test]
    fn highlighting() {
        let input = "$foo 123 $bar";
        let mut lexer = Lexer::new(input.chars());
        while lexer.next().is_some() {}

        assert_eq!(
            lexer.highlight_spans(),
            vec![
                HighlightSpan {
                    char_range: 0..4,
                    kind: HighlightKind::Variable,
                },
                HighlightSpan {
                    char_range: 9..13,
                    kind: HighlightKind::Variable,
                },
            ]
        );
    }
}
