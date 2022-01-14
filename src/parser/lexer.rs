/// A fragment of a word.
#[derive(Debug, PartialEq)]
pub enum Span {
    /// A plain text.
    Plain(String),
    /// A variable substitution, e.g. `$foo`.
    Variable(String),
    /// A command substitution, e.g. `$(echo "hi")`.
    Command(Vec<Token>),
}

#[derive(Debug, PartialEq)]
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
    /// ```
    ClosingBackTick,
    /// A word.
    Word(Vec<Span>),
}

#[derive(Debug, PartialEq)]
pub enum LexerError {
    Eof,
    NoMatchingRightParen,
    NoMatchingClosingBackTick,
}

pub struct Lexer<I: Iterator<Item = char>> {
    input: I,
    push_back_stack: Vec<char>,
    in_backtick: bool,
}

impl<I: Iterator<Item = char>> Lexer<I> {
    pub fn new(input: I) -> Lexer<I> {
        Lexer {
            input,
            push_back_stack: Vec::new(),
            in_backtick: false,
        }
    }

    /// Returns the next token.
    fn next_token(&mut self) -> Result<Token, LexerError> {
        // Skip whitespace characters.
        loop {
            let c = self.pop().ok_or(LexerError::Eof)?;
            if !matches!(c, ' ' | '\t') {
                self.push_back(c);
                break;
            }
        }

        let first = self.pop().ok_or(LexerError::Eof)?;
        let second = self.peek();
        let token = match (first, second) {
            ('\n', _) => Token::Newline,
            ('|', Some('|')) => Token::DoubleOr,
            ('|', _) => Token::Or,
            ('&', Some('&')) => Token::DoubleAnd,
            ('&', _) => Token::And,
            (';', Some(';')) => Token::DoubleSemi,
            (';', _) => Token::Semi,
            (')', _) => Token::RightParen,
            // The end of A command substitution.
            ('`', _) if self.in_backtick => Token::ClosingBackTick,
            // Comment.
            ('#', _) => {
                // Skip until the end of the line or EOF.
                loop {
                    // If the comment is in the last line and there's no newline
                    // at EOF, return None from the `?` operator.
                    if self.pop().ok_or(LexerError::Eof)? == '\n' {
                        break Token::Newline;
                    }
                }
            }
            // Word.
            _ => {
                let mut c = first;
                let mut spans = Vec::new();
                let mut plain = String::new();
                loop {
                    match c {
                        ' ' | '\t' | '\n' | '|' | '&' | ';' | '#' => {
                            self.push_back(c);
                            break;
                        }
                        '`' if self.in_backtick => {
                            self.push_back(c);
                            break;
                        }
                        // The beginning of A command substitution.
                        '`' => {
                            if !plain.is_empty() {
                                spans.push(Span::Plain(plain));
                                plain = String::new();
                            }

                            self.in_backtick = true;
                            let tokens = self.consume_tokens_until(
                                Token::ClosingBackTick,
                                LexerError::NoMatchingClosingBackTick,
                            )?;
                            self.in_backtick = false;
                            spans.push(Span::Command(tokens));
                        }
                        '$' => {
                            if !plain.is_empty() {
                                spans.push(Span::Plain(plain));
                                plain = String::new();
                            }

                            spans.push(self.parse_variable_exp()?);
                        }
                        // Escaped character.
                        '\\' => {
                            plain.push(self.pop().unwrap_or('\\' /* backslash at EOF */));
                        }
                        _ => {
                            plain.push(c);
                        }
                    }

                    c = match self.pop() {
                        Some(c) => c,
                        None => break,
                    };
                }

                if !plain.is_empty() {
                    spans.push(Span::Plain(plain));
                    plain = String::new();
                }
                Token::Word(spans)
            }
        };

        Ok(token)
    }

    /// Parses a variable expansion (after `$`).
    fn parse_variable_exp(&mut self) -> Result<Span, LexerError> {
        let span = match self.pop() {
            // `$(echo foo)`
            Some('(') => Span::Command(
                self.consume_tokens_until(Token::RightParen, LexerError::NoMatchingRightParen)?,
            ),
            // `$foo`
            Some(c) if is_identifier_char(c) => {
                let mut plain = String::new();
                plain.push(c);
                while let Some(c) = self.pop() {
                    if !is_identifier_char(c) {
                        self.push_back(c);
                        break;
                    }
                    plain.push(c);
                }
                Span::Variable(plain)
            }
            // Not a variable expansion. Handle it as a plain `$`.
            Some(c) => {
                self.push_back(c);
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
        end_marker: Token,
        err_on_eof: LexerError,
    ) -> Result<Vec<Token>, LexerError> {
        let mut tokens = Vec::new();
        loop {
            let token = match self.next_token() {
                Ok(token) => token,
                Err(LexerError::Eof) => return Err(err_on_eof),
                Err(err) => return Err(err),
            };

            if token == end_marker {
                break;
            }
        }

        Ok(tokens)
    }

    /// Pops a character from the input stream without consuming the character,
    /// that is, the same character will be returned next time `pop` or `peek`
    /// is called.
    fn peek(&mut self) -> Option<char> {
        if let Some(c) = self.push_back_stack.pop() {
            Some(c)
        } else {
            let c = self.input.next()?;
            self.push_back(c);
            Some(c)
        }
    }

    /// Consumes a character from the input stream.
    fn pop(&mut self) -> Option<char> {
        if let Some(c) = self.push_back_stack.pop() {
            Some(c)
        } else {
            self.input.next()
        }
    }

    /// Pushes a character back to the input stream. The character will be
    /// returned next time `pop` or `peek` is called.
    fn push_back(&mut self, c: char) {
        self.push_back_stack.push(c);
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
        let input = "if true then";
        assert_eq!(lex(input), Ok(vec![Token::Newline]));
    }
}
