use std::pin::Pin;

use futures::{Stream, StreamExt};

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
    /// A word.
    Word(Vec<Span>),
}

pub struct Lexer {
    input: Pin<Box<dyn Stream<Item = char>>>,
    push_back_stack: Vec<char>,
}

impl Lexer {
    pub fn new(input: impl Stream<Item = char> + 'static) -> Lexer {
        Lexer {
            input: Box::pin(input),
            push_back_stack: Vec::new(),
        }
    }

    #[cfg(test)]
    pub async fn tokenize(input: &'static str) -> Vec<Token> {
        Lexer::new(tokio_stream::iter(input.chars()))
            .into_stream()
            .collect::<Vec<Token>>()
            .await
    }

    pub fn into_stream(mut self) -> impl Stream<Item = Token> {
        async_stream::stream! {
            while let Some(token) = self.next().await {
                yield token;
            }
        }
    }

    /// Returns the next token, just like `Iterator::next()`.
    async fn next(&mut self) -> Option<Token> {
        // Skip whitespace characters.
        loop {
            let c = self.pop().await?;
            if !matches!(c, ' ' | '\t') {
                self.push_back(c);
                break;
            }
        }

        let first = self.pop().await?;
        let second = self.peek().await;
        let token = match (first, second) {
            ('\n', _) => Token::Newline,
            ('|', Some('|')) => Token::DoubleOr,
            ('|', _) => Token::Or,
            ('&', Some('&')) => Token::DoubleAnd,
            ('&', _) => Token::And,
            (';', Some(';')) => Token::DoubleSemi,
            (';', _) => Token::Semi,
            // Comment.
            ('#', _) => {
                // Skip until the end of the line or EOF.
                loop {
                    // If the comment is in the last line and there's no newline
                    // at EOF, return None from the `?` operator.
                    let c = self.pop().await?;
                    if c == '\n' {
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
                        // Escaped character.
                        '\\' => {
                            plain.push(self.pop().await.unwrap_or('\\' /* backslash at EOF */));
                        }
                        '$' => {
                            if !plain.is_empty() {
                                spans.push(Span::Plain(plain));
                                plain = String::new();
                            }

                            spans.push(self.parse_variable_exp().await);
                        }
                        _ => {
                            plain.push(c);
                        }
                    }

                    c = match self.pop().await {
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

        Some(token)
    }

    /// Parse a variable expansion (after `$`).
    async fn parse_variable_exp(&mut self) -> Span {
        match self.pop().await {
            // `$foo`
            Some(c) if is_identifier_char(c) => {
                let mut plain = String::new();
                plain.push(c);
                while let Some(c) = self.pop().await {
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
        }
    }

    /// Pops a character from the input stream without consuming the character,
    /// that is, the same character will be returned next time `pop` or `peek`
    /// is called.
    async fn peek(&mut self) -> Option<char> {
        if let Some(c) = self.push_back_stack.pop() {
            Some(c)
        } else {
            let c = self.input.next().await?;
            self.push_back(c);
            Some(c)
        }
    }

    /// Consumes a character from the input stream.
    async fn pop(&mut self) -> Option<char> {
        if let Some(c) = self.push_back_stack.pop() {
            Some(c)
        } else {
            self.input.next().await
        }
    }

    /// Pushes a character back to the input stream. The character will be
    /// returned next time `pop` or `peek` is called.
    fn push_back(&mut self, c: char) {
        self.push_back_stack.push(c);
    }
}

fn is_identifier_char(c: char) -> bool {
    matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_')
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn simple_command() {
        let input = "if true then";
        assert_eq!(Lexer::tokenize(input).await, vec![Token::Newline]);
    }
}
