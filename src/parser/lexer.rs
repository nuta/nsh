use std::pin::Pin;

use futures::{Stream, StreamExt};

/// A fragment of a word.
#[derive(Debug, PartialEq)]
pub enum Span {
    /// A plain text.
    Plain(String),
    /// A command substitution, e.g. `$(echo "hi")`.
    Command(Vec<Token>),
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Newline,
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

    pub async fn next(&mut self) -> Option<Token> {
        // Skip whitespace characters.
        loop {
            let c = self.pop().await?;
            if !matches!(c, ' ' | '\t') {
                self.push_back(c);
                break;
            }
        }

        let token = match self.pop().await? {
            '\n' => Token::Newline,
            // Word.
            mut c => {
                let mut spans = Vec::new();
                let mut plain = String::new();
                loop {
                    match c {
                        ' ' | '\t' | '\n' | '|' | '&' | ';' | '#' => {
                            self.push_back(c);
                            break;
                        }
                        '$' => {
                            if !plain.is_empty() {
                                spans.push(Span::Plain(plain));
                                plain = String::new();
                            }

                            spans.push(self.parse_variable_exp().await?);
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
    async fn parse_variable_exp(&mut self) -> Option<Span> {
        None
    }

    async fn peek(&mut self) -> Option<char> {
        if let Some(c) = self.push_back_stack.pop() {
            Some(c)
        } else {
            let c = self.input.next().await?;
            self.push_back(c);
            Some(c)
        }
    }

    async fn pop(&mut self) -> Option<char> {
        if let Some(c) = self.push_back_stack.pop() {
            Some(c)
        } else {
            self.input.next().await
        }
    }

    fn push_back(&mut self, c: char) {
        self.push_back_stack.push(c);
    }
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
