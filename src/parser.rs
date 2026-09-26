use std::ops::Range;

use crate::lexer::{LexError, Lexer, Op, TokenKind};

#[derive(Debug, PartialEq, Eq)]
pub struct Word<'a> {
    pub raw: &'a [u8],
    pub span: Range<usize>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct SimpleCommand<'a> {
    pub words: Vec<Word<'a>>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParseError {
    Incomplete(LexError),
    UnexpectedOperator { operator: Op, offset: usize },
}

/// Reads one complete simple command at a time from the lexer.
pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a [u8]) -> Self {
        Self {
            lexer: Lexer::new(input),
        }
    }

    pub fn next_command(&mut self) -> Result<Option<SimpleCommand<'a>>, ParseError> {
        let mut words = Vec::new();

        // Consume words until a command separator or the end of input.
        loop {
            let token = match self.lexer.next_token() {
                Ok(token) => token,
                Err(error) => return Err(ParseError::Incomplete(error)),
            };
            let Some(token) = token else {
                break;
            };

            match token.kind {
                TokenKind::Word => words.push(Word {
                    raw: token.raw,
                    span: token.span,
                }),
                TokenKind::Newline => {
                    if !words.is_empty() {
                        break;
                    }
                }
                TokenKind::Op(Op::Semi) if !words.is_empty() => break,
                TokenKind::Op(operator) => {
                    return Err(ParseError::UnexpectedOperator {
                        operator,
                        offset: token.span.start,
                    });
                }
            }
        }

        if words.is_empty() {
            Ok(None)
        } else {
            Ok(Some(SimpleCommand { words }))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{ParseError, Parser};
    use crate::lexer::Op;

    #[test]
    fn parses_commands_one_at_a_time() {
        let mut parser = Parser::new(b"echo hello; echo world\n");
        let first = parser.next_command().unwrap().unwrap();
        assert_eq!(first.words[0].raw, b"echo");
        assert_eq!(first.words[1].raw, b"hello");

        let second = parser.next_command().unwrap().unwrap();
        assert_eq!(second.words[0].raw, b"echo");
        assert_eq!(second.words[1].raw, b"world");
        assert_eq!(parser.next_command(), Ok(None));
    }

    #[test]
    fn rejects_unsupported_operators() {
        let mut parser = Parser::new(b"echo hello | cat");
        assert_eq!(
            parser.next_command(),
            Err(ParseError::UnexpectedOperator {
                operator: Op::Pipe,
                offset: 11,
            })
        );
    }
}
