use thiserror::Error;

use crate::ast::*;
use crate::lexer::{Lexer, LexerError};

#[derive(Error, Debug, PartialEq)]
pub enum ParseError {
    #[error("{0}")]
    LexerError(LexerError),
    #[error("Expected {0}.")]
    Expected(&'static str),
    #[error("EOF (BUG: this is internally used).")]
    Eof,
}

impl From<LexerError> for ParseError {
    fn from(err: LexerError) -> ParseError {
        ParseError::LexerError(err)
    }
}

pub struct Parser {
    pub lexer: Lexer,
    peeked_token: Option<Option<Token>>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        Parser {
            lexer,
            peeked_token: None,
        }
    }

    pub fn parse(mut self) -> Result<Ast, ParseError> {
        let terms = self.parse_terms()?;
        Ok(Ast { terms })
    }

    fn parse_terms(&mut self) -> Result<Vec<Term>, ParseError> {
        let mut terms = vec![self.parse_term()?];
        loop {
            let term = match self.parse_term() {
                Ok(term) => term,
                Err(ParseError::Eof) => break,
                Err(err) => return Err(err),
            };

            terms.push(term);
        }

        Ok(terms)
    }

    fn parse_term(&mut self) -> Result<Term, ParseError> {
        let mut pipelines = vec![self.parse_pipeline()?];
        while let Some(token) = self.peek_token_maybe_argv0()? {
            match token {
                Token::Semi | Token::And => {
                    self.consume_token()?;
                    break;
                }
                _ => {
                    pipelines.push(self.parse_pipeline()?);
                }
            }
        }

        let background = matches!(self.peek_token_maybe_argv0()?, Some(Token::And));
        Ok(Term {
            pipelines,
            background,
        })
    }

    fn parse_pipeline(&mut self) -> Result<Pipeline, ParseError> {
        let run_if = match self.peek_token_maybe_argv0()? {
            None | Some(Token::Semi) => {
                self.consume_token_maybe_argv0()?;
                RunIf::Always
            }
            Some(Token::DoubleAnd) => {
                self.consume_token_maybe_argv0()?;
                RunIf::Success
            }
            Some(Token::DoubleOr) => {
                self.consume_token_maybe_argv0()?;
                RunIf::Failure
            }
            _ => RunIf::Always,
        };

        let mut commands = vec![self.parse_command()?];
        while let Some(token) = self.peek_token_maybe_argv0()? {
            match token {
                Token::Or => {
                    self.consume_token()?;
                    commands.push(self.parse_command()?);
                }
                _ => {
                    break;
                }
            }
        }

        Ok(Pipeline { run_if, commands })
    }

    fn parse_command(&mut self) -> Result<Command, ParseError> {
        let command = match self.peek_token_maybe_argv0()? {
            _ => self.parse_simple_command()?,
        };

        Ok(command)
    }

    fn parse_simple_command(&mut self) -> Result<Command, ParseError> {
        let mut assignments = Vec::new();
        let mut argv = Vec::new();
        let mut redirections = Vec::new();

        // Read until argv0.
        while let Some(token) = self.consume_token_maybe_argv0()? {
            match token {
                Token::Assignment(Assignment { name, initializer }) => {
                    assignments.push(Assignment { name, initializer });
                }
                Token::Argv0(word) => {
                    debug_assert!(argv.is_empty());
                    argv.push(word);
                    break;
                }
                Token::Word(_) => {
                    unreachable!();
                }
                Token::Redirection(r) => redirections.push(r),
                _ => break,
            }
        }

        while let Some(token) = self.consume_token()? {
            match token {
                Token::Assignment(_) | Token::Argv0(_) => {
                    unreachable!();
                }
                Token::Word(word) => {
                    debug_assert!(!argv.is_empty());
                    argv.push(word);
                    break;
                }
                Token::Redirection(r) => redirections.push(r),
                _ => break,
            }
        }

        if argv.is_empty() {
            return Err(ParseError::Eof);
        }

        Ok(Command::SimpleCommand {
            assignments,
            argv,
            redirections,
        })
    }

    fn peek_token_maybe_argv0(&mut self) -> Result<&Option<Token>, ParseError> {
        self.lexer.set_argv0_mode(true);

        // We don't need to restore argv0 mode: we'll do so when we consume it.
        self.peek_token()
    }

    fn consume_token_maybe_argv0(&mut self) -> Result<Option<Token>, ParseError> {
        self.lexer.set_argv0_mode(true);
        let token = self.consume_token();
        self.lexer.set_argv0_mode(false);
        token
    }

    fn peek_token(&mut self) -> Result<&Option<Token>, ParseError> {
        if self.peeked_token.is_none() {
            self.peeked_token = match self.lexer.next() {
                Some(Ok(token)) => Some(Some(token)),
                Some(Err(err)) => return Err(err.into()),
                None => Some(None),
            };
        }

        Ok(self.peeked_token.as_ref().unwrap())
    }

    fn consume_token(&mut self) -> Result<Option<Token>, ParseError> {
        if let Some(token) = self.peeked_token.take() {
            return Ok(token);
        }

        match self.lexer.next() {
            Some(Ok(token)) => Ok(Some(token)),
            Some(Err(err)) => Err(err.into()),
            None => Ok(None),
        }
    }
}
