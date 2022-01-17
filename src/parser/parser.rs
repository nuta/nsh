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

impl Into<ParseError> for LexerError {
    fn into(self) -> ParseError {
        ParseError::LexerError(self)
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
        let mut terms = Vec::new();

        terms.push(self.parse_term()?);
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
        let mut pipelines = Vec::new();

        pipelines.push(self.parse_pipeline()?);
        while let Some(token) = self.peek_token()? {
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

        let background = match self.peek_token()? {
            Some(Token::And) => true,
            _ => false,
        };

        Ok(Term {
            pipelines,
            background,
        })
    }

    fn parse_pipeline(&mut self) -> Result<Pipeline, ParseError> {
        self.lexer.set_argv0_mode(true);

        let run_if = match self.peek_token()? {
            None | Some(Token::Semi) => {
                self.consume_token()?;
                RunIf::Always
            }
            Some(Token::DoubleAnd) => {
                self.consume_token()?;
                RunIf::Success
            }
            Some(Token::DoubleOr) => {
                self.consume_token()?;
                RunIf::Failure
            }
            _ => RunIf::Always,
        };

        let mut commands = Vec::new();
        commands.push(self.parse_command()?);
        while let Some(token) = self.consume_token()? {
            match token {
                Token::Or => {
                    commands.push(self.parse_command()?);
                }
                _ => {
                    break;
                }
            }
        }

        return Ok(Pipeline { run_if, commands });
    }

    fn parse_command(&mut self) -> Result<Command, ParseError> {
        let command = match self.peek_token()? {
            _ => self.parse_simple_command()?,
        };

        Ok(command)
    }

    fn parse_simple_command(&mut self) -> Result<Command, ParseError> {
        let mut assignments = Vec::new();
        let mut argv = Vec::new();
        let mut redirections = Vec::new();

        self.lexer.set_argv0_mode(true);

        while let Some(token) = self.consume_token()? {
            match token {
                Token::Assignment(Assignment { name, initializer }) => {
                    assignments.push(Assignment { name, initializer });
                }
                Token::Argv0(word) => {
                    debug_assert!(argv.is_empty());
                    argv.push(word);
                    self.lexer.set_argv0_mode(false);
                }
                Token::Word(word) => {
                    debug_assert!(!argv.is_empty());
                    argv.push(word)
                }
                Token::Redirection(r) => redirections.push(r),
                _ => break,
            }
        }

        self.lexer.set_argv0_mode(false);

        if argv.is_empty() {
            return Err(ParseError::Eof);
        }

        Ok(Command::SimpleCommand {
            assignments,
            argv,
            redirections,
        })
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
            Some(Err(err)) => return Err(err.into()),
            None => Ok(None),
        }
    }
}
