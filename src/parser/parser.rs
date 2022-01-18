use thiserror::Error;

use crate::ast::*;
use crate::lexer::{Lexer, LexerError};

#[derive(Error, Debug, PartialEq)]
pub enum ParseError {
    #[error("{0}")]
    LexerError(LexerError),
    #[error("Unexpected token near {0:?}.")]
    UnexpectedTokenNear(Token),
    #[error("Expected {0}.")]
    Expected(&'static str),
}

enum EmptyOrError {
    Empty,
    Error(ParseError),
}

impl From<LexerError> for ParseError {
    fn from(err: LexerError) -> ParseError {
        ParseError::LexerError(err)
    }
}

impl From<ParseError> for EmptyOrError {
    fn from(err: ParseError) -> EmptyOrError {
        EmptyOrError::Error(err)
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
        let terms = match self.parse_terms() {
            // Check if the lexer still have more tokens.
            Ok(terms) => match self.peek_token() {
                Ok(Some(token)) => {
                    // Nobody consumed the token. Handle it as a syntax error.
                    return Err(ParseError::UnexpectedTokenNear(token.clone()));
                }
                _ => terms,
            },
            Err(EmptyOrError::Empty) => vec![],
            Err(EmptyOrError::Error(err)) => return Err(err),
        };

        Ok(Ast { terms })
    }

    fn parse_terms(&mut self) -> Result<Vec<Term>, EmptyOrError> {
        // Skip empty lines.
        self.skip_term_separators()?;

        let mut terms = vec![self.parse_term()?];
        while let Some(token) = self.peek_token_maybe_argv0()? {
            match token {
                Token::Semi | Token::And | Token::Newline => {
                    self.consume_token()?;

                    // Skip empty lines.
                    self.skip_term_separators()?;

                    terms.push(match self.parse_term() {
                        Ok(term) => term,
                        Err(EmptyOrError::Empty) => break,
                        Err(err) => return Err(err),
                    });
                }
                _ => {
                    break;
                }
            }
        }

        Ok(terms)
    }

    fn skip_term_separators(&mut self) -> Result<(), EmptyOrError> {
        loop {
            if matches!(
                self.peek_token_maybe_argv0()?,
                Some(Token::Semi) | Some(Token::And) | Some(Token::Newline)
            ) {
                self.consume_token_maybe_argv0()?;
                continue;
            }

            break;
        }

        Ok(())
    }

    fn parse_term(&mut self) -> Result<Term, EmptyOrError> {
        let mut pipelines = vec![self.parse_pipeline(RunIf::Always)?];
        while let Some(token) = self.peek_token_maybe_argv0()? {
            match token {
                Token::DoubleAnd | Token::DoubleOr => {
                    let run_if = match self.consume_token().unwrap().unwrap() {
                        Token::DoubleAnd => RunIf::Success,
                        Token::DoubleOr => RunIf::Failure,
                        _ => unreachable!(),
                    };

                    pipelines.push(match self.parse_pipeline(run_if) {
                        Ok(pipeline) => pipeline,
                        Err(EmptyOrError::Empty) => break,
                        Err(err) => return Err(err),
                    });
                }
                _ => {
                    break;
                }
            }
        }

        let background = matches!(self.peek_token_maybe_argv0()?, Some(Token::And));
        Ok(Term {
            pipelines,
            background,
        })
    }

    fn parse_pipeline(&mut self, run_if: RunIf) -> Result<Pipeline, EmptyOrError> {
        let mut commands = vec![self.parse_command()?];
        while let Some(token) = self.peek_token_maybe_argv0()? {
            match token {
                Token::Or => {
                    self.consume_token()?;
                    commands.push(match self.parse_command() {
                        Ok(command) => command,
                        Err(EmptyOrError::Empty) => break,
                        Err(err) => return Err(err),
                    });
                }
                _ => {
                    break;
                }
            }
        }

        Ok(Pipeline { run_if, commands })
    }

    fn parse_command(&mut self) -> Result<Command, EmptyOrError> {
        let token = match self.peek_token_maybe_argv0()? {
            Some(token) => token,
            _ => return Err(EmptyOrError::Empty),
        };

        let command = match token {
            Token::Argv0(_) | Token::Assignment(_) | Token::Redirection(_) => {
                self.parse_simple_command()?
            }
            _ => return Err(EmptyOrError::Empty),
        };

        Ok(command)
    }

    fn parse_simple_command(&mut self) -> Result<Command, EmptyOrError> {
        let mut assignments = Vec::new();
        let mut argv = Vec::new();
        let mut redirections = Vec::new();

        // Read until argv0.
        while let Some(token) = self.peek_token_maybe_argv0()?.clone() {
            match token {
                Token::Assignment(Assignment { name, initializer }) => {
                    self.consume_token()?;
                    assignments.push(Assignment { name, initializer });
                }
                Token::Argv0(word) => {
                    self.consume_token()?;
                    debug_assert!(argv.is_empty());
                    argv.push(word);
                    break;
                }
                Token::Word(_) => {
                    unreachable!();
                }
                Token::Redirection(r) => {
                    self.consume_token()?;
                    redirections.push(r);
                }
                _ => {
                    break;
                }
            }
        }

        while let Some(token) = self.peek_token()?.clone() {
            match token {
                Token::Assignment(_) | Token::Argv0(_) => {
                    unreachable!();
                }
                Token::Word(word) => {
                    debug_assert!(!argv.is_empty());
                    self.consume_token()?;
                    argv.push(word);
                }
                Token::Redirection(r) => {
                    self.consume_token()?;
                    redirections.push(r);
                }
                _ => {
                    dbg!("out of command");
                    break;
                }
            }
        }

        if assignments.is_empty() && argv.is_empty() {
            return Err(EmptyOrError::Empty);
        }

        Ok(Command::SimpleCommand {
            assignments,
            argv,
            redirections,
        })
    }

    fn peek_token_maybe_argv0(&mut self) -> Result<&Option<Token>, ParseError> {
        self.lexer.set_argv0_mode(true);
        self.do_peek_token()
    }

    fn consume_token_maybe_argv0(&mut self) -> Result<Option<Token>, ParseError> {
        self.lexer.set_argv0_mode(true);
        let token = self.consume_token();
        token
    }

    fn peek_token(&mut self) -> Result<&Option<Token>, ParseError> {
        self.lexer.set_argv0_mode(false);
        self.do_peek_token()
    }

    fn consume_token(&mut self) -> Result<Option<Token>, ParseError> {
        self.lexer.set_argv0_mode(false);
        self.do_consume_token()
    }

    fn do_peek_token(&mut self) -> Result<&Option<Token>, ParseError> {
        if self.peeked_token.is_none() {
            self.peeked_token = match self.lexer.next() {
                Some(Ok(token)) => Some(Some(token)),
                Some(Err(err)) => return Err(err.into()),
                None => Some(None),
            };
        }

        Ok(self.peeked_token.as_ref().unwrap())
    }

    fn do_consume_token(&mut self) -> Result<Option<Token>, ParseError> {
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
