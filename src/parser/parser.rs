use thiserror::Error;

use crate::ast::*;
use crate::lexer::{Lexer, LexerError};

#[derive(Error, Debug, PartialEq)]
pub enum ParseError {
    #[error("{0}")]
    LexerError(LexerError),
}

impl Into<ParseError> for LexerError {
    fn into(self) -> ParseError {
        ParseError::LexerError(self)
    }
}

pub struct Parser {
    pub lexer: Lexer,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        Self { lexer }
    }

    pub fn parse(self, text: &str) -> Result<Ast, ParseError> {
        todo!()
    }
}
