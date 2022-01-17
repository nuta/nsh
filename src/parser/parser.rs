use crate::lexer::Lexer;

pub struct Parser<I> {
    pub lexer: Lexer<I>,
}

impl<I: Iterator<Item = char>> Parser<I> {
    pub fn new(input: I) -> Parser<I> {
        Self {
            lexer: Lexer::new(input),
        }
    }
}
