use std::ops::Range;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Op {
    /// `;`
    Semicolon,
    /// `;;`
    DoubleSemicolon,
    /// `&`
    Ampersand,
    /// `&&`
    AndIf,
    /// `|`
    Pipe,
    /// `||`
    OrIf,
    /// `(`
    LeftParen,
    /// `)`
    RightParen,
    /// `<`
    Less,
    /// `>`
    Greater,
    /// `<<`
    DoubleLess,
    /// `>>`
    DoubleGreater,
    /// `<&`
    LessAnd,
    /// `>&`
    GreaterAnd,
    /// `<>`
    LessGreater,
    /// `<<-`
    DoubleLessDash,
    /// `>|`
    Clobber,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
    Word,
    Newline,
    Op(Op),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub raw: &'a [u8],
    pub span: Range<usize>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IncompleteKind {
    Quote,
    Escape,
    Substitution,
    ParameterExpansion,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct LexError {
    pub kind: IncompleteKind,
    pub offset: usize,
}

/// Pulls raw shell tokens from a byte slice without performing expansion.
pub struct Lexer<'a> {
    input: &'a [u8],
    offset: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a [u8]) -> Self {
        Self { input, offset: 0 }
    }

    pub fn next_token(&mut self) -> Result<Option<Token<'a>>, LexError> {
        // Skip blanks while preserving newlines as command separators.
        while self.offset < self.input.len()
            && (self.input[self.offset] == b' ' || self.input[self.offset] == b'\t')
        {
            self.offset += 1;
        }

        if self.offset == self.input.len() {
            return Ok(None);
        }

        let start = self.offset;
        let kind = if self.input[self.offset] == b'\n' {
            self.offset += 1;
            TokenKind::Newline
        } else if let Some(operator) = self.scan_operator() {
            TokenKind::Op(operator)
        } else {
            self.scan_word()?;
            TokenKind::Word
        };

        Ok(Some(Token {
            kind,
            raw: &self.input[start..self.offset],
            span: start..self.offset,
        }))
    }

    fn scan_operator(&mut self) -> Option<Op> {
        // Match the longest POSIX operator at the current byte.
        let rest = &self.input[self.offset..];
        let operators: &[(&[u8], Op)] = &[
            (b"<<-", Op::DoubleLessDash),
            (b"&&", Op::AndIf),
            (b"||", Op::OrIf),
            (b";;", Op::DoubleSemicolon),
            (b"<<", Op::DoubleLess),
            (b">>", Op::DoubleGreater),
            (b"<&", Op::LessAnd),
            (b">&", Op::GreaterAnd),
            (b"<>", Op::LessGreater),
            (b">|", Op::Clobber),
            (b";", Op::Semicolon),
            (b"&", Op::Ampersand),
            (b"|", Op::Pipe),
            (b"(", Op::LeftParen),
            (b")", Op::RightParen),
            (b"<", Op::Less),
            (b">", Op::Greater),
        ];

        for (spelling, operator) in operators {
            if rest.starts_with(spelling) {
                self.offset += spelling.len();
                return Some(*operator);
            }
        }

        None
    }

    fn scan_word(&mut self) -> Result<(), LexError> {
        // A word ends only at an unquoted blank, newline, or operator.
        while self.offset < self.input.len() {
            let byte = self.input[self.offset];
            if byte == b' ' || byte == b'\t' || byte == b'\n' || is_operator_byte(byte) {
                break;
            }

            if byte == b'\'' || byte == b'"' {
                self.scan_quote(byte)?;
            } else if byte == b'\\' {
                self.scan_escape()?;
            } else if self.starts_with(b"$(") {
                self.scan_substitution()?;
            } else if self.starts_with(b"${") {
                self.scan_parameter_expansion()?;
            } else {
                self.offset += 1;
            }
        }

        Ok(())
    }

    fn scan_quote(&mut self, quote: u8) -> Result<(), LexError> {
        // Quotes stay in the raw token for later quote removal.
        let start = self.offset;
        self.offset += 1;

        while self.offset < self.input.len() {
            if self.input[self.offset] == quote {
                self.offset += 1;
                return Ok(());
            }

            if quote == b'"' && self.input[self.offset] == b'\\' {
                self.scan_escape()?;
            } else if quote == b'"' && self.starts_with(b"$(") {
                self.scan_substitution()?;
            } else if quote == b'"' && self.starts_with(b"${") {
                self.scan_parameter_expansion()?;
            } else {
                self.offset += 1;
            }
        }

        Err(LexError {
            kind: IncompleteKind::Quote,
            offset: start,
        })
    }

    fn scan_escape(&mut self) -> Result<(), LexError> {
        // A trailing backslash needs another input byte.
        let start = self.offset;
        self.offset += 1;
        if self.offset == self.input.len() {
            return Err(LexError {
                kind: IncompleteKind::Escape,
                offset: start,
            });
        }

        self.offset += 1;
        Ok(())
    }

    fn scan_substitution(&mut self) -> Result<(), LexError> {
        // Balance parentheses inside command or arithmetic substitution.
        let start = self.offset;
        self.offset += 2;
        let mut depth = 1;

        while self.offset < self.input.len() {
            let byte = self.input[self.offset];
            if byte == b'\'' || byte == b'"' {
                self.scan_quote(byte)?;
            } else if byte == b'\\' {
                self.scan_escape()?;
            } else if self.starts_with(b"${") {
                self.scan_parameter_expansion()?;
            } else if byte == b'(' {
                depth += 1;
                self.offset += 1;
            } else if byte == b')' {
                depth -= 1;
                self.offset += 1;
                if depth == 0 {
                    return Ok(());
                }
            } else {
                self.offset += 1;
            }
        }

        Err(LexError {
            kind: IncompleteKind::Substitution,
            offset: start,
        })
    }

    fn scan_parameter_expansion(&mut self) -> Result<(), LexError> {
        // Keep nested braced expansions inside the same word.
        let start = self.offset;
        self.offset += 2;
        let mut depth = 1;

        while self.offset < self.input.len() {
            let byte = self.input[self.offset];
            if byte == b'\'' || byte == b'"' {
                self.scan_quote(byte)?;
            } else if byte == b'\\' {
                self.scan_escape()?;
            } else if self.starts_with(b"$(") {
                self.scan_substitution()?;
            } else if byte == b'{' {
                depth += 1;
                self.offset += 1;
            } else if byte == b'}' {
                depth -= 1;
                self.offset += 1;
                if depth == 0 {
                    return Ok(());
                }
            } else {
                self.offset += 1;
            }
        }

        Err(LexError {
            kind: IncompleteKind::ParameterExpansion,
            offset: start,
        })
    }

    fn starts_with(&self, bytes: &[u8]) -> bool {
        self.input[self.offset..].starts_with(bytes)
    }
}

fn is_operator_byte(byte: u8) -> bool {
    matches!(byte, b'&' | b'|' | b';' | b'(' | b')' | b'<' | b'>')
}

#[cfg(test)]
mod tests {
    use super::{IncompleteKind, LexError, Lexer, Op, Token, TokenKind};

    #[test]
    fn lex_echo_hello() {
        let mut lexer = Lexer::new(b"echo hello");

        assert_eq!(
            lexer.next_token(),
            Ok(Some(Token {
                kind: TokenKind::Word,
                raw: b"echo",
                span: 0..4,
            }))
        );
        assert_eq!(
            lexer.next_token(),
            Ok(Some(Token {
                kind: TokenKind::Word,
                raw: b"hello",
                span: 5..10,
            }))
        );
        assert_eq!(lexer.next_token(), Ok(None));
    }

    #[test]
    fn keeps_quoted_and_nested_syntax_in_words() {
        let mut lexer = Lexer::new(b"echo \"hello world\" $(echo $(date))|cat\n");
        let expected = [
            (TokenKind::Word, b"echo".as_slice()),
            (TokenKind::Word, b"\"hello world\"".as_slice()),
            (TokenKind::Word, b"$(echo $(date))".as_slice()),
            (TokenKind::Op(Op::Pipe), b"|".as_slice()),
            (TokenKind::Word, b"cat".as_slice()),
            (TokenKind::Newline, b"\n".as_slice()),
        ];

        for (kind, raw) in expected {
            let token = lexer.next_token().unwrap().unwrap();
            assert_eq!(token.kind, kind);
            assert_eq!(token.raw, raw);
        }

        assert_eq!(lexer.next_token(), Ok(None));
    }

    #[test]
    fn reports_incomplete_input() {
        let mut lexer = Lexer::new(b"echo 'hello");
        assert!(lexer.next_token().unwrap().is_some());
        assert_eq!(
            lexer.next_token(),
            Err(LexError {
                kind: IncompleteKind::Quote,
                offset: 5,
            })
        );
    }

    #[test]
    fn recognizes_longest_operators_and_incomplete_substitution() {
        let mut lexer = Lexer::new(b"echo x>>out && echo $(date");
        assert_eq!(lexer.next_token().unwrap().unwrap().raw, b"echo");
        assert_eq!(lexer.next_token().unwrap().unwrap().raw, b"x");

        let operator = lexer.next_token().unwrap().unwrap();
        assert_eq!(operator.kind, TokenKind::Op(Op::DoubleGreater));
        assert_eq!(operator.raw, b">>");

        assert_eq!(lexer.next_token().unwrap().unwrap().raw, b"out");
        let operator = lexer.next_token().unwrap().unwrap();
        assert_eq!(operator.kind, TokenKind::Op(Op::AndIf));
        assert_eq!(lexer.next_token().unwrap().unwrap().raw, b"echo");
        assert_eq!(
            lexer.next_token(),
            Err(LexError {
                kind: IncompleteKind::Substitution,
                offset: 20,
            })
        );
    }
}
