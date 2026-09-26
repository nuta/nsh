use std::fmt;
use std::io::{self, Write};

use crate::parser::{ParseError, Parser, Word};

#[derive(Debug)]
pub enum Error {
    Parse(ParseError),
    UnsupportedExpansion { offset: usize },
    UnknownCommand(Vec<u8>),
    Io(io::Error),
}

impl fmt::Display for Error {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Parse(error) => write!(formatter, "syntax error: {error:?}"),
            Self::UnsupportedExpansion { offset } => {
                write!(formatter, "unsupported expansion at byte {offset}")
            }
            Self::UnknownCommand(name) => {
                write!(
                    formatter,
                    "unknown command: {}",
                    String::from_utf8_lossy(name)
                )
            }
            Self::Io(error) => write!(formatter, "I/O error: {error}"),
        }
    }
}

impl std::error::Error for Error {}

impl From<io::Error> for Error {
    fn from(error: io::Error) -> Self {
        Self::Io(error)
    }
}

pub struct Shell {}

impl Shell {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run(&self, cmdline: &str) -> Result<(), Error> {
        let stdout = io::stdout();
        let mut output = stdout.lock();
        self.run_with(cmdline.as_bytes(), &mut output)
    }

    fn run_with<W: Write>(&self, input: &[u8], stdout: &mut W) -> Result<(), Error> {
        let mut parser = Parser::new(input);

        // Parse and execute each complete simple command in source order.
        loop {
            let command = match parser.next_command() {
                Ok(Some(command)) => command,
                Ok(None) => return Ok(()),
                Err(error) => return Err(Error::Parse(error)),
            };

            let mut arguments = Vec::new();
            for word in &command.words {
                arguments.push(literal_word(word)?);
            }

            // The first runnable slice supports the echo builtin.
            if arguments[0] == b"echo" {
                for (index, argument) in arguments[1..].iter().enumerate() {
                    if index > 0 {
                        stdout.write_all(b" ")?;
                    }

                    stdout.write_all(argument)?;
                }

                stdout.write_all(b"\n")?;
            } else {
                return Err(Error::UnknownCommand(arguments.remove(0)));
            }
        }
    }
}

fn literal_word(word: &Word<'_>) -> Result<Vec<u8>, Error> {
    let raw = word.raw;
    let mut value = Vec::new();
    let mut offset = 0;
    let mut quote = None;

    // Remove literal quotes and escapes; leave expansions for a later stage.
    while offset < raw.len() {
        let byte = raw[offset];
        if quote == Some(b'\'') {
            if byte == b'\'' {
                quote = None;
            } else {
                value.push(byte);
            }
            offset += 1;
            continue;
        }

        if byte == b'\'' || byte == b'"' {
            if quote == Some(byte) {
                quote = None;
            } else if quote.is_none() {
                quote = Some(byte);
            } else {
                value.push(byte);
            }
            offset += 1;
            continue;
        }

        if byte == b'\\' {
            let next = raw[offset + 1];
            if quote == Some(b'"')
                && next != b'$'
                && next != b'`'
                && next != b'"'
                && next != b'\\'
                && next != b'\n'
            {
                value.push(byte);
            }

            if next != b'\n' {
                value.push(next);
            }
            offset += 2;
            continue;
        }

        if byte == b'`' || byte == b'$' && starts_expansion(raw.get(offset + 1).copied()) {
            return Err(Error::UnsupportedExpansion {
                offset: word.span.start + offset,
            });
        }

        value.push(byte);
        offset += 1;
    }

    Ok(value)
}

fn starts_expansion(next: Option<u8>) -> bool {
    match next {
        Some(b'(' | b'{' | b'@' | b'*' | b'#' | b'?' | b'$' | b'!' | b'-') => true,
        Some(byte) if byte.is_ascii_alphanumeric() || byte == b'_' => true,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::{Error, Shell};

    #[test]
    fn runs_echo_with_literal_quotes_and_separators() {
        let mut output = Vec::new();
        Shell::new()
            .run_with(b"echo hello; echo \"two words\" 'and more'\n", &mut output)
            .unwrap();
        assert_eq!(output, b"hello\ntwo words and more\n");
    }

    #[test]
    fn rejects_unimplemented_expansion() {
        let mut output = Vec::new();
        let result = Shell::new().run_with(b"echo $HOME", &mut output);
        assert!(matches!(
            result,
            Err(Error::UnsupportedExpansion { offset: 5 })
        ));
        assert!(output.is_empty());
    }
}
