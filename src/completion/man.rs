use std::iter::Peekable;

use crate::completion::{Completion, Opt, OptSuffix};

#[derive(Debug)]
enum State {
    SynopsisFirst,
    SynopsisWrapped { indent_len: usize },
    MaybeOptions,
    OptDesc { indent_len: usize, option: String },
    OptDescSkipped { indent_len: usize },
    Unknown,
}

pub struct ManParser {}

impl ManParser {
    pub fn new() -> ManParser {
        ManParser {}
    }

    pub fn parse(self, text: &str) -> Option<Completion> {
        let mut command = None;
        let mut options = Vec::new();
        let mut arguments = Vec::new();
        let mut subcommands = Vec::new();

        let mut lines = text.lines().peekable();
        let mut state = State::Unknown;
        while let Some(line) = lines.next() {
            let trimmed_line = line.trim();
            if trimmed_line.is_empty() {
                if matches!(state, State::SynopsisFirst | State::SynopsisWrapped { .. }) {
                    state = State::MaybeOptions;
                }

                continue;
            }

            let lowered_line = trimmed_line.to_ascii_lowercase();
            let indent_len = line.len() - trimmed_line.len();

            if lowered_line.starts_with("synopsis") {
                state = State::SynopsisFirst;
                continue;
            }

            if lowered_line.starts_with("options") {
                state = State::SynopsisFirst;
                continue;
            }

            let mut words = trimmed_line.split_ascii_whitespace().peekable();

            // Safety: lowered_line includes at least one character by
            //         checking if `trimmed_line` is empty above.
            let first_word = *words.peek().unwrap();
            loop {
                match state {
                    State::SynopsisFirst => {
                        command = Some(first_word.to_owned());
                        state = State::SynopsisWrapped { indent_len };
                    }
                    State::SynopsisWrapped { .. } => {
                        // TODO:
                    }
                    State::MaybeOptions if first_word.starts_with('-') => {
                        // Skip `first_word`.
                        let option = words.next();

                        let next_indent_len = lines
                            .peek()
                            .map(|l| l.len() - l.trim_start().len())
                            .unwrap_or(indent_len);

                        if let Some(opt) = extract_option(first_word, &mut words) {
                            options.push(opt);
                            state = State::OptDescSkipped {
                                indent_len: next_indent_len,
                            };
                        } else {
                            state = State::OptDesc {
                                indent_len: next_indent_len,
                                option: first_word.to_owned(),
                            };
                        }
                    }
                    State::OptDesc {
                        indent_len: prev_indent_len,
                        ..
                    }
                    | State::OptDescSkipped {
                        indent_len: prev_indent_len,
                        ..
                    } if indent_len < prev_indent_len => {
                        state = State::MaybeOptions;
                        continue;
                    }
                    State::OptDesc { option, .. } => {
                        dbg!(words.peek());
                        if let Some(opt) = extract_option(&option, &mut words) {
                            options.push(opt);
                        }

                        state = State::MaybeOptions;
                    }
                    State::MaybeOptions | State::OptDescSkipped { .. } | State::Unknown => {
                        /* ignore */
                    }
                }

                break;
            }
        }

        Some(Completion {
            command: command?,
            options,
            arguments,
            subcommands,
        })
    }
}

fn extract_option<'a, I>(option: &str, words: &mut Peekable<I>) -> Option<Opt>
where
    I: Iterator<Item = &'a str>,
{
    //    -C <path>       - Change the working directory to <path>.
    //       ^^^^^^       ^
    //    SKip here       and here
    while let Some(word) = words.peek() {
        if matches!(word.chars().next(), Some(c) if c.is_uppercase()) {
            break;
        }

        words.next();
    }

    let mut description = String::new();
    dbg!(words.peek());
    for word in words {
        if !description.is_empty() {
            description.push(' ');
        }

        description.push_str(word);
        if word.contains('.') {
            break;
        }
    }

    if !description.is_empty() {
        return Some(Opt {
            name: option.to_owned(),
            description,
            suffix: OptSuffix::Whitespace,
            arg: None,
        });
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::assert_eq;
    use std::{process::Command, str::from_utf8};

    use crate::completion::Completion;

    fn string(s: &str) -> String {
        s.to_string()
    }

    fn read_man(command: &str) -> String {
        from_utf8(
            &Command::new("man")
                .arg(command)
                .output()
                .expect("failed to run man")
                .stdout,
        )
        .expect("man page includes a non-UTF8 sequence")
        .to_owned()
    }

    fn parse_man(command: &str) -> Option<Completion> {
        ManParser::new().parse(&read_man(command))
    }

    #[test]
    fn parse_hexdump_man() {
        assert_eq!(
            parse_man("hexdump"),
            Some(Completion {
                command: string("hexdump2"),
                options: vec![],
                arguments: vec![],
                subcommands: vec! {}
            })
        )
    }
}
