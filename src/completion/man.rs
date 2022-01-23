use std::{iter::Peekable, ops::RangeBounds};

use crate::completion::{Completion, Opt, OptSuffix, Value};

#[derive(Debug)]
enum State {
    Usage,
    MaybeOption,
    SkipIfDeeperIndent { indent_len: usize },
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
                if matches!(state, State::Usage) {
                    state = State::MaybeOption;
                }

                continue;
            }

            let lowered_line = trimmed_line.to_ascii_lowercase();
            let indent_len = line.len() - trimmed_line.len();

            if lowered_line.starts_with("synopsis") {
                state = State::Usage;
                continue;
            }

            if lowered_line.starts_with("options") {
                state = State::Usage;
                continue;
            }

            let mut words = trimmed_line.split_ascii_whitespace().peekable();

            // Safety: lowered_line includes at least one character by
            //         checking if `trimmed_line` is empty above.
            let first_word = *words.peek().unwrap();
            loop {
                match state {
                    State::Usage => {
                        command = Some(first_word.to_owned());
                        state = State::SkipIfDeeperIndent { indent_len };
                    }
                    State::MaybeOption if first_word.starts_with('-') => {
                        // Skip `first_word`.
                        words.next();

                        let mut flags = Vec::new();
                        let (suffix, value) = parse_option(trimmed_line, &mut flags);
                        'outer: loop {
                            if let Some(description) = extract_option_desc(words) {
                                options.push(Opt {
                                    flags,
                                    description,
                                    suffix,
                                    value,
                                });

                                break;
                            }

                            // The description is not in the same line.
                            loop {
                                let next_line = match lines.next() {
                                    Some(l) if l.is_empty() => continue,
                                    Some(l) => l.trim_start(),
                                    None => break 'outer,
                                };

                                words = next_line.split_ascii_whitespace().peekable();

                                // Handle the case where multiple flags defined:
                                //
                                //   -f, --foo
                                //   --bar
                                //   Do something.
                                //
                                let first_word = words.peek().unwrap();
                                if first_word.starts_with("-") {
                                    parse_option(next_line, &mut flags);
                                    continue;
                                }

                                break;
                            }
                        }

                        state = State::SkipIfDeeperIndent { indent_len };
                    }
                    State::SkipIfDeeperIndent {
                        indent_len: prev_indent_len,
                        ..
                    } if indent_len <= prev_indent_len => {
                        state = State::MaybeOption;
                        continue;
                    }
                    State::MaybeOption | State::SkipIfDeeperIndent { .. } | State::Unknown => {
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

fn extract_option_desc<'a, I>(mut words: Peekable<I>) -> Option<String>
where
    I: Iterator<Item = &'a str>,
{
    //    -C <path>       - Change the working directory to <path>.
    //       ^^^^^^
    //   Parse here
    let mut value = Value::Any;
    match words.peek() {
        Some(word)
            if word.starts_with(|c: char| c.is_ascii_lowercase() || c == '[' || c == '<') =>
        {
            value = guess_value(word);
        }
        _ => {}
    }

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
    for word in words {
        if !description.is_empty() {
            description.push(' ');
        }

        description.push_str(word);
        if word.contains('.') {
            break;
        }
    }

    if description.is_empty() {
        None
    } else {
        Some(description)
    }
}

fn parse_option(line: &str, flags: &mut Vec<String>) -> (OptSuffix, Value) {
    //
    //  --base-dir=<path>
    //            ^^^^^^^
    //            Parse here
    let mut name = String::new();
    let mut suffix = OptSuffix::Whitespace;
    let mut value = Value::Any;
    let mut chars = line.char_indices().peekable();
    while let Some((offset, c)) = chars.next() {
        if c == ',' {
            flags.push(name);
            name = String::new();

            // Skip whitespaces.
            while let Some((_, c)) = chars.peek() {
                if !c.is_ascii_whitespace() {
                    break;
                }

                chars.next();
            }
        } else if !c.is_ascii_alphanumeric() && c != '-' && c != '_' {
            if c == '=' {
                suffix = OptSuffix::Equal;
            }

            value = guess_value(&line[(offset + 1)..]);
            break;
        } else {
            name.push(c);
        }
    }

    if !name.is_empty() && name != "-" && name != "--" {
        flags.push(name);
    }

    (suffix, value)
}

fn guess_value(arg: &str) -> Value {
    // "<path>" => "path"
    let mut s = String::new();
    for c in arg.chars() {
        if c.is_ascii_alphanumeric() || c == '_' || c == '-' {
            s.push(c.to_ascii_lowercase());
        }
    }

    if s.contains("directory") || s.ends_with("dir") {
        Value::Path {
            kind: crate::completion::PathKind::DirOnly,
        }
    } else {
        Value::Any
    }
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

    #[test]
    fn parse_hexdump_man() {
        assert_eq!(
            ManParser::new().parse(
                r"#
                MY_COMMAND(1)                 My Commands Manual                   MY_COMMAND(1)

                NAME
                     my_command - This is my command.

                SYNOPSIS
                     my_command [-ab]
                             file ...

                DESCRIPTION
                     Do Something.

                     The options are as follows:

                     -a      First sentence for -a. Second sentence.
                     -b      First sentence for -b. Second sentence.

                     -c
                     First sentence for -c.

                     -d
                         First sentence for -d.

                     --help, -h  First sentence for --help.

                    --env1=<VALUE>  First sentence for --env1.

                    --env2=<VALUE>
                    --env3=<VALUE>
                         First sentence for --env2 and --env3.

                    --base-dir=<DIR>  First sentence for --base-dir.
                #"
            ),
            Some(Completion {
                command: string("my_command"),
                options: vec![
                    Opt {
                        flags: vec![string("-a")],
                        description: string("First sentence for -a."),
                        suffix: OptSuffix::Whitespace,
                        value: Value::Any,
                    },
                    Opt {
                        flags: vec![string("-b")],
                        description: string("First sentence for -b."),
                        suffix: OptSuffix::Whitespace,
                        value: Value::Any,
                    },
                    Opt {
                        flags: vec![string("-c")],
                        description: string("First sentence for -c."),
                        suffix: OptSuffix::Whitespace,
                        value: Value::Any,
                    },
                    Opt {
                        flags: vec![string("-d")],
                        description: string("First sentence for -d."),
                        suffix: OptSuffix::Whitespace,
                        value: Value::Any,
                    },
                    Opt {
                        flags: vec![string("--help"), string("-h")],
                        description: string("First sentence for --help."),
                        suffix: OptSuffix::Whitespace,
                        value: Value::Any,
                    },
                    Opt {
                        flags: vec![string("--env1")],
                        description: string("First sentence for --env1."),
                        suffix: OptSuffix::Equal,
                        value: Value::Any,
                    },
                    Opt {
                        flags: vec![string("--env2"), string("--env3")],
                        description: string("First sentence for --env2 and --env3."),
                        suffix: OptSuffix::Equal,
                        value: Value::Any,
                    },
                    Opt {
                        flags: vec![string("--base-dir")],
                        description: string("First sentence for --base-dir."),
                        suffix: OptSuffix::Equal,
                        value: Value::Path {
                            kind: crate::completion::PathKind::DirOnly,
                        },
                    },
                ],
                arguments: vec![],
                subcommands: vec! {}
            })
        )
    }
}
