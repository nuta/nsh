use crate::builtins::INTERNAL_COMMANDS;
use crate::context_parser::{CommandSepType, InputContext, KeywordType, QuoteType, Span};
use crate::shell::Shell;
use std::path::Path;
use crossterm::style::{Color, Attribute, SetAttribute, SetForegroundColor};

pub fn highlight(ctx: &InputContext, shell: &mut Shell) -> String {
    use std::fmt::Write;

    let argv0_color = SetForegroundColor(Color::Green);
    let invalid_argv0_color = SetForegroundColor(Color::Red);
    let option_color = SetForegroundColor(Color::Yellow);
    let brace_color = SetForegroundColor(Color::Green);
    let quote_color = SetForegroundColor(Color::Cyan);
    let command_sep_color = SetForegroundColor(Color::Blue);
    let bold = SetAttribute(Attribute::Bold);
    let reset = SetAttribute(Attribute::Reset);

    let mut buf = String::new();
    let mut in_quote = false;
    for span in &ctx.spans {
        if in_quote {
            write!(buf, "{}", quote_color).ok();
        }

        match span {
            Span::Argv0(cmd) => {
                let command_exists = (cmd.starts_with('/') && Path::new(cmd.as_str()).exists())
                    || shell.path_table().contains(&cmd)
                    || INTERNAL_COMMANDS.contains_key(cmd.as_str())
                    || shell.lookup_alias(cmd.as_str()).is_some();

                if command_exists {
                    write!(buf, "{}{}{}", argv0_color, cmd, reset).ok();
                } else {
                    write!(buf, "{}{}{}", invalid_argv0_color, cmd, reset).ok();
                };
            }
            Span::Literal(span) => {
                if span.starts_with('-') {
                    write!(buf, "{}{}{}", option_color, span, reset).ok();
                } else {
                    buf += &span;
                }
            }
            Span::Space(span) => {
                buf += &span;
            }
            Span::Keyword(keyword) => {
                let keyword_str = match keyword {
                    KeywordType::If => "if",
                    KeywordType::Fi => "fi",
                    KeywordType::Then => "then",
                };

                write!(buf, "{}{}{}", bold, keyword_str, reset).ok();
            }
            Span::QuoteStart(quote) => {
                in_quote = true;
                let quote_ch = match quote {
                    QuoteType::Double => '"',
                    QuoteType::Single => '\'',
                };

                write!(buf, "{}{}", quote_color, quote_ch).ok();
            }
            Span::QuoteEnd(quote) => {
                in_quote = false;
                let quote_ch = match quote {
                    QuoteType::Double => '"',
                    QuoteType::Single => '\'',
                };

                write!(buf, "{}{}", quote_ch, reset).ok();
            }
            Span::CmdSubstStart => {
                write!(buf, "{}$({}", brace_color, reset).ok();
            }
            Span::CmdSubstEnd => {
                write!(buf, "{}){}", brace_color, reset).ok();
            }
            Span::ParamExpandStart => {
                write!(buf, "{}{}${{", bold, brace_color).ok();
            }
            Span::ParamExpandEnd => {
                write!(buf, "{}}}{}", brace_color, reset).ok();
            }
            Span::Name(name) => {
                write!(buf, "{}{}", name, reset).ok();
            }
            Span::Op(op) => {
                write!(buf, "{}{}{}{}", bold, reset, op, reset).ok();
            }
            Span::Param(name) => {
                write!(buf, "{}${}{}", bold, name, reset).ok();
            }
            Span::CommandSep(sep) => {
                let sep_str = match sep {
                    CommandSepType::Semi => ";",
                    CommandSepType::Pipe => "|",
                    CommandSepType::DoubleAnd => "&&",
                    CommandSepType::SingleAnd => "&",
                    CommandSepType::Newline => "\n",
                };

                write!(buf, "{}{}{}{}", bold, command_sep_color, sep_str, reset).ok();
            }
        }
    }

    buf
}

#[cfg(test)]
mod benchmarks {
    extern crate test;

    use super::*;
    use crate::context_parser::parse;
    use test::Bencher;

    #[bench]
    fn syntax_highlight_bench(b: &mut Bencher) {
        let mut shell = Shell::new(Path::new("/dev/null"));
        let parsed = parse(
            "ls -avh $(echo hello) \"string ${ls:=bar $(cowsay) } boo\" yay",
            0,
        );
        b.iter(|| {
            highlight(&parsed, &mut shell);
        })
    }
}
