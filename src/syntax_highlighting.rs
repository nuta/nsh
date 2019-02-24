use crate::path::lookup_external_command;
use crate::builtins::INTERNAL_COMMANDS;
use crate::context_parser::{
    InputContext, Span, KeywordType, QuoteType, CommandSepType
};

pub fn highlight(ctx: &InputContext) -> String {
    use std::fmt::Write;
    use termion::color::{Fg, Red, Blue, Yellow, Green, Cyan};
    use termion::style::{Bold, Reset};

    let argv0_color = Fg(Green);
    let invalid_argv0_color = Fg(Red);
    let option_color = Fg(Yellow);
    let brace_color = Fg(Green);
    let quote_color = Fg(Cyan);
    let command_sep_color = Fg(Blue);
    let reset_color = Fg(termion::color::Reset);

    let mut buf = String::new();
    let mut in_quote = false;
    for span in &ctx.spans {
        if in_quote {
            write!(buf, "{}", quote_color).ok();
        }

        match span {
            Span::Argv0(cmd) => {
                let command_exists = lookup_external_command(&cmd).is_some()
                    || INTERNAL_COMMANDS.contains_key(cmd.as_str());

                if command_exists {
                   write!(buf, "{}{}{}{}", Bold, argv0_color, cmd, Reset).ok();
                } else {
                   write!(buf, "{}{}{}{}", Bold, invalid_argv0_color, cmd, Reset).ok();
                };

            }
            Span::Literal(span) => {
                if span.starts_with("-") {
                    write!(buf, "{}{}{}", option_color, span, reset_color).ok();
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

                write!(buf, "{}{}{}", Bold, keyword_str, Reset).ok();
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

                write!(buf, "{}{}", quote_ch, Reset).ok();
            }
            Span::CmdSubstStart => {
                write!(buf, "{}$({}", brace_color, Reset).ok();
            }
            Span::CmdSubstEnd => {
                write!(buf, "{}){}", brace_color, Reset).ok();
            }
            Span::ParamExpandStart => {
                write!(buf, "{}{}${{", Bold, brace_color).ok();
            }
            Span::ParamExpandEnd => {
                write!(buf, "{}}}{}", brace_color, Reset).ok();
            }
            Span::Name(name) => {
                write!(buf, "{}{}", name, Reset).ok();
            }
            Span::Op(op) => {
                write!(buf, "{}{}{}{}", Bold, reset_color, op, Reset).ok();
            }
            Span::Param(name) => {
                write!(buf, "{}${}{}", Bold, name, Reset).ok();
            }
            Span::CommandSep(sep) => {
                let sep_str = match sep {
                    CommandSepType::Semi => ";",
                    CommandSepType::DoubleAnd => "&&",
                    CommandSepType::SingleAnd => "&",
                    CommandSepType::Newline => "\n",
                };

                write!(buf, "{}{}{}{}", Bold, command_sep_color, sep_str, Reset).ok();
            }
        }
    }

    buf
}

#[cfg(test)]
mod benchmarks {
    extern crate test;

    use test::Bencher;
    use super::*;
    use crate::context_parser::parse;

    #[bench]
    fn syntax_highlight_bench(b: &mut Bencher) {
        let parsed = parse("ls -avh $(echo hello) \"string ${ls:=bar $(cowsay) } boo\" yay", 0);
        b.iter(|| {
            highlight(&parsed);
        })
    }
}
