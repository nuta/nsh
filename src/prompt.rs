use nom::types::CompleteStr as Input;
use std::env;

#[derive(Debug, PartialEq, Eq)]
pub struct Prompt {
    spans: Vec<Span>
}

#[derive(Debug, PartialEq, Eq)]
pub enum Color {
    Red,
    Blue,
    Yellow,
    Green,
    Cyan,
    Magenta,
    Bold,
    Underline,
    Reset,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Span {
    Literal(String),
    Color(Color),
    Username,
    Hostname,
    CurrentDir,
    Newline,
}

named!(span<Input, Span>,
    alt!(
        map!(tag!("\\u"), |_| Span::Username)
        | map!(tag!("\\h"), |_| Span::Hostname)
        | map!(tag!("\\W"), |_| Span::CurrentDir)
        | map!(tag!("\\n"), |_| Span::Newline)
        | map!(tag!("\\c{reset}"), |_| Span::Color(Color::Reset))
        | map!(tag!("\\c{bold}"), |_| Span::Color(Color::Bold))
        | map!(tag!("\\c{underline}"), |_| Span::Color(Color::Underline))
        | map!(tag!("\\c{red}"), |_| Span::Color(Color::Red))
        | map!(tag!("\\c{blue}"), |_| Span::Color(Color::Blue))
        | map!(tag!("\\c{green}"), |_| Span::Color(Color::Green))
        | map!(tag!("\\c{yellow}"), |_| Span::Color(Color::Yellow))
        | map!(tag!("\\c{cyan}"), |_| Span::Color(Color::Cyan))
        | map!(tag!("\\c{magenta}"), |_| Span::Color(Color::Magenta))
        | map!(take_while!(|c| c != '\\'), |s| Span::Literal(s.to_string()))
    )
);

named!(prompt_parser<Input, Prompt>,
    map!(many0!(span), |spans| ( Prompt { spans }))
);

pub fn parse_prompt(prompt: &str) -> Result<Prompt, ()> {
    match prompt_parser(Input(prompt)) {
        Ok((_, prompt)) => {
            Ok(prompt)
        }
        Err(err) => {
            trace!("prompt parse error: '{}'", &err);
            Err(())
        }
    }
}

pub fn draw_prompt(prompt: &Prompt, buf: &mut String) {
    for span in &prompt.spans {
        match span {
            Span::Literal(s) => buf.push_str(&s),
            Span::Color(c) => match c {
                Color::Red => buf.push_str("\x1b[31m"),
                Color::Blue => buf.push_str("\x1b[34m"),
                Color::Yellow => buf.push_str("\x1b[33m"),
                Color::Green => buf.push_str("\x1b[32m"),
                Color::Cyan => buf.push_str("\x1b[36m"),
                Color::Magenta => buf.push_str("\x1b[35m"),
                Color::Bold => buf.push_str("\x1b[1m"),
                Color::Underline => buf.push_str("\x1b[4m"),
                Color::Reset => buf.push_str("\x1b[0m"),
            },
            Span::Newline => buf.push_str("\n"),
            // TODO:
            Span::Username => buf.push_str("spam"),
            // TODO:
            Span::Hostname => buf.push_str("egg"),
            Span::CurrentDir => {
                if let Ok(current_dir) = env::current_dir() {
                    buf.push_str(current_dir.to_str().unwrap());
                }
            },
        }
    }
}

#[test]
fn test_prompt_parser() {
    assert_eq!(
        parse_prompt("\\u at \\h in \\W\\n$ "),
        Ok(Prompt {
            spans: vec![
                Span::Username,
                Span::Literal(" at ".into()),
                Span::Hostname,
                Span::Literal(" in ".into()),
                Span::CurrentDir,
                Span::Newline,
                Span::Literal("$ ".into()),
            ]
        })
    );
}