use nom::types::CompleteStr as Input;

#[derive(Debug, PartialEq, Eq)]
pub struct Prompt {
    spans: Vec<Span>
}

#[derive(Debug, PartialEq, Eq)]
pub enum Span {
    Literal(String),
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
            Span::Newline => buf.push_str("\n"),
            // TODO:
            Span::Username => buf.push_str("spam"),
            // TODO:
            Span::Hostname => buf.push_str("egg"),
            // TODO:
            Span::CurrentDir => buf.push_str("sausage"),
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