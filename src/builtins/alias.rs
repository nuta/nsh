use crate::builtins::InternalCommandContext;
use crate::exec::ExitStatus;
use crate::parser::{Input, Word, SyntaxError, is_whitespace, whitespaces, word};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Alias {
    pub name: String,
    pub words: Vec<Word>,
}

named!(parse_alias_line<Input, Alias>,
    do_parse!(
        name: take_while1!(|c| !is_whitespace(c) && c != '=') >>
        tag!("=") >>
        words: many0!(word) >>
        whitespaces >>
        ( Alias { name: name.to_string(), words } )
    )
);

pub fn parse_alias(line: &str) -> Result<Alias, SyntaxError> {
    match parse_alias_line(Input(line)) {
        Ok((_, alias)) => Ok(alias),
        Err(err) => {
            trace!("parse error: '{}'", &err);
            Err(SyntaxError::Fatal(err.to_string()))
        }
    }
}

pub fn command(ctx: &mut InternalCommandContext) -> ExitStatus {
    trace!("alias: {:?}", ctx.argv);
    if let Some(alias) = ctx.argv.get(1) {
        if let Ok(Alias { name, words }) = parse_alias(alias) {
            ctx.isolate.add_alias(&name, words);
        }
    }

    ExitStatus::ExitedWith(0)
}
