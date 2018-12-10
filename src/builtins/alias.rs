use crate::builtins::InternalCommandContext;
use crate::exec::ExitStatus;
use crate::parser;
use std::io::Write;
use pest::Parser;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Alias {
    pub name: String,
    pub body: String,
}

#[derive(Parser)]
#[grammar = "builtins/alias.pest"]
struct AliasParser;

fn parse_alias(alias: &str) -> Result<Alias, parser::ParseError> {
    AliasParser::parse(Rule::alias, alias)
    .map_err(|err| parser::ParseError::Fatal(err.to_string()))
    .and_then(|mut pairs| {
        let mut inner = pairs.next().unwrap().into_inner();
        let name = inner.next().unwrap().as_span().as_str().to_owned();
        let body = inner.next().unwrap().as_str().to_owned();
        Ok(Alias { name, body })
    })
}

pub fn command(ctx: &mut InternalCommandContext) -> ExitStatus {
    trace!("alias: {:?}", ctx.argv);
    if let Some(alias) = ctx.argv.get(1) {
        match parse_alias(alias) {
            Ok(Alias { name, body }) => {
                ctx.isolate.add_alias(&name, body);
                ExitStatus::ExitedWith(0)
            }
            Err(parser::ParseError::Fatal(err)) => {
                writeln!(ctx.stderr, "nsh: alias: {}", err).ok();
                ExitStatus::ExitedWith(1)
            }
            Err(parser::ParseError::Empty) => {
                writeln!(ctx.stderr, "nsh: alias: alias can't be empty string").ok();
                ExitStatus::ExitedWith(1)
            }
        }
    } else {
        // TODO: list defined aliases
        ExitStatus::ExitedWith(0)
    }
}
