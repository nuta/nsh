use crate::builtins::InternalCommandContext;
use crate::exec::ExitStatus;
use crate::parser::{parse_alias, Alias};

pub fn command(ctx: &mut InternalCommandContext) -> ExitStatus {
    trace!("alias: {:?}", ctx.argv);
    if let Some(alias) = ctx.argv.get(1) {
        if let Ok(Alias { name, words }) = parse_alias(alias) {
            ctx.scope.add_alias(&name, words);
        }
    }

    ExitStatus::ExitedWith(0)
}
