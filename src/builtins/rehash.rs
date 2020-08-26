use crate::builtins::InternalCommandContext;
use crate::process::ExitStatus;

pub fn command(ctx: &mut InternalCommandContext) -> ExitStatus {
    ctx.shell.path_table_mut().rehash();
    ExitStatus::ExitedWith(0)
}
