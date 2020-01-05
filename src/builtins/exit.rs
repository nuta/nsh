use crate::builtins::InternalCommandContext;
use crate::process::ExitStatus;

pub fn command(ctx: &mut InternalCommandContext) -> ExitStatus {
    let exit_with = if let Some(exit_with) = ctx.argv.get(1) {
        exit_with.parse().unwrap_or(1)
    } else {
        0
    };

    std::process::exit(exit_with);
}
