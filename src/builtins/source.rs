use crate::builtins::InternalCommandContext;
use crate::exec::{ExitStatus, exec_file};
use std::io::Write;

pub fn command(ctx: &mut InternalCommandContext) -> ExitStatus {
    if let Some(filepath) = ctx.argv.get(1) {
        exec_file(ctx.scope, std::path::PathBuf::from(&filepath))
    } else {
        write!(ctx.stderr, "nsh: source: filename argument required\n").ok();
        ctx.stderr.flush().ok();
        ExitStatus::ExitedWith(0)
    }
}
