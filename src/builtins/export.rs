use crate::builtins::InternalCommandContext;
use crate::exec::ExitStatus;
use dirs;
use std::io::Write;

pub fn command(ctx: &mut InternalCommandContext) -> ExitStatus {
    if let Some(name) = ctx.argv.get(1) {
        ctx.isolate.export(&name);
    } else {
        for name in ctx.isolate.exported_names() {
            if let Some(var) = ctx.isolate.get(name) {
                write!(ctx.stdout, "{}={}\n", name, var.as_str()).ok();
            }
        }
    }

    ExitStatus::ExitedWith(0)
}
