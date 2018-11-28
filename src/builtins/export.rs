use crate::builtins::InternalCommandContext;
use crate::exec::ExitStatus;
use dirs;
use std::io::Write;

pub fn command(ctx: &mut InternalCommandContext) -> ExitStatus {
    if let Some(name) = ctx.argv.get(1) {
        ctx.env.export(&name);
    } else {
        for name in ctx.env.exported_names() {
            if let Some(var) = ctx.env.get(name) {
                write!(ctx.stdout, "{}={}\n", name, var.value()).ok();
            }
        }
    }

    ExitStatus::ExitedWith(0)
}
