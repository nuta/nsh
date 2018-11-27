use crate::builtins::InternalCommandContext;
use crate::exec::ExitStatus;
use std::io::Write;

pub fn command(ctx: &mut InternalCommandContext) -> ExitStatus {
    let (newline, skip) = match ctx.argv.get(1).map(|s| s.as_str()) {
        Some("-n") => (false, 2),
        _ => (true, 1),
    };

    for (i, arg) in ctx.argv.iter().skip(skip).enumerate() {
        if i > 0 {
            write!(ctx.stdout, " {}", arg).ok();
        } else {
            write!(ctx.stdout, "{}", arg).ok();
        }
    }

    if newline {
        write!(ctx.stdout, "\n").ok();
    }
    ctx.stdout.flush().ok();
    ExitStatus::ExitedWith(0)
}
