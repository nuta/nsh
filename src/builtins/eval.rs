use crate::builtins::InternalCommandContext;
use crate::exec::ExitStatus;

pub fn command(ctx: &mut InternalCommandContext) -> ExitStatus {
    // Concatenate arguemts into a string.
    let mut program = String::new();
    for arg in ctx.argv.iter().skip(1) {
        program += arg;
        program.push(' ');
    }

    ctx.isolate.run_str(&program)
}
