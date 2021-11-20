use crate::builtins::InternalCommandContext;
use crate::process::ExitStatus;
use crate::variable::Value;
use std::io::Write;

pub fn command(ctx: &mut InternalCommandContext) -> ExitStatus {
    if ctx.argv.is_empty() {
        for name in ctx.shell.exported_names() {
            if let Some(var) = ctx.shell.get(name) {
                writeln!(ctx.stdout, "{}={}", name, var.as_str()).ok();
            }
        }

        return ExitStatus::ExitedWith(0);
    }

    for arg in ctx.argv {
        let frags: Vec<&str> = arg.splitn(2, '=').collect();
        let mut iter = frags.iter();
        match (iter.next(), iter.next()) {
            (Some(name), Some(value)) => {
                ctx.shell.export(name);
                ctx.shell
                    .set(&name, Value::String(value.to_owned().to_string()), false);
            }
            (Some(name), None) => {
                ctx.shell.export(name);
            }
            _ => {}
        }
    }

    ExitStatus::ExitedWith(0)
}
