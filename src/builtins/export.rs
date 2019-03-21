use crate::builtins::InternalCommandContext;
use crate::exec::ExitStatus;
use crate::variable::Value;
use std::io::Write;

pub fn command(ctx: &mut InternalCommandContext) -> ExitStatus {
    if let Some(arg) = ctx.argv.get(1) {
        let frags: Vec<&str> = arg.splitn(2, '=').collect();
        let mut iter = frags.iter();
        let name = iter.next().unwrap();
        ctx.isolate.export(&name);

        if let Some(value) = iter.next() {
            ctx.isolate.set(&name, Value::String(value.to_owned().to_string()), false);
        }
    } else {
        for name in ctx.isolate.exported_names() {
            if let Some(var) = ctx.isolate.get(name) {
                writeln!(ctx.stdout, "{}={}", name, var.as_str()).ok();
            }
        }
    }

    ExitStatus::ExitedWith(0)
}
