use crate::builtins::InternalCommandContext;
use crate::exec::ExitStatus;
use crate::variable::Value;
use structopt::StructOpt;
use std::io::Write;

#[derive(Debug, StructOpt)]
#[structopt(name = "shift", about = "shift command.")]
struct Opt {
    #[structopt(name = "n")]
    n: Option<usize>,
}

pub fn command(ctx: &mut InternalCommandContext) -> ExitStatus {
    trace!("shift: {:?}", ctx.argv);
    match Opt::from_iter_safe(ctx.argv) {
        Ok(opts) => {
            let mut args = Vec::new();
            for i in 1.. {
                let name = &i.to_string();
                if let Some(var) = ctx.isolate.get(&name) {
                    args.push(var);
                    ctx.isolate.remove(&name);
                } else {
                    break;
                }
            }

            for (i, var) in args.iter().skip(opts.n.unwrap_or(1)).enumerate() {
                let value = Value::String(var.as_str().to_string());
                trace!("value: ${} = {:?}", i + 1, value);
                ctx.isolate.set(&(i + 1).to_string(), value, true);
            }

            ExitStatus::ExitedWith(0)
        },
        Err(err) => {
            write!(ctx.stdout, "shift: {}", err).ok();
            ExitStatus::ExitedWith(1)
        }
    }
}
