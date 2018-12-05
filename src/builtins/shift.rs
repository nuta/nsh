use crate::builtins::InternalCommandContext;
use crate::exec::ExitStatus;
use crate::variable::{Value, Variable};
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
            let current = ctx.isolate.current_frame_mut();
            let mut args = Vec::new();
            for i in 1.. {
                if let Some(var) = current.get_nth_arg(i) {
                    args.push(var);
                    current.remove_nth_arg(i);
                } else {
                    break;
                }
            }

            for (i, var) in args.iter().skip(opts.n.unwrap_or(1)).enumerate() {
                let value = Value::String(var.as_str().to_string());
                trace!("value: ${} = {:?}", i + 1, value);
                current.set_nth_arg(i + 1, Variable::new(value));
            }

            ExitStatus::ExitedWith(0)
        },
        Err(err) => {
            writeln!(ctx.stdout, "shift: {}", err).ok();
            ExitStatus::ExitedWith(1)
        }
    }
}
