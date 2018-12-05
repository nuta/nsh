use crate::builtins::InternalCommandContext;
use crate::exec::ExitStatus;
use structopt::StructOpt;
use std::io::Write;

#[derive(Debug, StructOpt)]
#[structopt(name = "unset", about = "Set command.")]
struct Opt {
    #[structopt(name = "NAME")]
    name: String,
}

pub fn command(ctx: &mut InternalCommandContext) -> ExitStatus {
    // TODO: Support -f, -v, and -n

    match Opt::from_iter_safe(ctx.argv) {
        Ok(opts) => {
            ctx.isolate.remove(&opts.name);
            ExitStatus::ExitedWith(0)
        },
        Err(err) => {
            writeln!(ctx.stdout, "nsh: unset: {}", err).ok();
            ExitStatus::ExitedWith(1)
        }
    }
}
