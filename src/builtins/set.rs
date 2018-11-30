use crate::builtins::InternalCommandContext;
use crate::exec::ExitStatus;
use structopt::StructOpt;
use std::io::Write;

#[derive(Debug, StructOpt)]
#[structopt(name = "set", about = "Set command.")]
struct Opt {
    #[structopt(short = "e")]
    errexit: bool,
    #[structopt(short = "u")]
    nounset: bool,
    #[structopt(short = "n")]
    noexec: bool,
}

pub fn command(ctx: &mut InternalCommandContext) -> ExitStatus {
    // TODO: Support more options
    // TODO: Support +e, +u, ...

    match Opt::from_iter_safe(ctx.argv) {
        Ok(opts) => {
            ctx.isolate.errexit = opts.errexit;
            ctx.isolate.nounset = opts.nounset;
            ctx.isolate.noexec = opts.noexec;
            ExitStatus::ExitedWith(0)
        },
        Err(err) => {
            write!(ctx.stdout, "nsh: set: {}\n", err).ok();
            ExitStatus::ExitedWith(1)
        }
    }
}
