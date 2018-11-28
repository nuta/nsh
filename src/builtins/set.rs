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
            ctx.env.errexit = opts.errexit;
            ctx.env.nounset = opts.nounset;
            ctx.env.noexec = opts.noexec;
            ExitStatus::ExitedWith(0)
        },
        Err(err) => {
            write!(ctx.stdout, "set: {}", err).ok();
            ExitStatus::ExitedWith(1)
        }
    }
}
