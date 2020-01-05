use crate::builtins::InternalCommandContext;
use crate::process::ExitStatus;
use std::io::Write;
use structopt::StructOpt;

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
            ctx.shell.errexit = opts.errexit;
            ctx.shell.nounset = opts.nounset;
            ctx.shell.noexec = opts.noexec;
            ExitStatus::ExitedWith(0)
        }
        Err(err) => {
            writeln!(ctx.stderr, "nsh: set: {}", err).ok();
            ExitStatus::ExitedWith(1)
        }
    }
}
