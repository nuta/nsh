use crate::builtins::InternalCommandContext;
use crate::process::ExitStatus;
use std::io::Write;
use structopt::StructOpt;

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
            ctx.shell.remove(&opts.name);
            ExitStatus::ExitedWith(0)
        }
        Err(err) => {
            writeln!(ctx.stderr, "nsh: unset: {}", err).ok();
            ExitStatus::ExitedWith(1)
        }
    }
}
