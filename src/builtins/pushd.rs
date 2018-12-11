use crate::builtins::InternalCommandContext;
use crate::exec::ExitStatus;
use structopt::StructOpt;
use std::io::Write;

#[derive(Debug, StructOpt)]
#[structopt(name = "pushd", about = "Pushd command.")]
struct Opt {
    #[structopt(name = "dir")]
    dir: Option<String>,
}

pub fn command(ctx: &mut InternalCommandContext) -> ExitStatus {
    match Opt::from_iter_safe(ctx.argv) {
        Ok(opts) => {
            let dir = if let Some(dir) = opts.dir {
                dir
            } else {
                std::env::current_dir().unwrap().to_str().unwrap().to_owned()
            };

            ctx.isolate.pushd(dir);
            ExitStatus::ExitedWith(0)
        },
        Err(err) => {
            writeln!(ctx.stderr, "nsh: pushd: {}", err).ok();
            ExitStatus::ExitedWith(1)
        }
    }
}
