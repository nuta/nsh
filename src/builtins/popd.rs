use crate::builtins::InternalCommandContext;
use crate::exec::ExitStatus;
use structopt::StructOpt;
use std::io::Write;

#[derive(Debug, StructOpt)]
#[structopt(name = "popd", about = "Popd command.")]
struct Opt {
}

pub fn command(ctx: &mut InternalCommandContext) -> ExitStatus {
    match Opt::from_iter_safe(ctx.argv) {
        Ok(_opts) => {
            match ctx.isolate.popd() {
                Some(dir) => {
                    match std::env::set_current_dir(&dir) {
                        Ok(_) => {
                            ExitStatus::ExitedWith(0)
                        },
                        Err(err) => {
                            writeln!(ctx.stderr, "nsh: popd: {}: `{}'", err, dir).ok();
                            ExitStatus::ExitedWith(1)
                        }
                    }
                },
                None => {
                    writeln!(ctx.stderr, "nsh: popd: directory stack empty").ok();
                    ExitStatus::ExitedWith(1)
                }
            }
        },
        Err(err) => {
            writeln!(ctx.stderr, "nsh: popd: {}", err).ok();
            ExitStatus::ExitedWith(1)
        }
    }
}
