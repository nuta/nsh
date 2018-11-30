use crate::builtins::InternalCommandContext;
use crate::exec::ExitStatus;
use structopt::StructOpt;
use std::io::Write;

#[derive(Debug, StructOpt)]
#[structopt(name = "jobs", about = "jobs command.")]
struct Opt {
}

pub fn command(ctx: &mut InternalCommandContext) -> ExitStatus {
    trace!("jobs: {:?}", ctx.argv);
    match Opt::from_iter_safe(ctx.argv) {
        Ok(_) => {
            for job in ctx.isolate.jobs() {
                write!(ctx.stdout, "[{}] {}: {}\n",
                    job.id(), job.state(ctx.isolate), job.cmd()).ok();
            }
            ExitStatus::ExitedWith(0)
        },
        Err(err) => {
            write!(ctx.stdout, "fg: {}", err).ok();
            ExitStatus::ExitedWith(1)
        }
    }
}
