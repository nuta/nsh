use crate::builtins::InternalCommandContext;
use crate::exec::ExitStatus;
use structopt::StructOpt;
use std::io::Write;
use super::fg::parse_job_id;

#[derive(Debug, StructOpt)]
#[structopt(name = "bg", about = "bg command.")]
struct Opt {
    #[structopt(name = "job_id")]
    job_id: Option<String>,
}

pub fn command(ctx: &mut InternalCommandContext) -> ExitStatus {
    trace!("bg: argv={:?}", ctx.argv);
    match Opt::from_iter_safe(ctx.argv) {
        Ok(opts) => {
            match parse_job_id(ctx, opts.job_id) {
                Ok(job) => {
                    ctx.isolate.continue_job(&job, true);
                    ExitStatus::ExitedWith(0)
                },
                Err(status) => status,
            }
        },
        Err(err) => {
            writeln!(ctx.stderr, "bg: {}", err).ok();
            ExitStatus::ExitedWith(1)
        }
    }
}
