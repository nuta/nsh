use crate::builtins::InternalCommandContext;
use crate::exec::ExitStatus;
use structopt::StructOpt;
use std::io::Write;
use super::fg::parse_job_id;

#[derive(Debug, StructOpt)]
#[structopt(name = "wait", about = "wait command.")]
struct Opt {
    #[structopt(name = "job_id")]
    job_id: Option<String>,
}

pub fn command(ctx: &mut InternalCommandContext) -> ExitStatus {
    trace!("wait: {:?}", ctx.argv);
    match Opt::from_iter_safe(ctx.argv) {
        Ok(opts) => {
            if opts.job_id.is_some() {
                match parse_job_id(ctx, opts.job_id) {
                    Ok(job) => {
                        ctx.isolate.wait_for_job(&job);
                        ExitStatus::ExitedWith(0)
                    },
                    Err(status) => status,
                }
            } else {
                // Wait for all jobs.
                for job in &ctx.isolate.jobs() {
                    ctx.isolate.wait_for_job(job);
                }

                ExitStatus::ExitedWith(0)
            }
        },
        Err(err) => {
            writeln!(ctx.stderr, "wait: {}", err).ok();
            ExitStatus::ExitedWith(1)
        }
    }
}
