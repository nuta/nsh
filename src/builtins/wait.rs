use super::fg::parse_job_id;
use crate::builtins::InternalCommandContext;
use crate::process::{wait_for_job, ExitStatus, Job};
use std::io::Write;
use std::rc::Rc;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(name = "wait", about = "wait command.")]
struct Opt {
    #[structopt(name = "job_id")]
    job_id: Option<String>,
}

pub fn command(ctx: &mut InternalCommandContext) -> ExitStatus {
    trace!("wait: argv={:?}", ctx.argv);
    match Opt::from_iter_safe(ctx.argv) {
        Ok(opts) => {
            if opts.job_id.is_some() {
                match parse_job_id(ctx, opts.job_id) {
                    Ok(job) => {
                        wait_for_job(ctx.shell, &job);
                        ExitStatus::ExitedWith(0)
                    }
                    Err(status) => status,
                }
            } else {
                // Wait for all jobs.
                let jobs: Vec<Rc<Job>> =
                    ctx.shell.jobs().values().cloned().collect();
                for job in &jobs {
                    wait_for_job(ctx.shell, job);
                }

                ExitStatus::ExitedWith(0)
            }
        }
        Err(err) => {
            writeln!(ctx.stderr, "wait: {}", err).ok();
            ExitStatus::ExitedWith(1)
        }
    }
}
