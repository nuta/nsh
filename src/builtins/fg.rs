use crate::builtins::InternalCommandContext;
use crate::exec::ExitStatus;
use structopt::StructOpt;
use std::io::Write;

#[derive(Debug, StructOpt)]
#[structopt(name = "fg", about = "fg command.")]
struct Opt {
    #[structopt(name = "job_id")]
    job_id: Option<String>,
}

// Used by bg.
pub(super) fn parse_job_id(ctx: &mut InternalCommandContext, job_id: Option<String>) -> Result<usize, ExitStatus> {
    match job_id {
        Some(job_id) => {
            let job_id = job_id.as_str();
            if job_id.chars().nth(0) == Some('%') {
                match (&job_id[1..]).parse() {
                    Ok(job_id) => Ok(job_id),
                    Err(_) => {
                        write!(ctx.stdout, "nsh: fg: invalid job id\n").ok();
                        return Err(ExitStatus::ExitedWith(1));
                    },
                }
            } else {
                write!(ctx.stdout, "nsh: fg: invalid job id\n").ok();
                return Err(ExitStatus::ExitedWith(1));
            }
        },
        None => {
            match ctx.env.last_fore_job() {
                Some(job) => Ok(job.id()),
                None => {
                    write!(ctx.stdout, "nsh: fg: no jobs to run\n").ok();
                    return Err(ExitStatus::ExitedWith(1));
                }
            }
        }
    }
}

pub fn command(ctx: &mut InternalCommandContext) -> ExitStatus {
    trace!("fg: {:?}", ctx.argv);
    match Opt::from_iter_safe(ctx.argv) {
        Ok(opts) => {
            match parse_job_id(ctx, opts.job_id) {
                Ok(job_id) => {
                    ctx.env.continue_job(job_id, false);
                    ExitStatus::ExitedWith(0)
                },
                Err(status) => status,
            }
        },
        Err(err) => {
            write!(ctx.stdout, "fg: {}", err).ok();
            ExitStatus::ExitedWith(1)
        }
    }
}
