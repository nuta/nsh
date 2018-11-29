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
    trace!("bg: {:?}", ctx.argv);
    match Opt::from_iter_safe(ctx.argv) {
        Ok(opts) => {
            match parse_job_id(ctx, opts.job_id) {
                Ok(job_id) => {
                    ctx.env.continue_job(job_id, true);
                    ExitStatus::ExitedWith(0)
                },
                Err(status) => status,
            }
        },
        Err(err) => {
            write!(ctx.stdout, "bg: {}", err).ok();
            ExitStatus::ExitedWith(1)
        }
    }
}
