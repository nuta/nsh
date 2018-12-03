use crate::builtins::InternalCommandContext;
use crate::exec::ExitStatus;
use crate::variable::Value;
use structopt::StructOpt;
use std::io::{BufRead, Write};

#[derive(Debug, StructOpt)]
#[structopt(name = "read", about = "read command.")]
struct Opt {
    #[structopt(short = "p")]
    prompt: Option<String>,
    #[structopt(name = "VAR")]
    var_name: String,
}

pub fn command(ctx: &mut InternalCommandContext) -> ExitStatus {
    trace!("read: {:?}", ctx.argv);
    match Opt::from_iter_safe(ctx.argv) {
        Ok(opts) => {
            if let Some(prompt) = opts.prompt {
                write!(ctx.stderr, "{}", prompt).ok();
                ctx.stderr.flush().ok();
            }

            let mut value = String::new();
            match ctx.stdin.read_line(&mut value) {
                Ok(_) => {
                    if value.is_empty() {
                        // EOF
                        return ExitStatus::ExitedWith(1);
                    }

                    let trimed_value = value.trim_end();
                    let value = Value::String(trimed_value.to_owned());
                    ctx.isolate.set(&opts.var_name, value, false);
                    ExitStatus::ExitedWith(0)
                },
                Err(err) => {
                    writeln!(ctx.stdout, "read: {}", err).ok();
                    ExitStatus::ExitedWith(1)
                }
            }
        },
        Err(err) => {
            writeln!(ctx.stdout, "read: {}", err).ok();
            ExitStatus::ExitedWith(1)
        }
    }
}
