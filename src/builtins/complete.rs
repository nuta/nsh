use crate::builtins::InternalCommandContext;
use crate::exec::ExitStatus;
use crate::completion::CompSpecBuilder;
use structopt::StructOpt;
use std::io::Write;

#[derive(Debug, StructOpt)]
#[structopt(name = "complete", about = "Compgen command.")]
struct Opt {
    #[structopt(short = "o")]
    options: Vec<String>,
    #[structopt(short = "F")]
    func_name: Option<String>,
    #[structopt(name = "command")]
    commands: Vec<String>,
}

pub fn command(ctx: &mut InternalCommandContext) -> ExitStatus {
    match Opt::from_iter_safe(ctx.argv) {
        Ok(opts) => {
            let mut compspec = CompSpecBuilder::new();
            for option in opts.options {
                match option.as_str() {
                    "dirnames"  => { compspec.dirnames_if_empty(true); },
                    "filenames" => { compspec.filenames_if_empty(true); },
                    "bashdefault" | "default" => {
                        compspec.dirnames_if_empty(true);
                    },
                    _ => {
                        writeln!(ctx.stderr, "nsh: complete: unknown option: `{}'",
                            option).ok();
                        return ExitStatus::ExitedWith(1);
                    }
                }
            }

            if let Some(func_name) = opts.func_name {
                compspec.func_name(func_name);
            }

            let compspec = compspec.build();
            for command in opts.commands {
                ctx.isolate.set_compspec(&command, compspec.clone());
            }

            ExitStatus::ExitedWith(0)
        },
        Err(err) => {
            writeln!(ctx.stderr, "nsh: complete: {}", err).ok();
            ExitStatus::ExitedWith(1)
        }
    }
}
