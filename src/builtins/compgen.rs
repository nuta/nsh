use crate::builtins::InternalCommandContext;
use crate::exec::ExitStatus;
use crate::completion::CompGen;
use structopt::StructOpt;
use std::io::Write;

#[derive(Debug, StructOpt)]
#[structopt(name = "compgen", about = "Compgen command.")]
struct Opt {
    #[structopt(short = "A")]
    actions: Vec<String>,
    #[structopt(short = "W")]
    wordlist: Option<String>,
    #[structopt(name = "query")]
    query: Option<String>,
}

pub fn command(ctx: &mut InternalCommandContext) -> ExitStatus {
    match Opt::from_iter_safe(ctx.argv) {
        Ok(opts) => {
            let mut compgen = CompGen::new();
            for action in opts.actions {
                match action.as_str() {
                    "command"   => { compgen.include_commands(true); },
                    "file"      => { compgen.include_files(true); },
                    "directory" => { compgen.include_dirs(true); },
                    _ => {
                        write!(ctx.stderr, "nsh: compgen: unknown action: `{}'",
                            action).ok();
                        return ExitStatus::ExitedWith(1);
                    }
                }
            }

            if let Some(wordlist) = opts.wordlist {
                compgen.wordlist(&wordlist, &ctx.isolate.ifs());
            }

            if let Some(query) = opts.query {
                compgen.filter_by(&query);
            }

            for entry in compgen.generate() {
                writeln!(ctx.stdout, "{}", entry).ok();
            }

            ExitStatus::ExitedWith(0)
        },
        Err(err) => {
            writeln!(ctx.stderr, "nsh: compgen: {}", err).ok();
            ExitStatus::ExitedWith(1)
        }
    }
}
