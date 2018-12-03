use crate::exec::{ExitStatus, Isolate};
use crate::utils::FdFile;
use std::collections::BTreeMap;
use std::io::{Write, BufReader, BufWriter};

mod alias;
mod cd;
mod echo;
mod exit;
mod export;
mod source;
mod set;
mod fg;
mod bg;
mod jobs;
mod shift;

pub struct InternalCommandContext<'a> {
    pub argv: &'a [String],
    pub isolate: &'a mut Isolate,
    pub stdin: BufReader<FdFile>,
    pub stdout: BufWriter<FdFile>,
    pub stderr: BufWriter<FdFile>
}

#[derive(Debug)]
pub enum InternalCommandError {
    NotFound,
    BadRedirection,
}

/// https://xkcd.com/221/
pub fn xkcd_rand_command(ctx: &mut InternalCommandContext) -> ExitStatus {
    writeln!(ctx.stdout, "4").ok();
    ctx.stdout.flush().ok();

    ExitStatus::ExitedWith(0)
}

type InternalCommand = fn(&mut InternalCommandContext) -> ExitStatus;
lazy_static! {
    // TODO: Construct this map in compile time.
    pub static ref INTERNAL_COMMANDS: BTreeMap<&'static str, InternalCommand> = {
        let mut commands: BTreeMap<&'static str, InternalCommand> = BTreeMap::new();
        // A hidden command which is quite useful for some cryptographers.
        commands.insert(
            "get-xkcd-true-random-number",
            xkcd_rand_command
        );

        commands.insert("alias", crate::builtins::alias::command);
        commands.insert("echo", crate::builtins::echo::command);
        commands.insert("cd", crate::builtins::cd::command);
        commands.insert("source", crate::builtins::source::command);
        commands.insert("exit", crate::builtins::exit::command);
        commands.insert("export", crate::builtins::export::command);
        commands.insert("set", crate::builtins::set::command);
        commands.insert("fg", crate::builtins::fg::command);
        commands.insert("bg", crate::builtins::bg::command);
        commands.insert("jobs", crate::builtins::jobs::command);
        commands.insert("shift", crate::builtins::shift::command);
        commands
    };
}
