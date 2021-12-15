use crate::process::ExitStatus;
use crate::shell::Shell;
use crate::utils::FdFile;
use phf::phf_map;
use std::io::Write;

mod alias;
mod bg;
mod cd;
mod echo;
mod eval;
mod exec;
mod exit;
mod export;
mod fg;
mod jobs;
mod popd;
mod pushd;
mod read;
mod rehash;
mod set;
mod shift;
mod source;
mod unset;
mod wait;

pub struct InternalCommandContext<'a> {
    pub argv: &'a [String],
    pub shell: &'a mut Shell,
    pub stdin: FdFile,
    pub stdout: FdFile,
    pub stderr: FdFile,
}

#[derive(Debug, Fail)]
pub enum InternalCommandError {
    #[fail(display = "command not found")]
    NotFound,
    #[fail(display = "failed to create redirections")]
    BadRedirection,
}

/// A super powerful hidden command for some cryptographers.
/// https://xkcd.com/221/
pub fn xkcd_rand_command(ctx: &mut InternalCommandContext) -> ExitStatus {
    writeln!(ctx.stdout, "4").ok();
    ctx.stdout.flush().ok();
    ExitStatus::ExitedWith(0)
}

type InternalCommand = fn(&mut InternalCommandContext) -> ExitStatus;
pub static INTERNAL_COMMANDS: phf::Map<&'static str, InternalCommand> = phf_map! {
    "xkcd-true-random-number" => xkcd_rand_command,
    "alias" => crate::builtins::alias::command,
    "echo" => crate::builtins::echo::command,
    "cd" => crate::builtins::cd::command,
    "source" => crate::builtins::source::command,
    "exit" => crate::builtins::exit::command,
    "exec" => crate::builtins::exec::command,
    "export" => crate::builtins::export::command,
    "set" => crate::builtins::set::command,
    "fg" => crate::builtins::fg::command,
    "bg" => crate::builtins::bg::command,
    "wait" => crate::builtins::wait::command,
    "jobs" => crate::builtins::jobs::command,
    "shift" => crate::builtins::shift::command,
    "read" => crate::builtins::read::command,
    "unset" => crate::builtins::unset::command,
    "pushd" => crate::builtins::pushd::command,
    "popd" => crate::builtins::popd::command,
    "eval" => crate::builtins::eval::command,
    "rehash" => crate::builtins::rehash::command,
};
