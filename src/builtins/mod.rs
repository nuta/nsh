use crate::exec::{ExitStatus, Env};
use crate::utils::FdFile;
use std::collections::BTreeMap;
use std::io::Write;
use std::os::unix::io::RawFd;

mod alias;
mod cd;
mod echo;
mod exit;
mod export;
mod source;

pub struct InternalCommandContext<'a> {
    pub argv: &'a [String],
    pub env: &'a mut Env,
    pub stdin: FdFile,
    pub stdout: FdFile,
    pub stderr: FdFile
}

#[derive(Debug)]
pub enum InternalCommandError {
    NotFound,
}

/// https://xkcd.com/221/
pub fn xkcd_rand_command(ctx: &mut InternalCommandContext) -> ExitStatus {
    write!(ctx.stdout, "4\n").ok();
    ctx.stdout.flush().ok();

    ExitStatus::ExitedWith(0)
}

type InternalCommand = fn(&mut InternalCommandContext) -> ExitStatus;
lazy_static! {
    static ref INTERNAL_COMMANDS: BTreeMap<&'static str, InternalCommand> = {
        let mut commands: BTreeMap<&'static str, InternalCommand> = BTreeMap::new();
        commands.insert("alias", crate::builtins::alias::command);
        commands.insert("echo", crate::builtins::echo::command);
        commands.insert("cd", crate::builtins::cd::command);
        commands.insert("source", crate::builtins::source::command);
        commands.insert("exit", crate::builtins::exit::command);
        commands.insert("export", crate::builtins::export::command);
        commands.insert(
            "get-xkcd-true-random-number-chosen-by-fair-dice-roll",
            xkcd_rand_command,
        );
        commands
    };
}

pub fn run_internal_command(
    env: &mut Env,
    argv: &[String],
    stdin: RawFd,
    stdout: RawFd,
    stderr: RawFd
) -> Result<ExitStatus, InternalCommandError> {

    let mut ctx = InternalCommandContext {
        argv,
        env,
        stdin: FdFile::new(stdin),
        stdout: FdFile::new(stdout),
        stderr: FdFile::new(stderr),
    };

    let cmd = argv[0].as_str();
    match INTERNAL_COMMANDS.get(cmd) {
        Some(func) => Ok(func(&mut ctx)),
        _ => Err(InternalCommandError::NotFound),
    }
}
