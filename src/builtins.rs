use crate::alias::alias_command;
use crate::exec::{ExitStatus, Scope, exec_file};
use crate::utils::FdFile;
use dirs;
use std::collections::BTreeMap;
use std::env;
use std::io::Write;
use std::path::Path;
use std::process;
use std::os::unix::io::RawFd;

pub struct InternalCommandContext<'a> {
    pub argv: &'a [String],
    pub scope: &'a mut Scope,
    pub stdin: FdFile,
    pub stdout: FdFile,
    pub stderr: FdFile
}

#[derive(Debug)]
pub enum InternalCommandError {
    NotFound,
}

pub fn exit_command(ctx: &mut InternalCommandContext) -> ExitStatus {
    let exit_with = if let Some(exit_with) = ctx.argv.get(1) {
        exit_with.parse().unwrap_or(1)
    } else {
        0
    };

    process::exit(exit_with);
}

pub fn cd_command(ctx: &mut InternalCommandContext) -> ExitStatus {
    trace!("cd: {:?}", ctx.argv);
    let dir = match ctx.argv.get(1) {
        Some(dir) => {
            if dir.starts_with('/') {
                dir.clone()
            } else {
                let current_dir = env::current_dir().expect("failed to getcwd()");
                Path::new(&current_dir)
                    .join(dir.clone())
                    .to_string_lossy()
                    .into_owned()
            }
        }
        None => {
            if let Some(home_dir) = dirs::home_dir() {
                home_dir.to_string_lossy().into_owned()
            } else {
                String::from("/")
            }
        }
    };

    if env::set_current_dir(&dir).is_ok() {
        ExitStatus::ExitedWith(0)
    } else {
        error!("failed to cd into {}", dir);
        ExitStatus::ExitedWith(1)
    }
}

pub fn echo_command(ctx: &mut InternalCommandContext) -> ExitStatus {
    let (newline, skip) = match ctx.argv.get(1).map(|s| s.as_str()) {
        Some("-n") => (false, 2),
        _ => (true, 1),
    };

    for (i, arg) in ctx.argv.iter().skip(skip).enumerate() {
        if i > 0 {
            write!(ctx.stdout, " {}", arg).ok();
        } else {
            write!(ctx.stdout, "{}", arg).ok();
        }
    }

    if newline {
        write!(ctx.stdout, "\n").ok();
    }
    ctx.stdout.flush().ok();
    ExitStatus::ExitedWith(0)
}

pub fn source_command(ctx: &mut InternalCommandContext) -> ExitStatus {
    if let Some(filepath) = ctx.argv.get(1) {
        exec_file(ctx.scope, std::path::PathBuf::from(&filepath))
    } else {
        write!(ctx.stderr, "nsh: source: filename argument required\n").ok();
        ctx.stderr.flush().ok();
        ExitStatus::ExitedWith(0)
    }
}

pub fn export_command(ctx: &mut InternalCommandContext) -> ExitStatus {
    if let Some(name) = ctx.argv.get(1) {
        ctx.scope.export(&name);
    } else {
        for name in ctx.scope.exported_names() {
            if let Some(var) = ctx.scope.get(name) {
                write!(ctx.stdout, "{}={}\n", name, var.value()).ok();
            }
        }
    }

    ExitStatus::ExitedWith(0)
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
        commands.insert("alias", alias_command);
        commands.insert("echo", echo_command);
        commands.insert("cd", cd_command);
        commands.insert("source", source_command);
        commands.insert("exit", exit_command);
        commands.insert("export", export_command);
        commands.insert(
            "get-xkcd-true-random-number-chosen-by-fair-dice-roll",
            xkcd_rand_command,
        );
        commands
    };
}

pub fn run_internal_command(
    scope: &mut Scope,
    argv: &[String],
    stdin: RawFd,
    stdout: RawFd,
    stderr: RawFd
) -> Result<ExitStatus, InternalCommandError> {

    let mut ctx = InternalCommandContext {
        argv,
        scope,
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
