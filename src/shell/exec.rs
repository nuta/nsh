use std::{ffi::CString, os::unix::prelude::RawFd};

use nsh_parser::ast::{Assignment, Redirection};

use anyhow::Result;
use nix::{
    libc::TCSADRAIN,
    sys::{
        signal::{sigaction, SaFlags, SigAction, SigHandler, SigSet, Signal},
        termios::{tcsetattr, SetArg, Termios},
    },
    unistd::{close, dup2, execv, fork, getpid, setpgid, tcsetpgrp, ForkResult, Pid},
};

use crate::shell::Shell;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Pgid(usize);

/// The exit status or reason why the command exited.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum ExitStatus {
    ExitedWith(i32),
    Running(Pid /* pgid */),
    Break,
    Continue,
    Return,
    // The command is not executed because of `noexec`.
    NoExec,
}

/// The process execution environment.
pub struct Context<'a> {
    pub shell: &'a mut Shell,
    pub stdin: RawFd,
    pub stdout: RawFd,
    pub stderr: RawFd,
    pub pgid: Option<Pid>,
    /// The process should be executed in background.
    pub background: bool,
    /// Is the shell interactive?
    pub interactive: bool,
}

fn move_fd(src: RawFd, dst: RawFd) {
    if src != dst {
        dup2(src, dst).expect("failed to dup2");
        close(src).expect("failed to close");
    }
}

/// Put the given pgid (job) into the foreground.
fn set_terminal_process_group(pgid: Pid) {
    tcsetpgrp(0, pgid).expect("failed to tcsetpgrp");
}

/// Spawn a child process and execute a command.
pub fn run_external_command(
    ctx: &Context<'_>,
    argv: &[String],
    redirects: &[Redirection],
    assignments: &[Assignment],
) -> Result<ExitStatus> {
    let mut fds = Vec::new();
    for r in redirects {
        // TODO:
        // match r.target {
        //     parser::RedirectionType::Fd(ref fd) => {
        //         fds.push((*fd, r.fd as RawFd));
        //     }
        //     parser::RedirectionType::File(ref wfilepath) => {
        //         let mut options = OpenOptions::new();
        //         match &r.direction {
        //             parser::RedirectionDirection::Input => {
        //                 options.read(true);
        //             }
        //             parser::RedirectionDirection::Output => {
        //                 options.write(true).create(true);
        //             }
        //             parser::RedirectionDirection::Append => {
        //                 options.write(true).append(true);
        //             }
        //         };

        //         trace!("redirection: options={:?}", options);
        //         let filepath = expand_word_into_string(shell, wfilepath)?;
        //         if let Ok(file) = options.open(&filepath) {
        //             fds.push((file.into_raw_fd(), r.fd as RawFd))
        //         } else {
        //             warn!("failed to open file: `{}'", filepath);
        //             return Ok(ExitStatus::ExitedWith(1));
        //         }
        //     }
        //     parser::RedirectionType::HereDoc(ref heredoc) => {
        //         fds.push((evaluate_heredoc(shell, heredoc)?, r.fd as RawFd))
        //     }
        //     parser::RedirectionType::UnresolvedHereDoc(_) => {
        //         // must be resolved in the parser
        //         unreachable!()
        //     }
        // }
    }

    // Use provided (e.g. pipeline) stdin/stdout/stderr if no redirections speicfied.
    if !fds.iter().any(|(_, dst)| *dst == 0) {
        fds.push((ctx.stdin, 0));
    }
    if !fds.iter().any(|(_, dst)| *dst == 1) {
        fds.push((ctx.stdout, 1));
    }
    if !fds.iter().any(|(_, dst)| *dst == 2) {
        fds.push((ctx.stderr, 2));
    }

    // Determine the absolute path of the command.
    let argv0 = if argv[0].starts_with('/') || argv[0].starts_with("./") {
        CString::new(argv[0].as_str())?
    } else {
        match ctx.shell.path_table().lookup(&argv[0]) {
            Some(path) => CString::new(path.to_str().unwrap().clone())?,
            None => {
                error!("command not found `{}'", argv[0]);
                return Ok(ExitStatus::ExitedWith(1));
            }
        }
    };

    // Construct CString argv.
    let mut args = Vec::new();
    for arg in argv {
        args.push(CString::new(arg.clone())?);
    }

    // Spawn a child.
    match unsafe { fork() }.expect("failed to fork") {
        ForkResult::Parent { child } => Ok(ExitStatus::Running(child)),
        ForkResult::Child => {
            // Create or join a process group.
            if ctx.interactive {
                let pid = getpid();
                let pgid = match ctx.pgid {
                    Some(pgid) => {
                        setpgid(pid, pgid).expect("failed to setpgid");
                        pgid
                    }
                    None => {
                        setpgid(pid, pid).expect("failed to setpgid");
                        pid
                    }
                };

                if !ctx.background {
                    set_terminal_process_group(pgid);
                    ctx.shell.restore_termios();
                }

                // Accept job-control-related signals (refer https://www.gnu.org/software/libc/manual/html_node/Launching-Jobs.html)
                let action = SigAction::new(SigHandler::SigDfl, SaFlags::empty(), SigSet::empty());
                unsafe {
                    sigaction(Signal::SIGINT, &action).expect("failed to sigaction");
                    sigaction(Signal::SIGQUIT, &action).expect("failed to sigaction");
                    sigaction(Signal::SIGTSTP, &action).expect("failed to sigaction");
                    sigaction(Signal::SIGTTIN, &action).expect("failed to sigaction");
                    sigaction(Signal::SIGTTOU, &action).expect("failed to sigaction");
                    sigaction(Signal::SIGCHLD, &action).expect("failed to sigaction");
                }
            }

            // Initialize stdin/stdout/stderr and redirections.
            for (src, dst) in fds {
                move_fd(src, dst);
            }

            // Set exported variables.
            // TODO:
            // for name in shell.exported_names() {
            // if let Some(var) = shell.get(name) {
            //     std::env::set_var(name, var.as_str());
            // }
            // }

            // Load assignments.
            for assignment in assignments {
                // TODO:
                // let value = evaluate_initializer(shell, &assignment.initializer)
                //     .expect("failed to evaluate the initializer");
                // match value {
                //     Value::String(s) => std::env::set_var(&assignment.name, s),
                //     Value::Array(_) => {
                //         error!("Array assignments in a command is not supported.");
                //         std::process::exit(1);
                //     }
                //     Value::Function(_) => (),
                // }
            }

            let args: Vec<&std::ffi::CStr> = args.iter().map(|s| s.as_c_str()).collect();
            match execv(&argv0, &args) {
                Ok(_) => {
                    unreachable!();
                }
                Err(nix::errno::Errno::EACCES) => {
                    error!("Failed to exec {:?} (EACCESS). chmod(1) may help.", &argv0);
                    std::process::exit(1);
                }
                Err(err) => {
                    error!("Failed to exec {:?} ({})", &argv0, err);
                    std::process::exit(1);
                }
            }
        }
    }
}
