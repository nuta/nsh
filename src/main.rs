#![recursion_limit = "256"]

#[macro_use]
extern crate slog;
extern crate slog_scope;
extern crate slog_stdlog;
extern crate slog_term;
#[macro_use]
extern crate log;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate nom;
extern crate structopt;
extern crate dirs;
extern crate nix;
extern crate syntect;
extern crate termion;
extern crate crossbeam;
extern crate crossbeam_channel;
extern crate glob;
extern crate globset;

mod builtins;
mod completion;
mod exec;
mod input;
mod parser;
mod path;
mod worker;
mod prompt;
mod utils;
mod history;
mod fuzzy;
mod variable;

use std::path::{Path, PathBuf};
use std::time::SystemTime;
use std::process;
use structopt::StructOpt;
use nix::unistd::{getpid, setpgid, tcsetpgrp};
use nix::sys::signal::{SigHandler, SigAction, SaFlags, SigSet, Signal, sigaction};
use crate::exec::ExitStatus;

fn interactive_mode(isolate: &mut exec::Isolate) -> ExitStatus {
    // Eval nshrc.
    if let Some(home_dir) = dirs::home_dir() {
        let nshrc_path = Path::new(&home_dir).join(".nshrc");
        if nshrc_path.exists() {
            isolate.run_file(nshrc_path);
        }
    }

    loop {
        // Read a line.
        let line = match input::input() {
            Ok(line) => {
                println!();
                line
            }
            Err(input::InputError::Eof) => {
                return ExitStatus::ExitedWith(0);
            }
        };

        isolate.run_str(&line);
    }
}

static mut GLOBAL_LOGGER: Option<slog_scope::GlobalLoggerGuard> = None;

fn init_log() {
    use slog::Drain;
    use std::fs::OpenOptions;

    let file = OpenOptions::new()
        .create(true)
        .truncate(false)
        .append(true)
        .open("nsh.log")
        .unwrap();

    let decorator = slog_term::PlainSyncDecorator::new(file);
    let drain = slog_term::FullFormat::new(decorator).build().fuse();
    let logger = slog::Logger::root(drain, o!());
    let guard = slog_scope::set_global_logger(logger);
    unsafe {
        GLOBAL_LOGGER = Some(guard);
    }
    slog_stdlog::init().unwrap();
}

#[derive(StructOpt, Debug)]
#[structopt(name="nsh", about="A command-line shell that focuses on performance and productivity.")]
struct Opt {
    /// admin_level to consider
    #[structopt(short="c")]
    command: Option<String>,

    /// Files to process
    #[structopt(name="FILE", parse(from_os_str))]
    file: Option<PathBuf>,
}


pub static mut TIME_STARTED: Option<SystemTime> = None;

fn main() {
    let opt = Opt::from_args();
    let interactive = opt.command.is_none() && opt.file.is_none();

    if interactive {
        // Create a process group.
        let pid = getpid();
        setpgid(pid, pid).expect("failed to setpgid");
        tcsetpgrp(0 /* stdin */, pid).expect("failed to tcsetpgrp");

        // Ignore job-control-related signals in order not to stop the shell.
        // (refer https://www.gnu.org/software/libc/manual)
        let action = SigAction::new(SigHandler::SigIgn, SaFlags::empty(), SigSet::empty());
        unsafe {
            sigaction(Signal::SIGINT, &action).expect("failed to sigaction");
            sigaction(Signal::SIGQUIT, &action).expect("failed to sigaction");
            sigaction(Signal::SIGTSTP, &action).expect("failed to sigaction");
            sigaction(Signal::SIGTTIN, &action).expect("failed to sigaction");
            sigaction(Signal::SIGTTOU, &action).expect("failed to sigaction");
            // Don't ignore SIGCHLD! If you do so waitpid(2) returns ECHILD.
            // sigaction(Signal::SIGCHLD, &action).expect("failed to sigaction");
        }
    }

    unsafe {
        TIME_STARTED = Some(SystemTime::now());
    }

    init_log();

    worker::start_worker_threads();
    path::init();
    history::init();

    let mut isolate = exec::Isolate::new(getpid(), interactive);
    let status = match (opt.command, opt.file) {
        (Some(command), _) => isolate.run_str(&command),
        (_, Some(file)) => isolate.run_file(file),
        (_, _) => interactive_mode(&mut isolate),
    };

    match status {
        ExitStatus::ExitedWith(status) => process::exit(status),
        ExitStatus::NoExec=> process::exit(0),
        _ => panic!("unexpected {:?}", status),
    }
}
