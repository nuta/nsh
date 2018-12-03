#![recursion_limit = "256"]
#![feature(proc_macro_hygiene, decl_macro)]

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
#[macro_use]
extern crate rocket;
extern crate serde;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;

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
mod config;

use std::path::{Path, PathBuf};
use std::time::SystemTime;
use std::process;
use std::sync::{Arc, Mutex};
use structopt::StructOpt;
use nix::unistd::{getpid, setpgid, tcsetpgrp};
use nix::sys::signal::{SigHandler, SigAction, SaFlags, SigSet, Signal, sigaction};
use crate::exec::ExitStatus;
use crate::config::Config;

fn interactive_mode(config: &Config, raw_isolate: exec::Isolate) -> ExitStatus {
    let isolate_lock = Arc::new(Mutex::new(raw_isolate));
    let isolate_lock2 = isolate_lock.clone();

    //Evaluate ~/.nshrc asynchronously since it may take too long.
    let nshrc_loader = std::thread::spawn(move || {
        let mut isolate = isolate_lock2.lock().unwrap();
        if let Some(home_dir) = dirs::home_dir() {
            let nshrc_path = Path::new(&home_dir).join(".nshrc");
            if nshrc_path.exists() {
                isolate.run_file(nshrc_path);
            }
        }
    });

    // Read a line.
    let mut line = match input::input(config) {
        Ok(line) => {
            println!();
            line
        }
        Err(input::InputError::Eof) => {
            return ExitStatus::ExitedWith(0);
        }
    };

    // Now we have to execute the first command from the prompt. Wait for the
    // nshrc loader to finish and enter the main loop.
    nshrc_loader.join().unwrap();
    let mut isolate = isolate_lock.lock().unwrap();

    loop {
        trace!("line {}", line);
        isolate.run_str(&line);

        // Read the next line.
        line = match input::input(config) {
            Ok(line) => {
                println!();
                line
            }
            Err(input::InputError::Eof) => {
                return ExitStatus::ExitedWith(0);
            }
        };
    }
}

static mut GLOBAL_LOGGER: Option<slog_scope::GlobalLoggerGuard> = None;

fn init_log() {
    use slog::Drain;
    use std::fs::OpenOptions;

    let mut log_file_path = dirs::home_dir().unwrap();
    log_file_path.push(".nsh.log");

    let file = OpenOptions::new()
        .create(true)
        .truncate(false)
        .append(true)
        .open(log_file_path)
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
    /// Open the web-based configuration tool.
    #[structopt(long = "config")]
    open_config: bool,

    /// Run the given string.
    #[structopt(short = "c")]
    command: Option<String>,

    /// Run the file.
    #[structopt(name = "FILE", parse(from_os_str))]
    file: Option<PathBuf>,
}


pub static mut TIME_STARTED: Option<SystemTime> = None;

/// The entry point. In the intreactive mode, we utilize asynchronous initializaiton
/// (background worker threads) to render the prompt as soon as possible for hunan.
fn main() {
    init_log();
    let opt = Opt::from_args();

    if opt.open_config {
        config::main();
        return;
    }

    unsafe {
        TIME_STARTED = Some(SystemTime::now());
    }

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

    // Load ~/.nshconfig.
    let home_dir = dirs::home_dir().unwrap();
    let nshconfig_path = Path::new(&home_dir).join(".nshconfig");
    let config: config::Config = match std::fs::File::open(nshconfig_path) {
        Ok(file) => serde_json::from_reader(file).expect("failed to parse ~/.nshrc"),
        Err(_) => {
            // Use default values.
            serde_json::from_str("{}").expect("failed to parse '{}'")
        }
    };

    worker::start_worker_threads();
    path::init(&config);
    history::init(&config);

    let mut isolate = exec::Isolate::new(config.clone(), getpid(), interactive);
    let status = match (opt.command, opt.file) {
        (Some(command), _) => isolate.run_str(&command),
        (_, Some(file)) => isolate.run_file(file),
        (_, _) => interactive_mode(&config, isolate),
    };

    match status {
        ExitStatus::ExitedWith(status) => process::exit(status),
        ExitStatus::NoExec=> process::exit(0),
        _ => panic!("unexpected {:?}", status),
    }
}
