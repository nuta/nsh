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

mod alias;
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

use std::fs::File;
use std::path::{Path, PathBuf};
use std::time::SystemTime;
use std::sync::Mutex;
use std::process;
use structopt::StructOpt;
use crate::exec::ExitStatus;

fn resolve_and_create_history_file() -> Option<PathBuf> {
    if let Some(home_dir) = dirs::home_dir() {
        let history_path = Path::new(&home_dir).join(".nsh_history");
        if history_path.exists() {
            return Some(history_path);
        }

        if File::create(&history_path).is_ok() {
            return Some(history_path);
        }
    }

    None
}

fn interactive_mode(scope: &mut exec::Scope) -> ExitStatus {
    // Eval nshrc.
    if let Some(home_dir) = dirs::home_dir() {
        let nshrc_path = Path::new(&home_dir).join(".nshrc");
        if nshrc_path.exists() {
            exec::exec_file(scope, nshrc_path);
        }
    }

    // Load histories.
    let history_path = resolve_and_create_history_file();
    if let Some(ref _path) = history_path {
        // TODO: load history
    } else {
        warn!("disabling history");
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

        exec::exec_str(scope, &line);
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

lazy_static! {
    static ref CONTEXT: Mutex<exec::Scope> = Mutex::new(exec::Scope::new());
}

fn main() {
    unsafe {
        TIME_STARTED = Some(SystemTime::now());
    }

    init_log();
    worker::start_worker_threads();
    path::init();
    history::init();
    let opt = Opt::from_args();

    let scope = &mut CONTEXT.lock().unwrap();
    let status = match (opt.command, opt.file) {
        (Some(command), _) => exec::exec_str(scope, &command),
        (_, Some(file)) => exec::exec_file(scope, file),
        (_, _) => interactive_mode(scope),
    };

    match status {
        ExitStatus::ExitedWith(status) => process::exit(status),
        _ => unreachable!(),
    }
}
