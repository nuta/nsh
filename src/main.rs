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
extern crate clap;
extern crate dirs;
extern crate nix;
extern crate syntect;
extern crate termion;
extern crate crossbeam;
extern crate crossbeam_channel;

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

use clap::{App, Arg};
use std::fs::File;
use std::io::prelude::*;
use std::path::{Path, PathBuf};
use std::time::SystemTime;

fn exec_file(script_file: &str) {
    let mut f = File::open(script_file).expect("failed to open a file");
    let mut script = String::new();
    f.read_to_string(&mut script)
        .expect("failed to load a file");

    match parser::parse_line(script.as_str()) {
        Ok(cmd) => {
            exec::exec(&cmd);
        }
        Err(parser::SyntaxError::Empty) => (), // Just ignore.
        Err(err) => {
            eprintln!("nsh: parse error: {:?}", err);
        }
    }
}

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

fn interactive_mode() {
    // Eval nshrc.
    if let Some(home_dir) = dirs::home_dir() {
        let nshrc_path = Path::new(&home_dir).join(".nshrc");
        if nshrc_path.exists() {
            exec_file(nshrc_path.to_str().unwrap());
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
                return;
            }
        };

        match parser::parse_line(line.as_str()) {
            Ok(cmd) => {
                exec::exec(&cmd);
            }
            Err(parser::SyntaxError::Empty) => (), // Just ignore.
            Err(err) => {
                eprintln!("nsh: parse error: {:?}", err);
            }
        };
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

pub static mut TIME_STARTED: Option<SystemTime> = None;

fn main() {
    unsafe {
        TIME_STARTED = Some(SystemTime::now());
    }

    init_log();
    worker::start_worker_threads();
    path::init();
    history::init();

    let args = App::new("nsh")
        .version("0.0.0")
        .author("Seiya Nuta <nuta@seiya.me>")
        .about("A command-line shell that focuses on performance and productivity.")
        .arg(
            Arg::with_name("script")
                .help("The shell script file.")
                .required(false)
                .index(1),
        )
        .get_matches();

    if let Some(script) = args.value_of("script") {
        exec_file(script);
    } else {
        // Interactive mode.
        interactive_mode();
    }
}
