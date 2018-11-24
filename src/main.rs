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
extern crate nix;
extern crate dirs;

mod mainloop;
mod path_loader;
mod exec;
mod parser;
mod internals;
mod alias;
mod builtins;
mod prompt;

use clap::{App, Arg};
use std::fs::File;
use std::io::prelude::*;

fn exec_file(script_file: &str) {
    let mut f = File::open(script_file).expect("failed to open a file");
    let mut script = String::new();
    f.read_to_string(&mut script)
        .expect("failed to load a file");

    match parser::parse_line(script.as_str()) {
        Ok(cmd) => {
            exec::exec(&cmd);
        }
        Err(err) => {
            eprintln!("nsh: parse error: {:?}", err);
        }
    }
}

static mut GLOBAL_LOGGER: Option<slog_scope::GlobalLoggerGuard> = None;
fn init_log() {
    use std::fs::OpenOptions;
    use slog::Drain;

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

fn main() {
    init_log();
    path_loader::init();

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
        mainloop::mainloop();
    }
}
