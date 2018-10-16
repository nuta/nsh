#![recursion_limit = "256"]
#[macro_use]
extern crate log;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate nom;
extern crate clap;
extern crate nix;
extern crate pretty_env_logger;
extern crate rustyline;
use clap::{App, Arg};
use rustyline::error::ReadlineError;
use std::fs::File;
use std::io::prelude::*;
use std::process;
mod path_loader;
mod exec;
mod parser;

fn read_line_from_stdin() -> String {
    let mut rl = rustyline::Editor::<()>::new();
    match rl.readline("nsh$ ") {
        Ok(line) => {
            return line;
        }
        Err(ReadlineError::Eof) => process::exit(0),
        Err(err) => {
            panic!("something went wrong with readline: {:?}", err);
        }
    }
}

fn exec_file(script_file: &str) {
    let mut f = File::open(script_file).expect("failed to open a file");
    let mut script = String::new();
    f.read_to_string(&mut script)
        .expect("failed to load a file");

    match parser::parse_line(script.as_str()) {
        Ok(cmd) => {
            exec::exec(cmd);
        }
        Err(err) => {
            eprintln!("nsh: parse error: {:?}", err);
        }
    }
}

fn mainloop() {
    let line = read_line_from_stdin();
    let cmd = match parser::parse_line(line.as_str()) {
        Ok(cmd) => cmd,
        Err(err) => {
            eprintln!("nsh: parse error: {:?}", err);
            return;
        }
    };

    exec::exec(cmd);
}

fn main() {
    pretty_env_logger::init();
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
        loop {
            mainloop();
        }
    }
}
