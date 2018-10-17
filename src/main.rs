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

mod mainloop;
mod path_loader;
mod exec;
mod parser;
mod internals;
mod alias;
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
            exec::exec(cmd);
        }
        Err(err) => {
            eprintln!("nsh: parse error: {:?}", err);
        }
    }
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
        mainloop::mainloop();
    }
}
