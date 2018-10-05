#![recursion_limit = "256"]
#[macro_use]
extern crate log;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate nom;
extern crate pretty_env_logger;
extern crate rustyline;
use rustyline::error::ReadlineError;
use std::process;
mod exec;
mod parser;

pub fn read_line_from_stdin() -> String {
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

    loop {
        mainloop();
    }
}
