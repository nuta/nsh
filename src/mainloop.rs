use rustyline::error::ReadlineError;
use std::env;
use std::process;
use rustyline;
use exec;
use parser;
use prompt::{parse_prompt, draw_prompt};

static DEFAULT_PROMPT: &'static str = "\\W $ ";

fn read_line_from_stdin() -> String {
    let mut rl = rustyline::Editor::<()>::new();

    // Parse and print the prompt.
    let mut prompt_str = String::new();
    let ps1 = env::var("PS1").unwrap_or(DEFAULT_PROMPT.to_owned());
    if let Ok(fmt) = parse_prompt(ps1.as_str()) {
        draw_prompt(&fmt, &mut prompt_str);
    }

    // Read a line.
    match rl.readline(&prompt_str) {
        Ok(line) => {
            return line;
        }
        Err(ReadlineError::Eof) => process::exit(0),
        Err(err) => {
            panic!("something went wrong with readline: {:?}", err);
        }
    }
}

pub fn mainloop() {
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