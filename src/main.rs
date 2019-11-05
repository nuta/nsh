#![cfg_attr(test, feature(test))]

#[macro_use]
extern crate log;
#[macro_use]
extern crate lazy_static;
extern crate structopt;
extern crate dirs;
extern crate nix;
extern crate termion;
extern crate glob;
#[macro_use]
extern crate failure;
extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate backtrace;

#[cfg(test)] #[macro_use] extern crate pretty_assertions;
#[cfg(test)] extern crate test;

mod logger;
mod builtins;
mod completion;
mod exec;
mod pattern;
mod input;
mod parser;
mod context_parser;
mod path;
mod prompt;
mod syntax_highlighting;
mod utils;
mod history;
mod fuzzy;
mod variable;
mod doctor;

use std::process;
use std::path::PathBuf;
use structopt::StructOpt;
use nix::sys::signal::{SigHandler, SigAction, SaFlags, SigSet, Signal, sigaction};
use termion::is_tty;
use crate::exec::ExitStatus;

fn interactive_mode(mut isolate: exec::Isolate) -> ExitStatus {

    // Ignore job-control-related signals in order not to stop the shell.
    // (refer https://www.gnu.org/software/libc/manual)
    // Don't ignore SIGCHLD! If you ignore it waitpid(2) returns ECHILD.
    let action = SigAction::new(SigHandler::SigIgn, SaFlags::empty(), SigSet::empty());
    unsafe {
        sigaction(Signal::SIGINT, &action).expect("failed to sigaction");
        sigaction(Signal::SIGQUIT, &action).expect("failed to sigaction");
        sigaction(Signal::SIGTSTP, &action).expect("failed to sigaction");
        sigaction(Signal::SIGTTIN, &action).expect("failed to sigaction");
        sigaction(Signal::SIGTTOU, &action).expect("failed to sigaction");
    }

    // Render the prompt and wait for an user input.
    let mut line = match input::input(&mut isolate) {
        Ok(line) => {
            println!();
            line
        }
        Err(input::InputError::Eof) => {
            return ExitStatus::ExitedWith(0);
        }
    };

    loop {
        isolate.run_str(&line);
        isolate.check_background_jobs();

        // Read the next line.
        line = match input::input(&mut isolate) {
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

pub fn load_nshrc() -> String {
    use std::path::Path;
    use std::io::Read;

    let home_dir = dirs::home_dir().unwrap();
    let nshrc_path = Path::new(&home_dir).join(".nshrc");
    let mut nshrc = String::with_capacity(2048);
    let mut file = match std::fs::File::open(nshrc_path) {
        Ok(file) => file,
        Err(_) => return String::new(),
    };
    
    file.read_to_string(&mut nshrc).expect("failed to load ~/.nshrc");

    nshrc
}

fn shell_main(opt: Opt) {
    // Load and execute nshrc.
    let mut isolate = exec::Isolate::new();
    let nshrc = load_nshrc();
    isolate.run_str(&nshrc);

    // Initialize subsystems.
    history::init();

    let stdout = std::fs::File::create("/dev/stdout").unwrap();
    isolate.set_interactive(is_tty(&stdout) && opt.command.is_none()
        && opt.file.is_none());
    let status = match (opt.command, opt.file) {
        (Some(command), _) => {
            isolate.run_str(&command)
        },
        (_, Some(file)) => {
            isolate.set_script_name(file.to_str().unwrap());
            isolate.run_file(file)
        },
        (_, _) => {
            if !isolate.interactive() {
                eprintln!("nsh: warning: stdout is not a tty");
                process::exit(0);
            }

            interactive_mode(isolate)
        },
    };

    match status {
        ExitStatus::ExitedWith(status) => process::exit(status),
        ExitStatus::Running(_) => {
            eprintln!("nsh: warning: some jobs are running in background");
            process::exit(0);
        },
        ExitStatus::NoExec=> process::exit(0),
        _ => panic!("unexpected {:?}", status),
    }
}

#[derive(StructOpt, Debug)]
#[structopt(name="nsh", about="A command-line shell that focuses on performance and productivity.")]
struct Opt {
    /// Check your terminal environment.
    #[structopt(long = "doctor")]
    doctor: bool,

    /// Run the given string.
    #[structopt(short = "c")]
    command: Option<String>,

    /// Behave like a login shell.
    #[structopt(short = "l", long = "login")]
    login: bool,

    /// Run the file.
    #[structopt(name = "FILE", parse(from_os_str))]
    file: Option<PathBuf>,
}

fn main() {
    logger::init();
    let opt = Opt::from_args();

    if opt.doctor {
        doctor::main();
        return;
    }

    // Dump the panic reason and backtrace into the log file.
    std::panic::set_hook(Box::new(|info| {
        error!("{}", info);
        error!("{:#?}", backtrace::Backtrace::new());

        eprintln!("{}", info);
        eprintln!("{:#?}", backtrace::Backtrace::new());
        eprintln!("nsh: Something went wrong. Check out ~/.nsh.log and please file this bug on GitHub: https://github.com/seiyanuta/nsh/issues")
    }));

    shell_main(opt);
}
