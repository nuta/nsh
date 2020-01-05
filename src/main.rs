#![cfg_attr(test, feature(test))]

#[macro_use]
extern crate log;
#[macro_use]
extern crate lazy_static;
extern crate dirs;
extern crate glob;
extern crate nix;
extern crate structopt;
extern crate termion;
#[macro_use]
extern crate failure;
extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate backtrace;

#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;
#[cfg(test)]
extern crate test;

mod builtins;
mod completion;
mod context_parser;
mod eval;
mod expand;
mod fuzzy;
mod history;
mod input;
mod logger;
mod parser;
mod path;
mod pattern;
mod process;
mod prompt;
mod shell;
mod syntax_highlighting;
mod utils;
mod variable;

use crate::process::{check_background_jobs, ExitStatus};
use nix::sys::signal::{sigaction, SaFlags, SigAction, SigHandler, SigSet, Signal};
use std::path::PathBuf;
use std::process::exit;
use structopt::StructOpt;
use termion::is_tty;

fn interactive_mode(mut shell: crate::shell::Shell) -> ExitStatus {
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
    let mut line = match input::input(&mut shell) {
        Ok(line) => {
            println!();
            line
        }
        Err(input::InputError::Eof) => {
            return ExitStatus::ExitedWith(0);
        }
    };

    loop {
        shell.run_str(&line);
        check_background_jobs(&mut shell);

        // Read the next line.
        line = match input::input(&mut shell) {
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
    use std::io::Read;
    use std::path::Path;

    let home_dir = dirs::home_dir().unwrap();
    let nshrc_path = Path::new(&home_dir).join(".nshrc");
    let mut nshrc = String::with_capacity(2048);
    let mut file = match std::fs::File::open(nshrc_path) {
        Ok(file) => file,
        Err(_) => return String::new(),
    };

    file.read_to_string(&mut nshrc)
        .expect("failed to load ~/.nshrc");

    nshrc
}

const DEFAULT_PATH: &str = "/sbin:/usr/sbin:/usr/local/sbin:/bin:/usr/bin:/usr/local/bin";

fn shell_main(opt: Opt) {
    // Load and execute nshrc.
    let mut shell = crate::shell::Shell::new();
    let nshrc = load_nshrc();
    shell.run_str(&nshrc);

    if shell.get("PATH").is_none() {
        crate::path::reload_paths(DEFAULT_PATH);
    }

    // Initialize subsystems.
    history::init();

    let stdout = std::fs::File::create("/dev/stdout").unwrap();
    shell.set_interactive(is_tty(&stdout) && opt.command.is_none() && opt.file.is_none());
    let status = match (opt.command, opt.file) {
        (Some(command), _) => shell.run_str(&command),
        (_, Some(file)) => {
            shell.set_script_name(file.to_str().unwrap());
            shell.run_file(file)
        }
        (_, _) => {
            if !shell.interactive() {
                eprintln!("nsh: warning: stdout is not a tty");
                exit(0);
            }

            interactive_mode(shell)
        }
    };

    match status {
        ExitStatus::ExitedWith(status) => exit(status),
        ExitStatus::Running(_) => {
            eprintln!("nsh: warning: some jobs are running in background");
            exit(0);
        }
        ExitStatus::NoExec => exit(0),
        _ => panic!("unexpected {:?}", status),
    }
}

#[derive(StructOpt, Debug)]
#[structopt(
    name = "nsh",
    about = "A command-line shell that focuses on performance and productivity."
)]
struct Opt {
    /// The command to be executed.
    #[structopt(short = "c")]
    command: Option<String>,
    /// Behave like a login shell.
    #[structopt(short = "l", long = "login")]
    login: bool,
    /// The file to be executed.
    #[structopt(name = "FILE", parse(from_os_str))]
    file: Option<PathBuf>,
}

fn main() {
    logger::init();
    let opt = Opt::from_args();

    // Dump the panic reason and backtrace into the log file.
    std::panic::set_hook(Box::new(|info| {
        error!("{}", info);
        error!("{:#?}", backtrace::Backtrace::new());

        eprintln!("{}", info);
        eprintln!("{:#?}", backtrace::Backtrace::new());
        eprintln!("nsh: Something went wrong. Check out ~/.nsh.log and please file this bug on GitHub: https://github.com/nuta/nsh/issues")
    }));

    shell_main(opt);
}
