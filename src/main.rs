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

mod bash_server;
mod builtins;
mod context_parser;
mod eval;
mod expand;
mod fuzzy;
mod history;
mod logger;
mod mainloop;
mod parser;
mod path;
mod pattern;
mod process;
mod prompt;
mod shell;
mod syntax_highlighting;
mod utils;
mod variable;

use crate::process::ExitStatus;
use crate::variable::Value;
use nix::sys::signal::{sigaction, SaFlags, SigAction, SigHandler, SigSet, Signal};
use std::fs::File;
use std::path::{Path, PathBuf};
use std::process::exit;
use structopt::StructOpt;
use termion::is_tty;

fn interactive_mode(shell: shell::Shell) -> ExitStatus {
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

    mainloop::Mainloop::new(shell).run()
}

const DEFAULT_PATH: &str = "/sbin:/usr/sbin:/usr/local/sbin:/bin:/usr/bin:/usr/local/bin";

fn shell_main(opt: Opt) {
    // Create a history file if it does not exist.
    let home_dir = dirs::home_dir()
        .expect("failed to get the path to the home directory");
    let history_path = Path::new(&home_dir).join(".nsh_history");
    if !history_path.exists() {
        File::create(&history_path).unwrap();
    }

    let mut shell = crate::shell::Shell::new(&history_path);

    if !opt.norc {
        // Try executing ~/.nshrc
        let home_dir = dirs::home_dir().unwrap();
        shell.run_file(home_dir.join(".nshrc")).ok();

        // Try executing $XDG_CONFIG_HOME/nsh/nshrc
        let config_dir = std::env::var("XDG_CONFIG_HOME")
            .map(|dir| PathBuf::from(dir))
            .unwrap_or_else(|_| home_dir.join(".config"));
        shell.run_file(config_dir.join("nsh").join("nshrc")).ok();
    }

    if shell.get("PATH").is_none() {
        shell.set("PATH", Value::String(DEFAULT_PATH.to_owned()), false);
    }

    let stdout = std::fs::File::create("/dev/stdout").unwrap();
    shell.set_interactive(is_tty(&stdout) && opt.command.is_none() && opt.file.is_none());
    let status = match (opt.command, opt.file) {
        (Some(command), _) => shell.run_str(&command),
        (_, Some(file)) => {
            shell.set_script_name(file.to_str().unwrap());
            shell.run_file(file).expect("failed to open the file")
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
    /// Print the version.
    #[structopt(long = "version")]
    version: bool,
    /// Behave like a login shell.
    #[structopt(short = "l", long = "login")]
    login: bool,
    /// The file to be executed.
    #[structopt(name = "FILE", parse(from_os_str))]
    file: Option<PathBuf>,
    /// Don't load nshrc.
    #[structopt(long = "norc")]
    norc: bool,
}

fn main() {
    logger::init();

    // Dump the panic reason and backtrace into the log file.
    std::panic::set_hook(Box::new(|info| {
        error!("{}", info);
        error!("{:#?}", backtrace::Backtrace::new());

        eprintln!("{}", info);
        eprintln!("{:#?}", backtrace::Backtrace::new());
        eprintln!("nsh: Something went wrong. Check out ~/.nsh.log and please file this bug on GitHub: https://github.com/nuta/nsh/issues")
    }));

    let opt = Opt::from_args();
    if opt.version {
        println!("{}", env!("CARGO_PKG_VERSION"));
        exit(0);
    }

    shell_main(opt);
}
