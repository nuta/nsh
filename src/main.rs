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
mod config;
mod doctor;

use std::process;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::os::unix::io::IntoRawFd;
use structopt::StructOpt;
use nix::sys::signal::{SigHandler, SigAction, SaFlags, SigSet, Signal, sigaction};
use nix::unistd;
use std::os::unix::io::FromRawFd;
use std::io::prelude::*;
use termion::is_tty;
use crate::exec::ExitStatus;
use crate::config::Config;
use crate::variable::Value;

fn interactive_mode(config: &Config, raw_isolate: exec::Isolate) -> ExitStatus {
    let isolate_lock = Arc::new(Mutex::new(raw_isolate));

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

    //Evaluate rc scripts asynchronously since it may take too long.
    let rc = config.rc.clone();
    let isolate_lock2 = isolate_lock.clone();
    let (pipe_out, pipe_in) = unistd::pipe().expect("failed to create a pipe");
    let nshrc_loader = std::thread::spawn(move || {
        let mut isolate = isolate_lock2.lock().unwrap();

        // Disallow tweaking the terminal by nshrc scripts; it causes EIO on macOS.
        isolate.disable_tcsetpgrp();

        // Execute nshrc.
        let stdin = std::fs::File::open("/dev/null").unwrap();
        isolate.run_str_with_stdio(&rc, stdin.into_raw_fd(), pipe_in, pipe_in);
        unistd::close(pipe_in).unwrap();
    });

    // TODO: Ensure that nshrc loader grabs the lock.

    // Render the prompt and wait for an user input.
    let mut line = match input::input(config, isolate_lock.clone()) {
        Ok(line) => {
            println!();
            line
        }
        Err(input::InputError::Eof) => {
            return ExitStatus::ExitedWith(0);
        }
    };

    // Now we have to execute the first command from the prompt. Wait for the
    // nshrc loader to finish and enter the main loop.
    nshrc_loader.join().unwrap();

    // Print stdout/stderr from nshrc.
    let mut nshrc_out = String::new();
    let mut nshrc_out_file = unsafe { std::fs::File::from_raw_fd(pipe_out) };
    nshrc_out_file.read_to_string(&mut nshrc_out).unwrap();
    if !nshrc_out.is_empty() {
        eprintln!(
            concat!(
                "{}nshrc stdout/stderr -----------------------{}\n",
                "{}\n",
                "{}-------------------------------------------{}"
            ),
            termion::style::Bold, termion::style::Reset,
            &nshrc_out,
            termion::style::Bold, termion::style::Reset);
    }

    loop {
        let mut isolate = isolate_lock.lock().unwrap();
        isolate.enable_tcsetpgrp();
        isolate.run_str(&line);
        isolate.check_background_jobs();
        drop(isolate);

        // Read the next line.
        line = match input::input(config, isolate_lock.clone()) {
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

fn shell_main(opt: Opt) {
    // Load ~/.nshrc
    let config = match config::load_nshrc() {
        Ok(config) => config,
        Err(err) => {
            warn!("nsh: warning: failed to read nshrc: {}", err);
            config::default_config()
        }
    };

    // Initialize subsystems.
    path::init(&config);
    history::init(&config);

    let stdout = std::fs::File::create("/dev/stdout").unwrap();
    let interactive = is_tty(&stdout) && opt.command.is_none() && opt.file.is_none();
    let status = match (opt.command, opt.file) {
        (Some(command), _) => {
            let mut isolate = exec::Isolate::new("nsh", interactive);
            isolate.set_and_export("PATH", Value::String(config.path.clone()));
            isolate.run_str(&command)
        },
        (_, Some(file)) => {
            let script_name = file.to_str().unwrap();
            let mut isolate = exec::Isolate::new(script_name, interactive);
            isolate.set_and_export("PATH", Value::String(config.path.clone()));
            isolate.run_file(file)
        },
        (_, _) => {
            if !interactive {
                eprintln!("nsh: warning: stdout is not a tty");
                process::exit(0);
            }

            let mut isolate = exec::Isolate::new("nsh", interactive);
            isolate.set_and_export("PATH", Value::String(config.path.clone()));
            interactive_mode(&config, isolate)
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
