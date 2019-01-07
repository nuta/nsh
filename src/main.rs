#![cfg_attr(test, feature(test))]
#![cfg_attr(test, feature(extern_crate_item_prelude))]

#[macro_use]
extern crate log;
#[macro_use]
extern crate lazy_static;
extern crate structopt;
extern crate dirs;
extern crate nix;
extern crate syntect;
extern crate termion;
extern crate crossbeam;
extern crate crossbeam_channel;
extern crate glob;
extern crate globset;
extern crate iron;
extern crate router;
extern crate params;
extern crate serde;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;
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
mod input;
mod parser;
mod context_parser;
mod path;
mod prompt;
mod utils;
mod history;
mod fuzzy;
mod variable;
mod config;
mod doctor;

use std::path::{Path, PathBuf};
use std::process;
use std::sync::{Arc, Mutex};
use structopt::StructOpt;
use nix::unistd::{getpid, setpgid, tcsetpgrp};
use nix::sys::signal::{SigHandler, SigAction, SaFlags, SigSet, Signal, sigaction};
use crate::exec::ExitStatus;
use crate::config::Config;

fn interactive_mode(config: &Config, raw_isolate: exec::Isolate) -> ExitStatus {
    let isolate_lock = Arc::new(Mutex::new(raw_isolate));

    // Create a process group.
    let pid = getpid();
    setpgid(pid, pid).expect("failed to setpgid");
    tcsetpgrp(0 /* stdin */, pid).expect("failed to tcsetpgrp");

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


    //Evaluate rc script asynchronously since it may take too long.
    let rc = config.rc.clone();
    let isolate_lock2 = isolate_lock.clone();
    let nshrc_loader = std::thread::spawn(move || {
        let mut isolate = isolate_lock2.lock().unwrap();
        isolate.run_str(&rc);
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

    loop {
        let mut isolate = isolate_lock.lock().unwrap();
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

#[derive(StructOpt, Debug)]
#[structopt(name="nsh", about="A command-line shell that focuses on performance and productivity.")]
struct Opt {
    /// Open the web-based configuration tool.
    #[structopt(long = "config")]
    open_config: bool,

    /// Check your terminal environment.
    #[structopt(long = "doctor")]
    doctor: bool,

    /// Run the given string.
    #[structopt(short = "c")]
    command: Option<String>,

    /// Run the file.
    #[structopt(name = "FILE", parse(from_os_str))]
    file: Option<PathBuf>,
}


fn shell_main(opt: Opt) {
    // Load ~/.nshconfig.
    let home_dir = dirs::home_dir().unwrap();
    let nshconfig_path = Path::new(&home_dir).join(".nshconfig");
    let config: config::Config = match std::fs::File::open(nshconfig_path) {
        Ok(file) => serde_json::from_reader(file).expect("failed to parse ~/.nshrc"),
        Err(_) => {
            // Use default values.
            serde_json::from_str("{}").expect("failed to parse '{}'")
        }
    };

    // Initialize subsystems.
    path::init(&config);
    history::init(&config);

    let interactive = opt.command.is_none() && opt.file.is_none();
    let status = match (opt.command, opt.file) {
        (Some(command), _) => {
            let mut isolate = exec::Isolate::new("nsh", interactive);
            isolate.run_str(&command)
        },
        (_, Some(file)) => {
            let script_name = file.to_str().unwrap();
            let mut isolate = exec::Isolate::new(script_name, interactive);
            isolate.run_file(file)
        },
        (_, _) => {
            let isolate = exec::Isolate::new("nsh", interactive);
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

fn main() {
    logger::init();
    let opt = Opt::from_args();

    if opt.doctor {
        doctor::main();
        return;
    }

    if opt.open_config {
        config::main();
        return;
    }

    // Dump the panic reason and backtrace into the log file.
    std::panic::set_hook(Box::new(|info| {
        error!("{}", info);
        error!("{:#?}", backtrace::Backtrace::new());
    }));

    shell_main(opt);
}
