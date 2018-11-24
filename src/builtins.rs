use std::env;
use std::process;
use std::path::Path;
use exec::ExitStatus;
use dirs;

pub fn exit_command(argv: &[String]) -> ExitStatus {
    let exit_with = if let Some(exit_with) = argv.get(1) {
        exit_with.parse().unwrap_or(1)
    } else {
        0
    };

    process::exit(exit_with);
}

pub fn cd_command(argv: &[String]) -> ExitStatus {
    trace!("cd: {:?}", argv);
    let dir = match argv.get(1) {
        Some(dir) => {
            if dir.starts_with('/') {
                dir.clone()
            } else {
                let current_dir = env::current_dir().expect("failed to getcwd()");
                Path::new(&current_dir).join(dir.clone()).to_string_lossy().into_owned()
            }
        }
        None => {
            if let Some(home_dir) = dirs::home_dir() {
                home_dir.to_string_lossy().into_owned()
            } else {
                String::from("/")
            }
        }
    };

    if env::set_current_dir(&dir).is_ok() {
        0
    } else {
        error!("failed to cd into {}", dir);
        1
    }
}