use crate::builtins::InternalCommandContext;
use crate::process::ExitStatus;
use std::io::Write;
use std::path::Path;

pub fn command(ctx: &mut InternalCommandContext) -> ExitStatus {
    trace!("cd: argv={:?}", ctx.argv);
    let old_dir = std::env::current_dir().expect("failed to getcwd()");
    let (dir, pushd) = match ctx.argv.get(1).map(|s| s.as_str()) {
        Some("-") => {
            if let Some(d) = ctx.shell.popd() {
                (d, false)
            } else {
                return ExitStatus::ExitedWith(1);
            }
        }
        Some(dir) if dir.starts_with('/') => (dir.to_string(), true),
        Some(dir) => {
            (
                // relative path
                Path::new(&old_dir)
                    .join(dir.to_string())
                    .to_string_lossy()
                    .into_owned(),
                true,
            )
        }
        None => {
            // called with no arguments; defaults to the home directory
            (
                if let Some(home_dir) = dirs::home_dir() {
                    home_dir.to_string_lossy().into_owned()
                } else {
                    String::from("/")
                },
                true,
            )
        }
    };

    if pushd {
        // TODO: make this configurable
        ctx.shell.pushd(old_dir.to_str().unwrap().to_owned());
    }

    match std::env::set_current_dir(&dir) {
        Ok(_) => ExitStatus::ExitedWith(0),
        Err(err) => {
            writeln!(ctx.stderr, "nsh: cd: {}: `{}'", err, dir).ok();
            ExitStatus::ExitedWith(1)
        }
    }
}
