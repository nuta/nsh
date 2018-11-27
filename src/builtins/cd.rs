use crate::builtins::InternalCommandContext;
use crate::exec::ExitStatus;
use std::path::Path;

pub fn command(ctx: &mut InternalCommandContext) -> ExitStatus {
    trace!("cd: {:?}", ctx.argv);
    let dir = match ctx.argv.get(1) {
        Some(dir) => {
            if dir.starts_with('/') {
                dir.clone()
            } else {
                let current_dir = std::env::current_dir().expect("failed to getcwd()");
                Path::new(&current_dir)
                    .join(dir.clone())
                    .to_string_lossy()
                    .into_owned()
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

    if std::env::set_current_dir(&dir).is_ok() {
        ExitStatus::ExitedWith(0)
    } else {
        error!("failed to cd into {}", dir);
        ExitStatus::ExitedWith(1)
    }
}
