use crate::builtins::InternalCommandContext;
use crate::exec::ExitStatus;
use std::fs;
use std::path::Path;
use std::io::Write;

macro_rules! run_cmd {
    ($ctx:expr, $argv0:expr, $args:expr, $current_dir:expr) => {{
        let status = std::process::Command::new($argv0)
            .args($args)
            .current_dir($current_dir)
            .status();

        match status {
            Ok(status) if status.success() => (), /* success */
            Ok(status) => {
                writeln!($ctx.stderr, "nsh-config: {} returned {:?}",
                    $argv0, status).ok();
                return ExitStatus::ExitedWith(1);
            }
            Err(err) => {
                writeln!($ctx.stderr, "nsh-config: failed to invoke {}: {}",
                    $argv0, err).ok();
                return ExitStatus::ExitedWith(1);
            }
        }
    }};
}

pub fn command(ctx: &mut InternalCommandContext) -> ExitStatus {
    let home_dir = dirs::home_dir().unwrap();
    let cache_dir = Path::new(&home_dir).join(".cache");
    let git_repo_dir = cache_dir.join("nsh-config-server");
    let nsh_config_dir = git_repo_dir.join("config");

    // Download the git repo if needed.
    if !git_repo_dir.exists() {
        if let Err(err) = fs::create_dir_all(&cache_dir) {
            writeln!(
                ctx.stderr,
                "nsh-config: failed to create the cache directory: {}",
                err
            ).ok();

            return ExitStatus::ExitedWith(1);
        }

        writeln!(ctx.stdout, "Downloading nsh git repository...").ok();

        run_cmd!(ctx, "git", &["clone", "https://github.com/seiyanuta/nsh"], &cache_dir);
    }

    writeln!(ctx.stdout, "nsh-config: updating nsh-config...").ok();
    run_cmd!(ctx, "git", &["pull"], &git_repo_dir);
    run_cmd!(ctx, "yarn", &[""], &nsh_config_dir);
    run_cmd!(ctx, "yarn", &["build"], &nsh_config_dir);

    writeln!(ctx.stdout, "nsh-config: starting a local web server...").ok();
    run_cmd!(ctx, "yarn", &["run", "main"], &nsh_config_dir);

    ExitStatus::ExitedWith(0)
}
