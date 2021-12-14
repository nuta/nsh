use crate::builtins::InternalCommandContext;
use crate::process::ExitStatus;
use nix::errno::Errno;
use std::ffi::CString;
use std::io::Write;

pub fn command(ctx: &mut InternalCommandContext) -> ExitStatus {
    if let Some(command) = ctx.argv.get(1) {
        let command = match CString::new(command.as_bytes()) {
            Ok(args) => args,
            Err(_) => {
                writeln!(ctx.stderr, "nsh: exec: invalid command (perhaps it includes a NUL character?)").ok();
                return ExitStatus::Return;
            }
        };
        // args should include `command`
        let args = match ctx.argv[1..]
            .into_iter()
            .map(|s| CString::new(s.as_bytes()))
            .collect::<Result<Vec<_>, _>>()
        {
            Ok(args) => args,
            Err(_) => {
                writeln!(ctx.stderr, "nsh: exec: nul found in inputs").ok();
                return ExitStatus::Return;
            }
        };

        match nix::unistd::execvp(&command, &args) {
            Ok(never) => match never {},
            Err(e) => {
                writeln!(ctx.stderr, "nsh: exec: {}", e).ok();
                let code = match e {
                    // file not executable
                    Errno::ENOEXEC | Errno::EACCES | Errno::EINVAL => 126,
                    // file not found
                    Errno::ENOENT => 127,
                    _ => 1,
                };
                ExitStatus::ExitedWith(code)
            }
        }
    } else {
        // do nothing for now
        ExitStatus::ExitedWith(0)
    }
}
