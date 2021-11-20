use crate::builtins::InternalCommandContext;
use crate::process::ExitStatus;
use std::io::Write;

fn handle_escape_sequence(escaped_arg: &str) -> String {
    let mut s = String::new();
    let mut escape = false;
    for ch in escaped_arg.chars() {
        match (escape, ch) {
            (true, 'n') => {
                s.push('\n');
                escape = false;
            }
            (true, 't') => {
                s.push('\t');
                escape = false;
            }
            (true, 'e') => {
                s.push('\u{1b}');
                escape = false;
            }
            (true, ch) => {
                s.push('\\');
                s.push(ch);
                escape = false;
            }
            (false, '\\') => escape = true,
            (false, ch) => s.push(ch),
        }
    }

    if escape {
        s.push('\\');
    }

    s
}

pub fn command(ctx: &mut InternalCommandContext) -> ExitStatus {
    let mut no_newline = false;
    let mut escape = false;
    let skip = match ctx.argv.get(1).map(|s| s.as_str()) {
        Some("-e") => {
            escape = true;
            true
        }
        Some("-n") => {
            no_newline = true;
            true
        }
        Some("-ne") | Some("-en") => {
            escape = true;
            no_newline = true;
            true
        }
        _ => false,
    };

    let iter = ctx
        .argv
        .iter()
        .skip(1 + if skip { 1 } else { 0 })
        .enumerate();
    for (i, escaped_arg) in iter {
        let arg = if escape {
            handle_escape_sequence(escaped_arg)
        } else {
            escaped_arg.to_string()
        };

        if i > 0 {
            write!(ctx.stdout, " {}", arg).ok();
        } else {
            write!(ctx.stdout, "{}", arg).ok();
        }
    }

    if !no_newline {
        writeln!(ctx.stdout).ok();
    }

    ctx.stdout.flush().ok();
    ExitStatus::ExitedWith(0)
}
