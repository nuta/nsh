use crate::builtins::InternalCommandContext;
use crate::exec::ExitStatus;
use std::io::Write;

fn handle_escape_sequence(escaped_arg: &str) -> String {
    let mut s = String::new();
    let mut escape = false;
    for ch in escaped_arg.chars() {
        match (escape, ch) {
            (true, 'n') => { s.push('\n');  escape = false; }
            (true, 't') => { s.push('\t');  escape = false; }
            (true, 'e') => { s.push('\u{1b}');  escape = false; }
            (true, ch)  => { s.push('\\'); s.push(ch); escape = false; }
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
    let mut skip = 0;
    if let Some(argv1) = ctx.argv.get(1) {
        if argv1.starts_with('-') {
            skip = 1;
            for ch in argv1.chars().skip(1) {
                match ch {
                    'e' => escape = true,
                    'n' => no_newline = true,
                    _ => (),
                }
            }
        }
    }

    for (i, escaped_arg) in ctx.argv.iter().skip(1 + skip).enumerate() {
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
