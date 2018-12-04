use crate::completion::Completions;
use crate::input::InputMode;
use nom::types::CompleteStr as Input;
use syntect::easy::HighlightLines;
use syntect::highlighting::Style;
use syntect::highlighting::ThemeSet;
use syntect::parsing::SyntaxSet;
use syntect::util::{as_24_bit_terminal_escaped, LinesWithEndings};
use std::fmt::Write;
use termion;
use nix::unistd;
use libc;

#[derive(Debug, PartialEq, Eq)]
pub struct Prompt {
    spans: Vec<Span>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Color {
    Red,
    Blue,
    Yellow,
    Green,
    Cyan,
    Magenta,
    Bold,
    Underline,
    Reset,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Span {
    Literal(String),
    Color(Color),
    Username,
    Hostname,
    CurrentDir,
    Newline,
}

named!(span<Input, Span>,
    alt!(
        map!(tag!("\\u"), |_| Span::Username)
        | map!(tag!("\\h"), |_| Span::Hostname)
        | map!(tag!("\\W"), |_| Span::CurrentDir)
        | map!(tag!("\n"), |_| Span::Newline)
        | map!(tag!("\\c{reset}"), |_| Span::Color(Color::Reset))
        | map!(tag!("\\c{bold}"), |_| Span::Color(Color::Bold))
        | map!(tag!("\\c{underline}"), |_| Span::Color(Color::Underline))
        | map!(tag!("\\c{red}"), |_| Span::Color(Color::Red))
        | map!(tag!("\\c{blue}"), |_| Span::Color(Color::Blue))
        | map!(tag!("\\c{green}"), |_| Span::Color(Color::Green))
        | map!(tag!("\\c{yellow}"), |_| Span::Color(Color::Yellow))
        | map!(tag!("\\c{cyan}"), |_| Span::Color(Color::Cyan))
        | map!(tag!("\\c{magenta}"), |_| Span::Color(Color::Magenta))
        | map!(take_while!(|c| c != '\\'), |s| Span::Literal(s.to_string()))
    )
);

named!(prompt_parser<Input, Prompt>,
    map!(many0!(span), |spans| ( Prompt { spans }))
);

fn parse_prompt(prompt: &str) -> Result<Prompt, ()> {
    match prompt_parser(Input(prompt)) {
        Ok((_, prompt)) => Ok(prompt),
        Err(err) => {
            trace!("prompt parse error: '{}'", &err);
            Err(())
        }
    }
}

// FIXME: remove unsafe or use external crate
fn get_current_username() -> String {
    let mut passwd_buf = Vec::with_capacity(512);
    let mut passwd: libc::passwd = unsafe { std::mem::zeroed() };
    let mut result = std::ptr::null_mut();
    unsafe {
        libc::getpwuid_r(
            libc::getuid(),
            &mut passwd,
            passwd_buf.as_mut_ptr(),
            passwd_buf.capacity(),
            &mut result
        );
    }

    if result.is_null() {
        "".to_owned()
    } else {
        let ptr = passwd.pw_name as *const _;
        unsafe {
            let cstr = std::ffi::CStr::from_ptr(ptr);
            cstr.to_string_lossy().into_owned()
        }
    }
}

fn get_hostname() -> String {
    let mut hostname_buf = [0u8; 128];
    let hostname_cstr = unistd::gethostname(&mut hostname_buf).expect("failed to get hostname");
    let hostname = hostname_cstr.to_str().expect("Hostname is not valid utf-8 string");
    hostname.to_owned()
}

/// Returns the length of the last line excluding escape sequences.
fn draw_prompt(prompt: &Prompt) -> (String, usize) {
    let mut len = 0;
    let mut buf = String::new();
    for span in &prompt.spans {
        match span {
            Span::Literal(s) => {
                len += s.len();
                buf.push_str(&s)
            }
            Span::Color(c) => match c {
                Color::Red => buf.push_str("\x1b[31m"),
                Color::Blue => buf.push_str("\x1b[34m"),
                Color::Yellow => buf.push_str("\x1b[33m"),
                Color::Green => buf.push_str("\x1b[32m"),
                Color::Cyan => buf.push_str("\x1b[36m"),
                Color::Magenta => buf.push_str("\x1b[35m"),
                Color::Bold => buf.push_str("\x1b[1m"),
                Color::Underline => buf.push_str("\x1b[4m"),
                Color::Reset => buf.push_str("\x1b[0m"),
            },
            Span::Newline => {
                len = 0;
                buf.push_str("\n")
            }
            Span::Username => {
                let username = get_current_username();
                len += username.len();
                buf.push_str(&username)
            }
            Span::Hostname => {
                let hostname = get_hostname();
                len += hostname.len();
                buf.push_str(&hostname)
            }
            Span::CurrentDir => {
                if let Ok(current_dir) = std::env::current_dir() {
                    let mut path = current_dir.to_str().unwrap().to_string();

                    // "/Users/chandler/games/doom" -> "~/venus/games/doom"
                    if let Some(home_dir) = dirs::home_dir() {
                        let home_dir = home_dir.to_str().unwrap();
                        if path.starts_with(&home_dir) {
                            path = path.replace(home_dir, "~");
                        }
                    }

                    len += path.len();
                    buf.push_str(&path);
                }
            }
        }
    }

    (buf, len)
}

pub struct SyntectStatic {
    syntax_set: SyntaxSet,
    theme_set: ThemeSet,
}

lazy_static! {
    static ref SYNTECT_STATIC: SyntectStatic = {
        let syntax_set = SyntaxSet::load_defaults_newlines();
        let theme_set = ThemeSet::load_defaults();
        SyntectStatic {
            syntax_set,
            theme_set,
        }
    };
}

fn create_highlighter(theme_name: &str) -> HighlightLines {
    let theme = &SYNTECT_STATIC.theme_set.themes[theme_name];
    let syntax = SYNTECT_STATIC
        .syntax_set
        .find_syntax_by_extension("sh")
        .unwrap();
    HighlightLines::new(syntax, theme)
}

/// `y` is 0-origin.
fn replace_newline_with_clear(text: &str, y: u16) -> String {
    let mut num_lines = 0;
    let mut buf = String::new();
    let mut line = String::new();

    for ch in text.chars() {
        if ch == '\n' {
            let goto = termion::cursor::Goto(1, 1 + y + num_lines);
            write!(buf, "{}{}{}", goto, termion::clear::CurrentLine, line).ok();
            num_lines += 1;
            line = String::new();
        } else {
            line.push(ch);
        }
    }

    if !line.is_empty() {
        let goto = termion::cursor::Goto(1, 1 + y + num_lines);
        write!(buf, "{}{}{}", goto, termion::clear::CurrentLine, line).ok();
    }

    buf
}

/// Returns the new `prompt_y` and the rendered prompt.
/// FIXME: too many arguments
pub fn render_prompt(
    prompt_fmt: &str,
    mode: InputMode,
    completions: &Completions,
    prompt_y: u16,
    y_max: u16,
    prev_rendered_lines: u16,
    user_cursor: usize,
    user_input: &str,
    current_theme: &str,
) -> (u16, u16, String) {

    // Apply syntax highlighting.
    let mut highlighter = create_highlighter(current_theme);
    let mut colored_user_input = String::new();
    for line in LinesWithEndings::from(user_input) {
        let ranges: Vec<(Style, &str)> = highlighter.highlight(line, &SYNTECT_STATIC.syntax_set);
        let escaped = as_24_bit_terminal_escaped(&ranges[..], false);
        colored_user_input += &escaped;
    }

    // Parse and render the prompt.
    let (prompt_str, prompt_last_line_len) = if let Ok(fmt) = parse_prompt(prompt_fmt) {
        draw_prompt(&fmt)
    } else {
        ("$ ".to_owned(), 2)
    };

    use termion::clear::CurrentLine;
    use termion::color::{Fg, Bg, White, Cyan};
    use termion::style::*;

    // Render completions.
    let mut completion_str = String::new();
    if mode == InputMode::Completion {
        let results = completions.entries();
        let iter = results
            .iter()
            .skip(completions.display_index())
            .take(completions.display_lines());

        if !results.is_empty() {
            for (i, entry) in iter.enumerate() {
                let current = completions.display_index() + i;
                let selected = current == completions.selected_index();
                if selected {
                    write!(completion_str, "{}{}", Underline, Bold).ok();
                } else {
                    write!(completion_str, "{}{}", NoUnderline, NoBold).ok();
                }
                write!(completion_str, "{}{}{}\n", CurrentLine, entry, Reset).ok();
            }

            write!(completion_str, "{}{}{}{} {}/{} ",
                CurrentLine, Bold, Fg(White), Bg(Cyan),
                completions.selected_index() + 1,
                completions.len()
            ).ok();
        } else {
            write!(
                completion_str,
                "{}{}{}{}no candidates",
                CurrentLine, Bold, Fg(White), Bg(Cyan)
            )
            .ok();
        }
    }

    let mut buf = String::new();
    let prompt_lines = prompt_str.chars().filter(|c| *c == '\n').count() as u16 + 1;
    let completion_lines = if completion_str.is_empty() {
        0
    } else {
        1 + completion_str.chars().filter(|c| *c == '\n').count() as u16
    };

    let rendered_lines = prompt_lines + completion_lines;
    let avail = std::cmp::max(0, i32::from(y_max) - i32::from(prompt_y));
    let scroll = std::cmp::max(0, i32::from(rendered_lines) - i32::from(avail) - 1) as u16;
    let new_prompt_y = prompt_y - scroll;
    let user_cursor_pos = (prompt_last_line_len + user_cursor) as u16;

    for _ in 0..std::cmp::max(0, scroll) {
        write!(buf, "\n").ok();
    }

    // Clear the current prompt.
    for i in 0..prev_rendered_lines {
        write!(
            buf,
            "{}{}",
            termion::cursor::Goto(1, 1 + prompt_y + i),
            termion::clear::CurrentLine,
        )
        .ok();
    }

    // Render the prompt and colored user input.
    write!(
        buf,
        "{}{}{}{}{}{}{}",
        replace_newline_with_clear(&prompt_str, new_prompt_y),
        Reset,
        colored_user_input,
        Reset,
        replace_newline_with_clear(&completion_str, new_prompt_y + prompt_lines),
        termion::cursor::Goto(1 + user_cursor_pos, 1 + new_prompt_y + (prompt_lines - 1)),
        Reset,
    ).ok();

    trace!("prompt_offset: S={} M={} base={}, new_y={}", scroll, y_max, prompt_y, new_prompt_y);
    (rendered_lines, new_prompt_y, buf)
}

#[test]
fn test_prompt_parser() {
    assert_eq!(
        parse_prompt("\\u at \\h in \\W\n$ "),
        Ok(Prompt {
            spans: vec![
                Span::Username,
                Span::Literal(" at ".into()),
                Span::Hostname,
                Span::Literal(" in ".into()),
                Span::CurrentDir,
                Span::Newline,
                Span::Literal("$ ".into()),
            ]
        })
    );
}


#[cfg(test)]
mod benchmarks {
    use test::Bencher;
    use super::*;

    #[bench]
    fn simple_prompt_rendering_bench(b: &mut Bencher) {
        b.iter(|| {
            let mode = InputMode::Normal;
            let completions = Completions::new(vec![]);
            let theme = "Solarized (dark)";
            render_prompt("$ ", mode, &completions, 0, 64, 0, 3, "ls -alhG", &theme);
        });
    }

    #[bench]
    fn complex_prompt_rendering_bench(b: &mut Bencher) {
        b.iter(|| {
            let prompt = "\\c{cyan}\\c{bold}\\u@\\h:\\c{reset} \\W\n$\\c{reset} ";
            let mode = InputMode::Normal;
            let completions = Completions::new(vec![]);
            let theme = "Solarized (dark)";
            render_prompt(&prompt, mode, &completions, 0, 64, 64, 3, "ls -alhG", &theme);
        });
    }
}
