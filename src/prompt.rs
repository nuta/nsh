use crate::completion::Completions;
use crate::input::InputMode;
use crate::utils::get_env;
use nom::types::CompleteStr as Input;
use std::env;
use syntect::easy::HighlightLines;
use syntect::highlighting::Style;
use syntect::highlighting::ThemeSet;
use syntect::parsing::SyntaxSet;
use syntect::util::{as_24_bit_terminal_escaped, LinesWithEndings};
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
        | map!(tag!("\\n"), |_| Span::Newline)
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
                if let Ok(current_dir) = env::current_dir() {
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

static DEFAULT_PROMPT: &'static str = "\\c{red}\\c{bold}[\\u@\\h:\\W]$\\c{reset} ";

/// Returns the number of lines of the rendered prompt and the rendered prompt.
/// FIXME: too many arguments
pub fn render_prompt(
    mode: InputMode,
    completions: &Completions,
    prompt_base_y: u16,
    user_cursor: usize,
    user_input: &str,
    current_theme: &str,
) -> (u16, String) {
    use std::fmt::Write;
    let mut buf = String::new();
    let rendered_lines;

    // Apply syntax highlighting.
    let mut highlighter = create_highlighter(current_theme);
    let mut colored_user_input = String::new();
    for line in LinesWithEndings::from(user_input) {
        let ranges: Vec<(Style, &str)> = highlighter.highlight(line, &SYNTECT_STATIC.syntax_set);
        let escaped = as_24_bit_terminal_escaped(&ranges[..], false);
        colored_user_input += &escaped;
    }

    // Parse and render the prompt.
    let ps1 = get_env("PS1", DEFAULT_PROMPT);
    let (prompt_str, prompt_len) = if let Ok(fmt) = parse_prompt(ps1.as_str()) {
        draw_prompt(&fmt)
    } else {
        ("$ ".to_owned(), 2)
    };

    // Render the prompt and colored user input.
    let user_cursor_pos = (prompt_len + user_cursor + 1) as u16;
    write!(
        buf,
        "{}{}{}{}{}",
        termion::style::Reset,
        termion::cursor::Goto(1, 1 + prompt_base_y),
        prompt_str,
        colored_user_input,
        termion::style::Reset,
    )
    .ok();

    // Render completions.
    match mode {
        InputMode::Normal => {
            rendered_lines = 1;
        }
        InputMode::Completion => {
            let entries_len = completions.len();
            let actual_lines = if entries_len < completions.display_lines() {
                entries_len as u16
            } else {
                completions.display_lines() as u16
            };

            if actual_lines > 0 {
                // The prompt line, completions, and "TAB to expand" line.
                rendered_lines = 1 + actual_lines + 1;
                // The beginning y of the completions.
                let completion_base_y = prompt_base_y + 1;

                let results = completions.entries();
                let iter = results
                    .iter()
                    .skip(completions.display_index())
                    .take(completions.display_lines());

                for (i, entry) in iter.enumerate() {
                    write!(
                        buf,
                        "\n{}{}",
                        termion::cursor::Goto(1, 1 + completion_base_y + i as u16),
                        termion::clear::CurrentLine,
                    )
                    .ok();

                    let selected =
                        (completions.display_index() + i) == completions.selected_index() as usize;
                    if selected {
                        write!(buf, "{}{}", termion::style::Underline, termion::style::Bold).ok();
                    } else {
                        write!(
                            buf,
                            "{}{}",
                            termion::style::NoUnderline,
                            termion::style::NoBold
                        )
                        .ok();
                    };

                    write!(buf, "{}{}", entry, termion::style::Reset).ok();
                }

                write!(
                    buf,
                    "\n{}{}{}{}{} {}/{} {}",
                    termion::cursor::Goto(1, 1 + completion_base_y + actual_lines),
                    termion::clear::CurrentLine,
                    termion::style::Bold,
                    termion::color::Fg(termion::color::White),
                    termion::color::Bg(termion::color::Cyan),
                    completions.selected_index() + 1,
                    completions.len(),
                    termion::style::Reset
                )
                .ok();
            } else {
                rendered_lines = 2;
                write!(
                    buf,
                    "\n{}{}{}{}{}no candidates{}",
                    termion::cursor::Goto(1, 1 + prompt_base_y + 1),
                    termion::clear::CurrentLine,
                    termion::style::Bold,
                    termion::color::Fg(termion::color::White),
                    termion::color::Bg(termion::color::Cyan),
                    termion::style::Reset
                )
                .ok();
            }
        }
    }

    write!(
        buf,
        "{}",
        termion::cursor::Goto(user_cursor_pos, 1 + prompt_base_y)
    )
    .ok();

    (rendered_lines, buf)
}

#[test]
fn test_prompt_parser() {
    assert_eq!(
        parse_prompt("\\u at \\h in \\W\\n$ "),
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
