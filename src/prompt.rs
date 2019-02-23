use crate::completion::CompletionSelector;
use crate::syntax_highlighting::highlight;
use crate::context_parser::InputContext;
use pest::Parser;
use pest::iterators::{Pairs, Pair};
use std::fmt::Write;
use termion;
use nix::unistd;
use libc;

#[derive(Debug, PartialEq, Eq)]
pub struct Prompt {
    spans: Vec<Span>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Condition {
    InGitRepo
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Span {
    Literal(String),
    Color(Color),
    Username,
    Hostname,
    CurrentDir,
    Newline,
    GitBranch,
    If {
        condition: Condition,
        then_part: Vec<Span>,
        else_part: Vec<Span>,
    }
}

#[derive(Parser)]
#[grammar = "prompt.pest"]
struct PromptParser;

fn visit_prompt(pair: Pair<Rule>) -> Prompt {
    let mut spans = Vec::new();
    for pair in pair.into_inner() {
        let span = match pair.as_rule() {
            Rule::username_span => Span::Username,
            Rule::hostname_span => Span::Hostname,
            Rule::current_dir_span => Span::CurrentDir,
            Rule::git_branch_span => Span::GitBranch,
            Rule::newline_span => Span::Newline,
            Rule::reset_span => Span::Color(Color::Reset),
            Rule::bold_span => Span::Color(Color::Bold),
            Rule::underline_span => Span::Color(Color::Underline),
            Rule::red_span => Span::Color(Color::Red),
            Rule::blue_span => Span::Color(Color::Blue),
            Rule::green_span => Span::Color(Color::Green),
            Rule::yellow_span => Span::Color(Color::Yellow),
            Rule::cyan_span => Span::Color(Color::Cyan),
            Rule::magenta_span => Span::Color(Color::Magenta),
            Rule::literal_span => Span::Literal(pair.as_span().as_str().to_owned()),
            Rule::if_span => {
                let mut inner = pair.into_inner();
                let condition = match inner.next().unwrap().as_span().as_str() {
                    "in_git_repo" => Condition::InGitRepo,
                    _ => unreachable!(),
                };

                let then_part = visit_prompt(inner.next().unwrap()).spans;
                let else_part = visit_prompt(inner.next().unwrap()).spans;
                Span::If { condition, then_part, else_part }
            },
            _ => unreachable!()
        };

        spans.push(span);
    }

    Prompt { spans }
}

fn pairs2prompt(mut pairs: Pairs<Rule>) -> Prompt {
    visit_prompt(pairs.next().unwrap())
}

fn parse_prompt(prompt: &str) -> Result<Prompt, pest::error::Error<Rule>> {
    PromptParser::parse(Rule::prompt, prompt).map(pairs2prompt)
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

fn get_current_git_branch() -> String {
    let result = std::process::Command::new("git")
        .arg("rev-parse")
        .arg("--abbrev-ref")
        .arg("HEAD")
        .output();

    if let Ok(output) = result {
        String::from_utf8_lossy(&output.stdout)
            .into_owned()
            .trim()
            .to_owned()
    } else {
        "".to_owned()
    }
}

fn evaluate_condition(cond: &Condition) -> bool {
    match cond {
        Condition::InGitRepo => {
            std::process::Command::new("git")
                .arg("rev-parse")
                .arg("--is-inside-work-tree")
                .stdout(std::process::Stdio::null())
                .stderr(std::process::Stdio::null())
                .status()
                .map(|status| status.success())
                .unwrap_or(false)
        }
    }
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
            Span::GitBranch => {
                let hostname = get_current_git_branch();
                len += hostname.len();
                buf.push_str(&hostname)
            },
            Span::If { condition, then_part, else_part } => {
                let spans = if evaluate_condition(condition) {
                    then_part
                } else {
                    else_part
                };

                let (result, result_len) = draw_prompt(&Prompt { spans: spans.to_vec() });
                len += result_len;
                buf.push_str(&result)
            }
        }
    }

    (buf, len)
}

/// Moves the cursor to down if `offset > 0` or up if `offset < 0` and
/// to the beginning of the line.
#[inline]
fn move_cursor_y(offset: i32, clear_line: bool) -> String {
    if offset == 0 {
        return "".to_owned();
    }

    match (offset > 0, clear_line) {
        (true,  false) => format!("{}\r", "\n".repeat(offset.abs() as usize)),
        (true,  true)  => format!("{}\r{}", "\n".repeat(offset.abs() as usize), termion::clear::CurrentLine),
        (false, false) => format!("{}\r", termion::cursor::Up(offset.abs() as u16)),
        (false, true)  => format!("\r{}{}", termion::clear::CurrentLine, termion::cursor::Up(offset.abs() as u16)),
    }
}

fn replace_newline_with_clear(text: &str) -> String {
    let mut buf = String::new();
    let mut line = String::new();

    for ch in text.chars() {
        if ch == '\n' {
            write!(buf, "{}{}", line, move_cursor_y(1, true)).ok();
            line = String::new();
        } else {
            line.push(ch);
        }
    }

    if !line.is_empty() {
        write!(buf, "{}", line).ok();
    }

    buf
}

pub struct PromptRenderer {
    prompt_str: String,
    prompt_last_line_len: usize,
    last_rendered_lines: u16,
    last_cursor_y: u16,
    last_cursor_x: u16,
    last_completion_lines: u16,
}

impl PromptRenderer {
    pub fn new(prompt_fmt: &str) -> PromptRenderer {
        // Parse and render the prompt.
        let (prompt_str, prompt_last_line_len) = match parse_prompt(prompt_fmt) {
            Ok(fmt) => draw_prompt(&fmt),
            Err(err) => {
                eprintln!("nsh: failed to parse $PROMPT: {}", err);
                ("$ ".to_owned(), 2)
            }
        };

        PromptRenderer {
            prompt_str,
            prompt_last_line_len,
            last_rendered_lines: 0,
            last_cursor_y: 0,
            last_cursor_x: 0,
            last_completion_lines: 0,
        }
    }

    /// Clear the screen. Don't forget to flush; this method does not flush `stdout` since
    /// this method is intended to be used just before printing the prompt.
    pub fn clear_screen(&mut self, stdout: &mut std::io::Stdout) {
        use std::io::Write;
        write!(stdout, "{}{}", termion::clear::All, termion::cursor::Goto(1, 1)).ok();
        self.last_rendered_lines = 0;
    }

    /// Renders the prompt, the user input, and completions (if supplied).
    /// TODO: needs refactoring
    /// TODO: handle terminal screen size changes
    pub fn render(&mut self, ctx: &InputContext, user_cursor: usize, x_max: usize, completions: Option<&CompletionSelector>) -> String {
        // Apply syntax highlighting.
        let colored_user_input = highlight(ctx);

        // Render completions.
        let (completion_str, completion_lines) = if let Some(completions) = completions {
            self.render_completions(completions)
        } else {
            ("".to_owned(), 0)
        };

        // Render the prompt and colored user input.
        let mut buf = String::new();
        write!(
            buf,
            "{}{}{}{}{}{}{}",
            self.render_clear(),
            replace_newline_with_clear(&self.prompt_str),
            termion::style::Reset,
            colored_user_input,
            termion::style::Reset,
            replace_newline_with_clear(&completion_str),
            termion::style::Reset,
        ).ok();


        //
        //                                      cursor_x
        //                    (horizontal offset from the beginning of the line)
        //                                         v
        //                      _________________________________________
        //           +---      > |[chandler@ross-macbook.local]            |
        //  cursor_y |       > > |$ for x in a b c d very_loooooooooooooooo|
        //     = 3   |       > | |ooooooooooooooooooooooooooooooooooooooong| <- wrapped
        //           +---    > | |_wrapped_word do ssh[TAB]                | <- wrapped
        //                 > | | |monica-hidden-door.local                 |
        //                 > | | |joey-chair.local                         |
        //              >  > | | |1/2                                      |
        //              |  | | | |                                         |
        //              |  | | | |                                         |
        //              |  | | | |                                         |
        //              |  | | |  -----------------------------------------
        //              |  | | |
        //              |  | | +- prompt_lines = 2
        //              |  | +- user_inptut_lines = 3
        //              |  +- completion_lines = 3
        //              |
        //              +- The cursor is now at the end of this line.
        //
        let user_input_len = ctx.input.len();

        let prompt_lines = self.prompt_str.chars().filter(|c| *c == '\n').count() as u16 + 1;
        let cursor_pos = self.prompt_last_line_len as u16 + user_cursor as u16;
        let cursor_y = cursor_pos / x_max as u16 + (prompt_lines - 1);
        let user_input_lines = ((self.prompt_last_line_len as u16 + user_input_len as u16) / x_max as u16) + 1;
        let rendered_lines = prompt_lines + (user_input_lines - 1) + completion_lines;

        // Move the cursor (y-axis).
        let cursor_y_offset = (cursor_y as i32 + 1) - rendered_lines as i32;
        write!(buf, "{}", move_cursor_y(cursor_y_offset, false)).ok();

        // Move the cursor to the beginning of the line.
        let cursor_x = cursor_pos % x_max as u16;
        write!(buf, "\r").ok();

        // Wrapping.
        if cursor_x == 0 && self.last_cursor_x > 0 && user_input_len == user_cursor {
            write!(buf, "\n").ok();
        }

        // Move the cursor (x-axis).
        if cursor_x > 0 {
            write!(buf, "\r{}", termion::cursor::Right(cursor_x)).ok();
        }

        trace!("cursor_y={} (last_y={}), cursor_y_offset={}, cursor_x={}",
            cursor_y, self.last_cursor_y, cursor_y_offset, cursor_x);
        trace!("rendered_lines={} (last={}), comp_lines={}, prompt_lines={}",
            rendered_lines, self.last_rendered_lines, completion_lines,
            prompt_lines);

        self.last_cursor_x = cursor_x;
        self.last_cursor_y = cursor_y;
        self.last_rendered_lines = rendered_lines;
        self.last_completion_lines = completion_lines;
        buf
    }

    /// Renders completions.
    fn render_completions(&self, completions: &CompletionSelector) -> (String, u16) {
        use termion::style::*;
        use termion::clear::CurrentLine;;

        let mut completion_str = String::new();
        let mut completion_lines = 0;
            write!(completion_str, "\n").ok();

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

                writeln!(completion_str, "{}{}{}", CurrentLine, entry, Reset).ok();
                completion_lines += 1;
            }

            write!(completion_str, "{}{}{}{} {}/{} ",
                CurrentLine, Reset, Invert, Bold,
                completions.selected_index() + 1,
                completions.len()
            ).ok();
        } else {
            write!(
                completion_str,
                "{}{}{}{}no candidates",
                termion::clear::CurrentLine,
                Bold,
                termion::color::Fg(termion::color::White),
                termion::color::Bg(termion::color::Red)
            )
            .ok();
        }

        // " 1/1024 " or "no candidates" line
        completion_lines += 1;

        (completion_str, completion_lines)
    }

    /// Renders a escape sequences which clears the current prompt.
    fn render_clear(&self) -> String {
        let mut buf = String::new();

        for _ in 0..(self.last_rendered_lines.saturating_sub(self.last_cursor_y + 1)) {
            write!(buf, "{}", move_cursor_y(1, false)).ok();
        }

        for _ in 0..self.last_rendered_lines.saturating_sub(1) {
            write!(buf, "{}", move_cursor_y(-1, true)).ok();
        }

        write!(buf, "\r{}", termion::clear::CurrentLine).ok();
        buf
    }

    /// Renders a escape sequences which clears the completions if it is enabled.
    pub fn render_clear_completions(&self) -> String {
        let mut buf = String::new();

        let offset = self.last_rendered_lines as i32 - (self.last_cursor_y as i32 + 1);
        write!(buf, "{}", move_cursor_y(offset, false)).ok();

        buf
    }
}

#[test]
fn test_prompt_parser() {
    assert_eq!(
        parse_prompt("\\{username} at \\{hostname} in \\{current_dir}\\n$ "),
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

    assert_eq!(
        parse_prompt("\\{current_dir} \\if{in_git_repo}{[\\{git_branch}]}{} $ "),
        Ok(Prompt {
            spans: vec![
                Span::CurrentDir,
                Span::Literal(" ".into()),
                Span::If {
                    condition: Condition::InGitRepo,
                    then_part: vec![
                        Span::Literal("[".into()),
                        Span::GitBranch,
                        Span::Literal("]".into()),
                    ],
                    else_part: vec![]
                },
                Span::Literal(" $ ".into()),
            ]
        })
    );
}

#[cfg(test)]
mod benchmarks {
    use test::Bencher;
    use super::*;
    use crate::context_parser;

    #[bench]
    fn simple_prompt_rendering(b: &mut Bencher) {
        let mut renderer = PromptRenderer::new("$ ");
        let ctx = context_parser::parse("ls -alhG ~", 0);
        b.iter(|| {
            renderer.render(&ctx, 0, 80, None);
        });
    }

    #[bench]
    fn complex_prompt_rendering(b: &mut Bencher) {
        let prompt = "\\{cyan}\\{bold}\\{username}@\\{hostname}:\\{reset} \\{current_dir} $\\{reset} ";
        let mut renderer = PromptRenderer::new(prompt);
        let ctx = context_parser::parse("ls -alhG ~", 0);

        use std::sync::Arc;
        let completions = CompletionSelector::new(vec![
            Arc::new("Desktop".to_owned()),
            Arc::new("Documents".to_owned()),
            Arc::new("Downloads".to_owned()),
            Arc::new("Pictures".to_owned()),
            Arc::new("Videos".to_owned()),
            Arc::new("Dropbox".to_owned()),
        ]);

        b.iter(|| {
            renderer.render(&ctx, 10, 80, Some(&completions));
        });
    }

    #[bench]
    fn complex_prompt_rendering_without_completions(b: &mut Bencher) {
        let prompt = "\\{cyan}\\{bold}\\{username}@\\{hostname}:\\{reset} \\{current_dir} $\\{reset} ";
        let mut renderer = PromptRenderer::new(prompt);
        let ctx = context_parser::parse("ls -alhG ~", 0);

        b.iter(|| {
            renderer.render(&ctx, 10, 80, None);
        });
    }
}
