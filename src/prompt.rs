use crate::completion::CompletionSelector;
use syntect::easy::HighlightLines;
use syntect::highlighting::Style;
use syntect::highlighting::ThemeSet;
use syntect::parsing::SyntaxSet;
use syntect::util::{as_24_bit_terminal_escaped, LinesWithEndings};
use pest::Parser;
use pest::iterators::{Pairs, Pair};
use std::fmt::Write;
use termion;
use termion::cursor::DetectCursorPos;
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

pub struct PromptRenderer {
    prompt_fmt: String,
    prompt_y: u16,
    y_max: u16,
    x_max: u16,
    prev_rendered_lines: u16,
    current_theme: String,
}

impl PromptRenderer {
    pub fn new(stdout: &mut std::io::Stdout, prompt_fmt: &str, current_theme: &str) -> PromptRenderer {
        let (x_max, y_max) = termion::terminal_size().map(|(x, y)| (x - 1, y - 1)).unwrap();
        let (_, prompt_y) = stdout.cursor_pos().map(|(x, y)| (x - 1, y - 1)).unwrap();
        PromptRenderer {
            prompt_fmt: prompt_fmt.to_owned(),
            current_theme: current_theme.to_owned(),
            prompt_y,
            y_max,
            x_max,
            prev_rendered_lines: 0,
        }
    }

    /// Clear the screen. Don't forget to flush; this method does not flush `stdout` since
    /// this method is intended to be used just before printing the prompt.
    pub fn clear_screen(&mut self, stdout: &mut std::io::Stdout) {
        use std::io::Write;
        write!(stdout, "{}", termion::clear::All).ok();
        self.prompt_y = 0;
    }

    /// Renders the prompt, the user input, and completions (if supplied).
    /// TODO: needs refactoring
    /// TODO: handle terminal screen size changes
    pub fn render(&mut self, user_input: &str, user_cursor: usize, completions: Option<&CompletionSelector>) -> String {
        // Apply syntax highlighting.
        let mut highlighter = create_highlighter(&self.current_theme);
        let mut colored_user_input = String::new();
        for line in LinesWithEndings::from(user_input) {
            let ranges: Vec<(Style, &str)> = highlighter.highlight(line, &SYNTECT_STATIC.syntax_set);
            let escaped = as_24_bit_terminal_escaped(&ranges[..], false);
            colored_user_input += &escaped;
        }

        // Parse and render the prompt.
        let (prompt_str, prompt_last_line_len) = match parse_prompt(&self.prompt_fmt) {
            Ok(fmt) => draw_prompt(&fmt),
            Err(err) => {
                eprintln!("nsh: failed to parse $PROMPT: {}", err);
                ("$ ".to_owned(), 2)
            }
        };

        use termion::clear::CurrentLine;
        use termion::color::{Fg, Bg, White, Red};
        use termion::style::*;

        // Render completions.
        let mut completion_str = String::new();
        if let Some(completions) =completions {
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
                    CurrentLine, Bold, Fg(White), Bg(Red)
                )
                .ok();
            }
        }

        //
        //                                    TERMINAL
        //                    _________________________________________
        //           > >     |[chandler@ross-macbook.local]            |
        //           > > >   |$ for x in a b c d very_loooooooooooooooo|
        //           > | >   |ooooooooooooooooooooooooooooooooooooooong| <- wrapped
        //           > | >   |_wrapped_word do ssh[TAB]                | <- wrapped
        //           > | | > |monica-hidden-door.local                 |
        //           > | | > |joey-chair.local                         |
        //           > | | > |1/2                                      |
        //           | | | | |                                         |
        //           | | | | |                                         |
        //           | | | | |                                         |
        //           | | | |  -----------------------------------------
        //           | | | |
        //           | | | +- completion_lines = 3
        //           | | +- user_input_lines = 4
        //           | +- prompt_lines = 2
        //           |
        //           +- rendered_lines = 9
        //
        let mut buf = String::new();
        let prompt_lines = prompt_str.chars().filter(|c| *c == '\n').count() as u16 + 1;
        let completion_lines = if completion_str.is_empty() {
            0
        } else {
            1 + completion_str.chars().filter(|c| *c == '\n').count() as u16
        };
        let user_input_lines = ((prompt_last_line_len as u16 + user_input.len() as u16) / (self.x_max + 1)) + 1;
        let rendered_lines = prompt_lines + (user_input_lines - 1) + completion_lines;
        let avail = std::cmp::max(0, i32::from(self.y_max) - i32::from(self.prompt_y));

        // In case the prompt at the bottom of the screen some scroll are needed.
        let scroll = std::cmp::max(0, i32::from(rendered_lines) - avail - 1) as u16;
        let new_prompt_y = self.prompt_y - scroll;

        for _ in 0..std::cmp::max(0, scroll) {
            writeln!(buf).ok();
        }

        // Clear the current prompt.
        for i in 0..self.prev_rendered_lines {
            write!(
                buf,
                "{}{}",
                termion::cursor::Goto(1, 1 + self.prompt_y + i),
                termion::clear::CurrentLine,
            )
            .ok();
        }

        // Render the prompt and colored user input.
        let cursor_y = new_prompt_y + (prompt_lines - 1) + ((prompt_last_line_len + user_cursor) as u16 / (self.x_max + 1));
        let cursor_x = (prompt_last_line_len as u16 + user_cursor as u16) % (self.x_max + 1);
        write!(
            buf,
            "{}{}{}{}{}{}{}",
            replace_newline_with_clear(&prompt_str, new_prompt_y),
            Reset,
            colored_user_input,
            Reset,
            replace_newline_with_clear(&completion_str, new_prompt_y + prompt_lines),
            termion::cursor::Goto(1 + cursor_x, 1 + cursor_y),
            Reset,
        ).ok();

        trace!("prompt_offset: S={} M={} base={}, new_y={}", scroll, self.y_max, self.prompt_y, new_prompt_y);
        self.prev_rendered_lines = rendered_lines;
        self.prompt_y = new_prompt_y;

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

/* FIXME: Refactor PromptRender not to modify stdout.
#[cfg(test)]
mod benchmarks {
    use test::Bencher;
    use super::*;

    #[bench]
    fn simple_prompt_rendering_bench(b: &mut Bencher) {
        b.iter(|| {
            let mode = InputMode::Normal;
            let completions = CompletionSelector::new(vec![]);
            let theme = "Solarized (dark)";
            render_prompt("$ ", mode, &completions, 0, 64, 0, 3, "ls -alhG", &theme);
        });
    }

    #[bench]
    fn complex_prompt_rendering_bench(b: &mut Bencher) {
        b.iter(|| {
            let prompt = "\\{cyan}\\{bold}\\{username}@\\{hostname}:\\{reset} \\{current_dir}\\n$\\{reset} ";
            let mode = InputMode::Normal;
            let completions = CompletionSelector::new(vec![]);
            let theme = "Solarized (dark)";
            render_prompt(&prompt, mode, &completions, 0, 64, 64, 3, "ls -alhG", &theme);
        });
    }
}
*/
