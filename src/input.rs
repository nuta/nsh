use std::env;
use std::io::{self, prelude::*};
use termion;
use termion::cursor::{DetectCursorPos};
use termion::input::{TermRead};
use termion::event::{Key, Event};
use termion::raw::IntoRawMode;
use syntect::highlighting::Style;
use syntect::util::{as_24_bit_terminal_escaped, LinesWithEndings};
use syntect::easy::HighlightLines;
use syntect::parsing::{SyntaxSet};
use syntect::highlighting::{ThemeSet};
use prompt::{parse_prompt, draw_prompt};

#[derive(Debug)]
pub enum InputError {
    Eof,
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

static DEFAULT_PROMPT: &'static str = "\\c{red}[\\c{bold}\\u@\\h:\\W]$\\c{reset} ";
static DEFAULT_THEME: &'static str = "base16-ocean.dark";

fn create_highlighter(theme_name: &str) -> HighlightLines {
    let theme = &SYNTECT_STATIC.theme_set.themes[theme_name];
    let syntax = SYNTECT_STATIC.syntax_set.find_syntax_by_extension("sh").unwrap();
    HighlightLines::new(syntax, theme)
}

fn get_env(name: &str, default: &str) -> String {
    env::var(name).unwrap_or(default.to_string())
}

pub fn input() -> Result<String, InputError> {
    let mut stdout = io::stdout().into_raw_mode().unwrap();
    let stdin = io::stdin();
    let mut line = String::new();
    let (_, cursor_y_base) = stdout.cursor_pos().unwrap();
    let mut cursor_x = 0; // The relative position in the input line.
    let mut stdin_events = stdin.events();
    let current_theme = get_env("NSH_THEME", DEFAULT_THEME);

    loop {
        // Parse and render the prompt.
        let ps1 = get_env("PS1", DEFAULT_PROMPT);
        let (prompt_str, prompt_len) = if let Ok(fmt) = parse_prompt(ps1.as_str()) {
            draw_prompt(&fmt)
        } else {
            ("$ ".to_owned(), 2)
        };

        // Apply syntax highlighting.
        let mut highlighter = create_highlighter(&current_theme);
        let mut colored_line = String::new();
        for line in LinesWithEndings::from(&line) {
            let ranges: Vec<(Style, &str)> = highlighter.highlight(line, &SYNTECT_STATIC.syntax_set);
            let escaped = as_24_bit_terminal_escaped(&ranges[..], false);
            colored_line += &escaped;
        }

        // Print the prompt.
        let cursor_x_pos = (prompt_len + cursor_x + 1) as u16;
        write!(stdout, "{}{}{}{}{}{}",
            termion::clear::CurrentLine,
            termion::cursor::Goto(0, cursor_y_base),
            prompt_str,
            colored_line,
            termion::style::Reset,
            termion::cursor::Goto(cursor_x_pos, cursor_y_base)
        ).ok();
        stdout.flush().ok();

        // Read a line from stdin.
        match stdin_events.next() {
            Some(event) => {
                let event = event.unwrap();
                trace!("event: {:?}", event);
                match event {
                    Event::Key(Key::Char('\n')) => break,
                    Event::Key(Key::Backspace) => {
                        if cursor_x > 0 {
                            line.remove(cursor_x - 1);
                            cursor_x -= 1;
                        }
                    },
                    Event::Key(Key::Left) => {
                        if cursor_x > 0 {
                            cursor_x -= 1;
                        }
                    },
                    Event::Key(Key::Right) => {
                        if cursor_x < line.len() {
                            cursor_x += 1;
                        }
                    },
                    Event::Key(Key::Ctrl('c')) => return Ok("".to_owned()),
                    Event::Key(Key::Ctrl('d')) => return Err(InputError::Eof),
                    Event::Key(Key::Char(ch)) => {
                        line.insert(cursor_x, ch);
                        cursor_x += 1;
                    }
                    _ => continue,
                }
            },
            None => break,
        }
    }

    trace!("input: '{}'", line);
    Ok(line)
}
