use std::env;
use std::io::{self, Stdout, prelude::*};
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
use completion::{Completions, CompletionContext, call_completion, extract_completion_context};
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

static DEFAULT_PROMPT: &'static str = "\\c{red}\\c{bold}[\\u@\\h:\\W]$\\c{reset} ";
static DEFAULT_THEME: &'static str = "base16-ocean.dark";

fn create_highlighter(theme_name: &str) -> HighlightLines {
    let theme = &SYNTECT_STATIC.theme_set.themes[theme_name];
    let syntax = SYNTECT_STATIC.syntax_set.find_syntax_by_extension("sh").unwrap();
    HighlightLines::new(syntax, theme)
}

fn get_env(name: &str, default: &str) -> String {
    env::var(name).unwrap_or_else(|_| default.to_string())
}

#[derive(PartialEq, Eq, Copy, Clone)]
enum InputMode {
    Normal,
    Completion,
}

/// Clears `n` lines from `base`th line in the terminal. The caller
/// should `stdout.flush()`. `base` is 0-origin.
fn clear_n_lines(stdout: &mut Stdout, base: u16, n: u16) {
    for i in 0..n {
        write!(
            stdout,
            "{}{}",
            termion::cursor::Goto(1, 1 + base + i),
            termion::clear::CurrentLine,
        ).ok();
    }
}

fn get_current_y(stdout: &mut Stdout) -> u16 {
    let (_, y) = stdout.cursor_pos().unwrap();
    y - 1
}

/// Returns the number of lines of the rendered prompt and the rendered prompt.
/// FIXME: too many arguments
fn render_prompt(mode: InputMode, completions: &Completions,  prompt_base_y: u16, user_cursor: usize, user_input: &str, current_theme: &str) -> (u16, String) {
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
    write!(buf, "{}{}{}{}{}",
        termion::style::Reset,
        termion::cursor::Goto(1, 1 + prompt_base_y),
        prompt_str,
        colored_user_input,
        termion::style::Reset,
    ).ok();

    // Render completions.
    match mode {
        InputMode::Normal => {
            rendered_lines = 1;
        },
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
                let iter = results.iter()
                    .skip(completions.display_index())
                    .take(completions.display_lines());

                for (i, entry) in iter.enumerate() {
                    write!(buf, "\n{}{}",
                        termion::cursor::Goto(1, 1 + completion_base_y + i as u16),
                        termion::clear::CurrentLine,
                    ).ok();

                    let selected = (completions.display_index() + i) == completions.selected_index() as usize;
                    if selected {
                        write!(buf, "{}{}", termion::style::Underline, termion::style::Bold).ok();
                    } else {
                        write!(buf, "{}{}", termion::style::NoUnderline, termion::style::NoBold).ok();
                    };

                    write!(buf, "{}{}", entry, termion::style::Reset).ok();
                }

                write!(buf, "\n{}{}{}{}{} {}/{} {}",
                    termion::cursor::Goto(1, 1 + completion_base_y + actual_lines),
                    termion::clear::CurrentLine,
                    termion::style::Bold,
                    termion::color::Fg(termion::color::White),
                    termion::color::Bg(termion::color::Cyan),
                    completions.selected_index() + 1,
                    completions.len(),
                    termion::style::Reset
                ).ok();
            } else {
                rendered_lines = 2;
                write!(buf, "\n{}{}{}{}{}no candidates{}",
                    termion::cursor::Goto(1, 1 + prompt_base_y + 1),
                    termion::clear::CurrentLine,
                    termion::style::Bold,
                    termion::color::Fg(termion::color::White),
                    termion::color::Bg(termion::color::Cyan),
                    termion::style::Reset
                ).ok();
            }
        },
    }

    write!(
        buf, "{}",
        termion::cursor::Goto(user_cursor_pos, 1 + prompt_base_y)
    ).ok();

    (rendered_lines, buf)
}

pub fn input() -> Result<String, InputError> {
    let mut stdout = io::stdout().into_raw_mode().unwrap();
    let stdin = io::stdin();
    let mut user_input = String::new();
    let mut prompt_base_y = get_current_y(&mut stdout); // 0-origin.
    let mut user_cursor = 0; // The relative position in the input line. 0-origin.
    let mut stdin_events = stdin.events();
    let current_theme = get_env("NSH_THEME", DEFAULT_THEME);
    let mut mode = InputMode::Normal;
    let mut rendered_lines = 0;
    // TODO: move these variables into InputMode::Completion.
    let mut completions = Completions::new(vec![]);
    let mut completion_ctx: CompletionContext = Default::default();

'input_line: loop {
        // Print the prompt.
        clear_n_lines(&mut stdout, prompt_base_y, rendered_lines);
        let (rendered_lines2, prompt) = render_prompt(mode, &completions, prompt_base_y, user_cursor, &user_input, &current_theme);
        rendered_lines = rendered_lines2;
        write!(stdout, "{}", prompt).ok();
        stdout.flush().ok();

        // Read a line from stdin.
        match stdin_events.next() {
            Some(event) => {
                let event = event.unwrap();
                trace!("event: {:?}", event);
                match event {
                    Event::Key(Key::Char('\n')) => {
                        match mode {
                            InputMode::Normal => break 'input_line,
                            InputMode::Completion => {
                                trace!("ctx: {:?}", completion_ctx);
                                let selected = completions.selected();
                                let prefix = user_input.get(..(completion_ctx.current_word_offset)).unwrap_or("").to_string();
                                let suffix_offset = completion_ctx.current_word_offset + completion_ctx.current_word_len;
                                let suffix = &user_input.get((suffix_offset)..).unwrap_or("").to_string();
                                user_input = format!("{}{}{}", prefix, selected, suffix);
                                user_cursor = completion_ctx.current_word_offset + selected.len();
                                mode = InputMode::Normal;
                                continue 'input_line;
                            }
                        }
                    },
                    Event::Key(Key::Char('\t')) => {
                        match mode {
                            InputMode::Completion => {
                                completions.move_cursor(1);
                            },
                            InputMode::Normal => {
                                completion_ctx = extract_completion_context(&user_input, user_cursor);
                                completions = call_completion(&completion_ctx);
                                mode = InputMode::Completion;
                            },
                        }
                    },
                    Event::Key(Key::Backspace) => {
                        if user_cursor > 0 {
                            user_input.remove(user_cursor - 1);
                            user_cursor -= 1;
                        }

                        if mode == InputMode::Completion {
                            completions = call_completion(&completion_ctx);
                        }
                    },
                    Event::Key(Key::Up) => {
                        match mode {
                            InputMode::Normal => (), // TODO: history
                            InputMode::Completion => {
                                // Move to the previous candidate.
                                completions.move_cursor(-1);
                            },
                        }
                    },
                    Event::Key(Key::Down) => {
                        match mode {
                            InputMode::Normal => (), // TODO: history
                            InputMode::Completion => {
                                // Move to the next candidate.
                                completions.move_cursor(1);
                            },
                        }
                    },
                    Event::Key(Key::Left) => {
                        if user_cursor > 0 {
                            user_cursor -= 1;
                        }

                        if mode == InputMode::Completion {
                            mode = InputMode::Normal;
                        }
                    },
                    Event::Key(Key::Right) => {
                        if user_cursor < user_input.len() {
                            user_cursor += 1;
                        }

                        if mode == InputMode::Completion {
                            mode = InputMode::Normal;
                        }
                    },
                    Event::Key(Key::Ctrl('c')) => {
                        match mode {
                            InputMode::Normal => {
                                return Ok("".to_owned())
                            },
                            InputMode::Completion => {
                                mode = InputMode::Normal;
                            },
                        }
                    },
                    Event::Key(Key::Ctrl('d')) => return Err(InputError::Eof),
                    Event::Key(Key::Ctrl('l')) => {
                        // Clear the screen. Will be flushed after the prompt rendering.
                        write!(stdout, "{}", termion::clear::All).ok();
                        prompt_base_y = 0;
                    },
                    Event::Key(Key::Char(ch)) => {
                        match (mode, ch) {
                            (_, ch) => {
                                user_input.insert(user_cursor, ch);
                                user_cursor += 1;

                                if mode == InputMode::Completion {
                                    completion_ctx = extract_completion_context(&user_input, user_cursor);
                                    completions = call_completion(&completion_ctx);
                                }
                            }
                        }

                    }
                    _ => continue,
                }
            },
            None => break 'input_line,
        }
    }

    trace!("input: '{}'", user_input);
    Ok(user_input)
}
