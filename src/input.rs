use crate::completion::{
    call_completion, extract_completion_context, CompletionContext, Completions,
};
use crate::history::{HistorySelector, append_history};
use crate::prompt::render_prompt;
use crate::utils::get_env;
use std::io::{self, prelude::*, Stdout};
use termion;
use termion::cursor::DetectCursorPos;
use termion::event::{Event, Key};
use termion::input::TermRead;
use termion::raw::IntoRawMode;

#[derive(Debug)]
pub enum InputError {
    Eof,
}

static DEFAULT_THEME: &'static str = "base16-ocean.dark";

#[derive(PartialEq, Eq, Copy, Clone)]
pub enum InputMode {
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
        )
        .ok();
    }
}

fn get_current_y(stdout: &mut Stdout) -> u16 {
    let (_, y) = stdout.cursor_pos().unwrap();
    y - 1
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
    let mut print_startup_time = true;
    let mut history = HistorySelector::new();

    'input_line: loop {
        // Print the prompt.
        clear_n_lines(&mut stdout, prompt_base_y, rendered_lines);
        let (rendered_lines2, prompt) = render_prompt(
            mode,
            &completions,
            prompt_base_y,
            user_cursor,
            &user_input,
            &current_theme,
        );
        rendered_lines = rendered_lines2;
        write!(stdout, "{}", prompt).ok();
        stdout.flush().ok();

        if print_startup_time {
            let now = std::time::SystemTime::now();
            trace!("startup time: {:?}", now.duration_since(unsafe { crate::TIME_STARTED.unwrap() }));
            print_startup_time = false;
        }

        // Read a line from stdin.
        match stdin_events.next() {
            Some(event) => {
                let event = event.unwrap();
                trace!("event: {:?}", event);
                match event {
                    Event::Key(Key::Char('\n')) => match mode {
                        InputMode::Normal => break 'input_line,
                        InputMode::Completion => {
                            trace!("ctx: {:?}", completion_ctx);
                            let selected = completions.selected();
                            let prefix = user_input
                                .get(..(completion_ctx.current_word_offset))
                                .unwrap_or("")
                                .to_string();
                            let suffix_offset = completion_ctx.current_word_offset
                                + completion_ctx.current_word_len;
                            let suffix =
                                &user_input.get((suffix_offset)..).unwrap_or("").to_string();
                            user_input = format!("{}{}{}", prefix, selected, suffix);
                            user_cursor = completion_ctx.current_word_offset + selected.len();
                            mode = InputMode::Normal;
                            continue 'input_line;
                        }
                    },
                    Event::Key(Key::Char('\t')) => match mode {
                        InputMode::Completion => {
                            completions.move_cursor(1);
                        }
                        InputMode::Normal => {
                            completion_ctx = extract_completion_context(&user_input, user_cursor);
                            completions = call_completion(&completion_ctx);
                            mode = InputMode::Completion;
                        }
                    },
                    Event::Key(Key::Backspace) => {
                        if user_cursor > 0 {
                            user_input.remove(user_cursor - 1);
                            user_cursor -= 1;
                        }

                        if mode == InputMode::Completion {
                            completion_ctx = extract_completion_context(&user_input, user_cursor);
                            completions = call_completion(&completion_ctx);
                        }
                    }
                    Event::Key(Key::Up) => {
                        match mode {
                            InputMode::Normal => {
                                history.prev(&user_input);
                                let line = history.current();
                                user_input = line.to_string();
                                user_cursor = user_input.len();
                            },
                            InputMode::Completion => {
                                // Move to the previous candidate.
                                completions.move_cursor(-1);
                            }
                        }
                    }
                    Event::Key(Key::Down) => {
                        match mode {
                            InputMode::Normal => {
                                history.next();
                                let line = history.current();
                                user_input = line.to_string();
                                user_cursor = user_input.len();
                            },
                            InputMode::Completion => {
                                // Move to the next candidate.
                                completions.move_cursor(1);
                            }
                        }
                    }
                    Event::Key(Key::Left) => {
                        if user_cursor > 0 {
                            user_cursor -= 1;
                        }

                        if mode == InputMode::Completion {
                            mode = InputMode::Normal;
                        }
                    }
                    Event::Key(Key::Right) => {
                        if user_cursor < user_input.len() {
                            user_cursor += 1;
                        }

                        if mode == InputMode::Completion {
                            mode = InputMode::Normal;
                        }
                    }
                    Event::Key(Key::Ctrl('c')) => match mode {
                        InputMode::Normal => return Ok("".to_owned()),
                        InputMode::Completion => {
                            mode = InputMode::Normal;
                        }
                    },
                    Event::Key(Key::Ctrl('d')) => return Err(InputError::Eof),
                    Event::Key(Key::Ctrl('l')) => {
                        // Clear the screen. Will be flushed after the prompt rendering.
                        write!(stdout, "{}", termion::clear::All).ok();
                        prompt_base_y = 0;
                    }
                    Event::Key(Key::Char(ch)) => match (mode, ch) {
                        (_, ch) => {
                            user_input.insert(user_cursor, ch);
                            user_cursor += 1;

                            if mode == InputMode::Completion {
                                completion_ctx =
                                    extract_completion_context(&user_input, user_cursor);
                                completions = call_completion(&completion_ctx);
                            }
                        }
                    },
                    _ => continue,
                }
            }
            None => break 'input_line,
        }
    }

    append_history(&user_input);
    trace!("input: '{}'", user_input);
    Ok(user_input)
}
