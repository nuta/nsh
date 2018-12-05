use crate::completion::{
    call_completion, extract_completion_context, CompletionContext, Completions,
};
use crate::history::{HistorySelector, append_history};
use crate::prompt::render_prompt;
use crate::config::Config;
use std::io::{self, Write, Stdout};
use termion;
use termion::cursor::DetectCursorPos;
use termion::event::{Event, Key};
use termion::input::TermRead;
use termion::raw::IntoRawMode;

#[derive(Debug)]
pub enum InputError {
    Eof,
}

#[derive(PartialEq, Eq, Copy, Clone)]
pub enum InputMode {
    Normal,
    Completion,
}

/// Returns the cursor position (0-origin).
fn get_current_yx(stdout: &mut Stdout) -> (u16, u16) {
    let (x, y) = stdout.cursor_pos().unwrap();
    (y - 1, x - 1)
}

fn select_completion(completions: &Completions, completion_ctx: &CompletionContext, selected: usize, user_input: &mut String, user_cursor: &mut usize) {
    if let Some(selected) = completions.get(selected) {
        let prefix = user_input
            .get(..(completion_ctx.current_word_offset))
            .unwrap_or("")
            .to_string();
        let suffix_offset = completion_ctx.current_word_offset
            + completion_ctx.current_word_len;
        let suffix =
            &user_input.get((suffix_offset)..).unwrap_or("").to_string();
        *user_input = format!("{}{}{}", prefix, selected, suffix);
        *user_cursor = completion_ctx.current_word_offset + selected.len();
    }
}

pub fn input(config: &Config) -> Result<String, InputError> {
    let mut stdout = io::stdout().into_raw_mode().unwrap();
    let stdin = io::stdin();
    let (_, current_x) = get_current_yx(&mut stdout);
    if current_x != 0 {
        // The prompt is not at the beginning of a line. This could be caused
        // if the previous command didn't print the trailing newline
        // (e.g. `echo -n hello`). Print a marker `%' and a newline.
        writeln!(stdout, "{}{}(no newline){}",
            termion::style::Bold,
            termion::style::Invert,
            termion::style::Reset
        ).ok();
    }

    let (mut prompt_y, _) = get_current_yx(&mut stdout);

    let mut user_input = String::new();
    let mut user_cursor = 0; // The relative position in the input line. 0-origin.
    let mut stdin_events = stdin.events();
    let current_theme = "Solarized (dark)";
    let mut mode = InputMode::Normal;
    let mut rendered_lines = 0;
    // TODO: move these variables into InputMode::Completion.
    let mut completions = Completions::new(vec![]);
    let mut completion_ctx: CompletionContext = Default::default();
    let mut history = HistorySelector::new();

    'input_line: loop {
        // Print the prompt.
        let y_max = termion::terminal_size().map(|(_, y)| y - 1).unwrap();
        let (rendered_lines2, prompt_y2, prompt) = render_prompt(
            &config.prompt,
            mode,
            &completions,
            prompt_y,
            y_max,
            rendered_lines,
            user_cursor,
            &user_input,
            &current_theme,
        );

        write!(stdout, "{}", prompt).ok();
        stdout.flush().ok();
        prompt_y = prompt_y2;
        rendered_lines = rendered_lines2;

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
                            select_completion(
                                &completions,
                                &completion_ctx,
                                completions.selected_index(),
                                &mut user_input,
                                &mut user_cursor
                            );
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
                            if completions.len() == 1 {
                                // There is only one completion candidate. Select it and continue
                                // normal input mode.
                                select_completion(
                                    &completions,
                                    &completion_ctx,
                                    0,
                                    &mut user_input,
                                    &mut user_cursor
                                );
                            } else {
                                mode = InputMode::Completion;
                            }
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
                    Event::Key(Key::Ctrl('a')) => {
                        user_cursor = 0;
                        mode = InputMode::Normal;
                    },
                    Event::Key(Key::Ctrl('e')) => {
                        user_cursor = user_input.len();
                        mode = InputMode::Normal;
                    },
                    Event::Key(Key::Ctrl('k')) => {
                        user_input.truncate(user_cursor);
                    },
                    Event::Key(Key::Ctrl('c')) => match mode {
                        InputMode::Normal => return Ok("".to_owned()),
                        InputMode::Completion => {
                            mode = InputMode::Normal;
                        }
                    },
                    Event::Key(Key::Ctrl('d')) => {
                        if user_cursor < user_input.len() {
                            user_input.remove(user_cursor);
                        } else if user_input.is_empty() {
                            return Err(InputError::Eof)
                        }
                    },
                    Event::Key(Key::Ctrl('l')) => {
                        // Clear the screen. Will be flushed after the prompt rendering.
                        write!(stdout, "{}", termion::clear::All).ok();
                        prompt_y = 0;
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
