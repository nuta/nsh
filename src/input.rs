use crate::completion::{CompletionSelector};
use crate::exec::Isolate;
use crate::context_parser;
use crate::history::{HistorySelector, append_history};
use crate::prompt::PromptRenderer;
use crate::config::Config;
use std::io::{self, Write};
use std::sync::{Arc, Mutex};
use termion;
use termion::cursor::DetectCursorPos;
use termion::event::{Event, Key};
use termion::input::TermRead;
use termion::raw::IntoRawMode;

#[derive(Debug)]
pub enum InputError {
    Eof,
}

enum InputMode {
    Normal,
    Completion(CompletionSelector),
}

/// Prints the prompt and read a line from stdin.
pub fn input(config: &Config, isolate_lock: Arc<Mutex<Isolate>>) -> Result<String, InputError> {
    let mut stdout = io::stdout().into_raw_mode().unwrap();
    let stdin = io::stdin();
    let current_x = stdout.cursor_pos().unwrap().0 - 1;
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

    let current_theme = "Solarized (dark)";
    let mut user_input = String::new();
    let mut user_cursor = 0; // The relative position in the input line. 0-origin.
    let mut mode = InputMode::Normal;
    let mut history = HistorySelector::new();
    let mut renderer = PromptRenderer::new(&mut stdout, &config.prompt, &current_theme);
    let mut stdin_events = stdin.events();

    'input_line: loop {
        // Print the prompt.
        let prompt = match &mode {
            InputMode::Completion(completion) => {
                renderer.render(&user_input, user_cursor, Some(completion))
            }
            InputMode::Normal => {
                renderer.render(&user_input, user_cursor, None)
            }
        };

        write!(stdout, "{}", prompt).ok();
        stdout.flush().ok();

        // Read a line from stdin.
        let event = stdin_events.next();
        // Parse the current line.
        let asa = context_parser::parse(&user_input, user_cursor);

        match event {
            Some(event) => {
                let event = event.unwrap();
                trace!("event: {:?}", event);
                match event {
                    Event::Key(Key::Char('\n')) => match &mut mode {
                        InputMode::Normal => break 'input_line,
                        InputMode::Completion(completion) => {
                            trace!("ctx: {:?}", asa);
                            completion.select_and_update_input_and_cursor(&asa, &mut user_input, &mut user_cursor);
                            mode = InputMode::Normal;
                            continue 'input_line;
                        }
                    },
                    Event::Key(Key::Char('\t')) => match &mut mode {
                        InputMode::Completion(completion) => {
                            completion.move_cursor(1);
                        }
                        InputMode::Normal => {
                            let mut isolate = isolate_lock.lock().unwrap();
                            let completion = CompletionSelector::new(isolate.complete(&asa));
                            if completion.len() == 1 {
                                // There is only one completion candidate. Select it and go back into
                                // normal input mode.
                                completion.select_and_update_input_and_cursor(&asa, &mut user_input, &mut user_cursor);
                            } else {
                                mode = InputMode::Completion(completion);
                            }
                        }
                    },
                    Event::Key(Key::Backspace) => {
                        if user_cursor > 0 {
                            user_input.remove(user_cursor - 1);
                            user_cursor -= 1;
                        }

                        if let InputMode::Completion(_) = mode {
                            let mut isolate = isolate_lock.lock().unwrap();
                            let new = CompletionSelector::new(isolate.complete(&asa));
                            mode = InputMode::Completion(new);
                        }
                    }
                    Event::Key(Key::Up) => {
                        match &mut mode {
                            InputMode::Normal => {
                                history.prev(&user_input);
                                let line = history.current();
                                user_input = line.to_string();
                                user_cursor = user_input.len();
                            },
                            InputMode::Completion(completion) => {
                                // Move to the previous candidate.
                                completion.move_cursor(-1);
                            }
                        }
                    }
                    Event::Key(Key::Down) => {
                        match &mut mode {
                            InputMode::Normal => {
                                history.next();
                                let line = history.current();
                                user_input = line.to_string();
                                user_cursor = user_input.len();
                            },
                            InputMode::Completion(completion) => {
                                // Move to the next candidate.
                                completion.move_cursor(1);
                            }
                        }
                    }
                    Event::Key(Key::Left) => {
                        if user_cursor > 0 {
                            user_cursor -= 1;
                        }

                        if let InputMode::Completion(_) = mode {
                            mode = InputMode::Normal;
                        }
                    }
                    Event::Key(Key::Right) => {
                        if user_cursor < user_input.len() {
                            user_cursor += 1;
                        }

                        if let InputMode::Completion(_) = mode {
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
                    Event::Key(Key::Alt('b')) => {
                        // Skip the whitespace at the current position.
                        user_cursor = user_cursor.saturating_sub(1);

                        while user_cursor > 0 {
                            if let Some(ch) = user_input.chars().nth(user_cursor.saturating_sub(1)) {
                                if ch == ' ' || ch == '/' {
                                    break;
                                }
                            } else {
                                break;
                            }

                            user_cursor -= 1;
                        }

                        mode = InputMode::Normal;
                    },
                    Event::Key(Key::Alt('f')) => {
                        // Skip the whitespace at the current position.
                        user_cursor += 1;
                        if user_cursor > user_input.len() {
                            user_cursor = user_input.len();
                        }

                        while user_cursor < user_input.len() {
                            if let Some(ch) = user_input.chars().nth(user_cursor.saturating_sub(1)) {
                                if ch == ' ' || ch == '/' {
                                    break;
                                }
                            } else {
                                break;
                            }

                            user_cursor += 1;
                        }

                        debug!("cursor: {}, {}", user_cursor, user_input.len());
                        mode = InputMode::Normal;
                    },
                    Event::Key(Key::Ctrl('k')) => {
                        user_input.truncate(user_cursor);
                    },
                    Event::Key(Key::Ctrl('c')) => match mode {
                        InputMode::Normal => return Ok("".to_owned()),
                        InputMode::Completion(_) => {
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
                        renderer.clear_screen(&mut stdout);
                    }
                    Event::Key(Key::Char(ch)) => match (&mut mode, ch) {
                        (_, ch) => {
                            user_input.insert(user_cursor, ch);
                            user_cursor += 1;

                            if let InputMode::Completion(_) = mode {
                                let mut isolate = isolate_lock.lock().unwrap();
                                let new = CompletionSelector::new(isolate.complete(&asa));
                                mode = InputMode::Completion(new);
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
