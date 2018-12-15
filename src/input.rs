use crate::completion::{CompletionSelector};
use crate::exec::Isolate;
use crate::context_parser;
use crate::history::{HistorySelector, search_history, append_history};
use crate::prompt::PromptRenderer;
use crate::config::Config;
use std::io::{self, Write, Stdout, Stdin};
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

#[inline]
fn restore_main_screen(stdout: &mut Stdout) {
    write!(stdout, "{}", termion::screen::ToMainScreen).ok();
    stdout.flush().ok();
}

#[inline]
fn truncate(s: &str, len: u16) -> String {
    let mut string = s.to_owned();
    string.truncate(len as usize);
    string
}

#[inline]
fn truncate_and_fill(s: &str, len: u16, fill: char) -> String {
    let mut s = truncate(s, len);
    for _ in s.len()..(len as usize) {
        s.push(fill);
    }

    s
}

/// Returns true if the user wants to execute the command immediately.
fn history_search_mode(
    stdout: &mut Stdout,
    y_max: u16,
    x_max: u16,
    events: &mut termion::input::Events<Stdin>,
    user_input: &mut String
) -> bool {
    let mut selected = 0;
    let saved_user_input = user_input.clone();
    let mut user_cursor = user_input.len();
    let display_len = y_max - 2;
    let user_input_max = x_max - 9;
    let prompt = "history> ";
    loop {
        //
        //         ____________________________________
        //         |history> user input here|          |  input_line
        //       > |ls -alG                            |  history_lines
        //       > |cowsay meow                        |  |
        //       > |brew update                        |  |
        //       > |ls                                 |  |
        //       > |cd ~/Documents                     |  |
        //       | |Enter: Execute, ^C: Quit           |  |
        //       | ------------------------------------   howto_line
        //       |
        //       +- display_len == 4
        //

        // Render input_line.
        let mut input_line = format!(
            "{}{}{}{}",
            termion::cursor::Goto(1, 1),
            termion::style::Bold,
            truncate(prompt, x_max),
            termion::style::Reset
        );

        input_line += &format!(
            "{}{}",
            termion::cursor::Goto(1 + prompt.len() as u16, 1),
            truncate(user_input, user_input_max)
        );

        // Render how_line.
        let howto_line = format!(
            "{}{}{}{}{}",
            termion::cursor::Goto(1, 1 + y_max),
            termion::style::Bold,
            termion::style::Invert,
            truncate(" Enter: Execute, Tab: Edit, ^C: Quit ", x_max),
            termion::style::Reset
        );

        // Search history for user input.
        let mut history_lines = String::new();
        let entries = search_history(user_input);
        let max = std::cmp::min(display_len as usize, entries.len());
        if selected > max.saturating_sub(1) {
            selected = max.saturating_sub(1);
        }

        // Render history_lines.
        for i in 0..y_max.saturating_sub(2) {
            if let Some(entry) = entries.get(i as usize) {
                if i == selected as u16 {
                    history_lines += &format!(
                        "{}{}{}{}{}{}",
                        termion::cursor::Goto(1, 2 + i),
                        termion::color::Fg(termion::color::Green),
                        termion::style::Bold,
                        termion::style::Underline,
                        truncate_and_fill(entry, x_max, ' '),
                        termion::style::Reset
                    );
                } else {
                    history_lines += &format!(
                        "{}{}",
                        termion::cursor::Goto(1, 2 + i),
                        truncate(entry, x_max)
                    );
                }
            }
        }

        // Print the screen.
        write!(
            stdout,
            "{}{}{}{}{}{}{}",
            termion::screen::ToAlternateScreen,
            termion::clear::All,
            termion::style::Reset,
            input_line,
            history_lines,
            howto_line,
            termion::cursor::Goto(1 + prompt.len() as u16 + user_cursor as u16, 1)
        ).ok();
        stdout.flush().ok();

        // Wait for keyboard events.
        match events.next() {
            Some(event) => {
                let event = event.unwrap();
                match event {
                    // Execute the selected command.
                    Event::Key(Key::Char('\n')) => {
                        restore_main_screen(stdout);
                        if let Some(s) = entries.get(selected) {
                            *user_input = s.as_str().to_owned();
                            return true;
                        } else {
                            // No history entries. Abort history mode.
                            *user_input = saved_user_input;
                            return false;
                        }
                    },
                    // Fill user input by the selected command and continue editing.
                    Event::Key(Key::Char('\t')) => {
                        restore_main_screen(stdout);
                        if let Some(s) = entries.get(selected) {
                            *user_input = s.as_str().to_owned();
                        } else {
                            // No history entries. Abort history mode.
                            *user_input = saved_user_input;
                        }

                        return false;
                    },
                    // Move the user input cursor to left.
                    Event::Key(Key::Left) => {
                        user_cursor = user_cursor.saturating_sub(1);
                    },
                    // Move the user input cursor to right.
                    Event::Key(Key::Right) => {
                        user_cursor += 1;
                        if user_cursor > user_input.len() {
                            user_cursor = user_input.len();
                        }
                    },
                    // Select the previous history.
                    Event::Key(Key::Up) => {
                        selected = selected.saturating_sub(1);
                    },
                    // Select the next history.
                    Event::Key(Key::Down) => {
                        selected += 1;
                        let max = std::cmp::min(display_len as usize, entries.len());
                        if selected > max.saturating_sub(1) {
                            selected = max.saturating_sub(1);
                        }
                    },
                    // Remove the previous character in the user input.
                    Event::Key(Key::Backspace) => {
                        if user_cursor > 0 {
                            user_input.remove(user_cursor - 1);
                            user_cursor -= 1;
                        }
                    },
                    // Remove the next character in the user input.
                    Event::Key(Key::Ctrl('d')) => {
                        if user_cursor < user_input.len() {
                            user_input.remove(user_cursor);
                        }
                    },
                    // Abort history mode.
                    Event::Key(Key::Ctrl('c')) => {
                        restore_main_screen(stdout);
                        *user_input = saved_user_input;
                        return false;
                    },
                    // An any key input.
                    Event::Key(Key::Char(ch)) => {
                        if user_input.len() < user_input_max as usize {
                            user_input.insert(user_cursor, ch);
                            user_cursor += 1;
                        }
                    },
                    ev => {
                        trace!("ignored event: {:?}", ev);
                    },
                }
            },
            _ => unreachable!()
        }
    }
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
    let (x_max, y_max) = termion::terminal_size().unwrap();
    let mut renderer = PromptRenderer::new(&mut stdout, &config.prompt, &current_theme, y_max, x_max);
    let mut stdin_events = stdin.events();
    let mut exec = false;

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

        if exec {
            // The user input is filled by history search.
            break;
        }

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
                    Event::Key(Key::Ctrl('r')) => {
                        exec = history_search_mode(&mut stdout, y_max, x_max, &mut stdin_events, &mut user_input);
                        user_cursor = user_input.len();
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
