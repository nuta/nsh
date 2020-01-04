use crate::completion::{CompletionSelector};
use crate::exec::Isolate;
use crate::context_parser;
use crate::history::{HistorySelector, search_history, append_history};
use crate::prompt::PromptRenderer;
use std::io::{self, Write, Stdout, Stdin};
use termion;
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
    s.chars().take(len as usize).collect()
}

#[inline]
fn truncate_and_fill(s: &str, len: u16, fill: char) -> String {
    let mut s = truncate(s, len);
    for _ in s.len()..(len as usize) {
        s.push(fill);
    }

    s
}

struct UserInput {
    input: String,
    indices: Vec<usize>,
}

impl UserInput {
    pub fn new() -> UserInput {
        UserInput {
            input: String::new(),
            indices: Vec::new(),
        }
    }

    pub fn from_str(s: &str) -> UserInput {
        let mut user_input = UserInput {
            input: s.to_owned(),
            indices: Vec::new(),
        };

        user_input.update_indices();
        user_input
    }

    pub fn clone(&self) -> UserInput {
        UserInput {
            input: self.input.clone(),
            indices: self.indices.clone(),
        }
    }

    pub fn insert(&mut self, index: usize, ch: char) {
        trace!("{}: {}", self.indices.len(), index);
        let byte_index = if index == self.indices.len() {
            self.input.len()
        } else {
            self.indices[index]
        };

        self.input.insert(byte_index, ch);
        self.update_indices();
    }

    pub fn remove(&mut self, index: usize) {
        self.input.remove(self.indices[index]);
        self.update_indices();
    }

    pub fn truncate(&mut self, index: usize) {
        self.input.truncate(self.indices[index]);
        self.update_indices();
    }

    pub fn nth(&mut self, index: usize) -> Option<char> {
        self.input.chars().nth(index)
    }

    pub fn as_str(&self) -> &str {
        self.input.as_str()
    }

    pub fn len(&self) -> usize {
        self.indices.len()
    }

    pub fn is_empty(&self) -> bool {
        self.input.is_empty()
    }

    fn update_indices(&mut self) {
        self.indices.clear();
        for index in self.input.char_indices() {
            self.indices.push(index.0);
        }
    }
}

/// Returns true if the user wants to execute the command immediately.
fn history_search_mode(stdout: &mut Stdout, events: &mut termion::input::Events<Stdin>, user_input: &mut UserInput) -> bool {
    let (x_max, y_max) = termion::terminal_size().unwrap();
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
            truncate(user_input.as_str(), user_input_max)
        );

        // Render howto_line.
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
        let entries = search_history(user_input.as_str());
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
                            *user_input = UserInput::from_str(s.as_str());
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
                            *user_input = UserInput::from_str(s.as_str());
                        } else {
                            // No history entries. Abort history mode.
                            *user_input = saved_user_input;
                        }

                        return false;
                    },
                    // Move the user input cursor to left.
                    Event::Key(Key::Left) | Event::Key(Key::Ctrl('b')) => {
                        user_cursor = user_cursor.saturating_sub(1);
                    },
                    // Move the user input cursor to right.
                    Event::Key(Key::Right) | Event::Key(Key::Ctrl('f')) => {
                        user_cursor += 1;
                        if user_cursor > user_input.len() {
                            user_cursor = user_input.len();
                        }
                    },
                    // Select the previous history.
                    Event::Key(Key::Up) | Event::Key(Key::Ctrl('p')) => {
                        selected = selected.saturating_sub(1);
                    },
                    // Select the next history.
                    Event::Key(Key::Down) | Event::Key(Key::Ctrl('n')) => {
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

const DEFAULT_PROMPT: &'static str = "\\{cyan}\\{bold}\\{current_dir} $\\{reset} ";

/// Prints the prompt and read a line from stdin.
pub fn input(isolate: &mut Isolate) -> Result<String, InputError> {
    let mut stdout = io::stdout().into_raw_mode().unwrap();
    let stdin = io::stdin();

    // Just like PROMPT_SP in zsh, in case the command didn't printed a newline
    // at the end of the output, print '$' and a carriage return to preserve the
    // content (e.g. foo of `echo -n foo`).
    let screen_width = termion::terminal_size().unwrap().0 as usize;
    write!(stdout, "{}{}${}{space:>width$}\r",
        termion::style::Bold,
        termion::style::Invert,
        termion::style::Reset,
        space = " ",
        width = screen_width - 1,
    ).ok();

    let word_split = " /\t";
    let mut user_input = UserInput::new();
    let mut user_cursor = 0; // The relative position in the input line. 0-origin.
    let mut mode = InputMode::Normal;
    let mut history = HistorySelector::new();
    let prompt = 
        &isolate
            .get("PROMPT")
            .map(|var| var.as_str().to_owned())
            .unwrap_or(DEFAULT_PROMPT.to_owned());
    let mut renderer = PromptRenderer::new(prompt);
    let mut stdin_events = stdin.events();
    let mut exec = false;

    'input_line: loop {
        // Print the prompt.
        let input_ctx = context_parser::parse(user_input.as_str(), user_cursor);
        let x_max = termion::terminal_size().unwrap().0 as usize;
        let prompt = match &mode {
            InputMode::Completion(completion) => {
                renderer.render(isolate, &input_ctx, user_cursor, x_max, Some(completion))
            }
            InputMode::Normal => {
                renderer.render(isolate, &input_ctx, user_cursor, x_max, None)
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

        match event {
            Some(event) => {
                let event = event.unwrap();
                trace!("event: {:?}", event);
                match event {
                    Event::Key(Key::Char('\n')) => match &mut mode {
                        InputMode::Normal => break 'input_line,
                        InputMode::Completion(completion) => {
                            let expanded = completion.select_and_update_input_and_cursor(
                                &input_ctx, user_input.as_str(), &mut user_cursor);
                            user_input = UserInput::from_str(&expanded);
                            mode = InputMode::Normal;
                            continue 'input_line;
                        }
                    },
                    Event::Key(Key::Char('\t')) => match &mut mode {
                        InputMode::Completion(completion) => {
                            completion.move_cursor(1);
                        }
                        InputMode::Normal => {
                            let completion = CompletionSelector::new(isolate.complete(&input_ctx));
                            if completion.len() == 1 {
                                // There is only one completion candidate. Select it and go back into
                                // normal input mode.
                                let expanded = completion.select_and_update_input_and_cursor(
                                    &input_ctx, user_input.as_str(), &mut user_cursor);
                                user_input = UserInput::from_str(&expanded);
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
                            let new = CompletionSelector::new(isolate.complete(&input_ctx));
                            mode = InputMode::Completion(new);
                        }
                    }
                    Event::Key(Key::Up) | Event::Key(Key::Ctrl('p')) => {
                        match &mut mode {
                            InputMode::Normal => {
                                history.prev(user_input.as_str());
                                let line = history.current();
                                user_input = UserInput::from_str(&line);
                                user_cursor = user_input.len();
                            },
                            InputMode::Completion(completion) => {
                                // Move to the previous candidate.
                                completion.move_cursor(-1);
                            }
                        }
                    }
                    Event::Key(Key::Down) | Event::Key(Key::Ctrl('n')) => {
                        match &mut mode {
                            InputMode::Normal => {
                                history.next();
                                let line = history.current();
                                user_input = UserInput::from_str(&line);
                                user_cursor = user_input.len();
                            },
                            InputMode::Completion(completion) => {
                                // Move to the next candidate.
                                completion.move_cursor(1);
                            }
                        }
                    }
                    Event::Key(Key::Left) | Event::Key(Key::Ctrl('b')) => {
                        if user_cursor > 0 {
                            user_cursor -= 1;
                        }

                        if let InputMode::Completion(_) = mode {
                            mode = InputMode::Normal;
                        }
                    }
                    Event::Key(Key::Right) | Event::Key(Key::Ctrl('f')) => {
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
                            if let Some(ch) = user_input.nth(user_cursor.saturating_sub(1)) {
                                if word_split.contains(ch) {
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
                            if let Some(ch) = user_input.nth(user_cursor.saturating_sub(1)) {
                                if word_split.contains(ch) {
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
                    // Removes the provious word.
                    Event::Key(Key::Ctrl('w')) => {
                        // Remove whitespaces and slashes.
                        while let Some(ch) = user_input.nth(user_cursor.saturating_sub(1)) {
                            if !word_split.contains(ch) {
                                break;
                            }

                            user_input.remove(user_cursor - 1);
                            user_cursor -= 1;
                        }

                        // Remove the word.
                        while let Some(ch) = user_input.nth(user_cursor.saturating_sub(1)) {
                            if word_split.contains(ch) {
                                break;
                            }

                            user_input.remove(user_cursor - 1);
                            user_cursor -= 1;
                        }

                        mode = InputMode::Normal;
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
                        exec = history_search_mode(&mut stdout, &mut stdin_events, &mut user_input);
                        user_cursor = user_input.len();
                    }
                    Event::Key(Key::Char(ch)) => match (&mut mode, ch) {
                        (_, ch) => {
                            user_input.insert(user_cursor, ch);
                            user_cursor += 1;

                            if let InputMode::Completion(_) = mode {
                                let new = CompletionSelector::new(isolate.complete(&input_ctx));
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

    write!(stdout, "{}", renderer.render_clear_completions()).ok();
    append_history(user_input.as_str());
    trace!("input: '{}'", user_input.as_str());
    Ok(user_input.as_str().to_owned())
}
