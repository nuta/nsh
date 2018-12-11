use crate::completion::complete;
use crate::context_parser::{InputContext, parse_input_context};
use crate::history::{HistorySelector, append_history};
use crate::prompt::render_prompt;
use crate::config::Config;
use std::sync::Arc;
use std::io::{self, Write, Stdout};
use termion;
use termion::cursor::DetectCursorPos;
use termion::event::{Event, Key};
use termion::input::TermRead;
use termion::raw::IntoRawMode;

pub struct CompletionState {
    /// Candidate completion entries.
    entries: Vec<Arc<String>>,
    // The currently selected entry.
    selected_index: usize,
    // The number of completion lines in the prompt.
    display_lines: usize,
    // The beginning of entries to be displayed.
    display_index: usize,
}

impl CompletionState {
    pub fn new(entries: Vec<Arc<String>>) -> CompletionState {
        const COMPLETION_LINES: usize = 5;

        CompletionState {
            entries,
            selected_index: 0,
            display_lines: COMPLETION_LINES,
            display_index: 0,
        }
    }

    /// Move to the next/previous entry.
    pub fn move_cursor(&mut self, offset: isize) {
        // FIXME: I think there's more sane way to handle a overflow.`
        let mut old_selected_index = self.selected_index as isize;
        old_selected_index += offset;

        let entries_len = self.len() as isize;
        if entries_len > 0 && old_selected_index > entries_len - 1 {
            old_selected_index = entries_len - 1;
        }

        if old_selected_index < 0 {
            old_selected_index = 0;
        }

        self.selected_index = old_selected_index as usize;

        if self.selected_index >= self.display_index + self.display_lines {
            self.display_index = self.selected_index - self.display_lines + 1;
        }

        if self.selected_index < self.display_index {
            self.display_index = self.selected_index;
        }

        trace!(
            "move_cursor: offset={}, index={}",
            offset,
            self.selected_index
        );
    }

    #[inline(always)]
    pub fn entries(&self) -> Vec<Arc<String>> {
        self.entries.clone()
    }

    #[inline(always)]
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    #[inline(always)]
    pub fn selected_index(&self) -> usize {
        self.selected_index
    }

    #[inline(always)]
    pub fn get(&self, index: usize) -> Option<Arc<String>> {
        self.entries.get(index).cloned()
    }

    #[inline(always)]
    pub fn display_lines(&self) -> usize {
        self.display_lines
    }

    #[inline(always)]
    pub fn display_index(&self) -> usize {
        self.display_index
    }

    pub fn select_and_update_input_and_cursor(&self, input_ctx: &InputContext, user_input: &mut String, user_cursor: &mut usize) {
        if let Some(selected) = self.get(self.selected_index()) {
            let prefix = user_input
                .get(..(input_ctx.current_word_offset))
                .unwrap_or("")
                .to_string();
            let suffix_offset = input_ctx.current_word_offset
                + input_ctx.current_word_len;
            let suffix =
                &user_input.get((suffix_offset)..).unwrap_or("").to_string();
            *user_input = format!("{}{}{}", prefix, selected, suffix);
            *user_cursor = input_ctx.current_word_offset + selected.len();
        }
    }
}

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
    let mut completion_state = CompletionState::new(vec![]);
    let mut input_ctx: InputContext = Default::default();
    let mut history = HistorySelector::new();

    'input_line: loop {
        // Print the prompt.
        let y_max = termion::terminal_size().map(|(_, y)| y - 1).unwrap();
        let (rendered_lines2, prompt_y2, prompt) = render_prompt(
            &config.prompt,
            mode,
            &completion_state,
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
                            trace!("ctx: {:?}", input_ctx);
                            completion_state.select_and_update_input_and_cursor(&input_ctx, &mut user_input, &mut user_cursor);
                            mode = InputMode::Normal;
                            continue 'input_line;
                        }
                    },
                    Event::Key(Key::Char('\t')) => match mode {
                        InputMode::Completion => {
                            completion_state.move_cursor(1);
                        }
                        InputMode::Normal => {
                            input_ctx = parse_input_context(&user_input, user_cursor);
                            completion_state = CompletionState::new(complete(&input_ctx));
                            if completion_state.len() == 1 {
                                // There is only one completion candidate. Select it and go back into
                                // normal input mode.
                                completion_state.select_and_update_input_and_cursor(&input_ctx, &mut user_input, &mut user_cursor);
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
                            input_ctx = parse_input_context(&user_input, user_cursor);
                            completion_state = CompletionState::new(complete(&input_ctx));
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
                                completion_state.move_cursor(-1);
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
                                completion_state.move_cursor(1);
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
                                input_ctx =
                                    parse_input_context(&user_input, user_cursor);
                                completion_state = CompletionState::new(complete(&input_ctx));
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
