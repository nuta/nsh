use crate::bash_server::{bash_server, BashRequest};
use crate::context_parser::{self, InputContext};
use crate::fuzzy::FuzzyVec;
use crate::history::History;
use crate::process::{check_background_jobs, ExitStatus};
use crate::prompt::{draw_prompt, parse_prompt};
use crate::shell::Shell;
use crate::syntax_highlighting;
use signal_hook::{self, iterator::Signals};
use std::cmp::{max, min};
use std::collections::VecDeque;
use std::fs;
use std::io::{self, Write};
use std::path::PathBuf;
use std::sync::mpsc;
use termion::event::{Event as TermEvent, Key};
use termion::input::TermRead;
use termion::raw::{IntoRawMode, RawTerminal};

const DEFAULT_PROMPT: &str = "\\{cyan}\\{bold}\\{current_dir} $\\{reset} ";

pub enum Event {
    Input(TermEvent),
    ScreenResized,
    Completion(FuzzyVec),
    NoCompletion,
}

pub struct Mainloop {
    stdout: RawTerminal<std::io::Stdout>,
    shell: Shell,
    history_selector: HistorySelector,
    exited: Option<ExitStatus>,
    prompt_len: usize,
    input: UserInput,
    clear_above: usize,
    clear_below: usize,
    input_ctx: Option<InputContext>,
    do_complete: bool,
    completions: FuzzyVec,
    comps_filtered: Vec<String>,
    comps_height: usize,
    comps_per_line: usize,
    comps_show_from: usize,
    comp_selected: usize,
    /// The width of the screen.
    columns: usize,
    /// The height of the screen.
    lines: usize,

    history_mode: bool,
    hist_selected: usize,
    hist_display_len: usize,
    hist_input_max: usize,
    hist_entries: Vec<String>,
    saved_user_input: UserInput,

    notification: Option<String>,
}

impl Mainloop {
    pub fn new(shell: Shell) -> Mainloop {
        Mainloop {
            stdout: io::stdout().into_raw_mode().unwrap(),
            shell,
            history_selector: HistorySelector::new(),
            input: UserInput::new(),
            clear_above: 0,
            clear_below: 0,
            input_ctx: None,
            do_complete: false,
            completions: FuzzyVec::new(),
            comps_filtered: Vec::new(),
            comps_height: 0,
            comps_per_line: 0,
            comps_show_from: 0,
            comp_selected: 0,
            exited: None,
            prompt_len: 0,
            columns: 0,
            lines: 0,
            history_mode: false,
            hist_selected: 0,
            hist_display_len: 0,
            hist_input_max: 0,
            hist_entries: Vec::new(),
            saved_user_input: UserInput::new(),
            notification: None,
        }
    }

    pub fn run(&mut self) -> ExitStatus {
        let screen_size = termion::terminal_size().unwrap();
        self.columns = screen_size.0 as usize;
        self.lines = screen_size.1 as usize;
        self.print_prompt();
        let (tx, rx) = mpsc::channel();

        // Read inputs.
        let tx1 = tx.clone();
        std::thread::spawn(move || {
            let stdin = io::stdin();
            let mut stdin_events = stdin.events();
            loop {
                if let Some(ev) = stdin_events.next() {
                    match ev {
                        Ok(ev) => {
                            tx1.send(Event::Input(ev)).ok();
                        }
                        Err(_) => { /* ignore errors */ }
                    }
                }
            }
        });

        // Read signals.
        let tx2 = tx.clone();
        std::thread::spawn(move || {
            let signals = Signals::new(&[signal_hook::SIGWINCH]).unwrap();
            for signal in &signals {
                match signal {
                    signal_hook::SIGWINCH => {
                        tx2.send(Event::ScreenResized).ok();
                    }
                    _ => {
                        warn!("unhandled signal: {}", signal);
                    }
                }
            }

            unreachable!();
        });

        // Start the external bash server.
        let tx_bash = bash_server(tx.clone());

        // We're all set! Start processing events such as key inputs.
        loop {
            self.handle_event(rx.recv().unwrap());

            if let Some(status) = self.exited {
                return status;
            }

            if self.do_complete {
                match self.input_ctx.take() {
                    Some(mut ctx) if ctx.words.len() > 1 => {
                        // Resolve aliased command names.
                        if let Some(alias) = self.shell.lookup_alias(&ctx.words[0]) {
                            // The alias should be a single word.
                            if !alias.contains(' ') {
                                ctx.words[0] = alias;
                            }
                        }

                        tx_bash
                            .send(BashRequest::Complete {
                                words: ctx.words,
                                current_word: ctx.current_word,
                            })
                            .ok();
                    }
                    _ => {
                        // Command name completion.
                        let comps = self.shell.path_table().fuzzy_vec().clone();
                        tx.send(Event::Completion(comps)).ok();
                    }
                }

                self.do_complete = false;
            }
        }
    }

    fn completion_mode(&self) -> bool {
        !self.completions.is_empty()
    }

    fn clear_completions(&mut self) {
        self.completions.clear();
    }

    fn notify(&mut self, msg: String) {
        trace!("notify: {}", msg);
        self.notification = Some(msg);
    }

    fn handle_event(&mut self, ev: Event) {
        match ev {
            Event::Input(key) if self.history_mode => {
                self.handle_key_event_in_history_mode(&key);
            }
            Event::Input(key) => {
                self.handle_key_event(&key);
            }
            Event::ScreenResized => {
                trace!("screen resize");
                let screen_size = termion::terminal_size().unwrap();
                self.columns = screen_size.0 as usize;
                self.lines = screen_size.1 as usize;
            }
            Event::NoCompletion => {
                trace!("completion not found, using path finder instead");
                self.path_completion();
            }
            Event::Completion(comps) => {
                if comps.is_empty() {
                    self.notify(format!("no completions: use path finder instead"));
                    self.path_completion();
                } else {
                    self.completions = comps;
                    self.comps_show_from = 0;
                    self.comp_selected = 0;
                    self.print_user_input();
                }
            }
        }
    }

    fn handle_key_event(&mut self, ev: &TermEvent) {
        trace!("key={:?}", ev);
        self.notification = None;

        let mut needs_redraw = true;
        match ev {
            TermEvent::Key(Key::Left) if self.completion_mode() => {
                self.comp_selected = self.comp_selected.saturating_sub(1);
            }
            TermEvent::Key(Key::Right) | TermEvent::Key(Key::Char('\t'))
                if self.completion_mode() =>
            {
                self.comp_selected = min(
                    self.comp_selected + 1,
                    self.comps_filtered.len().saturating_sub(1),
                );
            }
            TermEvent::Key(Key::Up) | TermEvent::Key(Key::Ctrl('p')) if self.completion_mode() => {
                self.comp_selected = self.comp_selected.saturating_sub(self.comps_per_line);
            }
            TermEvent::Key(Key::Down) | TermEvent::Key(Key::Ctrl('n'))
                if self.completion_mode() =>
            {
                self.comp_selected = min(
                    self.comp_selected + self.comps_per_line,
                    self.comps_filtered.len().saturating_sub(1),
                );
            }
            TermEvent::Key(Key::Esc)
            | TermEvent::Key(Key::Char('q'))
            | TermEvent::Key(Key::Ctrl('c'))
                if self.completion_mode() =>
            {
                self.clear_completions();
            }
            TermEvent::Key(Key::Char('\n')) if self.completion_mode() => {
                let selected = self.comps_filtered.get(self.comp_selected).unwrap();
                let offset = self.input.replace_current_word(selected);
                self.input.move_to(offset);
                self.clear_completions();
            }
            TermEvent::Key(Key::Char('\n')) => {
                self.run_command();
                needs_redraw = false;
            }
            TermEvent::Key(Key::Char('\t')) => {
                self.do_complete = true;
            }
            TermEvent::Key(Key::Ctrl('c')) => {
                // Clear the input.
                write!(self.stdout, "\r\n").ok();
                self.print_prompt();
                self.input.clear();
            }
            TermEvent::Key(Key::Ctrl('l')) => {
                // Clear the screen.
                write!(
                    self.stdout,
                    "{}{}",
                    termion::clear::All,
                    termion::cursor::Goto(1, 1)
                )
                .ok();
                self.print_prompt();
            }
            TermEvent::Key(Key::Up) => {
                self.history_selector
                    .prev(self.shell.history(), self.input.as_str());
                let line = self.history_selector.current(self.shell.history());
                self.input.reset(line);
            }
            TermEvent::Key(Key::Down) => {
                self.history_selector.next();
                let line = self.history_selector.current(self.shell.history());
                self.input.reset(line);
            }
            TermEvent::Key(Key::Backspace) => {
                self.input.backspace();
            }
            TermEvent::Key(Key::Ctrl('d')) => {
                if self.input.is_empty() {
                    self.exited = Some(ExitStatus::ExitedWith(0));
                } else {
                    self.input.delete();
                }
            }
            TermEvent::Key(Key::Ctrl('w')) => {
                self.clear_completions();
                self.input.remove_until_word_start();
            }
            TermEvent::Key(Key::Ctrl('k')) => {
                self.clear_completions();
                self.input.truncate();
            }
            TermEvent::Key(Key::Alt('f')) => {
                self.clear_completions();
                self.input.move_to_next_word();
            }
            TermEvent::Key(Key::Alt('b')) => {
                self.clear_completions();
                self.input.move_to_prev_word();
            }
            TermEvent::Key(Key::Ctrl('a')) => {
                self.clear_completions();
                self.input.move_to_begin();
            }
            TermEvent::Key(Key::Ctrl('e')) => {
                self.clear_completions();
                self.input.move_to_end();
            }
            TermEvent::Key(Key::Ctrl('r')) if !self.completion_mode() => {
                needs_redraw = false;
                self.history_mode = true;
                self.redraw_history_search();
            }
            TermEvent::Key(Key::Left) => {
                self.input.move_by(-1);
            }
            TermEvent::Key(Key::Right) => {
                self.input.move_by(1);
            }
            TermEvent::Key(Key::Char(ch)) => {
                self.input.insert(*ch);
            }
            _ => {
                warn!("unsupported key event: {:?}", ev);
            }
        }

        if needs_redraw {
            self.print_user_input();
        }
    }

    fn print_prompt(&mut self) {
        // Just like PROMPT_SP in zsh, in case the command didn't printed a newline
        // at the end of the output, print '$' and a carriage return to preserve the
        // content (e.g. foo of `echo -n foo`).
        write!(
            self.stdout,
            "{}{}${}{space:>width$}\r",
            termion::style::Bold,
            termion::style::Invert,
            termion::style::Reset,
            space = " ",
            width = self.columns - 1,
        )
        .ok();

        let prompt_fmt = &self
            .shell
            .get("PROMPT")
            .map(|var| var.as_str().to_owned())
            .unwrap_or_else(|| DEFAULT_PROMPT.to_owned());

        let (prompt_str, prompt_len) = match parse_prompt(prompt_fmt) {
            Ok(fmt) => draw_prompt(&fmt),
            Err(err) => {
                eprintln!("nsh: failed to parse $PROMPT: {}", err);
                ("$ ".to_owned(), 2)
            }
        };

        write!(self.stdout, "{}", prompt_str.replace("\n", "\r\n")).ok();
        self.stdout.flush().ok();
        self.prompt_len = prompt_len;
    }

    fn print_user_input(&mut self) {
        // Hide the cursor to prevent annoying flickering.
        write!(self.stdout, "{}", termion::cursor::Hide).ok();

        // Clear the previous user input and completions.
        if self.clear_below > 0 {
            for _ in 0..self.clear_below {
                write!(
                    self.stdout,
                    "{}{}",
                    termion::cursor::Down(1),
                    termion::clear::CurrentLine
                )
                .ok();
            }

            write!(
                self.stdout,
                "{}",
                termion::cursor::Up(self.clear_below as u16)
            )
            .ok();
        }

        for _ in 0..self.clear_above {
            write!(
                self.stdout,
                "{}{}",
                termion::clear::CurrentLine,
                termion::cursor::Up(1)
            )
            .ok();
        }

        // Parse and highlight the input.
        let c = context_parser::parse(self.input.as_str(), self.input.cursor());
        let h = syntax_highlighting::highlight(&c, &mut self.shell);
        self.input_ctx = Some(c);

        // Print the highlighted input.
        write!(
            self.stdout,
            "\r{}{}{}",
            termion::cursor::Right(self.prompt_len as u16),
            termion::clear::AfterCursor,
            h.replace("\n", "\r\n")
        )
        .ok();

        // Handle the case when the cursor is at the end of a line.
        let current_x = self.prompt_len + self.input.len();
        if current_x == self.columns {
            write!(self.stdout, "\r\n").ok();
        }

        // Print a notification message.
        if let Some(notification) = &self.notification {
            write!(
                self.stdout,
                "\r\n{}{}[!] {}{}{}",
                termion::color::Fg(termion::color::LightYellow),
                termion::style::Bold,
                notification,
                termion::style::Reset,
                termion::clear::AfterCursor,
            )
            .ok();
        }

        let notification_height = if self.notification.is_some() { 1 } else { 0 };
        let input_height = current_x / self.columns + notification_height;

        let mut comps_height = 0;
        if self.completion_mode() {
            // Determine the number of columns and its width of completions.
            let mut longest = 0;
            for comp in self.completions.iter() {
                longest = max(longest, comp.len());
            }

            let num_columns = max(1, self.columns / longest);
            let column_width = self.columns / num_columns;

            // Move `self.comps_show_from`.
            let comps_height_max = self.lines - input_height - 1;
            let num_comps_max = (comps_height_max - 1) * num_columns;
            if self.comp_selected < self.comps_show_from {
                self.comps_show_from = (self.comp_selected / num_columns) * num_columns;
            }

            if self.comp_selected >= self.comps_show_from + num_comps_max {
                self.comps_show_from =
                    (self.comp_selected / num_columns + 1) * num_columns - num_comps_max;
            }

            // Print completions.
            let filtered: Vec<String> = self
                .completions
                .search(self.input.current_word())
                .iter()
                .map(|s| s.to_string())
                .collect();
            self.comp_selected = min(self.comp_selected, filtered.len().saturating_sub(1));
            let mut remaining = filtered.len() - self.comps_show_from;
            let iter = filtered.iter().skip(self.comps_show_from);
            for (i, comp) in iter.enumerate() {
                if i % num_columns == 0 {
                    if comps_height == comps_height_max - 1 {
                        break;
                    }

                    write!(self.stdout, "\r\n").ok();
                    comps_height += 1;
                }

                let margin = column_width - min(comp.len(), column_width);
                if self.comps_show_from + i == self.comp_selected {
                    write!(
                        self.stdout,
                        "{}{}{}{}",
                        termion::style::Invert,
                        truncate(comp, self.columns),
                        termion::style::NoInvert,
                        termion::cursor::Right(margin as u16),
                    )
                    .ok();
                } else {
                    write!(
                        self.stdout,
                        "{}{}",
                        truncate(comp, self.columns),
                        termion::cursor::Right(margin as u16)
                    )
                    .ok();
                }

                remaining -= 1;
            }

            if remaining > 0 {
                comps_height += 2;
                write!(
                    self.stdout,
                    "\r\n{} {} more {}",
                    termion::style::Invert,
                    remaining,
                    termion::style::Reset,
                )
                .ok();
            }

            self.comps_filtered = filtered;
            self.comps_per_line = num_columns;
        }

        // Move the cursor to the correct position.
        let cursor_y = (self.prompt_len + self.input.cursor()) / self.columns;
        let cursor_x = (self.prompt_len + self.input.cursor()) % self.columns;
        let cursor_y_diff = (input_height - cursor_y) + comps_height;
        if cursor_y_diff > 0 {
            write!(self.stdout, "{}", termion::cursor::Up(cursor_y_diff as u16),).ok();
        }

        write!(self.stdout, "\r").ok();
        if cursor_x > 0 {
            write!(self.stdout, "{}", termion::cursor::Right(cursor_x as u16),).ok();
        }

        write!(self.stdout, "{}", termion::cursor::Show).ok();
        self.clear_above = cursor_y;
        self.clear_below = input_height - cursor_y + comps_height;
        self.comps_height = comps_height;
        self.stdout.flush().ok();
    }

    fn hide_completions(&mut self) {
        if self.comps_height > 0 {
            write!(self.stdout, "{}", termion::cursor::Hide).ok();

            let comps_y_diff = self.clear_below - self.comps_height;
            if comps_y_diff > 0 {
                write!(
                    self.stdout,
                    "{}",
                    termion::cursor::Down(comps_y_diff as u16)
                )
                .ok();
            }

            for _ in 0..self.comps_height {
                write!(
                    self.stdout,
                    "{}{}",
                    termion::cursor::Down(1),
                    termion::clear::CurrentLine
                )
                .ok();
            }

            write!(
                self.stdout,
                "{}{}",
                termion::cursor::Up((comps_y_diff + self.comps_height) as u16),
                termion::cursor::Show,
            )
            .ok();

            self.stdout.flush().ok();
        }
    }

    /// Runs the command (`self.input`). Note that this function blocks until
    /// the command exits.
    fn run_command(&mut self) {
        self.print_user_input();
        self.hide_completions();

        write!(self.stdout, "\r\n").ok();
        self.stdout.flush().ok();

        self.stdout.suspend_raw_mode().ok();
        self.shell.run_str(&self.input.as_str());
        self.stdout.activate_raw_mode().ok();
        check_background_jobs(&mut self.shell);

        self.shell.history_mut().append(self.input.as_str());
        self.print_prompt();
        self.input.clear();
        self.history_selector.reset();
        self.clear_above = 0;
        self.clear_below = 0;
    }

    fn restore_main_screen(&mut self) {
        write!(self.stdout, "{}", termion::screen::ToMainScreen).ok();
        self.stdout.flush().ok();
    }

    fn handle_key_event_in_history_mode(&mut self, ev: &TermEvent) {
        let mut leave_history_mode = false;
        match ev {
            // Execute the selected command.
            TermEvent::Key(Key::Char('\n')) => {
                if let Some(s) = self.hist_entries.get(self.hist_selected) {
                    self.input.reset(s.to_owned());
                } else {
                    // No history entries. Abort history mode.
                    self.input = self.saved_user_input.clone();
                }

                leave_history_mode = true;
            }
            // Fill user input by the selected command and continue editing.
            TermEvent::Key(Key::Char('\t')) => {
                if let Some(s) = self.hist_entries.get(self.hist_selected) {
                    self.input.reset(s.to_owned());
                } else {
                    // No history entries. Abort history mode.
                    self.input = self.saved_user_input.clone();
                }

                leave_history_mode = true;
            }
            // Move the user input cursor to left.
            TermEvent::Key(Key::Left) | TermEvent::Key(Key::Ctrl('b')) => {
                self.input.move_by(-1);
            }
            // Move the user input cursor to right.
            TermEvent::Key(Key::Right) | TermEvent::Key(Key::Ctrl('f')) => {
                self.input.move_by(1);
            }
            // Select the previous history.
            TermEvent::Key(Key::Up) | TermEvent::Key(Key::Ctrl('p')) => {
                self.hist_selected = self.hist_selected.saturating_sub(1);
            }
            // Select the next history.
            TermEvent::Key(Key::Down) | TermEvent::Key(Key::Ctrl('n')) => {
                self.hist_selected += 1;
                let max = min(self.hist_display_len as usize, self.hist_entries.len());
                if self.hist_selected > max.saturating_sub(1) {
                    self.hist_selected = max.saturating_sub(1);
                }
            }
            TermEvent::Key(Key::Backspace) => {
                self.input.backspace();
            }
            TermEvent::Key(Key::Ctrl('d')) => {
                self.input.delete();
            }
            // Abort history mode.
            TermEvent::Key(Key::Ctrl('c')) => {
                self.input = self.saved_user_input.clone();
                leave_history_mode = true;
            }
            // An any key input.
            TermEvent::Key(Key::Char(ch)) => {
                if self.input.len() < self.hist_input_max as usize {
                    self.input.insert(*ch);
                }
            }
            ev => {
                trace!("ignored event: {:?}", ev);
            }
        }

        if leave_history_mode {
            self.history_mode = false;
            self.restore_main_screen();
            self.print_user_input();
        } else {
            self.redraw_history_search();
        }
    }

    fn redraw_history_search(&mut self) {
        let prompt = "history> ";
        self.saved_user_input = self.input.clone();

        // TODO: Remove these fields from self.
        self.hist_display_len = self.lines - 2;
        self.hist_input_max = self.columns - prompt.len();

        // Search history for user input.
        // TODO: Don't copy entries.
        self.hist_entries = self
            .shell
            .history()
            .search(self.input.as_str())
            .iter()
            .map(|s| s.to_string())
            .collect();

        let max = min(self.hist_display_len, self.hist_entries.len());
        if self.hist_selected > max.saturating_sub(1) {
            self.hist_selected = max.saturating_sub(1);
        }

        write!(
            self.stdout,
            "{}{}{}{}{}{}{}{}\r\n",
            termion::screen::ToAlternateScreen,
            termion::clear::All,
            termion::style::Reset,
            termion::cursor::Goto(1, 1),
            termion::style::Bold,
            truncate(prompt, self.columns),
            termion::style::Reset,
            truncate(self.input.as_str(), self.hist_input_max),
        )
        .ok();

        // Render history_lines.
        for i in 0..self.lines.saturating_sub(2) {
            if let Some(entry) = self.hist_entries.get(i as usize) {
                if i == self.hist_selected {
                    write!(
                        self.stdout,
                        "{}{}{}{}{}\r\n",
                        termion::color::Fg(termion::color::Green),
                        termion::style::Bold,
                        termion::style::Underline,
                        truncate_and_fill(entry, self.columns, ' '),
                        termion::style::Reset
                    )
                    .ok();
                } else {
                    write!(self.stdout, "{}\r\n", truncate(entry, self.columns),).ok();
                }
            } else {
                write!(self.stdout, "\r\n").ok();
            }
        }

        // Print the screen.
        let cursor_x = prompt.len() + self.input.cursor();
        write!(
            self.stdout,
            "{}{}{}{}{}",
            termion::style::Bold,
            termion::style::Invert,
            truncate(" Enter: Execute, Tab: Edit, ^C: Quit ", self.columns),
            termion::style::Reset,
            termion::cursor::Goto(1 + cursor_x as u16, 1),
        )
        .ok();
        self.stdout.flush().ok();
    }

    fn path_completion(&mut self) {
        let (comps, aborted) = scan_path(std::env::current_dir().unwrap());

        if aborted {
            self.notify(format!("aborted scanning (too many files)"));
            info!("{:?}", comps.iter());
        }

        self.completions = comps;
        self.comps_show_from = 0;
        self.comp_selected = 0;
        self.print_user_input();
    }
}

#[inline]
fn truncate(s: &str, len: usize) -> String {
    // TODO: Return &str
    s.chars().take(len).collect()
}

#[inline]
fn truncate_and_fill(s: &str, len: usize, fill: char) -> String {
    let mut s = truncate(s, len);
    for _ in s.len()..(len) {
        s.push(fill);
    }

    s
}

fn scan_path(base_dir: PathBuf) -> (FuzzyVec, bool) {
    assert!(base_dir.is_absolute());
    let base_dir_str = base_dir.to_str().unwrap().to_owned();

    let mut vec = FuzzyVec::new();
    let mut aborted = false;

    // Breadth-first search under the given directory.
    let mut queue = VecDeque::new();
    queue.push_back(base_dir);
    'outer: while let Some(dir) = queue.pop_front() {
        let entries = match fs::read_dir(dir) {
            Ok(entries) => entries,
            Err(_) => continue,
        };

        for entry in entries {
            if vec.len() > 4000 {
                // Too many files. Abort scanning.
                aborted = true;
                break 'outer;
            }

            let entry = match entry {
                Ok(entry) => entry,
                Err(_) => continue,
            };

            let path = entry.path();
            if let Some(path) = path.to_str() {
                vec.append(path[(base_dir_str.len() + 1)..].to_string());
            }

            if path.is_dir() {
                queue.push_back(path);
            }
        }
    }

    (vec, aborted)
}

#[derive(Clone)]
struct UserInput {
    cursor: usize,
    input: String,
    indices: Vec<usize>,
    word_split: &'static str,
}

impl UserInput {
    pub fn new() -> UserInput {
        UserInput {
            cursor: 0,
            input: String::with_capacity(256),
            indices: Vec::with_capacity(256),
            word_split: " \t/",
        }
    }

    pub fn reset(&mut self, input: String) {
        self.input = input;
        self.update_indices();
        self.move_to_end();
    }

    pub fn cursor(&self) -> usize {
        self.cursor
    }

    pub fn as_str(&self) -> &str {
        self.input.as_str()
    }

    pub fn nth(&self, index: usize) -> Option<char> {
        self.input.chars().nth(index)
    }

    pub fn len(&self) -> usize {
        self.indices.len()
    }

    pub fn is_empty(&self) -> bool {
        self.input.is_empty()
    }

    pub fn clear(&mut self) {
        self.cursor = 0;
        self.input.clear();
        self.indices.clear();
    }

    fn current_word_range(&self) -> std::ops::Range<usize> {
        if self.is_empty() {
            return std::ops::Range { start: 0, end: 0 };
        }

        let mut start = self.cursor;
        while start > 0 && self.nth(start - 1).unwrap() != ' ' {
            start -= 1;
        }

        let mut end = start;
        while let Some(ch) = self.nth(end) {
            if ch == ' ' {
                break;
            }
            end += 1;
        }

        std::ops::Range {
            start: self
                .indices
                .get(start)
                .copied()
                .unwrap_or_else(|| self.input.len()),
            end: self
                .indices
                .get(end)
                .copied()
                .unwrap_or_else(|| self.input.len()),
        }
    }

    pub fn current_word(&self) -> &str {
        &self.input[self.current_word_range()]
    }

    pub fn replace_current_word(&mut self, replace_with: &str) -> usize {
        let range = self.current_word_range();
        let start = range.start;
        self.input.replace_range(range, replace_with);
        self.update_indices();

        start + replace_with.len()
    }

    pub fn move_to(&mut self, offset: usize) {
        self.cursor = offset;
    }

    pub fn move_by(&mut self, offset: isize) {
        if offset < 0 {
            self.cursor = self.cursor.saturating_sub(offset.abs() as usize);
        } else {
            self.cursor = min(self.len(), self.cursor + offset.abs() as usize);
        }
    }

    pub fn move_to_prev_word(&mut self) {
        // Skip the whitespace at the current position.
        self.cursor = self.cursor.saturating_sub(1);

        while self.cursor > 0 {
            if let Some(ch) = self.nth(self.cursor.saturating_sub(1)) {
                if self.word_split.contains(ch) {
                    break;
                }
            } else {
                break;
            }

            self.cursor -= 1;
        }
    }

    pub fn move_to_next_word(&mut self) {
        // Skip the whitespace at the current position.
        self.cursor += 1;
        if self.cursor > self.input.len() {
            self.cursor = self.input.len();
        }

        while self.cursor < self.input.len() {
            if let Some(ch) = self.nth(self.cursor.saturating_sub(1)) {
                if self.word_split.contains(ch) {
                    break;
                }
            } else {
                break;
            }

            self.cursor += 1;
        }
    }

    pub fn move_to_begin(&mut self) {
        self.cursor = 0;
    }

    pub fn move_to_end(&mut self) {
        self.cursor = self.len();
    }

    pub fn insert(&mut self, ch: char) {
        self.input.insert(self.byte_index(), ch);
        self.update_indices();
        self.cursor += 1;
    }

    pub fn backspace(&mut self) {
        if self.cursor > 0 {
            self.cursor -= 1;
            self.input.remove(self.byte_index());
            self.update_indices();
        }
    }

    pub fn delete(&mut self) {
        if self.cursor < self.len() {
            self.input.remove(self.byte_index());
            self.update_indices();
        }
    }

    pub fn remove_until_word_start(&mut self) {
        // Remove whitespaces and slashes.
        while let Some(ch) = self.nth(self.cursor.saturating_sub(1)) {
            if !self.word_split.contains(ch) {
                break;
            }

            self.input.remove(self.cursor - 1);
            self.cursor -= 1;
        }

        // Remove the word.
        while let Some(ch) = self.nth(self.cursor.saturating_sub(1)) {
            if self.cursor == 0 || self.word_split.contains(ch) {
                break;
            }

            self.input.remove(self.cursor - 1);
            self.cursor -= 1;
        }

        self.update_indices();
    }

    pub fn truncate(&mut self) {
        if self.cursor < self.len() {
            self.input.truncate(self.indices[self.cursor]);
            self.update_indices();
        }
    }

    fn byte_index(&self) -> usize {
        if self.cursor == self.indices.len() {
            self.input.len()
        } else {
            self.indices[self.cursor]
        }
    }

    fn update_indices(&mut self) {
        self.indices.clear();
        for index in self.input.char_indices() {
            self.indices.push(index.0);
        }
    }
}

struct HistorySelector {
    offset: usize,
    input: String,
}

impl HistorySelector {
    pub fn new() -> HistorySelector {
        HistorySelector {
            offset: 0,
            input: String::new(),
        }
    }

    pub fn reset(&mut self) {
        self.offset = 0;
    }

    /// Returns None if `self.offset` is 0 otherwise `self.offset - 1`th entry.
    pub fn current(&self, history: &History) -> String {
        if self.offset == 0 {
            //  Reached to the end of histories. Restore the saved state.
            self.input.clone()
        } else {
            history.nth_last(self.offset - 1).unwrap()
        }
    }

    /// Selects the previous history entry. Save the current user (not yet executed)
    /// input if needed.
    pub fn prev(&mut self, history: &History, input: &str) {
        if self.offset == 0 {
            // Entering the history selection. Save the current state.state.
            self.input = input.to_string();
        }

        let hist_len = history.len();
        self.offset += 1;
        if self.offset >= hist_len {
            self.offset = hist_len;
        }
    }

    /// Select the next history entry.
    pub fn next(&mut self) {
        if self.offset > 0 {
            self.offset -= 1;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_user_input() {
        let mut i = UserInput::new();
        assert_eq!(i.current_word_range(), (0..0));

        // a
        //  ^
        i.insert('a');
        assert_eq!(i.current_word_range(), (0..1));

        // ab
        //   ^
        i.insert('b');
        assert_eq!(i.current_word_range(), (0..2));

        // ab_
        //    ^
        i.insert(' ');
        assert_eq!(i.current_word_range(), (3..3));

        // ab_c
        //    ^
        i.insert('c');
        i.move_by(-1);
        assert_eq!(i.current_word_range(), (3..4));

        // ab_c
        //   ^
        i.move_by(-1);
        assert_eq!(i.current_word_range(), (0..2));
    }
}
