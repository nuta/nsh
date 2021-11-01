use crate::bash_server::{bash_server, BashRequest};
use crate::context_parser::{self, InputContext};
use crate::fuzzy::FuzzyVec;
use crate::theme::ThemeColor;
use crate::history::HistorySelector;
use crate::process::{check_background_jobs, ExitStatus};
use crate::prompt::{draw_prompt, parse_prompt};
use crate::shell::Shell;
use crate::highlight;
use crate::dircolor::DirColor;
use signal_hook::{self, iterator::Signals};
use std::cmp::{max, min};
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::mpsc;
use std::ops::Range;
use std::time::Duration;
use crossterm::{execute, queue};
use crossterm::event::{Event as TermEvent, KeyCode, KeyEvent};
use crossterm::event::KeyModifiers;
use crossterm::terminal::{
    self, enable_raw_mode, disable_raw_mode,
    EnterAlternateScreen, LeaveAlternateScreen,
    Clear, ClearType,
};
use crossterm::cursor::{self, MoveTo};
use crossterm::style::{
    Color, Attribute, SetAttribute, SetForegroundColor, Print,
};
const NONE: KeyModifiers = KeyModifiers::NONE;
const CTRL: KeyModifiers = KeyModifiers::CONTROL;
const ALT: KeyModifiers = KeyModifiers::ALT;
const SHIFT: KeyModifiers = KeyModifiers::SHIFT;

const DEFAULT_PROMPT: &str = "\\{cyan}\\{bold}\\{current_dir} $\\{reset} ";

fn restore_main_screen() {
    execute!(std::io::stdout(), LeaveAlternateScreen).ok();
}

pub enum Event {
    Input(TermEvent),
    ScreenResized,
    Completion(FuzzyVec),
    NoCompletion,
}

pub struct Mainloop {
    shell: Shell,
    history_selector: HistorySelector,
    exited: Option<ExitStatus>,
    prompt_len: usize,
    input: UserInput,
    clear_above: usize,
    clear_below: usize,
    input_ctx: InputContext,
    do_complete: bool,
    completions: FuzzyVec,
    comps_filtered: Vec<(Option<ThemeColor>, String)>,
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
    hist_filter_by_cwd: bool,
    saved_user_input: UserInput,
    notification: Option<String>,
    dircolor: DirColor,
    input_stack: Vec<String>,
}

impl Drop for Mainloop {
    fn drop(&mut self) {
        disable_raw_mode().ok();
    }
}

impl Mainloop {
    pub fn new(shell: Shell) -> Mainloop {
        Mainloop {
            shell,
            history_selector: HistorySelector::new(),
            input: UserInput::new(),
            clear_above: 0,
            clear_below: 0,
            input_ctx: context_parser::parse("", 0),
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
            hist_filter_by_cwd: false,
            saved_user_input: UserInput::new(),
            notification: None,
            dircolor: DirColor::new(),
            input_stack: Vec::new(),
        }
    }

    pub fn run(&mut self) -> ExitStatus {
        let screen_size = terminal::size().unwrap();
        self.columns = screen_size.0 as usize;
        self.lines = screen_size.1 as usize;

        enable_raw_mode().ok();
        self.print_prompt();

        if let Some(var) = self.shell.get("LS_COLORS") {
            self.dircolor.load(var.as_str());
        }

        // Read signals.
        let (tx, rx) = mpsc::channel();
        let tx2 = tx.clone();
        std::thread::spawn(move || {
            let signals = Signals::new(&[signal_hook::SIGWINCH]).unwrap();
            for signal in signals {
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
            let mut started_at = None;
            match crossterm::event::poll(Duration::from_millis(100)) {
                Ok(true) => {
                    loop {
                        if let Ok(ev) = crossterm::event::read() {
                            self.handle_event(Event::Input(ev))
                        }

                        match crossterm::event::poll(Duration::from_millis(0)) {
                            Ok(true) => (), // Continue reading stdin.
                            _ => break,
                        }
                    }
                }
                _ => {
                    if let Ok(ev) = rx.try_recv() {
                        started_at = Some(std::time::SystemTime::now());
                        self.handle_event(ev);       
                    }
                }
            }

            if let Some(status) = self.exited {
                return status;
            }

            if self.do_complete {
                let is_argv0 = if let Some(current_span) = self.input_ctx.current_span {
                    match &self.input_ctx.spans[current_span] {
                        context_parser::Span::Argv0(_) => true,
                        _ => false,
                    }
                } else {
                    false
                };

                if is_argv0 {
                    // Command name completion.
                    let argv0 = self.current_span_text().unwrap();
                    let comps = if argv0.starts_with('/')
                        || argv0.starts_with('.') || argv0.starts_with('~') {
                        path_completion(argv0, false)
                    } else {
                        self.shell.path_table().fuzzy_vec().clone()
                    };
                    tx.send(Event::Completion(comps)).ok();
                } else {
                    // Resolve aliased command names.
                    if let Some(alias) = self.shell.lookup_alias(&self.input_ctx.words[0]) {
                        // The alias should be a single word.
                        if !alias.contains(' ') {
                            self.input_ctx.words[0] = alias;
                        }
                    }

                    // For cd(1), invoek path completion quickly to improve UX.
                    if self.input_ctx.words[0] == "cd" {
                        let pattern = self.current_span_text().unwrap_or("");
                        let entries = path_completion(pattern, true);
                        if entries.is_empty() {
                            self.notify("completion: no files");
                        }
                        self.update_completion_entries(entries);        
                    } else {
                        tx_bash
                            .send(BashRequest::Complete {
                                words: self.input_ctx.words.clone(),
                                current_word: self.input_ctx.current_word,
                            })
                            .ok();
                    }
                }

                self.do_complete = false;
            }

            if let Some(started_at) = started_at {
                trace!("handle_event: took {}ms",
                    started_at.elapsed().unwrap().as_millis());
            }
        }
    }

    fn push_buffer_stack(&mut self) {
        self.input_stack.push(self.input.as_str().to_owned());
        self.input.clear();
    }

    fn completion_mode(&self) -> bool {
        !self.completions.is_empty()
    }

    fn clear_completions(&mut self) {
        self.completions.clear();
    }

    fn notify(&mut self, msg: &str) {
        trace!("notify: {}", msg);
        self.notification = Some(msg.to_owned());
    }

    fn handle_event(&mut self, ev: Event) {
        match ev {
            Event::Input(input) if self.history_mode => {
                match input {
                    TermEvent::Key(key) => self.handle_key_event_in_history_mode(&key),
                    _ => {},
                }
            }
            Event::Input(input) => {
                match input {
                    TermEvent::Key(key) => self.handle_key_event(&key),
                    _ => {},
                }
            }
            Event::ScreenResized => {
                trace!("screen resize");
                let screen_size = terminal::size().unwrap();
                self.columns = screen_size.0 as usize;
                self.lines = screen_size.1 as usize;
            }
            Event::NoCompletion => {
                trace!("completion not found, using path finder instead");
                let pattern = self.current_span_text().unwrap_or("");
                let entries = path_completion(pattern, false);
                if entries.is_empty() {
                    self.notify("completion: no files");
                }
                self.update_completion_entries(entries);
            }
            Event::Completion(comps) => {
                if comps.is_empty() {
                    self.notify("no completions");
                } else {
                    self.update_completion_entries(comps);
                }
            }
        }
    }

    fn handle_key_event(&mut self, ev: &KeyEvent) {
        trace!("key={:?}", ev);
        self.notification = None;

        let mut needs_redraw = true;
        match (ev.code, ev.modifiers) {
            (KeyCode::Left, NONE) | (KeyCode::BackTab, SHIFT) if self.completion_mode() => {
                self.comp_selected = self.comp_selected.saturating_sub(1);
            }
            (KeyCode::Right, NONE) | (KeyCode::Tab, NONE)
                if self.completion_mode() =>
            {
                if self.comps_filtered.is_empty() {
                    self.clear_completions();
                } else {
                    self.comp_selected = min(
                        self.comp_selected + 1,
                        self.comps_filtered.len().saturating_sub(1),
                    );
                }
            }
            (KeyCode::Up, NONE) | (KeyCode::Char('p'), CTRL) if self.completion_mode() => {
                self.comp_selected = self.comp_selected.saturating_sub(self.comps_per_line);
            }
            (KeyCode::Down, NONE) | (KeyCode::Char('n'), CTRL)
                if self.completion_mode() =>
            {
                self.comp_selected = min(
                    self.comp_selected + self.comps_per_line,
                    self.comps_filtered.len().saturating_sub(1),
                );
            }
            (KeyCode::Esc, NONE)
            | (KeyCode::Char('q'), NONE)
            | (KeyCode::Char('c'), CTRL)
                if self.completion_mode() =>
            {
                self.clear_completions();
            }
            (KeyCode::Enter, NONE) if self.completion_mode() => {
                self.select_completion();
            }
            (KeyCode::Enter, NONE) => {
                self.run_command();
                needs_redraw = false;
            }
            (KeyCode::Tab, NONE) => {
                self.do_complete = true;
            }
            (KeyCode::Char('c'), CTRL) => {
                // Clear the input.
                execute!(std::io::stdout(), Print("\r\n")).ok();
                self.print_prompt();
                self.input.clear();
            }
            (KeyCode::Char('l'), CTRL) => {
                // Clear the screen.
                execute!(
                    std::io::stdout(),
                    Clear(ClearType::All),
                    MoveTo(0, 0)
                ).ok();
                self.print_prompt();
            }
            (KeyCode::Up, NONE) => {
                self.history_selector
                    .prev(self.shell.history(), self.input.as_str());
                let line = self.history_selector.current(self.shell.history());
                self.input.reset(line);
            }
            (KeyCode::Down, NONE) => {
                self.history_selector.next();
                let line = self.history_selector.current(self.shell.history());
                self.input.reset(line);
            }
            (KeyCode::Backspace, NONE) => {
                self.input.backspace();
            }
            (KeyCode::Char('d'), CTRL) => {
                if self.input.is_empty() {
                    self.exited = Some(ExitStatus::ExitedWith(0));
                } else {
                    self.input.delete();
                }
            }
            (KeyCode::Char('w'), CTRL) => {
                self.clear_completions();
                self.input.remove_until_word_start();
            }
            (KeyCode::Char('k'), CTRL) => {
                self.clear_completions();
                self.input.truncate();
            }
            (KeyCode::Char('q'), ALT) => {
                self.push_buffer_stack();
            }
            (KeyCode::Char('f'), ALT) => {
                self.clear_completions();
                self.input.move_to_next_word();
            }
            (KeyCode::Char('b'), ALT) => {
                self.clear_completions();
                self.input.move_to_prev_word();
            }
            (KeyCode::Char('a'), CTRL) => {
                self.clear_completions();
                self.input.move_to_begin();
            }
            (KeyCode::Char('e'), CTRL) => {
                self.clear_completions();
                self.input.move_to_end();
            }
            (KeyCode::Char('r'), CTRL) if !self.completion_mode() => {
                needs_redraw = false;
                self.history_mode = true;
                self.hist_filter_by_cwd = false;
                self.redraw_history_search();
            }
            (KeyCode::Char('h'), CTRL) if !self.completion_mode() => {
                needs_redraw = false;
                self.history_mode = true;
                self.hist_filter_by_cwd = true;
                self.redraw_history_search();
            }
            (KeyCode::Left, NONE) => {
                self.input.move_by(-1);
            }
            (KeyCode::Right, NONE) => {
                self.input.move_by(1);
            }
            (KeyCode::Char(ch), NONE) => {
                self.input.insert(ch);
            }
            (KeyCode::Char(ch), SHIFT) => {
                self.input.insert(ch);
            }
            _ => {
                warn!("unsupported key event: {:?}", ev);
            }
        }

        if needs_redraw {
            self.reparse_input_ctx();
            self.filter_completion_entries();
            self.print_user_input();
        }
    }

    #[cfg(test)]
    fn input_str(&mut self, string: &str) {
        for ch in string.chars() {
            let code = match ch {
                '\n' => KeyCode::Enter,
                '\t' => KeyCode::Tab,
                _ => KeyCode::Char(ch),
            };

            self.handle_key_event(&KeyEvent::new(code, NONE));
        }
    }

    #[cfg(test)]
    fn input_event(&mut self, ev: Event) {
        self.handle_event(ev);
    }

    fn build_prompt(&mut self) -> (String, usize) {
        let prompt_fmt = &self
            .shell
            .get("PROMPT")
            .map(|var| var.as_str().to_owned())
            .unwrap_or_else(|| DEFAULT_PROMPT.to_owned());

        match parse_prompt(prompt_fmt) {
            Ok(fmt) => draw_prompt(&fmt),
            Err(err) => {
                print_err!("failed to parse $PROMPT: {}", err);
                ("$ ".to_owned(), 2)
            }
        }
    }

    fn print_prompt(&mut self) {
        if cfg!(test) {
            // Do nothing in tests.
            return;
        }

        // Just like PROMPT_SP in zsh, in case the command didn't printed a newline
        // at the end of the output, print '$' and a carriage return to preserve the
        // content (e.g. foo of `echo -n foo`).
        let mut stdout = std::io::stdout();
        queue!(
            stdout,
            SetAttribute(Attribute::Bold),
            SetAttribute(Attribute::Reverse),
            Print("$"),
            SetAttribute(Attribute::Reset),
            Print(&format!(
                "{space:>width$}\r",
                space = " ",
                width = self.columns - 1,
            ))
        ).ok();

        let (prompt_str, prompt_len) = self.build_prompt();
        queue!(stdout, Print(prompt_str.replace("\n", "\r\n"))).ok();        
        stdout.flush().ok();

        // Report the Time-To-First-Prompt (TTFP).
        if self.shell.get("NSH_TTFP").is_none() {
            let ttfp = crate::STARTED_AT.elapsed().unwrap().as_millis();
            info!("Time-To-First-Prompt: {}ms", ttfp);
            let val = crate::variable::Value::String(format!("{}", ttfp));
            self.shell.set("NSH_TTFP", val, false);
        }

        self.prompt_len = prompt_len;
    }

    fn print_user_input(&mut self) {
        if cfg!(test) {
            // Do nothing in tests.
            return;
        }

        let mut stdout = std::io::stdout();

        // Hide the cursor to prevent annoying flickering.
        queue!(stdout, cursor::Hide).ok();

        // Clear the previous user input and completions.
        // TODO: Don't clear the texts; overwrite instead to prevent flickering.
        if self.clear_below > 0 {
            for _ in 0..self.clear_below {
                queue!(
                    stdout,
                    cursor::MoveDown(1),
                    Clear(ClearType::CurrentLine)
                ).ok();
            }

            queue!(stdout, cursor::MoveUp(self.clear_below as u16)).ok();
        }

        for _ in 0..self.clear_above {
            queue!(
                stdout,
                Clear(ClearType::CurrentLine),
                cursor::MoveUp(1)
            ).ok();
        }

        if self.clear_above > 0 {
            // Redraw the prompt since it has been cleared.           
            let (prompt_str, _) = self.build_prompt();
            queue!(
                stdout,
                Print("\r"),
                Print(prompt_str.replace("\n", "\r\n"))
            ).ok();
        }

        // Print the highlighted input.
        let h = highlight::highlight(&self.input_ctx, &mut self.shell);
        queue!(
            stdout,
            Print("\r"),
            cursor::MoveRight(self.prompt_len as u16),
            Clear(ClearType::UntilNewLine),
            Print(h.replace("\n", "\r\n"))
        ).ok();

        // Handle the case when the cursor is at the end of a line.
        let current_x = self.prompt_len + self.input.len();
        if current_x % self.columns == 0 {
            queue!(stdout, Print("\r\n")).ok();
        }

        // Print a notification message.
        if let Some(notification) = &self.notification {
            queue!(
                stdout,
                Print("\r\n"),
                SetForegroundColor(Color::Yellow),
                SetAttribute(Attribute::Bold),
                Print("[!] "),
                Print(notification),
                SetAttribute(Attribute::Reset),
                Clear(ClearType::UntilNewLine),
            ).ok();
        }

        let notification_height = if self.notification.is_some() { 1 } else { 0 };
        let input_height = current_x / self.columns + notification_height;

        let mut comps_height = 0;
        if self.completion_mode() {
            // Determine the number of columns and its width of completions.
            let mut longest = 0;
            for (_, comp) in self.completions.iter() {
                longest = max(longest, comp.len() + 1);
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
            let mut remaining = self.comps_filtered.len() - self.comps_show_from;
            let iter = self.comps_filtered.iter().skip(self.comps_show_from);
            for (i, (color, comp)) in iter.enumerate() {
                if i % num_columns == 0 {
                    if comps_height == comps_height_max - 1 {
                        break;
                    }

                    queue!(stdout, Print("\r\n")).ok();
                    comps_height += 1;
                }

                let margin = column_width - min(comp.len(), column_width);
                if self.comps_show_from + i == self.comp_selected {
                    queue!(
                        stdout,
                        SetAttribute(Attribute::Reverse),
                        Print(truncate(comp, self.columns)),
                        SetAttribute(Attribute::NoReverse),
                        cursor::MoveRight(margin as u16),
                    )
                    .ok();
                } else {
                    match color {
                        Some(ThemeColor::DirColor) => {
                            self.dircolor.write(&mut stdout, Path::new(comp)).ok();
                        }
                        _ => {}
                    }

                    queue!(
                        stdout,
                        Print(truncate(comp, self.columns)),
                        SetAttribute(Attribute::Reset),
                        cursor::MoveRight(margin as u16)
                    )
                    .ok();
                }

                remaining -= 1;
            }

            if remaining > 0 {
                comps_height += 2;
                queue!(
                    stdout,
                    Clear(ClearType::UntilNewLine),
                    Print("\r\n"),
                    SetAttribute(Attribute::Reverse),
                    Print(" "),
                    Print(remaining),
                    Print(" more "),
                    SetAttribute(Attribute::Reset),
                )
                .ok();
            }

            self.comps_per_line = num_columns;
        }

        // Move the cursor to the correct position.
        let cursor_y = (self.prompt_len + self.input.cursor()) / self.columns;
        let cursor_x = (self.prompt_len + self.input.cursor()) % self.columns;
        let cursor_y_diff = (input_height - cursor_y) + comps_height;
        if cursor_y_diff > 0 {
            queue!(stdout, cursor::MoveUp(cursor_y_diff as u16),).ok();
        }

        queue!(stdout, Print("\r")).ok();
        if cursor_x > 0 {
            queue!(stdout, cursor::MoveRight(cursor_x as u16),).ok();
        }

        queue!(stdout, cursor::Show).ok();
        self.clear_above = cursor_y;
        self.clear_below = input_height - cursor_y + comps_height;
        self.comps_height = comps_height;
        stdout.flush().ok();
    }

    fn reparse_input_ctx(&mut self) {
        self.input_ctx
            = context_parser::parse(self.input.as_str(), self.input.cursor());
    }

    fn update_completion_entries(&mut self, entries: FuzzyVec) {
        self.completions = entries;
        self.comps_show_from = 0;
        self.filter_completion_entries();

        if self.comps_filtered.len() == 1 {
            self.select_completion();
            self.reparse_input_ctx();
        }

        self.print_user_input();
    }

    fn filter_completion_entries(&mut self) {
        self.comps_filtered = self
            .completions
            .search(self.current_span_text().unwrap_or(""))
            .iter()
            .map(|(c, s)| (*c, s.to_string().replace(" ", "\\ ")))
            .collect();
        self.comp_selected =
            min(self.comp_selected, self.comps_filtered.len().saturating_sub(1));
    }

    fn current_span_text(&self) -> Option<&str> {
        if let Some(current_span_index) = self.input_ctx.current_span {
            match &self.input_ctx.spans[current_span_index] {
                context_parser::Span::Literal(literal)
                | context_parser::Span::Argv0(literal) => {
                    return Some(literal);
                }
                _ => {}
            };
        }

        None
    }

    fn select_completion(&mut self) {
        if let Some(current_span) = &self.input_ctx.current_literal {
            if let Some(selected) = self.comps_filtered.get(self.comp_selected) {
                self.input.replace_range(current_span.clone(), &selected.1);
            }

            self.clear_completions();
        }
    }

    fn hide_completions(&mut self) {
        let mut stdout = std::io::stdout();
        if self.comps_height > 0 {
            queue!(stdout, cursor::Hide).ok();

            let comps_y_diff = self.clear_below - self.comps_height;
            if comps_y_diff > 0 {
                queue!(stdout, cursor::MoveDown(comps_y_diff as u16)).ok();
            }

            for _ in 0..self.comps_height {
                queue!(
                    stdout,
                    cursor::MoveDown(1),
                    Clear(ClearType::CurrentLine)
                )
                .ok();
            }

            queue!(
                stdout,
                cursor::MoveUp((comps_y_diff + self.comps_height) as u16),
                cursor::Show,
            )
            .ok();

            stdout.flush().ok();
        }
    }

    /// Runs the command (`self.input`). Note that this function blocks until
    /// the command exits.
    fn run_command(&mut self) {
        self.print_user_input();
        self.hide_completions();

        print!("\r\n");
        disable_raw_mode().ok();
        self.shell.run_str(&self.input.as_str());
        enable_raw_mode().ok();
        check_background_jobs(&mut self.shell);

        self.shell.history_mut().append(self.input.as_str());
        self.input.clear();
        self.history_selector.reset();
        self.clear_above = 0;
        self.clear_below = 0;
        
        if let Some(input) = self.input_stack.pop() {
            self.input.insert_str(&input);
        }

        self.reparse_input_ctx();
        self.print_prompt();
        self.print_user_input();
    }

    fn handle_key_event_in_history_mode(&mut self, ev: &KeyEvent) {
        let mut leave_history_mode = false;
        match (ev.code, ev.modifiers) {
            // Execute the selected command.
            (KeyCode::Enter, NONE) => {
                if let Some(s) = self.hist_entries.get(self.hist_selected) {
                    self.input.reset(s.to_owned());
                } else {
                    // No history entries. Abort history mode.
                    self.input = self.saved_user_input.clone();
                }

                leave_history_mode = true;
            }
            // Fill user input by the selected command and continue editing.
            (KeyCode::Tab, NONE) => {
                if let Some(s) = self.hist_entries.get(self.hist_selected) {
                    self.input.reset(s.to_owned());
                } else {
                    // No history entries. Abort history mode.
                    self.input = self.saved_user_input.clone();
                }

                leave_history_mode = true;
            }
            // Move the user input cursor to left.
            (KeyCode::Left, NONE) | (KeyCode::Char('b'), CTRL) => {
                self.input.move_by(-1);
            }
            // Move the user input cursor to right.
            (KeyCode::Right, NONE) | (KeyCode::Char('f'), CTRL) => {
                self.input.move_by(1);
            }
            // Select the previous history.
            (KeyCode::Up, NONE) | (KeyCode::Char('p'), CTRL) => {
                self.hist_selected = self.hist_selected.saturating_sub(1);
            }
            // Select the next history.
            (KeyCode::Down, NONE) | (KeyCode::Char('n'), CTRL) => {
                self.hist_selected += 1;
                let max = min(self.hist_display_len as usize, self.hist_entries.len());
                if self.hist_selected > max.saturating_sub(1) {
                    self.hist_selected = max.saturating_sub(1);
                }
            }
            (KeyCode::Backspace, NONE) => {
                self.input.backspace();
            }
            (KeyCode::Char('d'), CTRL) => {
                self.input.delete();
            }
            // Abort history mode.
            (KeyCode::Char('c'), CTRL) => {
                self.input = self.saved_user_input.clone();
                leave_history_mode = true;
            }
            // An any key input.
            (KeyCode::Char(ch), NONE) => {
                if self.input.len() < self.hist_input_max as usize {
                    self.input.insert(ch);
                }
            }
            _ => {
                trace!("ignored event: {:?}", ev);
            }
        }

        if leave_history_mode {
            self.history_mode = false;
            restore_main_screen();
            self.reparse_input_ctx();
            self.print_user_input();
        } else {
            self.redraw_history_search();
        }
    }

    fn redraw_history_search(&mut self) {
        let prompt = if self.hist_filter_by_cwd {
            "history (cwd)> "
        } else {
            "history> "
        };
        self.saved_user_input = self.input.clone();

        // TODO: Remove these fields from self.
        self.hist_display_len = self.lines - 2;
        self.hist_input_max = self.columns - prompt.len();

        // Search history for user input.
        // TODO: Don't copy entries.
        self.hist_entries = self
            .shell
            .history()
            .search(self.input.as_str(), self.hist_filter_by_cwd)
            .iter()
            .map(|(_, s)| s.to_string())
            .collect();

        let max = min(self.hist_display_len, self.hist_entries.len());
        if self.hist_selected > max.saturating_sub(1) {
            self.hist_selected = max.saturating_sub(1);
        }

        let mut stdout = std::io::stdout();
        queue!(
            stdout,
            EnterAlternateScreen,
            Clear(ClearType::All),
            SetAttribute(Attribute::Reset),
            MoveTo(0, 0),
            SetAttribute(Attribute::Bold),
            Print(truncate(prompt, self.columns)),
            SetAttribute(Attribute::Reset),
            Print(truncate(self.input.as_str(), self.hist_input_max)),
            Print("\r\n"),
        )
        .ok();

        // Render history_lines.
        for i in 0..self.lines.saturating_sub(2) {
            if let Some(entry) = self.hist_entries.get(i as usize) {
                if i == self.hist_selected {
                    queue!(
                        stdout,
                        SetForegroundColor(Color::Green),
                        SetAttribute(Attribute::Bold),
                        SetAttribute(Attribute::Underlined),
                        Print(truncate_and_fill(entry, self.columns, ' ')),
                        SetAttribute(Attribute::Reset),
                        Print("\r\n"),
                    )
                    .ok();
                } else {
                    queue!(
                        stdout,
                        Print(truncate(entry, self.columns)),
                        Print("\r\n"),
                    ).ok();
                }
            } else {
                queue!(stdout, Print("\r\n")).ok();
            }
        }

        // Print the screen.
        let cursor_x = prompt.len() + self.input.cursor();
        queue!(
            stdout,
            SetAttribute(Attribute::Bold),
            SetAttribute(Attribute::Reverse),
            Print(truncate(" Enter: Execute, Tab: Edit, ^C: Quit ", self.columns)),
            SetAttribute(Attribute::Reset),
            MoveTo(cursor_x as u16, 0),
        ).ok();
        stdout.flush().ok();
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

    pub fn replace_range(&mut self, range: Range<usize>, replace_with: &str) {
        let cursor = range.start + replace_with.len();
        self.input.replace_range(range, replace_with);
        self.update_indices();
        self.cursor = cursor;
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
            if let Some(next_ch) = self.nth(self.cursor.saturating_sub(1)) {
                if let Some(ch) = self.nth(self.cursor) {
                    if self.word_split.contains(next_ch) && !self.word_split.contains(ch) {
                        break;
                    }
                } else {
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
            if let Some(prev_ch) = self.nth(self.cursor.saturating_sub(1)) {
                if let Some(ch) = self.nth(self.cursor) {
                    if self.word_split.contains(prev_ch) && !self.word_split.contains(ch) {
                        break;
                    }
                } else {
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

    pub fn insert_str(&mut self, string: &str) {
        self.input.insert_str(self.byte_index(), string);
        self.update_indices();
        self.cursor += string.chars().count();
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

fn path_completion(pattern: &str, only_dirs: bool) -> FuzzyVec {
    let home_dir = dirs::home_dir().unwrap();
    let current_dir = std::env::current_dir().unwrap();
    let mut dir = if pattern.is_empty() {
        current_dir.to_path_buf()
    } else if pattern.starts_with('~') {
        home_dir.join(&pattern[1..].trim_start_matches('/'))
    } else {
        PathBuf::from(pattern)
    };

    // "/usr/loca" -> "/usr"
    dir = if dir.is_dir() {
        dir
    } else {
        dir.pop();
        if dir.to_str().unwrap().is_empty() {
            current_dir.to_path_buf()
        } else {
            dir
        }
    };

    trace!("path_completion: dir={}, pattern='{}', only_dirs={}", dir.display(), pattern, only_dirs);
    match fs::read_dir(&dir) {
        Ok(files) => {
            let mut entries = FuzzyVec::new();
            for file in files {
                let file = file.unwrap();
                if only_dirs && !file.file_type().unwrap().is_dir() {
                    continue;
                }

                let path = file.path();

                // Ignore dotfiles unless the pattern contains ".".
                if !pattern.starts_with(".") && !pattern.contains("/.") {
                    if let Some(filename) = path.file_name() {
                        if let Some(filename) = filename.to_str() {
                            if filename.starts_with(".") {
                                continue;
                            }
                        }
                    }
                }

                let (prefix, relpath) = if pattern.starts_with('~') {
                    ("~/", path.strip_prefix(&home_dir).unwrap())
                } else {
                    ("", path.strip_prefix(&current_dir).unwrap_or(&path))
                };

                let comp =
                    format!("{}{}", prefix, relpath.to_str().unwrap());
                entries.append_with_color(comp, ThemeColor::DirColor);
            }
            entries.sort();
            entries
        },
        Err(err) => {
            warn!("failed to readdir '{}': {}", dir.display(), err);
            FuzzyVec::new()
        }
    }
}

#[cfg(test)]
mod tests {
    use tempfile::NamedTempFile;
    use super::*;
    use crossterm::event::KeyModifiers;
    const NONE: KeyModifiers = KeyModifiers::NONE;
    const CTRL: KeyModifiers = KeyModifiers::CONTROL;
    const ALT: KeyModifiers = KeyModifiers::ALT;

    fn create_mainloop() -> Mainloop {
        let shell = Shell::new(NamedTempFile::new().unwrap().path());
        Mainloop::new(shell)
    }

    macro_rules! key_event {
        ($key:expr, $modifiers:expr) => {
            Event::Input(TermEvent::Key(KeyEvent::new($key, $modifiers)))
        };
    }

    macro_rules! comps_event {
        () => { Event::Completion(FuzzyVec::new()) };
        ($($x:expr,)*) => (Event::Completion(FuzzyVec::from_vec(vec![$($x), +])))
    }

    #[test]
    fn move_by_word() {
        let mut m = create_mainloop();
        assert_eq!(m.input.cursor(), 0);
        m.input_event(key_event!(KeyCode::Char('b'), ALT));
        assert_eq!(m.input.cursor(), 0);
        m.input_event(key_event!(KeyCode::Char('f'), ALT));
        assert_eq!(m.input.cursor(), 0);

        m.input_str("abc x  123");
        assert_eq!(m.input.cursor(), 10);
        m.input_event(key_event!(KeyCode::Char('b'), ALT));
        assert_eq!(m.input.cursor(), 7);
        m.input_event(key_event!(KeyCode::Char('b'), ALT));
        assert_eq!(m.input.cursor(), 4);
        m.input_event(key_event!(KeyCode::Char('b'), ALT));
        assert_eq!(m.input.cursor(), 0);
        m.input_event(key_event!(KeyCode::Char('b'), ALT));
        assert_eq!(m.input.cursor(), 0);
        m.input_event(key_event!(KeyCode::Char('f'), ALT));
        assert_eq!(m.input.cursor(), 4);
        m.input_event(key_event!(KeyCode::Char('f'), ALT));
        assert_eq!(m.input.cursor(), 7);
        m.input_event(key_event!(KeyCode::Char('f'), ALT));
        assert_eq!(m.input.cursor(), 10);

        m.input.clear();
        m.input_str("     ");
        m.input_event(key_event!(KeyCode::Char('f'), ALT));
        assert_eq!(m.input.cursor(), 5);
        m.input_event(key_event!(KeyCode::Char('b'), ALT));
        assert_eq!(m.input.cursor(), 0);
    }

    #[test]
    fn remove_until_word_start() {
        let mut m = create_mainloop();
        m.input_str("abc x  123@");
        m.input_event(key_event!(KeyCode::Left, NONE));
        assert_eq!(m.input.as_str(), "abc x  123@");
        m.input_event(key_event!(KeyCode::Char('w'), CTRL));
        assert_eq!(m.input.as_str(), "abc x  @");
        m.input_event(key_event!(KeyCode::Char('w'), CTRL));
        assert_eq!(m.input.as_str(), "abc @");
        m.input_event(key_event!(KeyCode::Char('w'), CTRL));
        assert_eq!(m.input.as_str(), "@");
        m.input_event(key_event!(KeyCode::Char('w'), CTRL));
        assert_eq!(m.input.as_str(), "@");
    }

    #[test]
    fn ctrl_c_to_clear_input() {
        let mut m = create_mainloop();
        m.input_str("abcd");
        m.input_event(key_event!(KeyCode::Char('c'), CTRL));
        assert_eq!(m.input.as_str(), "");
    }

    #[test]
    fn no_completions() {
        let mut m = create_mainloop();
        m.input_str("ls ");
        m.input_event(Event::NoCompletion);
        assert_eq!(m.input.as_str(), "ls ");
    }

    #[test]
    fn select_completion_at_the_end_of_input() {
        let mut m = create_mainloop();
        m.input_str("ls -l \t");
        m.input_event(comps_event![
            "README.md",
            "Makefile",
            "src",
        ]);
        m.input_str("\n"); // Select README.md in the completion
        assert_eq!(m.input.as_str(), "ls -l README.md");
    }

    #[test]
    fn automatically_select_single_completion() {
        let mut m = create_mainloop();
        m.input_str("ls -l Sc\t");
        m.input_event(comps_event![
            "Makefile",
            "SConstruct",
        ]);
        assert_eq!(m.input.as_str(), "ls -l SConstruct");
    }

    #[test]
    fn move_cursor_in_completion() {
        let mut m = create_mainloop();
        m.input_str("ls -l \t");
        m.input_event(comps_event![
            "README.md",
            "Makefile",
            "src",
        ]);
        m.input_event(key_event!(KeyCode::Right, NONE)); // Move the cursor to Makefile
        m.input_str("\n"); // Select Makefile in the completion
        assert_eq!(m.input.as_str(), "ls -l Makefile");
    }

    #[test]
    fn select_completion_in_current_word() {
        let mut m = create_mainloop();
        m.input_str("ls \t.lo");
        m.input_event(comps_event![
            "Cargo.toml",
            "Cargo.lock",
            "yarn.lock",
        ]);
        m.input_str("\n"); // Select Cargo.lock in the completion
        assert_eq!(m.input.as_str(), "ls Cargo.lock");
    }

    #[test]
    fn completion_updates() {
        let mut m = create_mainloop();
        m.input_str("ls \t");
        m.input_event(comps_event![
            "Cargo.toml",
            "Cargo.lock",
            "yarn.lock",
        ]);
        m.input_event(key_event!(KeyCode::Right, NONE)); // Move the cursor to Cargo.lock
        m.input_event(key_event!(KeyCode::Right, NONE)); // Move the cursor to yarn.lock
        m.input_event(comps_event![
            "package.json",
            "node_modules",
        ]);
        m.input_str("\n"); // Select node_modules in the completion
        assert_eq!(m.input.as_str(), "ls node_modules");
    }
}
