//! History management.
use crate::fuzzy::FuzzyVec;
use crate::theme::ThemeColor;
use std::fs::{File, OpenOptions};
use std::io::{BufRead, BufReader, Write};
use std::path::{Path, PathBuf};

/// Command history.
pub struct History {
    path: PathBuf,
    history: FuzzyVec,
}

impl History {
    pub fn new(history_file: &Path) -> History {
        // Loads the history file.
        let mut warned = false;
        let mut history = FuzzyVec::new();
        if let Ok(file) = File::open(history_file) {
            for (i, line) in BufReader::new(file).lines().enumerate() {
                if let Ok(line) = line {
                    let cmd = line.split('\t').nth(2);
                    match (cmd, warned) {
                        (Some(cmd), _) => {
                            history.append(cmd.to_string());
                        }
                        (None, false) => {
                            print_err!(
                                "nsh: warning: failed to parse ~/.nsh_history: at line {}",
                                i + 1
                            );
                            warned = true;
                        }
                        (_, _) => (),
                    }
                }
            }
        }

        History {
            path: history_file.to_owned(),
            history,
        }
    }

    pub fn len(&self) -> usize {
        self.history.len()
    }

    pub fn nth_last(&self, nth: usize) -> Option<String> {
        self.history.nth_last(nth)
    }

    pub fn search(&self, query: &str) -> Vec<(Option<ThemeColor>, &str)> {
        self.history.search(query)
    }

    /// Appends a history to the history file.
    pub fn append(&mut self, cmd: &str) {
        if cmd.is_empty() {
            return;
        }

        // Ignore if `cmd` is same as the last command.
        if let Some(last) = self.history.nth_last(0) {
            if last.as_str() == cmd {
                return;
            }
        }

        if let Ok(mut file) = OpenOptions::new().append(true).open(&self.path) {
            let dir = std::env::current_dir()
                .unwrap()
                .to_str()
                .unwrap()
                .to_owned();
            let time = std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .expect("failed to get the UNIX timestamp")
                .as_secs() as usize;
            file.write(format!("{}\t{}\t{}\n", time, dir, cmd).as_bytes()).ok();
        }

        self.history.append(cmd.to_string());
    }
}

pub struct HistorySelector {
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
