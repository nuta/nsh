//!
//! History management. `~/.nsh_history` is a text file where each line is a JSON represented
//! by `History` struct. We don't simply save the whole history as a JSON array since appending
//! to a *large* history array would be very slow.
//!
use std::sync::Arc;
use std::path::{Path, PathBuf};
use std::fs::{File, OpenOptions};
use std::io::{BufReader, BufRead, Write};
use std::sync::Mutex;
use crate::config::Config;
use crate::fuzzy::FuzzyVec;

/// A history entry.
#[derive(Debug, Serialize, Deserialize)]
pub struct History {
    /// The UNIX timestamp when the command executed.
    pub time: usize,
    /// The absolute path to the directory where the command executed.
    pub dir: String,
    /// The command.
    pub cmd: String,
}

lazy_static! {
    /// Command history.
    static ref HISTORY: Mutex<FuzzyVec> = Mutex::new(FuzzyVec::new());
}

/// Returns true if the `cmd' should NOT be saved in a file.
fn history_filter(cmd: &str) -> bool {
    cmd.starts_with(' ')
    || cmd.starts_with('\t')
}

/// Appends a history to the history file.
pub fn append_history(cmd: &str) {
    let mut  hist = HISTORY.lock().unwrap();

    let history_path = resolve_and_create_history_file();
    if let Ok(mut file) = OpenOptions::new().append(true).open(history_path) {
        if !history_filter(cmd) {
            // Construct JSON formatted:
            //
            //    {
            //      "time": number,     /* UNIX timestamp */
            //      "dir": string,      /* The current directory */
            //      "history": string,  /* The command line */
            //    }
            //
            //
            let time = std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .expect("failed to get the UNIX timestamp")
                .as_secs() as usize;
            let dir = std::env::current_dir().unwrap().to_str().unwrap().to_owned();
            let history = History { time, dir, cmd: cmd.to_owned() };
            let json = serde_json::to_string(&history).unwrap();
            file.write(format!("{}\n", json).as_bytes()).ok();
        }
    }

    hist.append(Arc::new(cmd.to_string()));
}

/// Returns the absolute path to the history file. Creates it if it doesn't exist.
fn resolve_and_create_history_file() -> PathBuf {
    if let Some(home_dir) = dirs::home_dir() {
        let history_path = Path::new(&home_dir).join(".nsh_history");
        if !history_path.exists() {
            File::create(&history_path).unwrap();
        }
        history_path
    } else {
        panic!("failed to get the path to the home directory");
    }
}

/// Loads the history file and fill `HISTORY`.
fn load_history() {
    let history_path = resolve_and_create_history_file();
    let mut warned = false;
    if let Ok(file) = File::open(history_path) {
        for (i, line) in BufReader::new(file).lines().enumerate() {
            if let Ok(line) = line {
                let parsed: Result<History, _> = serde_json::from_str(&line);
                match (parsed, warned) {
                    (Ok(history), _) => {
                        let mut hist = HISTORY.lock().unwrap();
                        hist.append(Arc::new(history.cmd));
                    },
                    (Err(err), false) => {
                        eprintln!("nsh: failed to parse ~/.nsh_history: at line {}: `{}'", i + 1, err);
                        warned = true;
                    },
                    (_, _) => (),
                }
            }
        }
    }
}

/// History-related state in the prompt.
pub struct HistorySelector {
    offset: usize,
    user_input: String,
}

impl HistorySelector {
    pub fn new() -> HistorySelector {
        HistorySelector {
            offset: 0,
            user_input: String::new(),
        }
    }

    /// Returns None if `self.offset` is 0 otherwise `self.offset - 1`th entry.
    pub fn current(&self) -> Arc<String> {
        if self.offset == 0 {
            //  Reached to the end of histories. Restore the saved state.
            Arc::new(self.user_input.clone())
        } else {
            let hist = HISTORY.lock().unwrap();
            hist.nth_last(self.offset - 1).unwrap()
        }
    }

    /// Selects the previous history entry. Save the current user (not yet executed)
    /// input if needed.
    pub fn prev(&mut self,  user_input: &str) {
        if self.offset == 0 {
            // Entering the history selection. Save the current state.state.
            self.user_input = user_input.to_string();
        }

        let hist_len = HISTORY.lock().unwrap().len();
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

pub fn init(_config: &Config) {
    std::thread::spawn(|| {
        load_history();
    });
}
