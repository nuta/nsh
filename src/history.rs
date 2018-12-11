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

#[derive(Debug, Serialize, Deserialize)]
pub struct History {
    pub time: usize,
    pub dir: String,
    pub cmd: String,
}

lazy_static! {
    static ref HISTORY: Mutex<FuzzyVec> = Mutex::new(FuzzyVec::new());
}

pub fn append_history(cmd: &str) {
    let mut  hist = HISTORY.lock().unwrap();

    let history_path = resolve_and_create_history_file();
    if let Ok(mut file) = OpenOptions::new().append(true).open(history_path) {
        let time = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("failed to get the UNIX timestamp")
            .as_secs() as usize;
        let dir = std::env::current_dir().unwrap().to_str().unwrap().to_owned();
        let history = History { time, dir, cmd: cmd.to_owned() };
        let json = serde_json::to_string(&history).unwrap();
        file.write(format!("{}\n", json).as_bytes()).ok();
    }

    hist.append(Arc::new(cmd.to_string()));
}

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
