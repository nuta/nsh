use rustyline::error::ReadlineError;
use std::fs::File;
use std::borrow::Cow::{self, Borrowed, Owned};
use std::env;
use std::process;
use rustyline::{Editor, Config, CompletionType, EditMode};
use rustyline::completion::{Completer, FilenameCompleter, Pair};
use rustyline::highlight::Highlighter;
use rustyline::hint::Hinter;
use rustyline::Helper;
use exec;
use parser;
use dirs;
use prompt::{parse_prompt, draw_prompt};
use std::path::{Path, PathBuf};

static DEFAULT_PROMPT: &'static str = "\\W $ ";

fn resolve_and_create_history_file() -> Option<PathBuf> {
    if let Some(home_dir) = dirs::home_dir() {
        let history_path = Path::new(&home_dir).join(".nsh_history");
        if history_path.exists() {
            return Some(history_path)
        }

        if File::create(&history_path).is_ok() {
            return Some(history_path)
        }
    }

    None
}

struct RustylineHelper {
    filename_completer: FilenameCompleter,
}

impl RustylineHelper {
    fn new() -> RustylineHelper {
        RustylineHelper {
            filename_completer: FilenameCompleter::new()
        }
    }
}

impl Completer for RustylineHelper {
    type Candidate = Pair;

    fn complete(&self, line: &str, pos: usize) -> Result<(usize, Vec<Pair>), ReadlineError> {
        println!("");
        trace!("complete: {}", line);
        trace!("         {}^", " ".repeat(pos));
        self.filename_completer.complete(line, pos)
    }
}

impl Hinter for RustylineHelper {
    fn hint(&self, _line: &str, _pos: usize) -> Option<String> {
        None
    }
}

impl Highlighter for RustylineHelper {
    fn highlight_prompt<'p>(&self, prompt: &'p str) -> Cow<'p, str> {
        Borrowed(prompt)
    }

    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        Owned(format!("\x1b[1m{}\x1b[", hint))
    }
}

impl Helper for RustylineHelper {}

pub fn mainloop() {
    let config = Config::builder()
        .edit_mode(EditMode::Emacs)
        .completion_type(CompletionType::List)
        .history_ignore_space(true)
        .build();
    let h = RustylineHelper::new();
    let mut rl = Editor::with_config(config);
    rl.set_helper(Some(h));

    // Load histories.
    let history_path = resolve_and_create_history_file();
    if let Some(ref path) = history_path {
        rl.load_history(path).expect("failed to load history");
    } else {
        warn!("disabling history");
    }

    loop {
        // Parse and print the prompt.
        let mut prompt_str = String::new();
        let ps1 = env::var("PS1").unwrap_or(DEFAULT_PROMPT.to_owned());
        if let Ok(fmt) = parse_prompt(ps1.as_str()) {
            draw_prompt(&fmt, &mut prompt_str);
        }

        // Read a line.
        let line = match rl.readline(&prompt_str) {
            Ok(line) => line,
            Err(ReadlineError::Eof) => {
                if let Some(ref path) = history_path {
                    rl.save_history(path).expect("failed to save history");
                }
                process::exit(0);
            },
            Err(err) => {
                panic!("something went wrong with readline: {:?}", err);
            }
        };

        rl.add_history_entry(line.as_ref());

        let cmd = match parser::parse_line(line.as_str()) {
            Ok(cmd) => cmd,
            Err(err) => {
                eprintln!("nsh: parse error: {:?}", err);
                return;
            }
        };

        exec::exec(cmd);
    }
}