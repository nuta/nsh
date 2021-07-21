use crate::fuzzy::FuzzyVec;
use crate::mainloop::Event;
use phf::phf_set;
use std::collections::HashSet;
use std::io::Write;
use std::process::{Child, Command, Stdio};
use std::sync::mpsc;
use std::time::SystemTime;

pub enum BashRequest {
    Complete {
        words: Vec<String>,
        current_word: usize,
    },
}

pub fn bash_server(tx_event: mpsc::Sender<Event>) -> mpsc::Sender<BashRequest> {
    let (tx, rx) = mpsc::channel();
    std::thread::spawn(move || {
        let mut bash = None;
        loop {
            if bash.is_none() {
                bash = Some(preload_bash());
            }

            let req = rx.recv().unwrap();
            match req {
                BashRequest::Complete {
                    words,
                    current_word,
                } => {
                    trace!("completion: query={:?}", words);
                    let started_at = SystemTime::now();

                    match run_bash(&mut bash, words, current_word) {
                        Some(comps) => {
                            tx_event.send(Event::Completion(comps)).ok();
                        }
                        None => {
                            tx_event.send(Event::NoCompletion).ok();
                        }
                    }

                    let elapsed = started_at.elapsed().unwrap();
                    trace!("bash took={}us", elapsed.as_micros());
                }
            }
        }
    });

    tx
}

static COMP_DIRS: &[&str] = &[
    "/usr/share/bash-completion/completions",
    "/usr/local/etc/bash_completion.d",
    "/usr/etc/bash_completion.d",
    "/etc/bash_completion.d",
];

static COMP_SUFFIXES: &[&str] =
    &["-completion.bash", "-completion.sh", ".bash", ".sh", ""];

static PRELOADED_COMPS: phf::Set<&'static str> = phf_set! {
    "git",
};

lazy_static! {
    static ref PRELOAD_SCRIPT: String = {
        let mut script = String::new();

        for cmd_name in PRELOADED_COMPS.iter() {
            if let Some(comp_file) = look_for_comp_file(cmd_name) {
                script += &format!("source \"{}\"", escape(&comp_file));
            }
        }

        script
    };
}

fn is_normal_file(s: &str) -> bool {
    let path = std::path::Path::new(s);
    path.exists() && path.is_file()
}

fn look_for_comp_file(cmd_name: &str) -> Option<String> {
    for dir in COMP_DIRS {
        for suffix in COMP_SUFFIXES {
            let s = format!("{}/{}{}", dir, cmd_name, suffix);
            if is_normal_file(&s) {
                return Some(s);
            }
        }
    }

    None
}

fn escape(s: &str) -> String {
    s.replace('\\', "\\\\").replace('$', "\\$")
}

fn preload_bash() -> Child {
    let mut bash = Command::new("bash")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to start an external Bash");

    // Preload completions for popular commands.
    let stdin = bash.stdin.as_mut().unwrap();
    writeln!(stdin, "{}", *PRELOAD_SCRIPT).ok();
    stdin.flush().ok();

    bash
}

// Guesses the completion function (assuming its name is "_{cmd_name}").
fn guess_completion_cmd_name(cmd_name: &str) -> String {
    match cmd_name {
        "git" => "__git_main".to_owned(),
        _ => format!("_{}", &cmd_name.replace('-', "_")),
    }
}

fn run_bash(bash: &mut Option<Child>, words: Vec<String>, current_word: usize) -> Option<FuzzyVec> {
    if words.is_empty() {
        return None;
    }

    let cmd_name = &words[0];
    let needs_comp_file = !PRELOADED_COMPS.contains(cmd_name.as_str());
    let comp_file = if needs_comp_file {
        if let Some(comp_file) = look_for_comp_file(cmd_name) {
            Some(comp_file)
        } else {
            return None;
        }
    } else {
        None
    };

    let mut bash = bash.take().unwrap();

    // Define $COMP_CWORD and $COMP_WORDS.
    let mut stdin = bash.stdin.take().unwrap();
    if let Some(comp_file) = comp_file {
        trace!("comp_file = {}", comp_file);
        writeln!(stdin, "source \"{}\"", escape(&comp_file)).ok();
    }
    writeln!(stdin, "COMP_CWORD={}", current_word).ok();
    writeln!(stdin, "COMP_WORDS=(").ok();
    for (i, word) in words.iter().enumerate() {
        if i == current_word {
            // We prefer filtering completions by ourselves using FuzzyVec
            // so don't pass the current word.
            writeln!(stdin, "\"\"").ok();
        } else {
            writeln!(stdin, "\"{}\"", escape(word)).ok();
        }
    }
    writeln!(stdin, ")").ok();

    // Run the completion function.
    let completion_cmd = guess_completion_cmd_name(&cmd_name);
    writeln!(stdin, "{} >/dev/null 2>>~/.nsh.log", completion_cmd).ok();

    // Print the results.
    writeln!(
        stdin,
        "{}",
        "for c in \"${COMPREPLY[@]}\"; do echo \"$c\"; done"
    )
    .ok();

    // Flush the scripts and terminate the bash by EOF.
    drop(stdin);

    // Read the results through the pipe.
    let output = bash.wait_with_output().unwrap();
    if output.status.success() {
        if let Ok(reply) = String::from_utf8(output.stdout) {
            // Remove duplicated and empty ones.
            let mut unique_comps = HashSet::new();
            for comp in reply.split('\n') {
                if !comp.trim().is_empty() {
                    unique_comps.insert(comp);
                }
            }

            // Turn it into a FuzzyVec.
            let mut comps = FuzzyVec::with_capacity(unique_comps.len());
            for comp in unique_comps.drain() {
                comps.append(comp.to_owned());
            }

            return Some(comps);
        }
    }

    Some(FuzzyVec::new())
}
