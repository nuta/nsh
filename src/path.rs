use std::collections::BTreeMap;
use std::fs::read_dir;
use std::sync::{Mutex, RwLock, Arc};
use crate::worker::Work;
use crate::fuzzy::FuzzyVec;
use crate::completion::Completions;

lazy_static! {
    static ref PATH_TABLE: Mutex<BTreeMap<String, String>> = Mutex::new(BTreeMap::new());
    static ref PATH_FUZZY_VEC: RwLock<FuzzyVec> = RwLock::new(FuzzyVec::new());
    static ref RELOAD_WORK: Work = Work::new(reload_paths);
}

static DEFAULT_PATH: &'static str = "/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/sbin";

pub fn lookup_external_command(cmd: &str) -> Option<String> {
    RELOAD_WORK.wait();

    if cmd.starts_with('/') {
        Some(cmd.to_string())
    } else {
        PATH_TABLE.lock().unwrap().get(cmd).cloned()
    }
}

/// Generate command name completions filtered by `query`.
pub fn complete(query: &str) -> Completions {
    let fuzzy_vec = PATH_FUZZY_VEC.read().unwrap();
    let entries = fuzzy_vec.search(query);
    Completions::new(entries)
}

fn reload_paths() {
    let mut table = PATH_TABLE.lock().unwrap();
    let mut fuzzy_vec = PATH_FUZZY_VEC.write().unwrap();
    let path = std::env::var("PATH").unwrap_or_else(|_| DEFAULT_PATH.to_owned());

    // Look for all executables in $PATH.
    for bin_dir in path.split(':') {
        if let Ok(files) = read_dir(bin_dir) {
            for entry in files {
                let file = entry.unwrap();
                let basename = file.file_name().to_str().unwrap().to_owned();
                let fullpath = file.path().to_str().unwrap().to_owned();
                table.insert(basename.clone(), fullpath);
                fuzzy_vec.append(Arc::new(basename));
            }
        }
    }
}

pub fn init() {
    RELOAD_WORK.enqueue();
}
