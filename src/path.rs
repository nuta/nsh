use std::collections::BTreeMap;
use std::fs::read_dir;
use std::sync::{RwLock, Arc};
use crate::fuzzy::FuzzyVec;
use crate::config::Config;

lazy_static! {
    /// A `(command name, absolute path to the executable)` table.
    static ref PATH_TABLE: RwLock<BTreeMap<String, String>> = RwLock::new(BTreeMap::new());
    /// A `FuzzyVec` which contains command names for completion.
    static ref PATH_FUZZY_VEC: RwLock<FuzzyVec> = RwLock::new(FuzzyVec::new());
}

/// Wait for the `reload_paths()` thread to finish its job.
pub fn wait_for_path_loader() {
    // XXX: Assumes that reload_path() thread aquires the writer lock before.
    PATH_TABLE.write().ok();
}

/// Returns the absolute path to the executable.
pub fn lookup_external_command(cmd: &str) -> Option<String> {
    if cmd.starts_with('/') {
        Some(cmd.to_string())
    } else {
        PATH_TABLE.read().unwrap().get(cmd).cloned()
    }
}

/// Generates command name completions filtered by `query`.
pub fn complete(query: &str) -> Vec<Arc<String>> {
    let fuzzy_vec = PATH_FUZZY_VEC.read().unwrap();
    let entries = fuzzy_vec.search(query);
    entries
}

/// Scans `$PATH` to fill `PATH_TABLE`.
fn reload_paths() {
    let mut table = PATH_TABLE.write().unwrap();
    let mut fuzzy_vec = PATH_FUZZY_VEC.write().unwrap();
    let path = std::env::var("PATH").unwrap();

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

pub fn init(config: &Config) {
    std::env::set_var("PATH", config.path.clone());
    std::thread::spawn(|| {
        reload_paths();
    });
}
