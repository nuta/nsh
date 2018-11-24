use std::env;
use std::fs::read_dir;
use std::collections::BTreeMap;
use std::sync::Mutex;

lazy_static! {
    static ref PATH_TABLE: Mutex<BTreeMap<String, String>> = Mutex::new(BTreeMap::new());
}

static DEFAULT_PATH: &'static str = "/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/sbin";

pub fn lookup_external_command(cmd: &str) -> Option<String> {
    if cmd.starts_with('/') {
        Some(cmd.to_string())
    } else {
        PATH_TABLE.lock().unwrap().get(cmd).cloned()
    }
}

fn reload_paths() {
    let mut table = PATH_TABLE.lock().unwrap();
    let path = env::var("PATH").unwrap_or_else(|_| DEFAULT_PATH.to_owned());

    // Look for all executables in $PATH.
    for bin_dir in path.split(':') {
        if let Ok(files) = read_dir(bin_dir) {
            for entry in files {
                let file = entry.unwrap();
                let basename = file.file_name().to_str().unwrap().to_owned();
                let fullpath = file.path().to_str().unwrap().to_owned();
                table.insert(basename, fullpath);
            }
        }
    }
}

pub fn init() {
    reload_paths();
}
