use crate::fuzzy::FuzzyVec;
use std::collections::HashMap;
use std::fs::read_dir;

/// A cached `$PATH` table.
pub struct PathTable {
    /// `$PATH`
    path: String,
    /// Key is command name and value is absolute path to the executable.
    table: HashMap<String, String>,
    /// Command names used for completion.
    fuzzy: FuzzyVec,
}

impl PathTable {
    pub fn new() -> PathTable {
        PathTable {
            path: String::new(),
            table: HashMap::new(),
            fuzzy: FuzzyVec::new(),
        }
    }

    pub fn fuzzy_vec(&self) -> &FuzzyVec {
        &self.fuzzy
    }

    /// Scans bin directories and caches all files in them. Call this method
    /// when you update `$PATH`!
    pub fn scan(&mut self, path: &str) {
        self.path = path.to_string();
        self.rehash();
    }

    pub fn rehash(&mut self) {
        self.table.clear();
        self.fuzzy.clear();
        for bin_dir in self.path.split(':').rev() {
            if let Ok(files) = read_dir(bin_dir) {
                for entry in files {
                    let file = entry.unwrap();
                    let basename = file.file_name().to_str().unwrap().to_owned();
                    let fullpath = file.path().to_str().unwrap().to_owned();
                    self.table.insert(basename.clone(), fullpath);
                    self.fuzzy.append(basename);
                }
            }
        }
    }

    /// Returns whether the specified command name exists in the `$PATH`.
    pub fn contains(&self, cmd: &str) -> bool {
        self.table.contains_key(cmd)
    }

    /// Returns the absolute path to the executable.
    pub fn lookup(&self, cmd: &str) -> Option<&str> {
        self.table.get(cmd).map(String::as_str)
    }
}
