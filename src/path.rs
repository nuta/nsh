use crate::fuzzy::FuzzyVec;
use std::collections::HashMap;
use std::fs::read_dir;

/// A cached `$PATH` table.
pub struct PathTable {
    /// Key is command name and value is absolute path to the executable.
    table: HashMap<String, String>,
    /// Command names used for completion.
    fuzzy: FuzzyVec,
}

impl PathTable {
    pub fn new() -> PathTable {
        PathTable {
            table: HashMap::new(),
            fuzzy: FuzzyVec::new(),
        }
    }

    /// Scans bin directories and caches all files in them. Call this method
    /// when you update `$PATH`!
    pub fn scan(&mut self, path: &str) {
        self.table.clear();
        self.fuzzy.clear();

        for bin_dir in path.split(':').rev() {
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

    /// Generates command name completions filtered by `query`.
    pub fn complete(&self, query: &str) -> Vec<&str> {
        self.fuzzy.search(query)
    }
}
