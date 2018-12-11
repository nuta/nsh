use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};
use std::path::PathBuf;
use crate::fuzzy::FuzzyVec;
use crate::context_parser::InputContext;

/// The `Completion` Builder.
pub struct CompletionBuilder {
    entries: Vec<Arc<String>>,
    query: Option<Arc<String>>,
}

impl CompletionBuilder {
    #[inline]
    pub fn new() -> CompletionBuilder {
        CompletionBuilder {
            entries: Vec::new(),
            query: None,
        }
    }

    #[inline]
    pub fn entries(mut self, entries: Vec<Arc<String>>) -> CompletionBuilder {
        self.entries = entries;
        self
    }

    #[inline]
    pub fn search(mut self, query: Option<Arc<String>>) -> CompletionBuilder {
        self.query = query;
        self
    }

    #[inline]
    pub fn build(self) -> Vec<Arc<String>> {
        let results = match self.query {
            Some(query) => FuzzyVec::from_vec(self.entries).search(&query),
            None => self.entries,
        };

        results
    }
}

fn path_completion(ctx: &InputContext) -> Vec<Arc<String>> {
    let given_dir = ctx.current_word().map(|s| (&*s).clone());
    trace!("path_completion: current='{:?}', dir='{:?}'", ctx.current_word(), given_dir);
    let dirent = match &given_dir {
        Some(given_dir) if given_dir.ends_with('/') => {
            std::fs::read_dir(given_dir)
        },
        Some(given_dir) if given_dir.contains('/') => {
            // Remove the last part: `/Users/chandler/Docum' -> `/users/chandler'
            std::fs::read_dir(PathBuf::from(given_dir.clone()).parent().unwrap())
        },
        _ => {
            std::fs::read_dir(".")
        }
    };

    let mut entries = Vec::new();
    if let Ok(dirent) = dirent {
        for entry in dirent {
            let mut path = entry
                .unwrap()
                .path();

            if path.starts_with("./") {
                path = path.strip_prefix("./").unwrap().to_path_buf();
            }

            entries.push(Arc::new(path.to_str().unwrap().to_owned()));
        }
    }

    CompletionBuilder::new()
        .entries(entries)
        .search(ctx.current_word())
        .build()
}

fn cmd_completion(ctx: &InputContext) -> Vec<Arc<String>> {
    match ctx.current_word() {
        Some(query) => crate::path::complete(&query),
        None => crate::path::complete(""),
    }
}

type CompletionFunc = fn(&InputContext) -> Vec<Arc<String>>;
struct CompSpec {
    func: CompletionFunc,
}

lazy_static! {
    static ref COMP_SPECS: Mutex<BTreeMap<String, CompSpec>> =
        { Mutex::new(BTreeMap::new()) };
}

pub fn complete(ctx: &InputContext) -> Vec<Arc<String>> {
    if ctx.current_word_index == 0 {
        // The cursor is at the first word, namely, the command.
        cmd_completion(ctx)
    } else {
        let funcs = COMP_SPECS.lock().unwrap();

        // This `ctx.words[0]` never panic: `ctx.words.len() > 0` is always true
        // since `current_word_index` is larger than 0.
        if let Some(compspec) = funcs.get(&*ctx.words[0]) {
            // A dedicated completion function is available.
            (compspec.func)(ctx)
        } else {
            // There are no completion fuctions. Use path completion
            // instead.
            path_completion(ctx)
        }
    }
}
