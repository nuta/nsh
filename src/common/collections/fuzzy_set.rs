use std::collections::{HashMap, HashSet};

use fuzzy_matcher::{skim::SkimMatcherV2, FuzzyMatcher};
use rayon::prelude::*;

struct Entry<T: Sync> {
    value: T,
    extra_score: isize,
}

pub struct FuzzySet<T: Sync> {
    matcher: SkimMatcherV2,
    keys: HashSet<String>,
    entries: HashMap<String, Entry<T>>,
}

impl<T: Sync> FuzzySet<T> {
    pub fn new() -> FuzzySet<T> {
        FuzzySet {
            matcher: SkimMatcherV2::default().smart_case().use_cache(true),
            keys: HashSet::new(),
            entries: HashMap::new(),
        }
    }

    pub fn query(&self, pattern: &str) -> Vec<(&str, &T, isize)> {
        let mut filtered: Vec<(&str, &T, isize)> = self
            .keys
            .par_iter()
            .filter_map(|key| {
                let entry = &self.entries[key];
                if pattern.is_empty() {
                    return Some((key.as_str(), &entry.value, entry.extra_score));
                }

                self.matcher.fuzzy_match(key, pattern).map(|score| {
                    (
                        key.as_str(),
                        &entry.value,
                        (score as isize) + entry.extra_score,
                    )
                })
            })
            .collect();

        filtered.sort_by_key(|(_, _, score)| *score);
        filtered
    }

    pub fn insert<K: Into<String>>(&mut self, key: K, value: T, extra_score: isize) {
        let key = key.into();
        self.keys.insert(key.clone());
        self.entries.insert(key, Entry { value, extra_score });
    }

    pub fn remove(&mut self, key: &str) {
        self.keys.remove(key);
        self.entries.remove(key);
    }
}

impl<T: Sync> Default for FuzzySet<T> {
    fn default() -> Self {
        Self::new()
    }
}
