use std::collections::BTreeMap;
use std::sync::Arc;

///
/// A ordered `Vec` which support fuzzy search.
/// TODO: Implement smart one.
///
pub struct FuzzyVec {
    entries: Vec<Arc<String>>,
}

impl FuzzyVec {
    pub fn new() -> FuzzyVec {
        FuzzyVec {
            entries: Vec::new(),
        }
    }

    pub fn from_vec(entries: Vec<Arc<String>>) -> FuzzyVec {
        FuzzyVec {
            entries,
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn nth_last(&self, nth: usize) -> Option<Arc<String>> {
        self.entries.get(self.entries.len() - nth - 1).cloned()
    }

    pub fn append(&mut self, entry: Arc<String>) {
        self.entries.push(entry);
    }

    pub fn search(&self, query: &str) -> Vec<Arc<String>> {
        fuzzy_search(&self.entries, query)
    }
}

fn fuzzy_search(entries: &[Arc<String>], query: &str) -> Vec<Arc<String>> {
        let mut filtered = Vec::new();

        // Filter entries by the query.
'entry_loop:
        for e in entries {
            let mut iter = e.as_str().chars();
            for ch in query.chars() {
                if iter.find(|c| *c == ch).is_none() {
                    // unmatch
                    continue 'entry_loop;
                }
            }

            filtered.push(e.clone());
        }

        // Sort results by computing scores (or similarity).
        let mut sorted_map = BTreeMap::new();
        for (i, entry) in filtered.iter().enumerate() {
            let score = compute_score(&entry, query);

            // The keys are sorted by `score`. Here we embed an unique number `i` in
            // lower 24 bits to make the key unique.
            let key = ((score as u32) << 24) | i as u32;
            sorted_map.insert(key, entry.clone());
        }

        let mut sorted = Vec::new();
        for value in sorted_map.values() {
            sorted.push(value.clone());
        }

        sorted
}

/// Computes the similarity. Lower is more similar.
fn compute_score(entry: &str, query: &str) -> u8 {
    let mut score: isize = 255;

    if entry.starts_with(query) {
        score -= 10;
    }

    std::cmp::max(score, 0) as u8
}
