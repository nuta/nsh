use std::collections::BTreeMap;

///
/// A ordered `Vec` which supports fuzzy search.
///
pub struct FuzzyVec {
    /// The *unordered* array of a haystack.
    entries: Vec<String>,
}

impl FuzzyVec {
    /// Creates a `FuzzyVec`.
    pub fn new() -> FuzzyVec {
        FuzzyVec {
            entries: Vec::new(),
        }
    }

    /// Creates a `FuzzyVec` from `entries`.
    pub fn from_vec(entries: Vec<String>) -> FuzzyVec {
        FuzzyVec {
            entries,
        }
    }

    /// Returns the number of entiries.
    #[inline]
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    // Clears the contents.
    pub fn clear(&mut self) {
        self.entries.clear();
    }

    /// Returns the nth entry from the end of the entries.
    pub fn nth_last(&self, nth: usize) -> Option<String> {
        self.entries.get(self.entries.len() - nth - 1).cloned()
    }

    /// appends a entry.
    pub fn append(&mut self, entry: String) {
        self.entries.push(entry);
    }

    /// Searches entiries for `query` in *fuzzy* way and returns the result
    /// ordered by the similarity.
    pub fn search(&self, query: &str) -> Vec<String> {
        fuzzy_search(&self.entries, query)
    }
}

/// Searches `entiries` for `query` in *fuzzy* way and returns the result
/// ordered by the similarity.
///
/// TODO: Implement smart one.
///
fn fuzzy_search(entries: &[String], query: &str) -> Vec<String> {
        let mut filtered = Vec::new();

        if query.is_empty() {
            // Return the all entries.
            for e in entries {
                filtered.push(e.clone());
            }

            return filtered;
        }

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
            let key = (u32::from(score) << 24) | i as u32;
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
