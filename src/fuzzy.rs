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
        FuzzyVec { entries }
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
        if self.entries.is_empty() {
            return None;
        }

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
    if query.is_empty() {
        // Return the all entries.
        return entries.iter().cloned().collect();
    }

    /// Check if entries contain the query characters with correct order.
    fn is_fuzzily_matched(s: &str, query: &str) -> bool {
        let mut iter = s.chars();
        for q in query.chars() {
            loop {
                match iter.next() {
                    None => return false,
                    Some(c) if c == q => break,
                    Some(_) => {}
                }
            }
        }
        true
    }

    // Filter entries by the query.
    let mut filtered = entries
        .iter()
        .filter(|s| is_fuzzily_matched(s, query))
        .cloned()
        .collect::<Vec<_>>();
    filtered.sort_by_cached_key(|entry| compute_score(entry, query));
    filtered
}

/// Computes the similarity. Lower is more similar.
fn compute_score(entry: &str, query: &str) -> u8 {
    let mut score: isize = 255;

    if entry.starts_with(query) {
        score -= 10;
    }

    std::cmp::max(score, 0) as u8
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fuzzy_search() {
        let entries = &["abc".to_owned(), "bca".to_owned(), "cba".to_owned()];
        let query = "bc";
        // "cba" does not contain "bc" with correct order, so "cba" must be removed.
        assert_eq!(
            fuzzy_search(entries, query),
            vec!["bca".to_owned(), "abc".to_owned()]
        );
    }
}
