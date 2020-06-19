use crate::theme::ThemeColor;

///
/// A ordered `Vec` which supports fuzzy search.
///
#[derive(Clone)]
pub struct FuzzyVec {
    /// The *unordered* array of a haystack.
    entries: Vec<(Option<ThemeColor>, String)>,
}

impl FuzzyVec {
    /// Creates a `FuzzyVec`.
    pub fn new() -> FuzzyVec {
        FuzzyVec {
            entries: Vec::new(),
        }
    }

    /// Creates a `FuzzyVec` with the given capacity.
    pub fn with_capacity(cap: usize) -> FuzzyVec {
        FuzzyVec {
            entries: Vec::with_capacity(cap),
        }
    }

    pub fn iter(&self) -> std::slice::Iter<(Option<ThemeColor>, String)> {
        self.entries.iter()
    }

    /// Returns the number of entiries.
    #[inline]
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
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

        self.entries.get(self.entries.len() - nth - 1).map(|(_, s)| s.to_owned())
    }

    /// appends a entry.
    pub fn append(&mut self, entry: String) {
        self.entries.push((None, entry));
    }

    /// appends a entry with color.
    pub fn append_with_color(&mut self, entry: String, color: ThemeColor) {
        self.entries.push((Some(color), entry));
    }

    /// Searches entiries for `query` in a fuzzy way and returns the result
    /// ordered by the similarity.
    pub fn search(&self, query: &str) -> Vec<(Option<ThemeColor>, &str)> {
        fuzzy_search(&self.entries, query)
    }

    /// Sorts the entries by string values.
    pub fn sort(&mut self) {
        self.entries.sort_by(|(_, a), (_, b)| a.cmp(b));
    }
}

/// Searches `entiries` for `query` in *fuzzy* way and returns the result
/// ordered by the similarity.
///
/// TODO: Implement smart one.
///
fn fuzzy_search<'a>(
    entries: &'a [(Option<ThemeColor>, String)],
    query: &str
) -> Vec<(Option<ThemeColor>, &'a str)> {
    if query.is_empty() {
        // Return the all entries.
        return entries.iter().map(|(c, s)| (*c, s.as_str())).collect();
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
        .filter(|(_, s)| is_fuzzily_matched(s, query))
        .map(|(c, s)| (*c, s.as_str()))
        .collect::<Vec<_>>();
    filtered.sort_by_cached_key(|entry| compute_score(entry.1, query));
    filtered
}

/// Computes the similarity. Lower is more similar.
fn compute_score(entry: &str, query: &str) -> u8 {
    let mut score = std::u8::MAX;

    if entry == query {
        score -= 100;
    }

    if entry.starts_with(query) {
        score -= 10;
    }

    score
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fuzzy_search() {
        {
            let entries = &[
                (None, "abc".to_owned()),
                (None, "bca".to_owned()),
                (None, "cba".to_owned()),
            ];
            let query = "bc";
            // "cba" does not contain "bc" with correct order, so "cba" must be removed.
            assert_eq!(
                fuzzy_search(entries, query),
                vec![
                    (None, "bca"),
                    (None, "abc")
                ]
            );
        }

        // Ensure that the exact match takes priority.
        {
            let entries = &[
                (None, "g++8".to_owned()),
                (None, "g++9".to_owned()),
                (None, "g++".to_owned()),
            ];
            let query = "g++";
            assert_eq!(
                fuzzy_search(entries, query),
                vec![
                    (None, "g++"),
                    (None, "g++8"),
                    (None, "g++9"),
                ]
            );
        }
    }
}
