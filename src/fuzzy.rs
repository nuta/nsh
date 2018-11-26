use std::sync::Arc;

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
        trace!("nth_last: {} {}", self.entries.len(), nth);
        self.entries.get(self.entries.len() - nth - 1).cloned()
    }

    pub fn append(&mut self, entry: Arc<String>) {
        self.entries.push(entry);
    }

    pub fn search(&self, query: &str) -> Vec<Arc<String>> {
        let mut results = Vec::new();
'entry_loop: for e in &self.entries {
            for ch in query.chars() {
                if !e.contains(ch) {
                    continue 'entry_loop;
                }
            }

            results.push(e.clone());
        }

        results
    }
}
