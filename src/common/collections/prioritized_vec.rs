use std::collections::BinaryHeap;

pub struct Entry<T> {
    pub priority: isize,
    pub value: T,
}

impl<T> PartialOrd for Entry<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for Entry<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.priority.cmp(&self.priority)
    }
}

impl<T> PartialEq for Entry<T> {
    fn eq(&self, other: &Self) -> bool {
        self.priority == other.priority
    }
}

impl<T> Eq for Entry<T> {}

impl<T: Clone> Clone for Entry<T> {
    fn clone(&self) -> Self {
        Entry {
            priority: self.priority,
            value: self.value.clone(),
        }
    }
}

pub struct PrioritizedVec<T> {
    heap: BinaryHeap<Entry<T>>,
    capacity: usize,
}

impl<T> PrioritizedVec<T> {
    pub fn new(capacity: usize) -> PrioritizedVec<T> {
        PrioritizedVec {
            heap: BinaryHeap::new(),
            capacity,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.heap.is_empty()
    }

    pub fn len(&self) -> usize {
        self.heap.len()
    }

    pub fn insert(&mut self, priority: isize, value: T) {
        self.heap.push(Entry { priority, value });

        while self.heap.len() >= self.capacity {
            self.heap.pop();
        }
    }

    pub fn clear(&mut self) {
        self.heap.clear();
    }

    pub fn sorted_vec(&self) -> Vec<T>
    where
        T: Clone,
    {
        let heap = self.heap.clone();
        heap.into_sorted_vec().drain(..).map(|e| e.value).collect()
    }
}
