use std::path::Path;

pub struct PathTable {}

impl PathTable {
    pub fn lookup(&self, argv0: &str) -> Option<&Path> {
        unimplemented!()
    }
}
