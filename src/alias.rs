use std::sync::Mutex;
use std::collections::BTreeMap;
use exec::ExitStatus;
use parser::{parse_alias, Alias, Word};

lazy_static! {
    static ref ALIASES: Mutex<BTreeMap<String, Vec<Word>>> = Mutex::new(BTreeMap::new());
}

fn add_alias(line: &str) {
    if let Ok(Alias { name, words }) = parse_alias(line) {
        ALIASES.lock().unwrap().insert(name, words);
    }
}

pub fn lookup_alias(alias: &str) -> Option<Vec<Word>> {
    ALIASES.lock().unwrap().get(&alias.to_string()).cloned()
}

pub fn alias_command(argv: &Vec<String>) -> ExitStatus {
    trace!("alias: {:?}", argv);
    if let Some(alias) = argv.get(1) {
        add_alias(alias);
    }

    0
}
