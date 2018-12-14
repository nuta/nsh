//! A yet another shell script parser. In contrast to [`parser::parse`]
//! this returns an ASA, Abstracted Syntax Array which makes it easy to
//! implement context-aware stuffs like completion and in the future,
//! syntax highlighting .
use std::sync::Arc;

#[derive(Default, Debug)]
pub struct Asa {
    pub words: Vec<Arc<String>>,
    /// The index of the current word in `words`.
    pub current_word_index: isize,
    /// The whole line.
    pub line: String,
    /// The offset of the beginning of the current word in `line`.
    pub current_word_offset: usize,
    pub current_word_len: usize,
    pub user_cursor: usize,
}

impl Asa {
    pub fn current_word(&self) -> Option<Arc<String>> {
        self.words
            .get(self.current_word_index as usize)
            .cloned()
    }
}

fn is_valid_word_char(ch: char) -> bool {
    match ch {
        ' ' | '\t' | '&' | '|' | ';' | '(' | ')' | '`' | '\n' | '\\' | '$' | '*' | '?' | '"' | '\''=> false,
        _ => true,
     }
 }


/// Parses the user input and returns a Abstracted Syntax Array.
pub fn parse(user_input: &str, user_cursor: usize) -> Asa {
    let line = user_input.to_string();

    // A Poor man's command line parser.
    // TODO: Support single-quoted string.
    // TODO: Skip envinroment variable assignments like: `RAILS_env=test rspec`
    // TODO: Use `parser::parse` instead.
    //
    // Example:
    //     echo $(read-file -avh --from-stdin --other-opt < hello.bin) | hexdump -C
    //                                 ^ cursor is here

    // Before the cursor:
    //     words = ['read-file", "-avh"]
    //     word = "--from-"
    let mut words = Vec::new();
    let mut word = String::new();
    let mut in_string = false;
    let mut prev_ch = '\x00';
    let mut current_word_offset = 0;
    for (offset, ch) in line.chars().take(user_cursor).enumerate() {
        match (in_string, prev_ch, ch) {
            (true, '\\', '"') => {
                word = word.trim_matches('\\').to_owned();
                word.push('"');
            }
            (true, _, '"') => in_string = false,
            (false, _, '"') => in_string = true,
            (false, _, ' ') => {
                words.push(Arc::new(word));
                word = String::new();
                current_word_offset = offset + 1;
            }
            (false, _, ch) if !is_valid_word_char(ch) && ch != '*' && ch != '?' => {
                words = Vec::new();
                word = String::new();
                current_word_offset = offset + 1;
            }
            (_, _, ch) => word.push(ch),
        }

        prev_ch = ch;
    }

    // Case #1:
    //   $ ls foo
    //            ^ user_cursor is here (the end of line)
    let mut current_word_index = words.len() as isize;
    let mut current_word_len = 0;

    // After the cursor:
    //     words = ['read-file", "-avh", "--form-stdin"]
    //     word = ""
    for ch in line.chars().skip(user_cursor) {
        match (in_string, prev_ch, ch) {
            (true, '\\', '"') => {
                word = word.trim_matches('\\').to_owned();
                word.push('"');
            }
            (true, _, '"') => in_string = false,
            (false, _, '"') => in_string = true,
            (false, _, ch) if !is_valid_word_char(ch) && ch != '*' && ch != '?' => break,
            (_, _, ch) => word.push(ch),
        }
    }

    // Case #2:
    //   $ ls foo
    //         ^ user_cursor is here (within a word)
    if !word.is_empty() {
        let word_len = word.len();
        words.push(Arc::new(word));
        current_word_index = words.len() as isize - 1;
        current_word_len = word_len;
    }

    trace!(
        "words={:?}, index={}, offset={}, len={}",
        words,
        current_word_index,
        current_word_offset,
        current_word_len
    );

    Asa {
        words,
        current_word_index,
        line,
        current_word_offset,
        current_word_len,
        user_cursor,
    }
}
