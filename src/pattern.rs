use glob::glob;
use failure::Error;

#[derive(Debug, Fail)]
#[fail(display = "no matches")]
pub struct NoMatchesError;

type Result<I> = std::result::Result<I, Error>;

#[derive(Debug, Clone)]
pub enum LiteralOrGlob {
    Literal(String),
    AnyString,
    AnyChar,
}

/// A word which includes patterns. We don't expand words
/// into the `Vec<String>` directly since the patterns has
/// two different meanings: path glob and match in `case`.
#[derive(Debug)]
pub struct PatternWord {
    fragments: Vec<LiteralOrGlob>
}

impl PatternWord {
    pub fn new(fragments: Vec<LiteralOrGlob>) -> PatternWord {
        PatternWord {
            fragments
        }
    }

    pub fn fragments(&self) -> &[LiteralOrGlob] {
        &self.fragments
    }

    /// Returns a string. Pattern characters such as `*` are treated as a literal.
    pub fn into_string(self) -> String {
        let mut string = String::new();
        for frag in self.fragments {
            match frag {
                LiteralOrGlob::Literal(lit) => string += &lit,
                LiteralOrGlob::AnyChar => string.push('?'),
                LiteralOrGlob::AnyString => string.push('*'),
            }
        }

        string
    }

    //// Expand patterns as a file path globbing.
    pub fn expand_glob(self) -> Result<Vec<String>> {
        let includes_glob = self.fragments.iter().any(|frag| {
            match frag {
                LiteralOrGlob::AnyString => true,
                LiteralOrGlob::AnyChar => true,
                _ => false,
            }
        });

        let mut expanded_words = Vec::new();
        if includes_glob {
            let mut pattern = String::new();
            for frag in self.fragments {
                match frag {
                    LiteralOrGlob::Literal(lit) => {
                        pattern += lit
                            .replace("*", "[*]")
                            .replace("?", "[?]")
                            .as_str();
                    },
                    LiteralOrGlob::AnyChar => {
                        pattern.push('?');
                    },
                    LiteralOrGlob::AnyString => {
                        pattern.push('*');
                    },
                }
            }

            let mut paths = Vec::new();
            for entry in glob(&pattern).expect("failed to glob") {
                match entry {
                    Ok(path) => {
                        paths.push(path.to_str().unwrap().to_string());
                    },
                    Err(e) => error!("glob error: {:?}", e),
                }
            }
            if paths.is_empty() {
                return Err(Error::from(NoMatchesError));
            }

            expanded_words.extend(paths);
        } else {
            let mut s = String::new();
            for frag in self.fragments {
                if let LiteralOrGlob::Literal(lit) = frag {
                    s += &lit;
                }
            }

            expanded_words.push(s);
        }

        Ok(expanded_words)
    }
}

#[derive(Debug, PartialEq)]
pub enum RegexSpan {
    Literal(char),
    /// Zero or arbitrary-length any characters. `*`.
    AnyString,
    /// `?`. Note that in the shell world, `?` means an any character; it
    /// consumes exactly one character. Not optional.
    AnyChar,
}

fn slice_or_empty<T>(slice: &[T], start: usize) -> &[T] {
    if slice.len() < start {
        &[]
    } else {
        &slice[start..]
    }
}

fn str_slice_or_empty(slice: &str, start: usize) -> &str {
    if slice.len() < start {
        ""
    } else {
        &slice[start..]
    }
}

fn match_one(pat: &RegexSpan, ch: char) -> bool {
    trace!("regex: one: pattern = {:?}, ch={:?}", pat, ch);

    match pat {
        RegexSpan::Literal(span_ch) => *span_ch == ch,
        RegexSpan::AnyChar => true,
        _ => false,
    }
}

#[derive(Debug, PartialEq)]
pub struct MatchResult {
    start: usize,
    end: usize,
}

/// A regex engine based on @nadrane's work: https://nickdrane.com/build-your-own-regex/
/// Returns the index of the matched part or None.
fn regex_match(pattern: &[RegexSpan], text: &str, index: usize) -> Option<usize> {
    trace!("regex: match: pattern = {:?}, text='{}', index = {}", pattern, text, index);

    match pattern.get(0) {
        Some(RegexSpan::AnyChar) | Some(RegexSpan::Literal(_)) => {
            if text.is_empty() {
                return None;
            }

            if !match_one(&pattern[0], text.chars().next().unwrap()) {
                return None;
            }

            regex_match(
                slice_or_empty(pattern, 1),
                str_slice_or_empty(text, 1),
                index + 1
            )
        }
        Some(RegexSpan::AnyString) => {
            if text.is_empty() {
                if pattern.len() > 1 {
                    // There're some remaining regex spans after this AnyString.
                    return regex_match(slice_or_empty(pattern, 1), text, index);
                } else {
                    // We've consumed all regex spans.
                    return Some(index);
                }
            }

            // A. consume a character by the wildcard.
            if let Some(index) = regex_match(pattern, str_slice_or_empty(text, 1), index + 1) {
                return Some(index);
            }

            // B. skip the wildcard.
            regex_match(slice_or_empty(pattern, 1), text, index)
        }
        None => {
            // The `pattern` is empty.
            Some(index)
        }
    }
}

fn pattern_word_match(pattern: &PatternWord, text: &str) -> Option<MatchResult> {
    trace!("pattern_word_match: text = '{}'", text);
    let mut spans = Vec::new();
    for frag in &pattern.fragments {
        match frag {
            LiteralOrGlob::AnyChar => {
                spans.push(RegexSpan::AnyChar);
            }
            LiteralOrGlob::AnyString => {
                spans.push(RegexSpan::AnyString);
            }
            LiteralOrGlob::Literal(s) => {
                for ch in s.chars() {
                    spans.push(RegexSpan::Literal(ch));
                }
            }
        }
    }

    for start in 0..text.len() {
        trace!("regex: from = {}", start);
        if let Some(end) = regex_match(&spans, &text[start..], start) {
            return Some(MatchResult { start, end: end.saturating_sub(1) });
        }
    }

    None
}

pub fn match_pattern(pattern: &PatternWord, text: &str) -> bool {
    pattern_word_match(pattern, text).is_some()
}

pub fn replace_pattern(
    pattern: &PatternWord,
    text: &str,
    replacement: &str,
    replace_all: bool
) -> String
{
    let mut remaining = text;
    let mut text = String::new();
    loop {
        if let Some(m) = pattern_word_match(pattern, remaining) {
            text += &remaining[..m.start];
            text += replacement;

            if remaining.len() < m.end + 1 {
                // Reached to the end of text.
                remaining = "";
                break;
            }

            remaining = &remaining[(m.end + 1)..];
        } else {
            // No matches.
            break;
        }

        if !replace_all {
            break;
        }
    }

    text += remaining;
    text
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn literal_only() {
        let pat = PatternWord{
            fragments: vec![
                LiteralOrGlob::Literal("abc".to_owned())
            ]
        };

        assert_eq!(
            pattern_word_match(&pat, "abc"),
            Some(MatchResult {
                start: 0,
                end: 2,
            })
        );

        assert_eq!(
          pattern_word_match(&pat, "xxabcxx"),
            Some(MatchResult {
                start: 2,
                end: 4,
            })
        );

        assert_eq!(
            pattern_word_match(&pat, ""),
            None,
        );

        assert_eq!(
            pattern_word_match(&pat, "xyz"),
            None
        );

        assert_eq!(
            replace_pattern(&pat, "_abc_abc_abc_", "X", false),
            "_X_abc_abc_".to_owned()
        );

        assert_eq!(
            replace_pattern(&pat, "_abc_abc_abc_", "X", true),
            "_X_X_X_".to_owned()
        );
    }

    #[test]
    fn wildcard() {
        // "?"
        let pat = PatternWord{
            fragments: vec![
                LiteralOrGlob::AnyChar,
            ]
        };

        assert_eq!(
            pattern_word_match(&pat, ""),
            None
        );

        assert_eq!(
            pattern_word_match(&pat, "@"),
            Some(MatchResult {
                start: 0,
                end: 0,
            })
        );

        assert_eq!(
            replace_pattern(&pat, "aaa", "X", false),
            "Xaa".to_owned()
        );

        assert_eq!(
            replace_pattern(&pat, "aaa", "X", true),
            "XXX".to_owned()
        );

        // "*"
        let pat = PatternWord{
            fragments: vec![
                LiteralOrGlob::AnyString,
            ]
        };

        assert_eq!(
            pattern_word_match(&pat, ""),
            None,
        );

        assert_eq!(
            pattern_word_match(&pat, "x"),
            Some(MatchResult {
                start: 0,
                end: 0,
            })
        );

        assert_eq!(
            pattern_word_match(&pat, "xyz"),
            Some(MatchResult {
                start: 0,
                end: 2,
            })
        );

        // "1?34"
        let pat = PatternWord{
            fragments: vec![
                LiteralOrGlob::Literal("1".to_owned()),
                LiteralOrGlob::AnyChar,
                LiteralOrGlob::Literal("34".to_owned()),
            ]
        };

        assert_eq!(
            pattern_word_match(&pat, "abc1234"),
            Some(MatchResult {
                start: 3,
                end: 6,
            })
        );

        assert_eq!(
            replace_pattern(&pat, "_1A34_1B34_1C34_", "X", false),
            "_X_1B34_1C34_".to_owned()
        );

        assert_eq!(
            replace_pattern(&pat, "_1A34_1B34_1C34_", "X", true),
            "_X_X_X_".to_owned()
        );

        // "1*4"
        let pat = PatternWord{
            fragments: vec![
                LiteralOrGlob::Literal("1".to_owned()),
                LiteralOrGlob::AnyString,
                LiteralOrGlob::Literal("4".to_owned()),
            ]
        };

        assert_eq!(
            pattern_word_match(&pat, "##1234##"),
            Some(MatchResult {
                start: 2,
                end: 5,
            })
        );
    }

    #[test]
    fn complex_pattern() {
        // "1?3*78*9"
        let pat = PatternWord{
            fragments: vec![
                LiteralOrGlob::Literal("1".to_owned()),
                LiteralOrGlob::AnyChar,
                LiteralOrGlob::Literal("3".to_owned()),
                LiteralOrGlob::AnyString,
                LiteralOrGlob::Literal("7".to_owned()),
                LiteralOrGlob::Literal("8".to_owned()),
                LiteralOrGlob::AnyString,
                LiteralOrGlob::Literal("9".to_owned())
            ]
        };

        assert_eq!(
            pattern_word_match(&pat, "__123456789__"),
            Some(MatchResult {
                start: 2,
                end: 10,
            })
        );

        assert_eq!(
            pattern_word_match(&pat, "__12x3456789__"),
            None
        );
    }
}
