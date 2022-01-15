use std::ops::Range;

#[derive(Clone, Debug, PartialEq)]
pub enum HighlightKind {
    /// A plain text.
    Plain,
    /// A variable substitution, e.g. `$foo`.
    Variable { name: String },
    /// A quoted string (e.g. `"foo"` and `'foo'`).
    QuotedString,
    /// A command substitution symbols, e.g. `$(` and `)` in `$(foo)`.
    CommandSymbol,
    /// An escaped sequence (e.g. `\"`).
    EscSeq,
    /// A command (e.g. "ls" in "ls /var").
    Argv0,
    /// A keyword.
    Keyword,
}

#[derive(Clone, Debug, PartialEq)]
pub struct HighlightSpan {
    pub kind: HighlightKind,
    pub char_range: Range<usize>,
}
