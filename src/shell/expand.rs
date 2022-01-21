use anyhow::Result;
use nsh_parser::ast::{BraceExpansion, Span, Tilde, Token, VarExpansion, Word};

use crate::shell::Shell;

impl Shell {
    pub fn expand_word(&mut self, word: &Word) -> Result<Vec<String>> {
        // Example of "$(echo x y){c,h}*" => vec!["a.txt", "bxc.txt", "byc.txt"] ":
        //
        // expanded_spans[0] = vec!["x y"]     // $(echo x y)
        // expanded_spans[1] = vec!["c", "h"]  // {c,h}
        // expanded_spans[2] = vec![AnyString] // *
        //
        // resplitted_spans[0] = vec!["x"]
        // resplitted_spans[1] = vec!["y.c", AnyString]
        // resplitted_spans[2] = vec!["y.h", AnyString]
        //
        // words[0] = vec!["x"]
        // words[1] = vec!["y.cpp"]
        // words[2] = vec!["y.hpp"]
        //
        // Both expanded_spans and flattened_spans contains only Span::Plain,
        // Span::AnyString, Span::AnyChar, or Span::AnyCharIn. The other types
        // will be expanded into a string (Span::Plain).
        let mut expanded_spans = Vec::new();
        for span in word.spans() {
            match span {
                Span::Plain(_) | Span::AnyChar | Span::AnyString | Span::AnyCharIn(_) => {
                    expanded_spans.push(vec![span.clone()]);
                }
                Span::Brace(brace) => {
                    let mut spans = Vec::new();
                    for s in self.expand_brace(brace)? {
                        spans.push(Span::Plain(s));
                    }
                    expanded_spans.push(spans);
                }
                Span::Variable {
                    name,
                    expansion,
                    quoted,
                } => {
                    let s = self.expand_variable(name, expansion, *quoted)?;
                    expanded_spans.push(vec![Span::Plain(s)]);
                }
                Span::Command { tokens, quoted } => {
                    let s = self.expand_command(tokens, *quoted)?;
                    expanded_spans.push(vec![Span::Plain(s)]);
                }
                Span::Tilde(tilde) => {
                    let s = self.expand_tilde(tilde)?;
                    expanded_spans.push(vec![Span::Plain(s)]);
                }
                Span::ProcessReadable(tokens) => {
                    let s = self.expand_process_substitution_readable(tokens)?;
                    expanded_spans.push(vec![Span::Plain(s)]);
                }
                Span::ProcessWritable(tokens) => {
                    let s = self.expand_process_substitution_writable(tokens)?;
                    expanded_spans.push(vec![Span::Plain(s)]);
                }
                Span::Arith(word) => {
                    let s = self.expand_arithmetic_expression(word)?;
                    expanded_spans.push(vec![Span::Plain(s)]);
                }
            }
        }

        // Expand brace expansions and then split into words by $IFS.
        let ifs = " \t\n"; // TODO:
        let resplitted_spans = flatten_and_resplit_spans(&expanded_spans, ifs, &[]);

        // Expand path wildcards.
        let mut words = Vec::new();
        for spans in resplitted_spans {
            let mut word = String::new();
            for span in spans {
                match span {
                    Span::Plain(s) => word.push_str(&s),
                    Span::AnyChar | Span::AnyString | Span::AnyCharIn(_) => {}
                    _ => unreachable!(),
                }
            }
            words.push(word);
        }

        Ok(words)
    }

    /// Appends the result of expanding the variable to `buf`.
    fn expand_variable(&mut self, name: &str, exp: &VarExpansion, quoted: bool) -> Result<String> {
        todo!()
    }

    /// Appends the result of expanding the variable to `buf`.
    fn expand_command(&mut self, tokens: &[Token], quoted: bool) -> Result<String> {
        todo!()
    }

    /// Appends the result of expanding the variable to `buf`.
    fn expand_tilde(&self, tilde: &Tilde) -> Result<String> {
        todo!()
    }

    /// Appends the result of expanding the variable to `buf`.
    fn expand_brace(&self, brace: &BraceExpansion) -> Result<Vec<String>> {
        todo!()
    }

    /// Appends the result of expanding the variable to `buf`.
    fn expand_process_substitution_readable(&mut self, token: &[Token]) -> Result<String> {
        todo!()
    }

    /// Appends the result of expanding the variable to `buf`.
    fn expand_process_substitution_writable(&mut self, token: &[Token]) -> Result<String> {
        todo!()
    }

    /// Appends the result of expanding the variable to `buf`.
    fn expand_arithmetic_expression(&mut self, word: &Word) -> Result<String> {
        todo!()
    }
}

/// spans_vec[0] = vec!["x y"]     // $(echo x y)
/// spans_vec[1] = vec!["c", "h"]  // {c,h}
/// spans_vec[2] = vec![AnyString] // *
///
/// ret[0] = vec!["x"]
/// ret[1] = vec!["y.c", AnyString]
/// ret[2] = vec!["y.h", AnyString]
fn flatten_and_resplit_spans(
    spans_vec: &[Vec<Span>],
    ifs: &str,
    prefix: &[Span],
) -> Vec<Vec<Span>> {
    if spans_vec.is_empty() {
        return vec![];
    }

    let head = &spans_vec[0];
    let rest = &spans_vec[1..];

    let mut results = Vec::new();
    for span in head {
        let mut new_prefix = Vec::new();
        new_prefix.extend_from_slice(prefix);
        new_prefix.push(span.clone());
        results.extend_from_slice(&flatten_and_resplit_spans(rest, ifs, &new_prefix));
    }

    results
}
