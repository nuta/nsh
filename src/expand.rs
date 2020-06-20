use crate::eval::*;
use crate::parser::{ExpansionOp, ProcSubstType, Span, Word};
use crate::pattern::{LiteralOrGlob, PatternWord};
use crate::shell::Shell;
use crate::variable::Value;
use failure::Error;
use std::fs::File;
use std::io::prelude::*;
use std::os::unix::io::FromRawFd;

type Result<I> = std::result::Result<I, Error>;

/// TODO: Aliases should be expanded in the parser in order to support
/// compound lists, e.g. alias cowsay-or-echo="cowsay hi || echo hi".
///
/// That said, I believe this feature is not widely-used feature.
pub fn expand_alias(shell: &Shell, argv: &[Word]) -> Vec<Word> {
    argv
        // Get the first word.
        .get(0)
        // Get the first span in the first word.
        .and_then(|word| word.spans().get(0))
        // Make sure that the span is a literal (not parameters, etc.).
        .and_then(|span| match span {
            Span::Literal(lit) => Some(lit),
            _ => None,
        })
        // The very first span is literal. Search the registered aliases.
        .and_then(|lit| shell.lookup_alias(lit.as_str()))
        .map(|alias_str| {
            // Found the alias. Split the alias string by whitespace into words.
            let mut alias_words: Vec<Word> = alias_str
                .trim()
                .split(' ')
                .map(|w| {
                    let span = Span::Literal(w.to_owned());
                    Word(vec![span])
                })
                .collect();

            // Append argv except the first word (alias name).
            for arg in argv.iter().skip(1) {
                alias_words.push(arg.clone());
            }

            alias_words
        })
        // Failed to expand alias. Return argv as it is.
        .unwrap_or_else(|| argv.to_owned())
}

/// Expands a parameter (`$foo` in e.g. `echo $foo`). It returns `Vec` since
/// `op` can be array expansion. `None` value represents *null*.
pub fn expand_param(
    shell: &mut Shell,
    name: &str,
    op: &ExpansionOp,
) -> Result<Vec<Option<String>>> {
    match name {
        "?" => {
            return Ok(vec![Some(shell.last_status().to_string())]);
        }
        "!" => {
            let pgid = match shell.last_back_job() {
                Some(job) => job.pgid.to_string(),
                None => 0.to_string(),
            };

            return Ok(vec![Some(pgid)]);
        }
        "0" => {
            return Ok(vec![Some(shell.script_name.clone())]);
        }
        "$" => {
            return Ok(vec![Some(shell.shell_pgid.to_string())]);
        }
        "#" => {
            return Ok(vec![Some(shell.current_frame().num_args().to_string())]);
        }
        "*" => {
            let args = shell.current_frame().get_string_args();
            let expanded = args.join(" ");
            return Ok(vec![Some(expanded)]);
        }
        "@" => {
            let args = shell.current_frame().get_string_args();
            return Ok(args.iter().map(|a| Some(a.to_owned())).collect());
        }
        _ => {
            if let Some(var) = shell.get(name) {
                // $<name> is defined.
                let value = var.value();
                match (op, value) {
                    (ExpansionOp::Length, Some(_)) => {
                        return Ok(vec![Some(var.as_str().len().to_string())]);
                    }
                    (ExpansionOp::Length, None) => {
                        return Ok(vec![Some(0.to_string())]);
                    }
                    (ExpansionOp::GetNullableOrDefaultAndAssign(_), None) => {
                        return Ok(vec![None]);
                    }
                    (ExpansionOp::GetNullableOrDefault(_), None) => {
                        return Ok(vec![None]);
                    }
                    (
                        ExpansionOp::Subst {
                            pattern,
                            replacement,
                            replace_all,
                        },
                        Some(_),
                    ) => {
                        let content = var.as_str().to_string();
                        let replaced =
                            replace_pattern(shell, pattern, &content,
                                replacement, *replace_all)?;
                        return Ok(vec![Some(replaced)]);
                    }
                    (_, _) => {
                        return Ok(vec![Some(var.as_str().to_string())]);
                    }
                };
            }
        }
    }

    // The variable is not defined or is nulll
    // http://pubs.opengroup.org/onlinepubs/009695399/utilities/xcu_chap02.html#tag_02_06_02
    match op {
        ExpansionOp::Length => {
            if shell.nounset {
                print_err!("undefined variable `{}'", name);
                std::process::exit(1);
            }

            Ok(vec![Some("0".to_owned())])
        }
        ExpansionOp::GetOrEmpty => {
            if shell.nounset {
                print_err!("undefined variable `{}'", name);
                std::process::exit(1);
            }

            Ok(vec![Some("".to_owned())])
        }
        ExpansionOp::GetOrDefault(word) | ExpansionOp::GetNullableOrDefault(word) => {
            expand_word_into_string(shell, word).map(|s| vec![Some(s)])
        }
        ExpansionOp::GetOrDefaultAndAssign(word)
        | ExpansionOp::GetNullableOrDefaultAndAssign(word) => {
            let content = expand_word_into_string(shell, word)?;
            shell.set(name, Value::String(content.clone()), false);
            Ok(vec![Some(content)])
        }
        ExpansionOp::Subst { .. } => Ok(vec![Some("".to_owned())]),
    }
}

/// Expands a word int a `Vec`.
pub fn expand_word_into_vec(shell: &mut Shell, word: &Word, ifs: &str) -> Result<Vec<PatternWord>> {
    let mut words = Vec::new();
    let mut current_word = Vec::new();
    for span in word.spans() {
        let (frags, expand) = match span {
            Span::LiteralChars(..) => {
                // Internally used by the parser.
                unreachable!()
            }
            Span::Literal(s) => (vec![LiteralOrGlob::Literal(s.clone())], false),
            Span::Parameter { name, op, quoted } => {
                let mut frags = Vec::new();
                for value in expand_param(shell, name, op)? {
                    let frag = value.unwrap_or_else(|| "".to_owned());
                    frags.push(LiteralOrGlob::Literal(frag));
                }
                (frags, !quoted)
            }
            Span::ArrayParameter {
                name,
                index,
                quoted,
            } => {
                let index = evaluate_expr(shell, index);
                if index < 0 {
                    warn!(
                        "the index must be larger than or equals 0: var={}, index={}",
                        name, index
                    );
                    (vec![], !quoted)
                } else {
                    let frag = shell
                        .get(name)
                        .map(|v| v.value_at(index as usize).to_string())
                        .unwrap_or_else(|| "".to_owned());
                    (vec![LiteralOrGlob::Literal(frag)], !quoted)
                }
            }
            Span::ArithExpr { expr } => {
                let result = evaluate_expr(shell, expr).to_string();
                (vec![LiteralOrGlob::Literal(result)], false)
            }
            Span::Tilde(user) => {
                if user.is_some() {
                    print_err!("warning: ~user is not yet supported");
                }

                let dir = dirs::home_dir().unwrap().to_str().unwrap().to_owned();
                (vec![LiteralOrGlob::Literal(dir)], false)
            }
            Span::Command { body, quoted } => {
                let (_, stdout) = eval_in_subshell(shell, body)?;

                let mut raw_stdout = Vec::new();
                unsafe { File::from_raw_fd(stdout).read_to_end(&mut raw_stdout).ok() };

                let output = std::str::from_utf8(&raw_stdout)
                    .map_err(|err| {
                        // TODO: support binary output
                        print_err!("binary in variable/expansion is not supported");
                        err
                    })?
                    .trim_end_matches('\n')
                    .to_owned();

                (vec![LiteralOrGlob::Literal(output)], !quoted)
            }
            Span::ProcSubst { body, subst_type } => {
                let (_, stdout) = eval_in_subshell(shell, body)?;
                match subst_type {
                    // <()
                    ProcSubstType::StdoutToFile => {
                        let file_name = format!("/dev/fd/{}", stdout);
                        (vec![LiteralOrGlob::Literal(file_name)], false)
                    }
                    // >()
                    ProcSubstType::FileToStdin => {
                        // TODO:
                        unimplemented!();
                    }
                }
            }
            Span::AnyChar { quoted } if !*quoted => (vec![LiteralOrGlob::AnyChar], false),
            Span::AnyString { quoted } if !*quoted => (vec![LiteralOrGlob::AnyString], false),
            Span::AnyChar { .. } => (vec![LiteralOrGlob::Literal("?".into())], false),
            Span::AnyString { .. } => (vec![LiteralOrGlob::Literal("*".into())], false),
        };

        // Expand `a${foo}b` into words: `a1` `2` `3b`, where `$foo="1 2 3"`.
        let frags_len = frags.len();
        for frag in frags {
            match frag {
                LiteralOrGlob::Literal(ref lit) if expand => {
                    if !current_word.is_empty() {
                        words.push(PatternWord::new(current_word));
                        current_word = Vec::new();
                    }

                    for word in lit.split(|c| ifs.contains(c)) {
                        words.push(PatternWord::new(vec![LiteralOrGlob::Literal(word.into())]));
                    }
                }
                frag => {
                    current_word.push(frag);
                }
            }

            if frags_len > 1 && !current_word.is_empty() {
                words.push(PatternWord::new(current_word));
                current_word = Vec::new();
            }
        }
    }

    if !current_word.is_empty() {
        words.push(PatternWord::new(current_word));
    }

    trace!("expand_word: word={:?}, to={:?}", word, words);
    if words.is_empty() {
        Ok(vec![PatternWord::new(vec![LiteralOrGlob::Literal(
            "".into(),
        )])])
    } else {
        Ok(words)
    }
}

/// Expands a word into a string. Words in a command span `"$(echo foo bar)"` are
/// joined by a whitespace.
pub fn expand_word_into_string(shell: &mut Shell, word: &Word) -> Result<String> {
    let ws: Vec<String> = expand_word_into_vec(shell, word, &shell.ifs())?
        .into_iter()
        .map(|w| w.into_string())
        .collect();

    Ok(ws.join(""))
}

/// Expands words into a `Vec<String>`. A pattern in a word are expanded as a
/// file path globbing.
pub fn expand_words(shell: &mut Shell, words: &[Word]) -> Result<Vec<String>> {
    let mut evaluated = Vec::new();
    for word in words {
        let mut ws = Vec::new();
        for w in expand_word_into_vec(shell, word, &shell.ifs())? {
            for f in w.expand_glob()? {
                ws.push(f);
            }
        }

        evaluated.extend(ws);
    }

    Ok(evaluated)
}

/// Expands and merges all pattern words into a single pattern word.
pub fn expand_into_single_pattern_word(
    shell: &mut Shell,
    pattern: &Word
) -> Result<PatternWord> {
    let mut frags = Vec::new();
    let ifs = ""; /* all whitespaces are treated as a literal */
    for word in expand_word_into_vec(shell, pattern, ifs)? {
        for frag in word.fragments() {
            frags.push(frag.clone());
        }
    }

    Ok(PatternWord::new(frags))
}

pub fn replace_pattern(
    shell: &mut Shell,
    pattern: &Word,
    text: &str,
    replacement: &Word,
    replace_all: bool,
) -> Result<String> {
    let pat = expand_into_single_pattern_word(shell, pattern)?;
    let dst = expand_word_into_string(shell, replacement)?;
    Ok(crate::pattern::replace_pattern(
        &pat,
        text,
        &dst,
        replace_all,
    ))
}
