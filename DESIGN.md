# nsh rewrite: architecture discussion draft

## Goal

Build a new Unix shell in Rust. Target the POSIX.1-2024 Shell Command Language and add a deliberate subset of Bash extensions. The existing implementation is not a feature specification. A small first milestone will be a subset of POSIX; compatibility is a later acceptance criterion.

Use one Rust package with internal modules. Plan on **`nix` as the only external crate**. Keep language parsing, expansion, globbing, line editing, and signal policy in this codebase. `nix` supplies Unix syscall wrappers; the standard library supplies ordinary collections, I/O, and tests. We can revisit this only for a demonstrated gap.

## Execution flow

```text
script / -c / interactive input
          |
 source stack (original input + alias replacement text)
          |
 pull lexer <----> parser (grammar context + alias lookup)
          |                |
          |        complete command AST
          |                |
          +---------- evaluator
                       |  |
                  expansion  shell state
                       \  /
                  builtins + Unix process/fd layer
```

The parser asks the lexer for one token at a time. Do **not** tokenize an entire script before parsing. The lexer recognizes operators and words while retaining quotes and nested substitution syntax. The parser classifies a word by position: reserved word, assignment word, ordinary word, or possible command name. Only then can it decide whether an unquoted word is eligible for alias substitution. If so, push the alias value as a new input source and lex it as shell syntax. Alias text can introduce operators, redirections, or compound syntax; it is not an argument list and cannot be substituted in a finished AST. [POSIX alias rules](https://pubs.opengroup.org/onlinepubs/9799919799/utilities/V3_chap02.html#tag_18_03_01).

Parse one **complete command**, execute it, then continue reading. This makes alias definitions visible at the POSIX-required boundary while avoiding premature effects within a command already being parsed. Keep an active-alias stack to stop recursive expansion. If replacement text ends in an unquoted blank, also check the following token for an alias even when it would normally be an argument. Bash documents read-time behavior for commands on the same line and normally disables aliases in non-interactive mode; our default should follow POSIX, with any Bash-like mode specified separately. [POSIX alias timing](https://pubs.opengroup.org/onlinepubs/9799919799/utilities/V3_chap02.html#tag_18_03_01), [Bash alias behavior](https://www.gnu.org/software/bash/manual/html_node/Aliases.html).

Words are structured syntax, not strings ready for execution. Preserve literal spans, quote modes, parameter expansions, command substitutions, and arithmetic expansions. Nested `$(...)` invokes the same parser recursively in a nested grammar context. The parser also queues here-document delimiters and reads their bodies after the appropriate newline, in source order. No variable or command expansion occurs merely to recognize a normal token. [POSIX token recognition and here-documents](https://pubs.opengroup.org/onlinepubs/9799919799/utilities/V3_chap02.html).

Immediately before execution, expand each word using a context such as `Argument`, `AssignmentValue`, `RedirectionOperand`, or `HereDocument`. POSIX order is tilde/parameter/command/arithmetic expansion, field splitting, pathname expansion, then quote removal; not every step applies in every context. The parser must not perform these expansions. [POSIX word expansions](https://pubs.opengroup.org/onlinepubs/9799919799/utilities/V3_chap02.html#tag_18_06).

## Internal boundaries and coding patterns

| Component | Responsibility | Boundary rule |
| --- | --- | --- |
| `input.rs` | Input frames, locations, alias origin, incremental reads | Preserve original input and expansion provenance for errors. |
| `tokenizer.rs` | Pull-based token recognition, quote and substitution nesting | Return raw syntax; ask parser for context where required. |
| `parser.rs` | Grammar, reserved/assignment classification, alias eligibility, here-document queue | Return `Complete(ast)`, `NeedMoreInput`, or syntax error. |
| `ast.rs` | Small owned syntax types with spans and structured words | No execution state or open file descriptors. |
| `expand.rs` | Context-specific word expansion and pattern matching | Carry quote/field metadata until final quote removal. |
| `eval.rs` | Control flow, command lookup, command status, state scope | Execute complete commands in order; no global mutable shell. |
| `shell.rs`, `variable.rs` | Shell state and byte-valued variables | Explicitly pass state to evaluator and expansion. |
| `process.rs` | Pipes, descriptors, process creation, waits, signals, process groups, terminal control | Hide `nix` types and unsafe edges from language modules. |
| `main.rs`, later `mainloop.rs` | CLI input and, later, terminal editing | Scripts use the same parser and evaluator as the REPL. |

Prefer enums for semantic distinctions over booleans: `ExpansionContext`, `CommandOutcome` (`Status`, `Return`, `Break`, `Continue`, `Exit`), and `ParseOutcome`. Keep Rust infrastructure errors distinct from shell exit status and shell control flow. Use RAII for owned file descriptors and scoped restoration when a parent-run builtin redirects its own descriptors. Keep the process-owning core single-threaded; the child side of `fork` should perform only the minimum descriptor setup and `exec`/exit path.

Represent Unix variable values and pathnames as bytes or `OsString`, since POSIX parameters may contain arbitrary non-NUL bytes. Source locations should use byte offsets. Keep character- and locale-sensitive operations behind explicit interfaces; this choice does not itself solve every locale rule. We should decide the supported input encodings before freezing the lexer API.

## Planned files and naming

Follow the existing layout: flat, snake_case modules under `src/`; `src/builtins/mod.rs` plus one snake_case file per builtin; no new workspace crates or nested module tree. These are planned replacement files in the rewrite, **not** files to create now. An existing path means its old contents would be replaced, not reused as a design constraint.

**First implementation slice:**

| Path | Purpose |
| --- | --- |
| `Cargo.toml` | Reset package metadata and dependencies to the agreed baseline. |
| `src/main.rs` | CLI argument handling and selection of `-c`, file, or standard input. |
| `src/input.rs` | Incremental input and stack of original/alias sources. New path. |
| `src/tokenizer.rs` | Pull tokenizer. New path. |
| `src/parser.rs` | Context-sensitive grammar, alias substitution, and here-document scheduling. |
| `src/ast.rs` | Syntax nodes, word parts, and source spans. New path. |
| `src/shell.rs` | Explicit shell state and top-level read/parse/evaluate loop. |
| `src/variable.rs` | Variable representation, attributes, and lookup. |
| `src/expand.rs` | Word expansion by context. |
| `src/eval.rs` | Command evaluation and control flow. |
| `src/process.rs` | The `nix` boundary for processes and file descriptors. |
| `src/builtins/mod.rs` | Builtin lookup, categories, and dispatch. |
| `src/builtins/alias.rs`, `cd.rs`, `exit.rs`, `export.rs`, `unset.rs` | Initial state-changing builtins, each in its own existing-style file. |

**As their milestones arrive:** add `src/pattern.rs` for shell patterns and pathname expansion; individual `src/builtins/*.rs` files for each remaining builtin; `src/mainloop.rs` for the interactive loop; and `src/history.rs`, `src/prompt.rs`, and `src/highlight.rs` only when those features are actually designed. Reuse the existing `test/*.sh` and expected `.stdout`/`.stderr` naming convention for black-box compatibility cases, with focused Rust unit tests beside their modules.

Do not create parallel names for the same responsibility (`lexer.rs` beside `tokenizer.rs`, `state.rs` beside `shell.rs`, or `unix.rs` beside `process.rs`). Split a file only when a concrete module boundary makes it easier to understand.

## Minimal build order

1. **Read/parse loop:** incremental input, complete-command boundary, structured words, source spans, aliases, and syntax errors. Test alias definitions separated by newlines and semicolons, recursive aliases, trailing blanks, aliases producing operators, nested substitutions, and here-document collection.
2. **Non-interactive execution subset:** `-c` and files, simple commands, assignments, quoting, core parameter expansion, redirections, external commands, and state-changing builtins. This is a useful shell subset, not yet POSIX compatible.
3. **POSIX completion:** compound commands, functions, complete expansions, command/arithmetic substitution, traps, shell options, special builtin semantics, and interactive job control. Use the standard as the normative behavior and compare black-box scripts with a system POSIX shell.
4. **Bash extensions and ergonomics:** select individual extensions and test them against Bash; add history, editing, completion, and highlighting after the language core works.

## Decisions to settle before coding

1. **Dialect promise:** POSIX by default with selected Bash extensions always enabled, or a POSIX mode plus a separate extended mode? This affects reserved words and parsing.
2. **Input encoding:** preserve arbitrary bytes in scripts as well as in values, or require UTF-8 source text while preserving byte-valued variables and pathnames?
3. **First platforms:** Linux and macOS, or one first? Terminal and process tests depend on this.
4. **First Bash extensions:** which of `[[ ... ]]`, `(( ... ))`, arrays, `=~`, process substitution, `pipefail`, `source`, and `&>` matter enough to shape syntax or state now?
5. **Coding style:** favor a hand-written recursive-descent parser with a pull lexer (my recommendation) or a parser generator written in-tree? The former gives direct control over context-dependent aliasing and incremental input.

## References

- [POSIX.1-2024 Shell Command Language](https://pubs.opengroup.org/onlinepubs/9799919799/utilities/V3_chap02.html): token recognition, alias substitution, grammar, expansion, and execution environment.
- [Bash Reference Manual: Aliases](https://www.gnu.org/software/bash/manual/html_node/Aliases.html): Bash-specific timing and non-interactive behavior.
- [`nix` crate documentation, version 0.31.3](https://docs.rs/nix/0.31.3/nix/): current syscall-wrapper candidate checked 2026-09-26; no dependency has been changed yet.
