This is a POSIX-compatible command-line shell in Rust.

See DESIGN.md.

## Coding style

Write in a "simple Rust".

- Prefer simple `for` loops over functional programming.
- Avoid higher-order functions like `and_then`. Write in `match` / `let Some(...) = ... else`.
- Avoid too deep nested levels.
- Add blank lines properly, like after an if block.
- Add a concise 1-lined code comments per logical code block to explain what it do.
- Add a comment when your intention is not obvious from the code.
