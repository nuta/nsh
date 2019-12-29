nsh
====
[![Build Status](https://travis-ci.com/nuta/nsh.svg?branch=master)](https://travis-ci.com/nuta/nsh)
[![Latest version](https://img.shields.io/crates/v/nsh.svg)](https://crates.io/crates/nsh)

**Currently nsh is incomplete and not yet stable. Succeeded in crashing nsh? [Let me know](https://github.com/nuta/nsh/issues)!**

A command-line shell that focuses on performance and productivity featuing:
- A not-yet-completed-but-aims-to-be **Bash compatible** interactive shell.
- **Tab completions** and **syntax highlighting** like **[fish](http://fishshell.com/)**.
- A builtin **interactive fuzzy completion filter** like **[fzf](https://github.com/junegunn/fzf)**.
- Builtin **zero configration** features.
- **Written in Rust** :crab:

![screenshot](https://gist.githubusercontent.com/seiyanuta/5747db6c43978d9aa1941ce321cc1741/raw/405b7a1156292fd0456010b657f299b1daa367ff/nsh.png)

Installation
------------
```
$ cargo install nsh
```

To enable completions, install ([bash-completion](https://github.com/scop/bash-completion)). If you are using macOS,
install newer Bash as well:

```
$ brew install bash bash-completion@2
```

Configuration
-------------
Set the following variables in `~/.nshrc`:

- `$PATH`
- `$PROMPT`: The prompt format. See [prompt.md](https://github.com/nuta/nsh/blob/master/docs/prompt.md) for details.

Key Shortcuts
-------------

|     **Key**     |                 **Action**                 |
|:---------------:|:------------------------------------------:|
| Up              | Select the previous history.               |
| Down            | Select the next history.                   |
| ^C              | Clear the input.                           |
| ^L              | Clear the screen.                          |
| ^W              | Delete the previous word.                  |
| ^K              | Delete from cursor to the end of input.    |
| M-f (Alt+Right) | Move the cursor to the next word.          |
| M-b (Alt+Left)  | Move the cursor to the previous word.      |
| ^A              | Move the cursor to the beginning of input. |
| ^E              | Move the cursor to the beginning of input. |
| TAB             | Enter the completion mode.                 |
| ^R              | Enter the history search mode.             |

----

Why create a new shell?
-----------------------
Bash is the best for executing shell scripts but its interactive mode is not satisfactory. I am
a zsh user for the last decade but I don't need *customizability* and got tired of making my zshrc
faster. Fish is really neat but I prefer old-fashioned, traditional, and ergonomic shell syntax.

Comparisons
-----------
| | **nsh**  | **[bash](https://www.gnu.org/software/bash)**  | **[zsh](http://www.zsh.org/)** | **[fish](http://fishshell.com/)** | **[PowerShell](https://github.com/PowerShell/PowerShell)** |
| :-: | :-: | :-: | :-: | :-: | :-: |
| **POSIX shell features**   | **Yes**              | **Yes**             | **Yes**                      | original syntax             | No             |
| **Bash compatibility**     | partially supported           | **100% compatible** | **provides `emulate(1)`**    | requires Bass               | No             |
| **Prompt UX**              | **(aims to be) awesome**           | minimum standard    | comfortable                  | **awesome**                 | comfortable    |
| **Name**                   | not bad              | **noble**           | **cool**                     | **cute**                    | **super cool** |


----

Building
--------
### Prerequisites
- macOS or Linux
- Rust 1.31.0 or higher

```
$ cargo build --release
$ ./target/release/nsh
```

Testing
-------
```
$ cargo test
$ ./run-tests.py
```

Debugging
---------
The debug log file is located at `~/.nsh.log`. To enable debug log, run nsh with
`RUST_LOG` evironment variable (i.e., `$ RUST_LOG=nsh=trace nsh`).

Contributing
------------
nsh is in *alpha* stage: there are many missing features which Bash provides, there are kludges in
source code, and there must be bugs. To make nsh practical for daily use, I need your help!

### How can I contribute?
- **Report bugs** in [GitHub issues](https://github.com/nuta/nsh/issues). Please attach
  a minimal reproducible example (e.g. shell script) *if possible*. It helps me to fix the bug easier.
- **Suggest enhancements** in [GitHub issues](https://github.com/nuta/nsh/issues).
- **Submit a Pull Request** which implements a new feature, fixes a bug, refactors code, rephrases sentences in documentation, etc.

License
-------
CC0 or MIT. Choose whichever you prefer.
