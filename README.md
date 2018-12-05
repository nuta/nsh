nsh
====
[![Build Status](https://travis-ci.com/seiyanuta/nsh.svg?branch=master)](https://travis-ci.com/seiyanuta/nsh)

A command-line shell that focuses on performance and productivity.

**Currently nsh is not yet stable. Succeeded in crashing nsh? [Let me know](https://github.com/seiyanuta/nsh/issues)!**

Goals
------
- A **Bash compatible** interactive shell.
- **Tab compeltions** and **syntax highlighting** like [fish](http://fishshell.com/).
- **Blazing fast startup times** by asynchronous initialization.
- **Zero configration** features out of the box and web-based configuration tool `nsh --config`.
- **Written in Rust** :crab:

![demo animation](https://gist.githubusercontent.com/seiyanuta/6deb34b183f30f45e1d239dba1e07dd8/raw/6db512bfa2be402046a878c32a367c379526d048/demo.gif)

Installation
------------
TODO: Publish to crates.io

Why create a new shell?
------------------------
I got tired of making my zshrc faster.

Comparisons
-----------
| | **nsh**  | **[bash](https://www.gnu.org/software/bash)**  | **[zsh](http://www.zsh.org/)** | **[fish](http://fishshell.com/)** | **[PowerShell](https://github.com/PowerShell/PowerShell)** |
| :-: | :-: | :-: | :-: | :-: | :-: |
| **POSIX shell features** | almost complete | **Yes** | **Yes** | original syntax | No|
| **Bash compatibility** | incomplete | **100% compatible** | **provides `emulate(1)`** | requires Bass | No |
| **Prompt UX** | *(work in progress)* | minimum standard | comfortable | **awesome** | comfortable |
| **Configuration easiness** | **web-based `nsh --config`** | insufficient | oh-my-zsh or very long zshrc | **web-based `fish_config`** | insufficient |
| **Name** | not bad | **noble** | **cool** | **cute** | **super cool** |

Future Plans
------------
- Support bash completions (`complete(1)` and `compgen(1)`).
- Friendly syntax error message like `rustc`.
- Smart fuzzy search in completion.
- Auto correction as [fuck](https://github.com/nvbn/thefuck) does.
- Plugins: Git information in prompt, rbenv, and more.

----

Building
--------
### Prerequisites
- macOS or Linux
- Rust [nightly](https://doc.rust-lang.org/book/2018-edition/appendix-06-nightly-rust.html) toolchain. Google `rustup` if you're unfamiliar with.
- [Node.js](https://nodejs.org/en/)
- [yarn](https://yarnpkg.com/lang/en/docs/install)

```
$ cd src/config/ui && yarn && cd ../../..
$ cargo build --release
$ ./target/release/nsh
```

Testing
-------
```
$ cargo test
$ ./tools/run-blackbox-tests.py
```

Contributing
------------
nsh is in *alpha* stage: there are many missing features which Bash provides, there are kludges in
source code, and there must be bugs. To make nsh practical for daily use, I need your help!

### How can I contribute?
- **Report bugs** in [GitHub issues](https://github.com/seiyanuta/nsh/issues). Please attach
  a minimal reproducible example (e.g. shell script) *if possible*. It helps me to fix the bug easier.
- **Suggest enhancements** in [GitHub issues](https://github.com/seiyanuta/nsh/issues).
- **Submit a Pull Request** which implements a new feature, fixes a bug, refactors code, rephrases sentences in documentation, etc.

License
-------
CC0 or MIT. Choose whichever you prefer.
