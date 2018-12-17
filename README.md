nsh
====
[![Build Status](https://travis-ci.com/seiyanuta/nsh.svg?branch=master)](https://travis-ci.com/seiyanuta/nsh)

**Currently nsh is incomplete and not yet stable. Succeeded in crashing nsh? [Let me know](https://github.com/seiyanuta/nsh/issues)!**

A command-line shell that focuses on performance and productivity featuing:
- A not-yet-completed-but-aims-to-be **Bash compatible** interactive shell.
- **Tab compeltions** and **syntax highlighting** like **[fish](http://fishshell.com/)**.
- **Blazing fast startup times** by asynchronous initialization.
- Builtin **zero configration** features and web-based configuration tool `nsh --config`.
- **Written in Rust** :crab:

![demo animation](https://gist.github.com/seiyanuta/6deb34b183f30f45e1d239dba1e07dd8/raw/61f38e3fb0d83560804c6bab2b14fd5421910782/demo.gif)

Installation
------------
TODO: Publish to crates.io

Configuration
-------------
```
$ nsh --config
```

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
| **POSIX shell features**   | almost complete              | **Yes**             | **Yes**                      | original syntax             | No             |
| **Bash compatibility**     | incomplete                   | **100% compatible** | **provides `emulate(1)`**    | requires Bass               | No             |
| **Prompt UX**              | *work-in-progress*           | minimum standard    | comfortable                  | **awesome**                 | comfortable    |
| **Configuration easiness** | **web-based `nsh --config`** | insufficient        | oh-my-zsh or very long zshrc | **web-based `fish_config`** | insufficient   |
| **Name**                   | not bad                      | **noble**           | **cool**                     | **cute**                    | **super cool** |

Future Plans
------------
- Full **[bash-completions](https://github.com/scop/bash-completion)** support.
- Plugins: rbenv integration, and more.
- Smart fuzzy search in completion.
- Auto correction as **[fuck](https://github.com/nvbn/thefuck)** does.

----

Building
--------
### Prerequisites
- macOS or Linux
- Rust 1.31.0 or higher
- [Node.js](https://nodejs.org/en/)
- [yarn](https://yarnpkg.com/lang/en/docs/install)

```
$ cd src/config/ui && yarn && yarn build && cd ../../..
$ cargo build --release
$ ./target/release/nsh
```

Testing
-------
Use nightly Rust toolchain to build.

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

----

Internals
---------

### Asynchronous initialization
For better user experience it is crucial to render the first prompt as soon as possible.
To achieve blazingly fast first prompt rendering, in constrast to bash, nsh utilizes threads
to initialize the shell environment such as history loading, `$PATH` scanning, and even
rc script execution.

### `~/.nshconfig`
Since we execute the rc script asynchronously we cannot use `$PROMPT` to render the first
prompt. Instead of using environment varibles defined in the rc script we use a declarative
configuration file named `.nshconfig`, a JSON file which contains the prompt format and `$PATH`.

### Debug log
`~/.nsh.log`

### Completion
Nsh has experimental support for Bash style compeltion system, i.e. `complete(1)` and `compgen(1)`. Unfortunately
[bunch of bash completion scripts](https://github.com/scop/bash-completion) uses Bash features which nsh does not yet
support so you cannot use them for now. I'm going to implement them in Q1 2019. Stay tuned!

### History Format
The history file (`~/.nsh_history`) is a text file where each line is a JSON. We don't simply save the whole history
as a big array since appending to a *large* array would be very slow. Each entry contains the command and its context:
the directory where the command was executed, and the UNIX timestamp when the command was executed. In future I want to
support genious history search mode as **[mcFly](https://github.com/cantino/mcfly)** does.
