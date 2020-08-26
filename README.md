nsh
====
![CI Status](https://github.com/nuta/nsh/workflows/CI/badge.svg?branch=master)
[![Latest version](https://img.shields.io/crates/v/nsh.svg)](https://crates.io/crates/nsh)

**Currently nsh is incomplete and not yet stable. Succeeded in crashing nsh? [Let me know](https://github.com/nuta/nsh/issues)!**

A command-line shell that focuses on productivity and swiftness featuring:

- A POSIX compliant interactive shell with some Bash extensions.
- Tab completions and syntax highlighting.
- A builtin interactive fuzzy completion filter.
- Builtin zero configration features.
- Written in Rust :crab:

![screenshot](https://gist.githubusercontent.com/nuta/5747db6c43978d9aa1941ce321cc1741/raw/405b7a1156292fd0456010b657f299b1daa367ff/nsh.png)

Installation
------------
```
$ cargo install nsh
```

Documentation
-------------
**[Documentation](https://seiya.me/nsh)**

Why create a new shell?
-----------------------
Bash is the best for executing shell scripts but its interactive mode is not satisfactory. I am
a zsh user for the last decade but I don't need *customizability* and got tired of making my zshrc
faster. Fish is really neat but I prefer old-fashioned, traditional, and ergonomic shell syntax.

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
