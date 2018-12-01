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
- **Zero configration** features out of the box and web-based configuration tool `nshconfig`.
- **Written in Rust** :crab:

TODO: add a GIF video

Installation
------------
TODO: Publish to crates.io

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
```
$ cargo build --release
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
  a minimal reproducible example (e.g. a valid shell script which causes a parser error)
  *if possible*. It helps me to fix the bug easier.
- **Suggest enhancements** in [GitHub issues](https://github.com/seiyanuta/nsh/issues).
- **Submit a Pull Request** which implements a new feature, fixes a bug, rephrases sentences in documentation, etc.

License
-------
CC0 or MIT. Choose whichever you prefer.
