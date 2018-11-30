nsh
====
[![Build Status](https://travis-ci.com/seiyanuta/nsh.svg?branch=master)](https://travis-ci.com/seiyanuta/nsh)

**nsh** is a command-line shell that focuses on performance and productivity.

**Currenly nsh is unstable. Succeeded in crashing nsh? [Let me know](https://github.com/seiyanuta/nsh/issues)!**

## Goals (not yet finished)
----------------------------
- Aims to be a **Bash compatible** interactive shell.
- **Tab compeltions** and **syntax highlighting** like fish.
- **Blazing fast startup times** by asynchronous initialization.
- **Zero configration** features out of the box and web-based configuration by `nshconfig` command.
- **Written in Rust** :crab:.

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
TODO

License
-------
CC0 or MIT. Choose whichever you prefer.
