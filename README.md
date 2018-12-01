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
TODO

License
-------
CC0 or MIT. Choose whichever you prefer.
