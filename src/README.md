nsh
====
A command-line shell that focuses on performance and productivity.

Road Map
--------
- [x] Parser
- [ ] Command Execution
- [ ] Builtins Commands: (`cd`, `set`, `alias`, ...)
- [ ] History
- [ ] TAB completion with fuzzy filter like fzf
- [ ] Completion
- [ ] Prompt theme (`$PS1`)
- [ ] Introduce [bash's blackbox tests](http://git.savannah.gnu.org/cgit/bash.git/tree/tests)
- [ ] Plugins

Building
--------
```
$ cargo build --release
```

License
-------
CC0 or MIT. Choose whichever you prefer.