# Change Log

## v0.1.1
- Reduce the size of the executable file (4MiB -> 2MiB).
- Breaking change: `nsh --config` is now `nsh-config`internal command.
- Breaking change: change the format of `.nsh_history`.
- Breaking change: deprecate `~/.nshconfig`, use `~/.nshrc`.
- Fix some bugs.

## v0.1.0
- Reimplement the context parser
- Roll our own syntax highlighter
- Support C-style for loop: `for (( i=0; i < 10; i++ ))`
- Support substitution: `${var/pattern/replacement}`

## v0.0.4
- Experimental support for `[[ ... ]]`
- Process subtitution
- grouped commands in a sub shell: `( ... )`
- Here document
- `eval(1)`
