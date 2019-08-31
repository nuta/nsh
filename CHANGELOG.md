# Change Log

## v0.1.5
No functional changes. Just fixed a build error.

## v0.1.4
- Experimental support for bash-completion.
  - It invokes an external bash *every time* you hit TAB. This is a workaround
    until nsh supports required Bash-specific features.
- Breaking change: `in_git_repo` and `git_branch` prompt tags are renamed to
  `in_repo` and `repo_status` respectively.
- prompt: support multi-byte characters.

## v0.1.3
- Fixed some bugs.
- Updated dependencies.

## v0.1.2
- `export(1)`: Support setting a new value by.
- Rescan `$PATH` when it is updated.
- Fix a bug that $PATH is not updated.
- Fix a bug that causes EIO on macOS by loading a nshrc.

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
