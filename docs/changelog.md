# Change Log

## v0.3.5 (Aug 26, 2020)
- Minor improvements and bug fixes.

## v0.3.4 (June 14, 2020)
- `export`: Accept multiple assignments
- Scan only the current directory in path completion.
- Support tilde expansion in a assignment-like word (e.g. `--prefix=~/usr`).

## v0.3.3 (Mar 7, 2020)
- Use failure 0.1.7 to fix a build error ([#6](https://github.com/nuta/nsh/issues/6)).

## v0.3.2 (Mar 5, 2020)
- Fixes the workaround introduced in v0.3.1.

## v0.3.1 (Mar 5, 2020)
- Fix a compile error ([#6](https://github.com/nuta/nsh/issues/6)).

## v0.3.0 (Feb 28, 2020)
- Rewrote UI
- Add completion support using (external) Bash
  - Currently, it automatically loads completions in `/etc/bash_completion.d`, etc.
- Remove builtin completion system (`compgen` and `complete`)

## v0.2.2 (Feb 1, 2020)
- Support ^N and ^P key bindings in the prompt (by **[@agatan](https://github.com/agatan)** in **[#2](https://github.com/nuta/nsh/pull/2)**).
- Support ^N, ^P, ^B, and ^F key bindings in the history search (by **[@agatan](https://github.com/agatan)** in **[#4](https://github.com/nuta/nsh/pull/4)**).
- Load `$XDG_CONFIG_HOME/nsh/nshrc` in addition to `~/.nshrc` (suggested by **[@rapha8l](https://github.com/rapha8l)** in **[#5](https://github.com/nuta/nsh/issues/5)**).
- Support --version and --norc option.
- Fixed some bugs.

## v0.2.1
-  Don't panic even if ~/.nshrc does not exist.

## v0.2.0
- Removed experimental support for bash-completion.
  - It's too buggy. We should implement bash features used by bash-completion
    instead of depending on an external Bash.
- Fixed some bugs.

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
