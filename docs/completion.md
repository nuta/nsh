# Completion
To prevent [*writing yet another completion shell script for nsh along with 
ones for bash/zsh/fish*](https://xkcd.com/927/), nsh does not provide built-in
completion mechanism. Instead, it uses *external* Bash for completion jobs.

Completions are enabled by default.

## How it works?
1. You hit the tab key.
2. Nsh sends the user input to a preloaded bash process.
3. Nsh receives completion candidates from the process.

Nsh automatically loads the completion script from the following directories:

- `/usr/local/etc/bash_completion.d`
- `/usr/etc/bash_completion.d`
- `/etc/bash_completion.d`

## Adding completions
Just copy a completion file for bash into `/usr/local/etc/bash_completion.d`.
