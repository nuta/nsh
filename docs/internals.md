# Internals

## Asynchronous initialization
For better user experience it is crucial to render the first prompt as soon as possible.
To achieve blazingly fast first prompt rendering, in constrast to bash, nsh utilizes threads
to initialize the shell environment such as history loading, `$PATH` scanning, and even
rc script execution.

## Completion
Nsh has experimental support for Bash style compeltion system, i.e. `complete(1)` and `compgen(1)`. Unfortunately
[bunch of bash completion scripts](https://github.com/scop/bash-completion) uses Bash features which nsh does not yet
support so you cannot use them for now. I'm going to implement them in Q1 2019. Stay tuned!

## History Format
The history file (`~/.nsh_history`) is a text file where each line is a JSON. We don't simply save the whole history
as a big array since appending to a *large* array would be very slow. Each entry contains the command and its context:
the directory where the command was executed, and the UNIX timestamp when the command was executed. In future I want to
support genious history search mode as **[mcFly](https://github.com/cantino/mcfly)** does.
