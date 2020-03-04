# Config File (.nshrc)
`.nshrc` is equivalent to `.bashrc` for bash or `.zshrc` for zsh. It is a
shell script executed when you launch nsh.

Nsh loads it from `$XDG_CONFIG_HOME/nsh/nshrc` or `~/.nshrc`.

Like other shells, you can configure nsh as you like. That said, nsh is 
opinionated compared to others: it does not provide flexible way so all
what you can configure are limited:

- `$PATH`
- `$PROMPT` (see [Customizing the Prompt](prompt.md))
- aliases
