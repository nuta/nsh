# Customizing the Prompt

## Examples ##
```bash
# seiya@Seiyas-MacBook-Air.local: ~/dev/nsh $ 
PROMPT="\{cyan}\{username}@\{hostname}: \{current_dir}\{reset} \$ "

# ~/dev/nsh $
PROMPT="\{bold}\{green}\{current_dir} \$\{reset} "

# ~/dev/nsh [master] $
PROMPT="\{current_dir}\if{in_repo}{ [\{repo_status}]}{} \$ "
```

## Current context values
|          **Value**            |                **Description**               |
|:-----------------------------:|:--------------------------------------------:|
| `\{username}`                 | User name                                    |
| `\{hostname}`                 | Host name                                    |
| `\{current_dir}`              | The current working directory                |


## Text styles and colors
|          **Value**            |                **Description**               |
|:-----------------------------:|:--------------------------------------------:|
| `\{reset}`                    | Reset text styles and colors                 |
| `\{bold}`                     | Bold                                         |
| `\{underline}`                | Underline                                    |
| `\{red}`                      | Red                                          |
| `\{blue}`                     | Blue                                         |
| `\{green}`                    | Green                                        |
| `\{yellow}`                   | Yellow                                       |
| `\{cyan}`                     | cyan                                         |
| `\{magenta}`                  | magenta                                      |

## Git
|          **Value**            |                **Description**               |
|:-----------------------------:|:--------------------------------------------:|
| `\{repo_status}`              | Git repository status                        |

## Ternary expression
```
\if{cond}{then}{else}
```

### Conditionals
|          **Value**            |                **Description**               |
|:-----------------------------:|:--------------------------------------------:|
| `in_repo`                     | True if the current directory is in a repo.  |

## misc.
|          **Value**            |                **Description**               |
|:-----------------------------:|:--------------------------------------------:|
| `\n`                          | Newline                                      |
