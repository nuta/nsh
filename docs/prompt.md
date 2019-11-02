# Prompt Format `($PROMPT)`

## Current context values
|          **Value**            |                **Description**               |
|:-----------------------------:|:--------------------------------------------:|
| \\{username}                  | User name                                    |
| \\{hostname}                  | Host name                                    |
| \\{current_dir}               | The current working directory                |


## Text styles and colors
|          **Value**            |                **Description**               |
|:-----------------------------:|:--------------------------------------------:|
| \\{reset}                     | Reset text styles and colors                 |
| \\{bold}                      | Bold                                         |
| \\{underline}                 | Underline                                    |
| \\{red}                       | Red                                          |
| \\{blue}                      | Blue                                         |
| \\{green}                     | Green                                        |
| \\{yellow}                    | Yellow                                       |
| \\{cyan}                      | cyan                                         |
| \\{magenta}                   | magenta                                      |

## Git
|          **Value**            |                **Description**               |
|:-----------------------------:|:--------------------------------------------:|
| \\{repo_status}               | Git repository status                        |

## Ternary expression
```
\\if{cond}{then}{else}
```

### Conditionals
|          **Value**            |                **Description**               |
|:-----------------------------:|:--------------------------------------------:|
| \\in_repo                     | User name                                    |

## misc.
|          **Value**            |                **Description**               |
|:-----------------------------:|:--------------------------------------------:|
| \\n                           | User name                                    |
