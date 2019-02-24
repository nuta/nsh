# History File Format
Each line represents the command and its context in the following format:

```
time cwd cmd
```

where *time* is the UNIX timestamp when the command is executed, *cwd*
is the directory where the command is executed, and *cmd* is the command
itself. Each field is separated by a tab character (`\t`).
