use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum OptSuffix {
    /// `"--foo arg"`.
    Whitespace,
    /// `"--foo=arg"`.
    Equal,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Opt {
    /// "--foo" (long option) or "-f" (short option).
    pub flags: Vec<String>,
    /// The brief description of the option.
    pub description: String,
    /// The character immediately following the option name.
    pub suffix: OptSuffix,
    /// The argument to the option.
    pub value: Value,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PathKind {
    FileOnly,
    DirOnly,
    Any,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Path { kind: PathKind },
    ExternalCommand { command: String },
    Any,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Argument {
    /// The name of the argument.
    pub name: String,
    /// The completion provider.
    pub value: Value,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Completion {
    pub command: String,
    pub options: Vec<Opt>,
    pub arguments: Vec<Argument>,
    pub subcommands: Vec<Completion>,
}
