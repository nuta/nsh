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
    /// "foo" (long option) or "f" (short option).
    pub name: String,
    /// The brief description of the option.
    pub description: String,
    /// The character immediately following the option name.
    pub suffix: OptSuffix,
    /// The argument to the option.
    pub arg: Option<Argument>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PathKind {
    FileOnly,
    DirOnly,
    Any,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Candidate {
    Path { kind: PathKind },
    ExternalCommand { command: String },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Argument {
    /// The name of the argument.
    pub name: String,
    /// The brief description of the argument.
    pub description: String,
    /// The completion item providers.
    pub candidates: Vec<Candidate>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Command {
    pub argv0: String,
    pub short_options: Vec<Opt>,
    pub long_options: Vec<Opt>,
    pub arguments: Vec<Argument>,
    pub subcommands: HashMap<String, Command>,
}
