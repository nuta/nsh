//! The shell language definition.
//!
//! # Overview
//!
//! ```text
//!
//! cat /var/log/syslog | grep trace   &&   echo found ; cowsay hi
//! ^^^^^^^^^^^^^^^^^^^   ^^^^^^^^^^        ^^^^^^^^^^   ^^^^^^^^^
//! simple_command    simple_command    simple_command   simple_command
//! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^        ^^^^^^^^^^   ^^^^^^^^^
//!           pipeline                        pipeline   pipeline
//! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^   ^^^^^^^^^
//!                     term                             term
//! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//!                             term_list
//!
//! ```
use std::os::unix::prelude::RawFd;

#[derive(Debug, PartialEq, Clone)]
pub struct Ast {
    pub terms: Vec<Term>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Term {
    pub pipelines: Vec<Pipeline>,
    pub background: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub enum RunIf {
    /// `;`.
    Always,
    /// `&&`. Run the command if the previous command returned 0.
    Success,
    /// `||`. Run the command if the previous command returned non-zero value.
    Failure,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Pipeline {
    // `;` or `&&' or `||'.
    pub run_if: RunIf,
    // Commands in a pipeline separated by `|'.
    pub commands: Vec<Command>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Command {
    SimpleCommand {
        assignments: Vec<Assignment>,
        argv: Vec<Word>,
        redirections: Vec<Redirection>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Initializer {
    String(Word),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Assignment {
    pub name: String,
    pub initializer: Initializer,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Tilde {
    /// `~`.
    Home,
    /// `~abc`.
    HomeOf(String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Sequence {
    Integer {
        /// The beginning of the range. Inclusive.
        start: isize,
        /// The end of the range. Exclusive.
        end: isize,
        /// The minimum number of digits (used for padding). For example `001`
        /// has 3 digits.
        num_digits: usize,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum BraceExpansion {
    /// `foo`.
    Word(Word),
    /// `0..8`.
    Sequence(Sequence),
    // `{0..5},{10..15}` or `{}` if the inner Vec is empty.
    List(Vec<BraceExpansion>),
}

/// Variable expansions. `null` is the case where the variable is set but it's
/// empty.
#[derive(Debug, PartialEq, Clone)]
pub enum VarExpansion {
    /// `${#parameter}`.
    ///
    /// set:   length
    /// null:  "0"
    /// unset: "0"
    Length,
    /// `$parameter and ${parameter}`.
    ///
    /// set:   parameter's value
    /// null:  ""
    /// unset: null or error if set -u is in effect
    GetOrEmpty,
    /// `${parameter:-word}`.
    ///
    /// set:   parameter's value
    /// null:  word
    /// unset: word
    GetOrDefault(Word),
    /// `${parameter-word}`.
    ///
    /// set:   parameter's value
    /// null:  ""
    /// unset: word
    GetNullableOrDefault(Word),
    /// `${parameter:=word}`.
    ///
    /// set:   parameter's value
    /// null:  assign and return word
    /// unset: assign and return word
    GetOrDefaultAndAssign(Word),
    /// `${parameter=word}`.
    ///
    /// set:   parameter's value
    /// null:  ""
    /// unset: assign and return word
    GetNullableOrDefaultAndAssign(Word),
    /// `${parameter:?word}`.
    ///
    /// set:   parameter's value
    /// null:  error
    /// unset: error
    GetOrExit(Word),
    /// `${parameter?word}`.
    ///
    /// set:   parameter's value
    /// null:  ""
    /// unset: error
    GetNullableOrExit(Word),
    /// `${parameter:+word}`.
    ///
    /// set:   word
    /// null:  null
    /// unset: null
    GetWordIfSet(Word),
    /// `${parameter+word}`.
    ///
    /// set:   word
    /// null:  word
    /// unset: null
    GetWordIfSetOrNull(Word),
    // ${parameter/pattern/replacement}
    Subst {
        pattern: Word,
        replacement: Word,
        replace_all: bool,
    },
}

/// A fragment of a word.
#[derive(Clone, Debug, PartialEq)]
pub enum Span {
    /// A plain text.
    Plain(String),
    /// A variable substitution, e.g. `$foo`.
    Variable {
        name: String,
        expansion: VarExpansion,
        quoted: bool,
    },
    /// A command substitution, e.g. `$(echo "hi")`.
    Command(Vec<Token>),
    /// A tilde expansion, e.g. `~`.
    Tilde(Tilde),
    /// A brace expansion, e.g. `a{b,c}d`.
    Brace(BraceExpansion),
    /// A process substitution, e.g. `<(echo "hi")`.
    ///
    /// It'll be substituted with a file path (`/dev/fd/<n>`) readable from the
    /// command.
    ProcessReadable(Vec<Token>),
    /// A process substitution, e.g. `>(grep foo)`.
    ///
    /// It'll be substituted with a file path (`/dev/fd/<n>`) writable from the
    /// command.
    ProcessWritable(Vec<Token>),
    /// An arithmetic expression, e.g. `$((1 + 2))`.
    Arith(Word),
    /// Wildcard (`*`). It matches zero or more characters.
    AnyString,
    /// Wildcard (`?`).
    AnyChar,
    /// Wildcard (`[a-z]`). The inner string has the pattern without `[` and `]`.
    AnyCharIn(String),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Word(pub(crate) Vec<Span>);

impl Word {
    pub fn new(spans: Vec<Span>) -> Word {
        Word(spans)
    }

    pub fn spans(&self) -> &[Span] {
        &self.0
    }

    /// Returns true if the word is a single plain text.
    ///
    /// It's useful for checking if a word is a keyword.
    pub fn equals(&self, text: &str) -> bool {
        self.0.len() == 1 && matches!(self.0[0], Span::Plain(ref s) if s == text)
    }
}

/// Contains heredoc body. The outer Vec represents lines and
/// `Vec<Word>` represents the contents of a line.
#[derive(Debug, PartialEq, Clone)]
pub struct HereDoc(pub(crate) Vec<Vec<Span>>);

impl HereDoc {
    pub fn new(lines: Vec<Vec<Span>>) -> HereDoc {
        HereDoc(lines)
    }

    pub fn lines(&self) -> &[Vec<Span>] {
        &self.0
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum RedirOp {
    /// `cat < foo.txt` or here document.
    Input(RawFd),
    /// `cat > foo.txt`
    Output(RawFd),
    /// `cat >> foo.txt`
    Append(RawFd),
    /// `cat &> stdout_and_err.log`
    OutputStdoutAndStderr,
    /// `cat &>> stdout_and_err.log`
    AppendStdoutAndStderr,
}

#[derive(Debug, PartialEq, Clone)]
pub enum RedirRhs {
    /// `foo.log` in `> foo.log`.
    File(Word),
    /// `1` in `2>&1`.
    Fd(RawFd),
    /// A here document. Contains the index for [`Lexer::heredoc`].
    HereDoc(usize),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Redirection {
    pub op: RedirOp,
    pub rhs: RedirRhs,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    /// A newline (`\n`).
    Newline,
    /// `|`
    Or,
    /// `&`.
    And,
    /// `;`
    Semi,
    /// `&&`
    DoubleAnd,
    /// `||`
    DoubleOr,
    /// `;;`
    DoubleSemi,
    /// `(`
    LeftParen,
    /// `)`
    RightParen,
    /// `{`
    LeftBrace,
    /// `}`
    RightBrace,
    /// `\``
    ClosingBackTick,
    /// A redirection like `echo > foo.log`.
    Redirection(Redirection),
    /// A word.
    Word(Word),
    /// The argv0 word: contains a function or command name.
    Argv0(Word),
    /// An assignment (e.g. `FOO=bar`).
    Assignment(Assignment),
    /// `if`
    If,
    /// `then`
    Then,
    /// `elif`
    ElIf,
    /// `else`
    Else,
    /// `fi`
    Fi,
    /// `while`
    While,
    /// `for`
    For,
    /// `in`
    In,
    /// `do`
    Do,
    /// `done`
    Done,
    /// `break`
    Break,
    /// `continue`
    Continue,
    /// `case`
    Case,
    /// `esac`
    Esac,
    /// `function`
    Function,
    /// `return`
    Return,
}
