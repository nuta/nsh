use crate::builtins::{InternalCommandContext, INTERNAL_COMMANDS, InternalCommandError};
use crate::completion::{CompSpec, cmd_completion, path_completion};
use crate::context_parser::{self, InputContext};
use crate::parser::{
    self, Ast, ExpansionOp, RunIf, Expr, BinaryExpr, Span, Word, Initializer,
    LocalDeclaration, Assignment, ProcSubstType, CondExpr, HereDoc
};
use crate::path::{lookup_external_command,wait_for_path_loader};
use crate::variable::{Variable, Value};
use crate::utils::FdFile;
use crate::pattern::{
    match_pattern, match_pattern_all, replace_pattern,
    LiteralOrGlob, PatternWord, NoMatchesError
};
use nix;
use nix::sys::wait::{waitpid, WaitStatus, WaitPidFlag};
use nix::sys::signal::{kill, SigHandler, SigAction, SaFlags, SigSet, Signal, sigaction};
use nix::sys::termios::{tcgetattr, tcsetattr, Termios, SetArg::TCSADRAIN};
use nix::unistd::{close, dup2, execv, fork, pipe, ForkResult, Pid, getpid, setpgid, tcsetpgrp};
use std::collections::{HashMap, HashSet};
use std::ffi::CString;
use std::fmt;
use std::fs::{File, OpenOptions};
use std::path::PathBuf;
use std::io::prelude::*;
use std::os::unix::io::IntoRawFd;
use std::os::unix::io::FromRawFd;
use std::os::unix::io::RawFd;
use std::sync::Arc;
use std::cell::RefCell;
use failure::Error;

type Result<I> = std::result::Result<I, Error>;

fn kill_process_group(pgid: Pid, signal: Signal) -> Result<()> {
    kill(Pid::from_raw(-pgid.as_raw()), signal).map_err(Error::from)
}

fn move_fd(src: RawFd, dst: RawFd) {
    if src != dst {
        dup2(src, dst).expect("failed to dup2");
        close(src).expect("failed to close");
    }
}

fn wait_child(pid: Pid) -> Result<i32> {
    let wait_status = waitpid(pid, None)?;
    match wait_status {
        WaitStatus::Exited(_, status) => Ok(status),
        // TODO: Handle errors.
        _ => {
            let err = format_err!("waitpid returned an unexpected value: {:?}",
                wait_status);

            warn!("waitpid: {}", err);
            Err(err)
        }
    }
}

/// The exit status or reason why the command exited.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum ExitStatus {
    ExitedWith(i32),
    Running(Pid /* pgid */),
    Break,
    Continue,
    Return,
    // The command is not executed because of `noexec`.
    NoExec,
}

/// The process execution environment.
#[derive(Debug, Copy, Clone)]
struct Context {
    stdin: RawFd,
    stdout: RawFd,
    stderr: RawFd,
    pgid: Option<Pid>,
    /// The process should be executed in background.
    background: bool,
    /// Is the shell interactive?
    interactive: bool,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum ProcessState {
    Running,
    /// Contains the exit status.
    Completed(i32),
    /// Suspended (Ctrl-Z).
    Stopped(Pid),
}

/// A variable scope.
#[derive(Debug)]
pub struct Frame {
    /// A `(variable name, varible)` map.
    vars: HashMap<String, Arc<Variable>>,
}

impl Frame {
    pub fn new() -> Frame {
        Frame {
            vars: HashMap::new(),
        }
    }

    pub fn define(&mut self, key: &str) {
        self.vars.insert(key.into(), Arc::new(Variable::new(None)));
    }

    pub fn set(&mut self, key: &str, value: Value) {
        self.vars.insert(key.into(), Arc::new(Variable::new(Some(value))));
    }

    pub fn remove(&mut self, key: &str) -> Option<Arc<Variable>> {
        self.vars.remove(key)
    }

    pub fn get(&self, key: &str) -> Option<Arc<Variable>> {
        self.vars.get(key).cloned()
    }

    /// Returns `$1`, `$2`, ...
    pub fn get_args(&self) -> Vec<Arc<Variable>> {
        let mut args = Vec::new();
        for i in 1.. {
            if let Some(var) = self.get(&i.to_string()) {
                args.push(var.clone());
            } else {
                break;
            }
        }

        args
    }

    /// Returns `$1`, `$2`, ...
    pub fn get_string_args(&self) -> Vec<String> {
        let mut args = Vec::new();
        for var in self.get_args() {
            if let Some(Value::String(value)) = var.value() {
                args.push(value.clone());
            }
        }

        args
    }

    /// Sets `$1`, `$2`, ...
    pub fn set_args(&mut self, args: &[String]) {
        for (i, arg) in args.iter().enumerate() {
            self.set(&(i + 1).to_string(), Value::String(arg.clone()));
        }
    }

    /// Sets `$<index>`.
    pub fn set_nth_arg(&mut self, index: usize, value: Value) {
        self.set(&index.to_string(), value)
    }

    /// Removes `$<index>`.
    pub fn remove_nth_arg(&mut self, index: usize) -> Option<Arc<Variable>> {
        self.remove(&index.to_string())
    }

    /// Returns `$<index>`.
    pub fn get_nth_arg(&self, index: usize) -> Option<Arc<Variable>> {
        self.get(&index.to_string())
    }

    /// The number of function arguments (`$1`, ...).
    pub fn num_args(&self) -> usize {
        let mut num_args = 0;
        for i in 1..=9 {
            if self.get(&i.to_string()).is_none() {
                break;
            }

            num_args += 1;
        }

        num_args

    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct JobId(usize);

impl JobId {
    pub fn new(id: usize) -> JobId {
        JobId(id)
    }
}

impl fmt::Display for JobId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

/// Represents a job. Refer https://www.gnu.org/software/libc/manual/html_node/Implementing-a-Shell.html
pub struct Job {
    id: JobId,
    pgid: Pid,
    cmd: String,
    processes: Vec<Pid>,
    termios: RefCell<Option<Termios>>,
}

impl Job {
    pub fn new(id: JobId, pgid: Pid, cmd: String, processes: Vec<Pid>) -> Job {
        Job {
            id,
            pgid,
            cmd,
            processes,
            termios: RefCell::new(None),
        }
    }

    #[inline]
    pub fn id(&self) -> JobId {
        self.id
    }

    #[inline]
    pub fn cmd(&self) -> &str {
        self.cmd.as_str()
    }

    pub fn state(&self, isolate: &Isolate) -> &'static str {
        if self.completed(isolate) {
            "done"
        } else if self.stopped(isolate) {
            "stopped"
        } else {
            "running"
        }
    }

    pub fn completed(&self, isolate: &Isolate) -> bool {
        self.processes.iter().all(|pid| {
            let state = isolate.get_process_state(*pid).unwrap();
            match state {
                ProcessState::Completed(_) => true,
                _ => false,
            }
        })
    }

    pub fn stopped(&self, isolate: &Isolate) -> bool {
        self.processes.iter().all(|pid| {
            let state = isolate.get_process_state(*pid).unwrap();
            match state {
                ProcessState::Stopped(_) => true,
                _ => false,
            }
        })
    }
}

/// A isolated shell execution environment. Please note that the shell
/// environment is not completely isolated because there are some global
/// states like `PATH_TABLE` in [`path`].
pub struct Isolate {
    shell_pgid: Pid,
    interactive: bool,
    /// $0
    script_name: String,
    term_fd: RawFd,
    shell_termios: Option<Termios>,

    /// `$?`
    last_status: i32,
    /// `$!`
    last_back_job: Option<Arc<Job>>,

    /// Global scope.
    global: Frame,
    /// Local scopes (variables declared with `local').
    frames: Vec<Frame>,
    exported: HashSet<String>,
    aliases: HashMap<String, String>,

    // Shell options.
    pub errexit: bool,
    pub nounset: bool,
    pub noexec: bool,

    jobs: HashMap<JobId, Arc<Job>>,
    background_jobs: HashSet<JobId>,
    last_fore_job: Option<Arc<Job>>,
    states: HashMap<Pid, ProcessState>,
    pid_job_mapping: HashMap<Pid, Arc<Job>>,

    // pushd(1) / popd(1) stack
    cd_stack: Vec<String>,

    completions: HashMap<String, CompSpec>,
}

unsafe impl Send for Isolate {}

macro_rules! bool_to_int {
    ($e:expr) => {
        if $e {
            1
        } else {
            0
        }
    };
}

impl Isolate {
    pub fn new(script_name: &str, interactive: bool) -> Isolate {
        let shell_pgid = getpid();
        let shell_termios = if interactive {
            Some(tcgetattr(0 /* stdin */).expect("failed to tcgetattr"))
        } else {
            None
        };

        Isolate {
            shell_pgid,
            script_name: script_name.to_string(),
            interactive,
            term_fd: 0 /* stdin */,
            shell_termios,
            last_status: 0,
            exported: HashSet::new(),
            aliases: HashMap::new(),
            global: Frame::new(),
            frames: Vec::new(),
            errexit: false,
            nounset: false,
            noexec: false,
            jobs: HashMap::new(),
            background_jobs: HashSet::new(),
            states: HashMap::new(),
            pid_job_mapping: HashMap::new(),
            cd_stack: Vec::new(),
            last_fore_job: None,
            last_back_job: None,
            completions: HashMap::new(),
        }
    }

    #[inline]
    pub fn interactive(&self) -> bool {
        self.interactive
    }

    #[inline]
    pub fn ifs(&self) -> String {
        self.get_str("IFS").unwrap_or_else(|| "\n\t ".to_owned())
    }

    #[inline]
    pub fn enter_frame(&mut self) {
        self.frames.push(Frame::new());
    }

    #[inline]
    pub fn leave_frame(&mut self) {
        self.frames.pop();
    }

    #[inline]
    pub fn in_global_frame(&self) -> bool {
        self.frames.is_empty()
    }

    #[inline]
    pub fn current_frame(&self) -> &Frame {
        self.frames.last().unwrap_or(&self.global)
    }

    #[inline]
    pub fn current_frame_mut(&mut self) -> &mut Frame {
        self.frames.last_mut().unwrap_or(&mut self.global)
    }

    #[inline]
    pub fn assign(&mut self, key: &str, value: Value) {
        let defined_as_local = self.current_frame().get(key).is_some();
        self.set(key, value, defined_as_local);
    }

    pub fn define(&mut self, key: &str, is_local: bool) {
        let frame = if is_local {
            self.current_frame_mut()
        } else {
            &mut self.global
        };

        frame.define(key);
    }

    pub fn set(&mut self, key: &str, value: Value, is_local: bool) {
        let frame = if is_local {
            self.current_frame_mut()
        } else {
            &mut self.global
        };

        frame.set(key, value);
    }

    pub fn remove(&mut self, key: &str) -> Option<Arc<Variable>> {
        if let Some(var) = self.current_frame_mut().remove(key) {
            return Some(var);
        }

        if let Some(var) = self.global.remove(key) {
            return Some(var);
        }

        None
    }

    pub fn get(&self, key: &str) -> Option<Arc<Variable>> {
        if let Some(var) = self.current_frame().get(key) {
            Some(var)
        } else {
            self.global.get(key)
        }
    }

    #[inline]
    pub fn get_str(&self, key: &str) -> Option<String> {
        match self.get(key) {
            Some(var) => {
                match var.value() {
                    Some(Value::String(ref s)) => Some(s.clone()),
                    _ => None,
                }
            },
            _ => None,
        }
    }

    pub fn export(&mut self, name: &str) {
        self.exported.insert(name.to_string());
    }

    pub fn exported_names(&self) -> std::collections::hash_set::Iter<String> {
        self.exported.iter()
    }

    pub fn add_alias(&mut self, name: &str, body: String) {
        self.aliases.insert(name.to_string(), body);
    }

    pub fn lookup_alias(&self, alias: &str) -> Option<String> {
        self.aliases.get(&alias.to_string()).cloned()
    }

    pub fn pushd(&mut self, path: String) {
        self.cd_stack.push(path);
    }

    pub fn popd(&mut self) -> Option<String> {
        self.cd_stack.pop()
    }

    fn call_completion_function(&mut self, func_name: &str, ctx: &InputContext) -> Vec<Arc<String>> {
        let locals = vec![
            ("COMP_WORDS", Value::Array(ctx.words.clone())),
            ("COMP_CWORD", Value::String(ctx.current_word.to_string()))
        ];

        match self.call_function_in_shell_context(func_name, &[], locals) {
            Ok(ExitStatus::ExitedWith(0)) => {
                self.get("COMPREPLY")
                    .and_then(|reply| {
                        match reply.value() {
                            Some(Value::Array(arr)) => {
                                debug!("arr = {:?}", arr);
                                let entries = arr.iter()
                                    .map(|elem| Arc::new(elem.clone()))
                                    .collect();
                                Some(entries)
                            },
                            _ => None,
                        }
                    })
                    .unwrap_or_else(Vec::new)
            },
            Ok(status) => {
                // Something went wrong within the function. Just ignore it.
                eprintln!("nsh: failed to invoke a completion function `{}': {:?}",
                    func_name, status);
                Vec::new()
            },
            Err(err) => {
                // Something went wrong (BUG).
                eprintln!("nsh: BUG: failed to invoke a function `{}': {}",
                    func_name, err);
                Vec::new()
            }
        }
    }

    /// Returns completion candidates.
    pub fn complete(&mut self, ctx: &InputContext) -> Vec<Arc<String>> {
        trace!("complete: ctx={:?}", ctx);

        let cmd_name = if let Some(name) = ctx.words.get(0) {
            let name = name.as_str().to_owned();
            match self.aliases.get(&name) {
                Some(alias) if !alias.contains(' ') => {
                    // The alias named `name' is defined and it is a command
                    // (does not contain whitespaces). Use its value as the
                    // target command name.
                    alias.to_owned()
                },
                _ => name
            }
        } else {
            "".to_owned()
        };

        let current_span = ctx.current_span.map(|index| &ctx.spans[index]);
        if let Some(context_parser::Span::Argv0(_)) = current_span {
            // The cursor is at the first word, namely, the command.
            cmd_completion(ctx)
        } else if let Some(compspec) = self.get_compspec(cmd_name.as_str()) {
            let compspec = compspec.clone();
            let mut results = Vec::new();
            if let Some (name) = compspec.func_name() {
                results = self.call_completion_function(&name, ctx);
            }

            if results.is_empty() {
                // No completion candidates. Try defaults.
                results.extend(
                    path_completion(
                        ctx,
                        compspec.filenames_if_empty(),
                        compspec.dirnames_if_empty(),
                        false,
                        true
                    )
                );
            }

            results
        } else {
            // Compspec if not defined for the command. Use path completion instead.
            path_completion(ctx, true, true, false, true)
        }
    }

    #[inline]
    pub fn get_compspec<'a>(&'a self, command: &str) -> Option<&'a CompSpec> {
        self.completions.get(command)
    }

    #[inline]
    pub fn set_compspec(&mut self, command: &str, compspec: CompSpec) {
        self.completions.insert(command.to_owned(), compspec);
    }

    fn get_var_as_i32(&self, name: &str) -> Option<i32> {
        self.get(name).and_then(|var|
            match var.value() {
                Some(Value::String(s)) => s.parse().ok(),
                _ => None,
            }
        )
    }

    fn evaluate_expr(&mut self, expr: &Expr) -> i32 {
        match expr {
            Expr::Expr(sub_expr) => self.evaluate_expr(sub_expr),
            Expr::Literal(value) => *value,
            Expr::Parameter { name } => {
                self.get_var_as_i32(&name).unwrap_or(0)
            },
            Expr::Add(BinaryExpr { lhs, rhs }) => {
                self.evaluate_expr(lhs) + self.evaluate_expr(rhs)
            },
            Expr::Sub(BinaryExpr { lhs, rhs }) => {
                self.evaluate_expr(lhs) - self.evaluate_expr(rhs)
            },
            Expr::Mul(BinaryExpr { lhs, rhs }) => {
                self.evaluate_expr(lhs) * self.evaluate_expr(rhs)
            },
            Expr::Div(BinaryExpr { lhs, rhs }) => {
                self.evaluate_expr(lhs) / self.evaluate_expr(rhs)
            },
            Expr::Assign { name, rhs } => {
                let value = self.evaluate_expr(rhs);
                self.assign(&name, Value::String(value.to_string()));
                value
            },
            Expr::Eq(lhs, rhs) => {
                bool_to_int!(self.evaluate_expr(lhs) == self.evaluate_expr(rhs))
            },
            Expr::Ne(lhs, rhs) => {
                bool_to_int!(self.evaluate_expr(lhs) != self.evaluate_expr(rhs))
            },
            Expr::Lt(lhs, rhs) => {
                bool_to_int!(self.evaluate_expr(lhs) < self.evaluate_expr(rhs))
            },
            Expr::Le(lhs, rhs) => {
                bool_to_int!(self.evaluate_expr(lhs) <= self.evaluate_expr(rhs))
            },
            Expr::Gt(lhs, rhs) => {
                bool_to_int!(self.evaluate_expr(lhs) > self.evaluate_expr(rhs))
            },
            Expr::Ge(lhs, rhs) => {
                bool_to_int!(self.evaluate_expr(lhs) >= self.evaluate_expr(rhs))
            },
            Expr::Inc(name) => {
                let value = self.get_var_as_i32(&name).unwrap_or(0) + 1;
                self.assign(&name, Value::String(value.to_string()));
                value
            },
            Expr::Dec(name) => {
                let value = self.get_var_as_i32(&name).unwrap_or(0) - 1;
                self.assign(&name, Value::String(value.to_string()));
                value
            },
        }
    }

    /// Expands a parameter (`$foo` in e.g. `echo $foo`). It returns `Vec` since `op` can be
    /// an operation which expands an array. `None` value represents *null*.
    fn expand_param(&mut self, name: &str, op: &ExpansionOp) -> Result<Vec<Option<String>>> {
        match name {
            "?" => {
                return Ok(vec![Some(self.last_status.to_string())]);
            },
            "!" => {
                let pgid = match &self.last_back_job {
                    Some(job) => job.pgid.to_string(),
                    None => 0.to_string(),
                };

                return Ok(vec![Some(pgid)]);
            },
            "0" => {
                return Ok(vec![Some(self.script_name.clone())]);
            },
            "$" => {
                return Ok(vec![Some(self.shell_pgid.to_string())]);
            },
            "#" => {
                return Ok(vec![Some(self.current_frame().num_args().to_string())]);
            },
            "*" => {
                let args = self.current_frame().get_string_args();
                let expanded = args.join(" ");
                return Ok(vec![Some(expanded)]);
            },
            "@" => {
                let args = self.current_frame().get_string_args();
                return Ok(args.iter().map(|a| Some(a.to_owned())).collect());
            },
            _ => {
                if let Some(var) = self.get(name) {
                    // $<name> is defined.
                    let value = var.value();
                    match (op, value) {
                        (ExpansionOp::Length, Some(_)) => {
                            return Ok(vec![Some(var.as_str().len().to_string())]);
                        }
                        (ExpansionOp::Length, None) => {
                            return Ok(vec![Some(0.to_string())]);
                        }
                        (ExpansionOp::GetNullableOrDefaultAndAssign(_), None) => {
                            return Ok(vec![None]);
                        },
                        (ExpansionOp::GetNullableOrDefault(_), None) => {
                            return Ok(vec![None]);
                        },
                        (ExpansionOp::Subst { pattern, replacement, replace_all }, Some(_)) => {
                            let content = var.as_str().to_string();
                            let replaced = self.replace_pattern(pattern, &content, replacement, *replace_all)?;
                            return Ok(vec![Some(replaced)])
                        }
                        (_, _) => {
                            return Ok(vec![Some(var.as_str().to_string())]);
                        }
                    };
                }
            }
        }

        // $<name> is not defined or null on GetOrDefault or GetOrDefaultAndAssign.
        // http://pubs.opengroup.org/onlinepubs/009695399/utilities/xcu_chap02.html#tag_02_06_02
        match op {
            ExpansionOp::Length => {
                if self.nounset {
                    eprintln!("nsh: undefined variable `{}'", name);
                    std::process::exit(1);
                }

                Ok(vec![Some("0".to_owned())])
            },
            ExpansionOp::GetOrEmpty => {
                if self.nounset {
                    eprintln!("nsh: undefined variable `{}'", name);
                    std::process::exit(1);
                }

                Ok(vec![Some("".to_owned())])
            },
            ExpansionOp::GetOrDefault(word)
            | ExpansionOp::GetNullableOrDefault(word) => {
                self.expand_word_into_string(word).map(|s| vec![Some(s)])
            },
            ExpansionOp::GetOrDefaultAndAssign(word)
            | ExpansionOp::GetNullableOrDefaultAndAssign(word) => {
                let content = self.expand_word_into_string(word)?;
                self.set(name, Value::String(content.clone()), false);
                Ok(vec![Some(content)])
            }
            ExpansionOp::Subst { .. } => {
                Ok(vec![Some("".to_owned())])
            }
        }
    }

    /// Expands a word int a `Vec`.
    fn expand_word_into_vec(&mut self, word: &Word, ifs: &str) -> Result<Vec<PatternWord>> {
        let mut words = Vec::new();
        let mut current_word = Vec::new();
        for span in word.spans() {
            let (frags, expand) = match span {
                Span::LiteralChars(..) => {
                    // Internally used by the parser.
                    unreachable!()
                }
                Span::Literal(s) => {
                    (vec![LiteralOrGlob::Literal(s.clone())], false)
                },
                Span::Parameter { name, op, quoted } => {
                    let mut frags = Vec::new();
                    for value in self.expand_param(name, op)? {
                        let frag = value.unwrap_or_else(|| "".to_owned());
                        frags.push(LiteralOrGlob::Literal(frag));
                    }
                    (frags, !quoted)
                },
                Span::ArrayParameter { name, index, quoted } => {
                    let index = self.evaluate_expr(index);
                    if index < 0 {
                        warn!("the index must be larger than or equals 0: var={}, index={}",
                            name, index);
                        (vec![], !quoted)
                    } else {
                        debug!("array_param: ${}[{}] == {:?}", name, index, self.get(name));
                        let frag = self
                                .get(name)
                                .map(|v| v.value_at(index as usize).to_string())
                                .unwrap_or_else(|| "".to_owned());
                        debug!("frag = {}", frag);
                        (vec![LiteralOrGlob::Literal(frag)], !quoted)
                    }
                },
                Span::ArithExpr { expr } => {
                    let result = self.evaluate_expr(expr).to_string();
                    (vec![LiteralOrGlob::Literal(result)], false)
                },
                Span::Tilde(user) => {
                    if user.is_some() {
                        // TODO: e.g. ~mike, ~chandler/venus
                        unimplemented!();
                    }

                    let dir = dirs::home_dir().unwrap().to_str().unwrap().to_owned();
                    (vec![LiteralOrGlob::Literal(dir)], false)
                },
                Span::Command { body, quoted } => {
                    let (_, stdout) = self.eval_in_subshell(body)?;

                    let mut raw_stdout = Vec::new();
                    unsafe {
                        File::from_raw_fd(stdout)
                            .read_to_end(&mut raw_stdout).ok()
                    };

                    let output = std::str::from_utf8(&raw_stdout)
                        .map_err(|err| {
                            // TODO: support binary output
                            eprintln!("binary (invalid utf-8) variable/expansion is not supported");
                            err
                        })?
                        .to_string()
                        .trim_end_matches('\n')
                        .to_owned();

                    (vec![LiteralOrGlob::Literal(output)], !quoted)
                },
                Span::ProcSubst { body, subst_type } => {
                    let (_, stdout) = self.eval_in_subshell(body)?;
                    match subst_type {
                        // <()
                        ProcSubstType::StdoutToFile => {
                            let file_name = format!("/dev/fd/{}", stdout);
                            (vec![LiteralOrGlob::Literal(file_name)], false)
                        },
                        // >()
                        ProcSubstType::FileToStdin => {
                            // TODO:
                            unimplemented!();
                        }
                    }
                },
                Span::AnyChar { quoted } if !*quoted => {
                    (vec![LiteralOrGlob::AnyChar], false)
                },
                Span::AnyString { quoted } if !*quoted => {
                    (vec![LiteralOrGlob::AnyString], false)
                },
                Span::AnyChar { .. } => {
                    (vec![LiteralOrGlob::Literal("?".into())], false)
                },
                Span::AnyString { .. } => {
                    (vec![LiteralOrGlob::Literal("*".into())], false)
                },
            };

            // Expand `a${foo}b` into words: `a1` `2` `3b`, where `$foo="1 2 3"`.
            let frags_len = frags.len();
            for frag in frags {
                match frag {
                    LiteralOrGlob::Literal(ref lit) if expand => {
                        if !current_word.is_empty() {
                            words.push(PatternWord::new(current_word));
                            current_word = Vec::new();
                        }

                        for word in lit.split(|c| ifs.contains(c)) {
                            words.push(PatternWord::new(vec![LiteralOrGlob::Literal(word.into())]));
                        }
                    },
                    frag => {
                        current_word.push(frag);
                    }
                }

                if frags_len > 1 && !current_word.is_empty() {
                    words.push(PatternWord::new(current_word));
                    current_word = Vec::new();
                }
            }
        }

        if !current_word.is_empty() {
            words.push(PatternWord::new(current_word));
        }

        trace!("expand_word: word={:?}, to={:?}", word, words);
        if words.is_empty() {
            Ok(vec![PatternWord::new(vec![LiteralOrGlob::Literal("".into())])])
        } else {
            Ok(words)
        }
    }

    /// Expands a word into a string. Words in a command span `"$(echo foo bar)"` are
    /// joined by a whitespace.
    fn expand_word_into_string(&mut self, word: &Word) -> Result<String> {
        let ws: Vec<String> = self.expand_word_into_vec(word, &self.ifs())?
            .into_iter()
            .map(|w| w.into_string())
            .collect();

        Ok(ws.join(""))
    }

    /// Expands words into a `Vec<String>`. A pattern in a word are expanded as a
    /// file path globbing.
    fn expand_words(&mut self, words: &[Word]) -> Result<Vec<String>> {
        let mut evaluated = Vec::new();
        for word in words {
            let mut ws = Vec::new();
            for w in self.expand_word_into_vec(word, &self.ifs())? {
                for f in w.expand_glob()? {
                    ws.push(f);
                }
            }

            evaluated.extend(ws);
        }

        Ok(evaluated)
    }

    /// Evaluates a variable initializer.
    ///
    /// ```
    /// this_is_string=hello_world
    ///                ^^^^^^^^^^^ a string initializer
    /// this_is_array=(a b c)
    ///               ^^^^^^^ an array initializer
    /// ```
    ///
    fn evaluate_initializer(&mut self, initializer: &Initializer) -> Result<Value> {
        match initializer {
            Initializer::String(ref word) => {
                Ok(Value::String(self.expand_word_into_string(word)?))
            },
            Initializer::Array(ref words) =>  {
                let elems = self.expand_words(words)?;
                match (elems.len(), elems.get(0)) {
                    (1, Some(ref body)) if body.is_empty() => {
                        // Make `foo=()' an empty array.
                        Ok(Value::Array(vec![]))
                    },
                    _ => Ok(Value::Array(elems))
                }
            }
        }
    }

    pub fn create_job(&mut self, name: String, pgid: Pid, childs: Vec<Pid>) -> Arc<Job> {
        let id = self.alloc_job_id();
        let job = Arc::new(Job::new(id, pgid, name, childs.clone()));
        for child in childs {
            self.set_process_state(child, ProcessState::Running);
            self.pid_job_mapping.insert(child, job.clone());
        }

        self.jobs.insert(id, job.clone());
        job
    }

    #[inline]
    pub fn jobs(&self) -> Vec<Arc<Job>> {
        let mut jobs = Vec::new();
        for job in self.jobs.values() {
            jobs.push(job.clone());
        }

        jobs
    }

    pub fn alloc_job_id(&mut self) -> JobId {
        let mut id = 1;
        while self.jobs.contains_key(&JobId::new(id)) {
            id += 1;
        }

        JobId::new(id)
    }

    #[inline]
    pub fn last_fore_job(&self) -> Option<Arc<Job>> {
        self.last_fore_job.as_ref().cloned()
    }

    pub fn find_job_by_id(&self, id: JobId) -> Option<Arc<Job>> {
        self.jobs.get(&id).cloned()
    }

    pub fn continue_job(&mut self, job: &Arc<Job>, background: bool) {
        // Mark all stopped processes as running.
        for proc in &job.processes {
            if let ProcessState::Stopped(_) = self.get_process_state(*proc).unwrap() {
                self.set_process_state(*proc, ProcessState::Running);
            }
        }

        if background {
            self.run_in_background(&job, true);
        } else {
            self.run_in_foreground(&job, true);
        }
    }

    pub fn run_in_foreground(&mut self, job: &Arc<Job>, sigcont: bool) -> ProcessState {
        self.last_fore_job = Some(job.clone());
        self.background_jobs.remove(&job.id);

        // Put the job into the foreground.
        tcsetpgrp(0 /* stdin */, job.pgid).expect("failed to tcsetpgrp");

        if sigcont {
            if let Some(ref termios) = *job.termios.borrow() {
                tcsetattr(self.term_fd, TCSADRAIN, termios).expect("failed to tcsetattr");
            }
            kill_process_group(job.pgid, Signal::SIGCONT).expect("failed to kill(SIGCONT)");
            trace!("sent sigcont");
        }

        // Wait for the job to exit.
        let status = self.wait_for_job(&job);

        // Go back into the shell.
        job.termios.replace(Some(tcgetattr(0 /* stdin */).expect("failed to tcgetattr")));
        let termios = self.shell_termios.as_ref().unwrap();
        tcsetpgrp(0 /* stdin */, self.shell_pgid).expect("failed to tcsetpgrp");
        tcsetattr(0 /* stdin */, TCSADRAIN, termios).expect("failed to tcgetattr");

        status
    }

    pub fn run_in_background(&mut self, job: &Arc<Job>, sigcont: bool) {
        self.last_back_job = Some(job.clone());
        self.background_jobs.insert(job.id);

        if sigcont {
            kill_process_group(job.pgid, Signal::SIGCONT).expect("failed to kill(SIGCONT)");
        }
    }

    pub fn destroy_job(&mut self, job: &Arc<Job>) {
        for proc in &*job.processes {
            self.pid_job_mapping.remove(&proc);
        }

        if self.background_jobs.remove(&job.id) {
            // The job was a background job. Notify the user that the job
            // has finished.
            println!("[{}] Done: {}", job.id, job.cmd);
        }

        self.jobs.remove(&job.id).unwrap();

        if let Some(ref last_job) = self.last_fore_job {
            if job.id == last_job.id {
                self.last_fore_job = None;
            }
        }
    }

    /// Checks if background jobs have been terminated and notify the user that some jobs
    /// have been finished.
    pub fn check_background_jobs(&mut self) {
        while let Some(pid) = self.wait_for_any_process(true) {
            // This `get` won't return None.
            let job = &self.pid_job_mapping[&pid].clone();
            if job.completed(self) {
                self.destroy_job(&job);
            } else if job.stopped(self) {
                println!("[{}] Done: {}", job.id, job.cmd);
            }
        }
    }

    /// Waits for all processes in the job to exit. Note that the job will be
    /// deleted from `Isolate` if the process has exited.
    pub fn wait_for_job(&mut self, job: &Arc<Job>) -> ProcessState {
        loop {
            if job.completed(self) || job.stopped(self) {
                break;
            }

            self.wait_for_any_process(false);
        }

        // Get the exit status of the last process.
        let state = self.get_process_state(*job.processes.iter().last().unwrap()).cloned();

        match state {
            Some(ProcessState::Completed(_)) => {
                // Remove the job and processes from the list.
                self.destroy_job(job);
                state.unwrap()
            },
            Some(ProcessState::Stopped(_)) => {
                eprintln!("[{}] Stopped: {}", job.id, job.cmd);
                state.unwrap()
            },
            _ => unreachable!(),
        }
    }

    /// Waits for an *any* process, i.e. `waitpid(-1)`, and then updates
    /// the process state recorded in the `Isolate`. Returns `None` it
    /// would block.
    pub fn wait_for_any_process(&mut self, no_block: bool) -> Option<Pid> {
        let options = if no_block {
            WaitPidFlag::WUNTRACED | WaitPidFlag::WNOHANG
        } else {
            WaitPidFlag::WUNTRACED
        };

        let result = waitpid(None, Some(options));
        let (pid, state) = match result {
            Ok(WaitStatus::Exited(pid, status)) => {
                trace!("exited: pid={} status={}", pid, status);
                (pid, ProcessState::Completed(status))
            },
            Ok(WaitStatus::Signaled(pid, _signal, _)) => {
                // The `pid` process has been killed by `_signal`.
                (pid, ProcessState::Completed(-1))
            },
            Ok(WaitStatus::Stopped(pid, _signal)) => {
                (pid, ProcessState::Stopped(pid))
            },
            Err(nix::Error::Sys(nix::errno::Errno::ECHILD)) | Ok(WaitStatus::StillAlive) => {
                // No childs to be reported.
                return None;
            },
            status => {
                panic!("unexpected waitpid event: {:?}", status);
            }
        };

        self.set_process_state(pid, state);
        Some(pid)
    }

    /// Updates the process state.
    fn set_process_state(&mut self, pid: Pid, state: ProcessState) {
        self.states.insert(pid, state);
    }

    /// Returns the process state.
    fn get_process_state(&self, pid: Pid) -> Option<&ProcessState> {
        self.states.get(&pid)
    }

    fn evaluate_heredoc(&mut self, heredoc: &HereDoc) -> Result<RawFd> {
        let mut lines = Vec::new();
        for line in heredoc.lines() {
            let mut words = Vec::new();
            for word in line {
                words.push(self.expand_word_into_string(word)?);
            }

            lines.push(words.join(" "));
        }

        let mut body = lines.join("\n");
        body += "\n";

        let (pipe_out, pipe_in) = pipe().expect("failed to create a pipe");
        unsafe {
            let mut file = File::from_raw_fd(pipe_in);
            file.write_all(body.as_bytes()).ok();
            // Ensure that pipe_in is closed.
            drop(file);
        };

        Ok(pipe_out)
    }

    /// Spawn a child process and execute a command.
    fn run_external_command(
        &mut self,
        ctx: &Context,
        argv: Vec<String>,
        redirects: &[parser::Redirection],
        assignments: &[parser::Assignment]
    ) -> Result<ExitStatus> {
        let mut fds = Vec::new();
        for r in redirects {
            match r.target {
                parser::RedirectionType::Fd(ref fd) => {
                    fds.push((*fd, r.fd as RawFd));
                },
                parser::RedirectionType::File(ref wfilepath) => {
                    let mut options = OpenOptions::new();
                    match &r.direction {
                        parser::RedirectionDirection::Input => {
                            options.read(true);
                        },
                        parser::RedirectionDirection::Output => {
                            options.write(true).create(true);
                        },
                        parser::RedirectionDirection::Append => {
                            options.write(true).append(true);
                        }
                    };

                    trace!("redirection: options={:?}", options);
                    let filepath = self.expand_word_into_string(wfilepath)?;
                    if let Ok(file) = options.open(&filepath) {
                        fds.push((file.into_raw_fd(), r.fd as RawFd))
                    } else {
                        warn!("failed to open file: `{}'", filepath);
                        return Ok(ExitStatus::ExitedWith(1));
                    }
                }
                parser::RedirectionType::HereDoc(ref heredoc) => {
                    fds.push((self.evaluate_heredoc(heredoc)?, r.fd as RawFd))
                }
                parser::RedirectionType::UnresolvedHereDoc(_) => {
                    // must be resolved in the parser
                    unreachable!()
                }
            }
        }

        // Use provided (e.g. pipeline) stdin/stdout/stderr if no redirections speicfied.
        if !fds.iter().any(|(_, dst)| *dst == 0) {
            fds.push((ctx.stdin, 0));
        }
        if !fds.iter().any(|(_, dst)| *dst == 1) {
            fds.push((ctx.stdout, 1));
        }
        if !fds.iter().any(|(_, dst)| *dst == 2) {
            fds.push((ctx.stderr, 2));
        }

        // Determine the absolute path of the command.
        let argv0 = if argv[0].starts_with('/') || argv[0].starts_with("./") {
            CString::new(argv[0].as_str())?
        } else {
            match lookup_external_command(&argv[0]) {
                Some(argv0) => CString::new(argv0)?,
                None => {
                    eprintln!("nsh: command not found `{}'", argv[0]);
                    return Ok(ExitStatus::ExitedWith(1));
                }
            }
        };

        // Construct CString argv.
        let mut args = Vec::new();
        for arg in argv {
            args.push(CString::new(arg)?);
        }

        // Spawn a child.
        match fork().expect("failed to fork") {
            ForkResult::Parent { child } => {
                Ok(ExitStatus::Running(child))
            },
            ForkResult::Child => {
                // Create or join a process group.
                if ctx.interactive {
                    let pid = getpid();
                    let pgid = match ctx.pgid {
                        Some(pgid) => {
                            setpgid(pid, pgid).expect("failed to setpgid");
                            pgid
                        },
                        None => {
                            setpgid(pid, pid).expect("failed to setpgid");
                            pid
                        }
                    };

                    if !ctx.background {
                        tcsetpgrp(0 /* stdin */, pgid).expect("failed to tcsetpgrp");

                        // Place the terminal out of raw mode.
                        let termios = self.shell_termios.as_ref().unwrap();
                        tcsetattr(0 /* stdin */, TCSADRAIN, termios).expect("tcsetattr");
                    }

                    // Accept job-control-related signals (refer https://www.gnu.org/software/libc/manual/html_node/Launching-Jobs.html)
                    let action = SigAction::new(SigHandler::SigDfl, SaFlags::empty(), SigSet::empty());
                    unsafe {
                        sigaction(Signal::SIGINT, &action).expect("failed to sigaction");
                        sigaction(Signal::SIGQUIT, &action).expect("failed to sigaction");
                        sigaction(Signal::SIGTSTP, &action).expect("failed to sigaction");
                        sigaction(Signal::SIGTTIN, &action).expect("failed to sigaction");
                        sigaction(Signal::SIGTTOU, &action).expect("failed to sigaction");
                        sigaction(Signal::SIGCHLD, &action).expect("failed to sigaction");
                    }
                }

                // Initialize stdin/stdout/stderr and redirections.
                for (src, dst) in fds {
                    move_fd(src, dst);
                }

                // Set exported variables.
                for name in self.exported_names() {
                    if let Some(var) = self.get(name) {
                        std::env::set_var(name, var.as_str());
                    }
                }

                // Load assignments.
                for assignment in assignments {
                    let value = self
                        .evaluate_initializer(&assignment.initializer)
                        .expect("failed to evaluate the initializer");
                    match value {
                        Value::String(s) => std::env::set_var(&assignment.name, s),
                        Value::Array(_) => {
                            eprintln!("nsh: Array assignments in a command is not supported.");
                            std::process::exit(1);
                        },
                        Value::Function(_) => (),
                    }
                }

                execv(&argv0, &args).expect("failed to exec");
                unreachable!();
            }
        }
    }

    /// Runs an internal (builtin) command.
    pub fn run_internal_command(
        &mut self,
        argv: &[String],
        mut stdin: RawFd,
        mut stdout: RawFd,
        mut stderr: RawFd,
        redirects: &[parser::Redirection]
    ) -> Result<ExitStatus> {

        let mut opened_fds = Vec::new();
        for r in redirects {
            match r.target {
                parser::RedirectionType::Fd(ref fd) => {
                    match r.fd {
                        0 => stdin = *fd,
                        1 => stdout = *fd,
                        2 => stderr = *fd,
                        _ => (),
                    }
                },
                parser::RedirectionType::File(ref wfilepath) => {
                    let mut options = OpenOptions::new();
                    match &r.direction {
                        parser::RedirectionDirection::Input => {
                            options.read(true);
                        },
                        parser::RedirectionDirection::Output => {
                            options.write(true).create(true);
                        },
                        parser::RedirectionDirection::Append => {
                            options.write(true).append(true);
                        }
                    };

                    trace!("redirection: options={:?}", options);
                    let filepath = self.expand_word_into_string(wfilepath)?;
                    if let Ok(file) = options.open(&filepath) {
                        let src = file.into_raw_fd();
                        let dst = r.fd as RawFd;
                        opened_fds.push(src);
                        match dst {
                            0 => stdin = src,
                            1 => stdout = src,
                            2 => stderr = src,
                            _ => (),
                        }
                    } else {
                        warn!("failed to open file: `{}'", filepath);
                        return Err(Error::from(InternalCommandError::BadRedirection));
                    }
                }
                parser::RedirectionType::HereDoc(ref heredoc) => {
                    let pipe_out = self.evaluate_heredoc(heredoc)?;
                    match r.fd {
                        0 => stdin = pipe_out,
                        _ => unreachable!(),
                    }

                    opened_fds.push(pipe_out);
                }
                parser::RedirectionType::UnresolvedHereDoc(_) => {
                    // must be resolved in the parser
                    unreachable!()
                }
            }
        }

        let mut ctx = InternalCommandContext {
            argv,
            isolate: self,
            stdin: FdFile::new(stdin),
            stdout: FdFile::new(stdout),
            stderr: FdFile::new(stderr),
        };

        let cmd = argv[0].as_str();
        let result = match INTERNAL_COMMANDS.get(cmd) {
            Some(func) => func(&mut ctx),
            _ => return Err(Error::from(InternalCommandError::NotFound)),
        };

        // Close redirections.
        for fd in opened_fds {
            close(fd).expect("failed to close");
        }

        Ok(result)
    }

    /// TODO: Aliases should be expanded in the parser in order to support
    /// compound lists, e.g. alias cowsay-or-echo="cowsay hi || echo hi"
    fn expand_alias(&self, argv: &[Word]) -> Vec<Word> {
        argv
            // Get the first word.
            .get(0)
            // Get the first span in the first word.
            .and_then(|word| {
                word.spans().get(0)
            })
            // Make sure that the span is a literal (not parameters, etc.).
            .and_then(|span| {
                match span {
                    Span::Literal(lit) => Some(lit),
                    _ => None,
                }
            })
            // The very first span is literal. Search the registered aliases.
            .and_then(|lit| self.lookup_alias(lit.as_str()))
            .map(|alias_str| {
                // Found the alias. Split the alias string by whitespace into words.
                let mut alias_words: Vec<Word> = alias_str
                    .split(' ')
                    .map(|w|{
                        let span = Span::Literal(w.to_owned());
                        Word(vec![span])
                    })
                    .collect();

                // Append argv except the first word (alias name).
                for arg in argv.iter().skip(1) {
                    alias_words.push(arg.clone());
                }

                alias_words
            })
            // Failed to expand alias. Return argv as it is.
            .unwrap_or_else(|| argv.to_owned())
    }

    #[inline]
    fn is_function(&self, name: &str) -> bool {
        self.get(name)
            .map(|var| {
                match var.value() {
                    Some(Value::Function(_)) => true,
                    _ => false,
                }
            })
            .unwrap_or(false)
    }

    #[inline]
    fn call_function_in_shell_context(&mut self, name: &str, args: &[String], locals: Vec<(&str, Value)>) -> Result<ExitStatus> {
            let ctx = Context {
                stdin: 0,
                stdout: 1,
                stderr: 2,
                pgid: None,
                background: false,
                interactive: false,
            };

            self.call_function(name, &ctx, args, locals)
    }

    fn call_function(&mut self, name: &str, ctx: &Context, args: &[String], locals: Vec<(&str, Value)>) -> Result<ExitStatus> {
        if let Some(var) = self.get(name) {
            if let Some(Value::Function(ref body)) = var.value() {
                self.enter_frame();
                let frame = self.current_frame_mut();
                // Set local variables.
                for (name, value) in locals {
                    frame.set(name, value);
                }

                // $1, $2, ...
                frame.set_args(&args);

                let result = match self.run_command(&body, ctx)? {
                    ExitStatus::Return => ExitStatus::ExitedWith(0),
                    result => result,
                };
                self.leave_frame();
                return Ok(result);
            }
        }

        // No such a function.
        Ok(ExitStatus::ExitedWith(1))
    }

    fn run_simple_command(
        &mut self,
        ctx: &Context,
        argv: &[Word],
        redirects: &[parser::Redirection],
        assignments: &[parser::Assignment],
    ) -> Result<ExitStatus> {

        let argv = self.expand_words(&self.expand_alias(argv))?;
        if argv.is_empty() {
            // `argv` is empty. For example bash accepts `> foo.txt`; it creates an empty file
            // named "foo.txt".
            return Ok(ExitStatus::ExitedWith(0));
        }

        // Functions
        let argv0 = argv[0].as_str();
        if self.is_function(argv0) {
            let args: Vec<String> = argv.iter().skip(1).cloned().collect();
            return self.call_function(argv0, ctx, &args, vec![]);
        }

        // Internal commands
        let result = self.run_internal_command(&argv, ctx.stdin, ctx.stdout, ctx.stderr, redirects);
        match result {
            Ok(status) => return Ok(status),
            Err(err) => {
                match err.find_root_cause().downcast_ref::<InternalCommandError>() {
                    Some(InternalCommandError::BadRedirection) => return Ok(ExitStatus::ExitedWith(1)),
                    Some(InternalCommandError::NotFound) => (), /* Try external command. */
                    _ => return Err(err),
                }
            }
        }

        // External commands
        self.run_external_command(&ctx, argv, redirects, assignments)
    }

    fn run_local_command(&mut self, declarations: &[parser::LocalDeclaration]) -> Result<ExitStatus> {
        if self.in_global_frame() {
            eprintln!("nsh: local variable can only be used in a function");
            Ok(ExitStatus::ExitedWith(1))
        } else {
            for decl in declarations {
                match decl {
                    LocalDeclaration::Assignment(Assignment { name, initializer, .. }) => {
                        let value = self.evaluate_initializer(&initializer)?;
                        self.set(&name, value, true)
                    },
                    LocalDeclaration::Name(name) =>  {
                        self.define(name, true)
                    }
                }
            }

            Ok(ExitStatus::ExitedWith(0))
        }
    }

    fn run_if_command(
        &mut self,
        ctx: &Context,
        condition: &[parser::Term],
        then_part: &[parser::Term],
        elif_parts: &[parser::ElIf],
        else_part: &Option<Vec<parser::Term>>,
        _redirections: &[parser::Redirection]
    ) -> Result<ExitStatus> {
        // then
        let result = self.run_terms(condition, ctx.stdin, ctx.stdout, ctx.stderr);
        if result == ExitStatus::ExitedWith(0) {
            return Ok(self.run_terms(then_part, ctx.stdin, ctx.stdout, ctx.stderr));
        }

        // elif
        for elif in elif_parts {
            let result = self.run_terms(&elif.condition, ctx.stdin, ctx.stdout, ctx.stderr);
            if result == ExitStatus::ExitedWith(0) {
                return Ok(self.run_terms(then_part, ctx.stdin, ctx.stdout, ctx.stderr));
            }
        }

        // else
        if let Some(else_part) = else_part {
            return Ok(self.run_terms(else_part, ctx.stdin, ctx.stdout, ctx.stderr));
        }

        Ok(ExitStatus::ExitedWith(0))
    }

    /// Expands and merges all pattern words into a single pattern
    /// word.
    fn expand_into_single_pattern_word(&mut self, pattern: &Word) -> Result<PatternWord> {
        let mut frags = Vec::new();
        let ifs = ""; /* all whitespaces are treated as a literal */
        for word in self.expand_word_into_vec(pattern, ifs)? {
            for frag in word.fragments() {
                frags.push(frag.clone());
            }
        }

        Ok(PatternWord::new(frags))
    }

    fn replace_pattern(
        &mut self,
        pattern: &Word,
        text: &str,
        replacement: &Word,
        replace_all: bool
    ) -> Result<String> {
        let pat = self.expand_into_single_pattern_word(pattern)?;
        let dst = self.expand_word_into_string(replacement)?;
        Ok(replace_pattern(&pat, text, &dst, replace_all))
    }

    fn run_case_command(
        &mut self,
        ctx: &Context,
        word: &parser::Word,
        cases: &[parser::CaseItem],
    ) -> Result<ExitStatus> {

        let word = self.expand_word_into_string(word)?;
        for case in cases {
            for pattern in &case.patterns {
                let pattern = self.expand_into_single_pattern_word(&pattern)?;
                if match_pattern(&pattern, &word) {
                    return Ok(self.run_terms(&case.body, ctx.stdin, ctx.stdout, ctx.stderr));
                }
            }
        }

        Ok(ExitStatus::ExitedWith(0))
    }

    fn run_while_command(
        &mut self,
        ctx: &Context,
        condition: &[parser::Term],
        body: &[parser::Term],
    ) -> Result<ExitStatus> {

        let mut last_result = ExitStatus::ExitedWith(0);
        loop {
            let result = self.run_terms(condition, ctx.stdin, ctx.stdout, ctx.stderr);
            if result != ExitStatus::ExitedWith(0) {
                break;
            }

            last_result = self.run_terms(body, ctx.stdin, ctx.stdout, ctx.stderr);
        }

        Ok(last_result)
    }

    fn run_for_command(&mut self, ctx: &Context, var_name: &str, words: &[Word], body: &[parser::Term]) -> Result<ExitStatus> {
    'for_loop:
        for unexpanded_word in words {
            let expanded_words = self.expand_word_into_vec(unexpanded_word, &self.ifs())?;
            for pattern_word in expanded_words {
                for value in pattern_word.expand_glob()? {
                    self.set(&var_name, Value::String(value), false);

                    let result = self.run_terms(body, ctx.stdin, ctx.stdout, ctx.stderr);
                    match result {
                        ExitStatus::Break => break 'for_loop,
                        ExitStatus::Continue => (),
                        ExitStatus::Return => return Ok(result),
                        _ => (),
                    }
                }
            }
        }

        Ok(ExitStatus::ExitedWith(0))
    }

    fn run_arith_for_command(
        &mut self,
        ctx: &Context,
        init: &Expr,
        cond: &Expr,
        update: &Expr,
        body: &[parser::Term]
    ) -> Result<ExitStatus>
    {
        self.evaluate_expr(init);

    'for_loop:
        while self.evaluate_expr(cond) == 1 {
            let result = self.run_terms(body, ctx.stdin, ctx.stdout, ctx.stderr);
            match result {
                ExitStatus::Break => break 'for_loop,
                ExitStatus::Continue => (),
                ExitStatus::Return => return Ok(result),
                _ => (),
            }

            self.evaluate_expr(update);
        }

        Ok(ExitStatus::ExitedWith(0))
    }

    fn run_command(&mut self, command: &parser::Command, ctx: &Context) -> Result<ExitStatus> {
        if self.noexec {
            return Ok(ExitStatus::NoExec);
        }

        trace!("run_command: {:?}", command);
       let result = match command {
            parser::Command::SimpleCommand { argv, redirects, assignments } => {
                self.run_simple_command(ctx, &argv, &redirects, &assignments)?
            }
            parser::Command::If { condition, then_part, elif_parts, else_part, redirects } => {
                self.run_if_command(ctx, &condition, &then_part, &elif_parts, &else_part, &redirects)?
            }
            parser::Command::While { condition, body } => {
                self.run_while_command(ctx, &condition, &body)?
            }
            parser::Command::Case { word, cases } => {
                self.run_case_command(ctx, &word, &cases)?
            }
            parser::Command::For { var_name, words, body } => {
                self.run_for_command(ctx, var_name, &words, &body)?
            },
            parser::Command::ArithFor { init, cond, update, body } => {
                self.run_arith_for_command(ctx, init, cond, update, &body)?
            },
            parser::Command::LocalDef { declarations } => {
                self.run_local_command(&declarations)?
            }
            parser::Command::FunctionDef { name, body } => {
                self.set(name, Value::Function(body.clone()), true);
                ExitStatus::ExitedWith(0)
            }
            parser::Command::Assignment { assignments } => {
                for assign in assignments {
                    let value = self.evaluate_initializer(&assign.initializer)?;
                    self.assign(&assign.name, value)
                }
                ExitStatus::ExitedWith(0)
            },
            parser::Command::Cond(expr) => {
                let result = self.evaluate_cond(expr)?;
                if result {
                    ExitStatus::ExitedWith(0)
                } else {
                    ExitStatus::ExitedWith(1)
                }
            },
            parser::Command::Group { terms } => {
                self.run_terms(terms, ctx.stdin, ctx.stdout, ctx.stderr)
            },
            parser::Command::SubShellGroup { terms } => {
                let pid = self.spawn_subshell(terms, ctx)?;
                let status = wait_child(pid).unwrap_or(1);
                ExitStatus::ExitedWith(status)
            },
            parser::Command::Return { status } => {
                if let Some(status) = status {
                    self.last_status = *status;
                }

                ExitStatus::Return
            }
            parser::Command::Break => {
                ExitStatus::Break
            }
            parser::Command::Continue => {
                ExitStatus::Continue
            }
        };

        Ok(result)
    }

    /// Runs commands in a subshell (`$()` or `<()`).
    pub fn eval_in_subshell(&mut self, terms: &[parser::Term]) -> Result<(i32, i32)> {
        let (pipe_out, pipe_in) = pipe().expect("failed to create a pipe");

        let ctx = Context {
                stdin: 0,
                stdout: pipe_in,
                stderr: 2,
                pgid: None,
                background: false,
                interactive: false,
        };

        let pid = self.spawn_subshell(terms, &ctx)?;
        close(pipe_in).ok();
        let status = wait_child(pid).unwrap_or(1);
        Ok((status, pipe_out))
    }


    fn spawn_subshell(&mut self, terms: &[parser::Term], ctx: &Context) -> Result<Pid> {
        // Since child process has its own isolated address space, `RELOAD_WORK.wait()`
        // would block forever. Wait for the path loader before forking to make sure
        // that `RELOAD_WORK.wait()` does not block.
        wait_for_path_loader();

        match fork().expect("failed to fork") {
            ForkResult::Parent { child } => {
                Ok(child)
            },
            ForkResult::Child => {
                let status = match self.run_terms(terms, ctx.stdin, ctx.stdout, ctx.stderr) {
                    ExitStatus::ExitedWith(status) => status,
                    _ => 1,
                };

                std::process::exit(status);
            }
        }
    }

    // Creates a pipeline and runs commands.
    fn run_pipeline(
        &mut self,
        code: &str,
        pipeline: &parser::Pipeline,
        pipeline_stdin: RawFd,
        pipeline_stdout: RawFd,
        stderr: RawFd,
        background: bool,
    ) -> ExitStatus {
        // Invoke commands in a pipeline.
        let mut last_result = None;
        let mut iter = pipeline.commands.iter().peekable();
        let mut childs = Vec::new();
        let mut stdin = pipeline_stdin;
        let mut pgid = None;
        while let Some(command) = iter.next() {
            let stdout;
            let pipes = if iter.peek().is_some() {
                // There is a next command in the pipeline (e.g. date in
                // `date | hexdump`). Create and connect a pipe.
                let (pipe_out, pipe_in) = pipe().expect("failed to create a pipe");
                stdout = pipe_in;
                Some((pipe_out, pipe_in))
            } else {
                // The last command in the pipeline.
                stdout = pipeline_stdout;
                None
            };

            let result = self.run_command(command, &Context {
                stdin,
                stdout,
                stderr,
                pgid,
                background,
                interactive: self.interactive
            });

            if let Some((pipe_out, pipe_in)) = pipes {
                stdin = pipe_out;
                // `pipe_in` is used by a child process and is no longer needed.
                close(pipe_in).expect("failed to close pipe_in");
            }

            last_result = match result {
                Ok(ExitStatus::Running(pid)) => {
                    if pgid.is_none() {
                        // The first child (the process group leader) pid is used for pgid.
                        pgid = Some(pid);
                    }

                    if self.interactive {
                        setpgid(pid, pgid.unwrap()).expect("failed to setpgid");
                    }

                    childs.push(pid);
                    Some(ExitStatus::Running(pid))
                },
                Ok(ExitStatus::ExitedWith(status)) => {
                    Some(ExitStatus::ExitedWith(status))
                },
                Ok(ExitStatus::Break) => {
                    last_result = Some(ExitStatus::Break);
                    break;
                },
                Ok(ExitStatus::Continue) => {
                    last_result = Some(ExitStatus::Continue);
                    break;
                },
                Ok(ExitStatus::Return) => {
                    last_result = Some(ExitStatus::Return);
                    break;
                },
                Ok(ExitStatus::NoExec) => {
                    last_result = Some(ExitStatus::NoExec);
                    break;
                },
                Err(err) => {
                    if err.find_root_cause().downcast_ref::<NoMatchesError>().is_some() {
                        eprintln!("nsh: error: no matches");
                        last_result = Some(ExitStatus::ExitedWith(1));
                        break;
                    }

                    // TODO:
                    unreachable!();
                },
            };
        }

        // Wait for the last command in the pipeline.
        let last_status = match last_result {
            Some(ExitStatus::ExitedWith(status)) => {
                self.last_status = status;
                ExitStatus::ExitedWith(status)
            },
            Some(ExitStatus::Running(_)) => {
                let cmd_name = code.to_owned();
                let job = self.create_job(cmd_name, pgid.unwrap(), childs);

                if !self.interactive {
                    if background {
                        // Update `$!`.
                        self.run_in_background(&job, false);
                    }

                    match self.wait_for_job(&job) {
                        ProcessState::Completed(status) => {
                            self.last_status = status;
                            ExitStatus::ExitedWith(status)
                        },
                        ProcessState::Stopped(_) => {
                            ExitStatus::Running(pgid.unwrap())
                        },
                        _ => unreachable!(),
                    }
                } else if background {
                    self.run_in_background(&job, false);
                    ExitStatus::Running(pgid.unwrap())
                } else {
                    match self.run_in_foreground(&job, false) {
                        ProcessState::Completed(status) => {
                            ExitStatus::ExitedWith(status)
                        },
                        ProcessState::Stopped(_) => {
                            ExitStatus::Running(pgid.unwrap())
                        },
                        _ => unreachable!(),
                    }
                }
            },
            Some(ExitStatus::Break) => {
                return ExitStatus::Break;
            },
            Some(ExitStatus::Continue) => {
                return ExitStatus::Continue;
            },
            Some(ExitStatus::Return) => {
                return ExitStatus::Return;
            },
            Some(ExitStatus::NoExec) => {
                return ExitStatus::NoExec;
            },
            None => {
                trace!("nothing to execute");
                ExitStatus::ExitedWith(0)
            }
        };

        if self.errexit {
            if let ExitStatus::ExitedWith(status) = last_status {
                if status != 0 {
                    std::process::exit(status);
                }
            }
        }

        last_status
    }

    /// Runs pipelines.
    pub fn run_terms(
        &mut self,
        terms: &[parser::Term],
        stdin: RawFd,
        stdout: RawFd,
        stderr: RawFd,
    ) -> ExitStatus {
        let mut last_status = ExitStatus::ExitedWith(0);
        for term in terms {
            for pipeline in &term.pipelines {
                // Should we execute the pipline?
                match (last_status, &pipeline.run_if) {
                    (ExitStatus::ExitedWith(0), RunIf::Success) => (),
                    (ExitStatus::ExitedWith(_), RunIf::Failure) => (),
                    (ExitStatus::Break, _) => return ExitStatus::Break,
                    (ExitStatus::Continue, _) => return ExitStatus::Continue,
                    (ExitStatus::Return, _) => return ExitStatus::Return,
                    (_, RunIf::Always) => (),
                    _ => continue,
                }

                last_status = self.run_pipeline(&term.code, pipeline, stdin, stdout, stderr, term.background);
            }
        }

        last_status
    }

    /// Run CondEx command (`[[ ... ]]`).
    pub fn evaluate_cond_primary(&mut self, cond: &CondExpr) -> Result<String> {
        match cond {
            CondExpr::Word(word) => {
                self.expand_word_into_string(word)
            },
            _ => {
                Err(format_err!("cond: expected word"))
            }
        }
    }

    /// Run CondEx command (`[[ ... ]]`).
    pub fn evaluate_cond(&mut self, cond: &CondExpr) -> Result<bool> {
        macro_rules! eval_as_string {
            ($expr:expr) => {
                self.evaluate_cond_primary($expr)?
            };
        }

        macro_rules! unwrap_word {
            ($expr:expr) => {
                match $expr {
                    CondExpr::Word(word) => {
                        word
                    },
                    _ => {
                        return Err(format_err!("cond: expected word"));
                    }
                }
            };
        }

        macro_rules! eval_as_bool {
            ($expr:expr) => {
                self.evaluate_cond($expr)?
            };
        }

        macro_rules! parse_as_int {
            ($expr:expr) => {
                self.evaluate_cond_primary($expr)?
                    .parse().unwrap_or(0) as i32
            };
        }

        let result = match cond {
            CondExpr::And(lhs, rhs) => eval_as_bool!(lhs) && eval_as_bool!(rhs),
            CondExpr::Or(lhs, rhs) => eval_as_bool!(lhs) || eval_as_bool!(rhs),
            CondExpr::StrEq(lhs, rhs) => {
                let pat = self.expand_into_single_pattern_word(unwrap_word!(rhs.as_ref()))?;
                match_pattern_all(&pat, &eval_as_string!(lhs))
            }
            CondExpr::StrNe(lhs, rhs) => {
                let pat = self.expand_into_single_pattern_word(unwrap_word!(rhs.as_ref()))?;
                !match_pattern_all(&pat, &eval_as_string!(lhs))
            }
            CondExpr::Eq(lhs, rhs) => parse_as_int!(lhs) == parse_as_int!(rhs),
            CondExpr::Ne(lhs, rhs) => parse_as_int!(lhs) != parse_as_int!(rhs),
            CondExpr::Lt(lhs, rhs) => parse_as_int!(lhs) < parse_as_int!(rhs),
            CondExpr::Le(lhs, rhs) => parse_as_int!(lhs) <= parse_as_int!(rhs),
            CondExpr::Gt(lhs, rhs) => parse_as_int!(lhs) > parse_as_int!(rhs),
            CondExpr::Ge(lhs, rhs) => parse_as_int!(lhs) >= parse_as_int!(rhs),
            CondExpr::Word(_) => true,
        };

        Ok(result)
    }

    /// Runs commands.
    pub fn eval(&mut self, ast: &Ast) -> ExitStatus {
        trace!("ast: {:#?}", ast);

        // Inherit shell's stdin/stdout/stderr.
        let stdin = 0;
        let stdout = 1;
        let stderr = 2;
        self.run_terms(&ast.terms, stdin, stdout, stderr)
    }

    /// Parses and runs a shell script file.
    pub fn run_file(&mut self, script_file: PathBuf) -> ExitStatus {
        let mut f = File::open(script_file).expect("failed to open a file");
        let mut script = String::new();
        f.read_to_string(&mut script)
            .expect("failed to load a file");

        self.run_str(script.as_str())
    }

    /// Parses and runs a script.
    pub fn run_str(&mut self, script: &str) -> ExitStatus {
        match parser::parse(script) {
            Ok(ast) => {
                self.eval(&ast)
            },
            Err(parser::ParseError::Empty) => {
                // Just ignore.
                ExitStatus::ExitedWith(0)
            },
            Err(parser::ParseError::Fatal(err)) => {
                eprintln!("nsh: parse error: {}", err);
                ExitStatus::ExitedWith(-1)
            }
        }
    }
}

#[test]
fn test_expr() {
    let mut isolate = Isolate::new("nsh", false);
    assert_eq!(
        isolate.evaluate_expr(&&Expr::Mul(BinaryExpr {
            lhs: Box::new(Expr::Literal(2)),
            rhs: Box::new(Expr::Add(BinaryExpr {
                lhs: Box::new(Expr::Literal(3)),
                rhs: Box::new(Expr::Literal(7)),
            })),
        })),
        2 * (3 + 7)
    );

    isolate.set("x", Value::String(3.to_string()), false);
    assert_eq!(
        isolate.evaluate_expr(&&Expr::Add(BinaryExpr {
            lhs: Box::new(Expr::Literal(1)),
            rhs: Box::new(Expr::Add(BinaryExpr {
                lhs: Box::new(Expr::Mul(BinaryExpr {
                    lhs: Box::new(Expr::Literal(2)),
                    rhs: Box::new(Expr::Parameter { name: "x".into() }),
                })),
                rhs: Box::new(Expr::Literal(4)),
            })),
        })),
        1 + 2 * 3 + 4
    );
}
