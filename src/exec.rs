use crate::builtins::{InternalCommandContext, INTERNAL_COMMANDS, InternalCommandError};
use crate::parser::{
    self, Ast, ExpansionOp, RunIf, Expr, BinaryExpr, Span, Word, Initializer,
    LocalDeclaration, Assignment
};
use crate::path::lookup_external_command;
use crate::utils::FdFile;
use nix;
use nix::sys::wait::{waitpid, WaitStatus, WaitPidFlag};
use nix::sys::signal::{kill, SigHandler, SigAction, SaFlags, SigSet, Signal, sigaction};
use nix::sys::termios::{tcgetattr, tcsetattr, Termios, SetArg::TCSADRAIN};
use nix::unistd::{close, dup2, execv, fork, pipe, ForkResult, Pid, getpid, setpgid, tcsetpgrp};
use std::collections::{HashMap, HashSet};
use std::ffi::CString;
use std::fs::{File, OpenOptions};
use std::path::PathBuf;
use std::io::prelude::*;
use std::os::unix::io::IntoRawFd;
use std::os::unix::io::FromRawFd;
use std::os::unix::io::RawFd;
use std::sync::Arc;
use std::cell::RefCell;
use glob::glob;

fn kill_process_group(pgid: Pid, signal: Signal) -> Result<(), nix::Error> {
    kill(Pid::from_raw(-pgid.as_raw()), signal)
}

#[derive(Debug)]
pub enum Value {
    String(String),
    Array(Vec<String>),
    Function(Box<parser::Command>),
}

#[derive(Debug)]
pub struct Variable {
    value: Value,
    is_local: bool,
}

impl Variable {
    pub fn new(value: Value, is_local: bool) -> Variable {
        Variable {
            value,
            is_local,
        }
    }

    // References value as `$foo`.
    pub fn value<'a>(&'a self) -> &'a str {
        match &self.value {
            Value::String(value) => value,
            Value::Function(_) => "(function)",
            // Bash returns the first element in the array.
            Value::Array(elems) => {
                match elems.get(0) {
                    Some(elem) => elem.as_str(),
                    _ => "",
                }
            }
        }
    }

    // References value as `$foo[expr]`.
    pub fn value_at<'a>(&'a self, index: usize) -> &'a str {
        match &self.value {
            Value::Array(elems) => {
                match elems.get(index) {
                    Some(elem) => elem.as_str(),
                    _ => "",
                }
            },
            _ => "",
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum ExitStatus {
    ExitedWith(i32),
    Background(Pid /* pgid */),
    Break,
    Continue,
    Return,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum ProcessState {
    Running,
    /// Contains the exit status.
    Completed(i32),
    Stopped(Pid),
}

pub struct Job {
    id: usize,
    pgid: Pid,
    cmd: String,
    processes: Vec<Pid>,
    termios: RefCell<Option<Termios>>,
}

impl Job {
    pub fn new(id: usize, pgid: Pid, cmd: String, processes: Vec<Pid>) -> Job {
        Job {
            id,
            pgid,
            cmd,
            processes,
            termios: RefCell::new(None),
        }
    }

    #[inline]
    pub fn id(&self) -> usize {
        self.id
    }

    #[inline]
    pub fn cmd(&self) -> &str {
        self.cmd.as_str()
    }

    pub fn state(&self, env: &Env) -> &'static str {
        if self.completed(env) {
            "done"
        } else if self.stopped(env) {
            "stopped"
        } else {
            "running"
        }
    }

    pub fn completed(&self, env: &Env) -> bool {
        self.processes.iter().all(|pid| {
            let state = env.get_process_state(*pid).unwrap();
            match state {
                ProcessState::Completed(_) => true,
                _ => false,
            }
        })
    }

    pub fn stopped(&self, env: &Env) -> bool {
        self.processes.iter().all(|pid| {
            let state = env.get_process_state(*pid).unwrap();
            match state {
                ProcessState::Stopped(_) => true,
                _ => false,
            }
        })
    }
}

#[derive(Debug, Copy, Clone)]
struct Context {
    stdin: RawFd,
    stdout: RawFd,
    stderr: RawFd,
    pgid: Option<Pid>,
    background: bool,
    interactive: bool,
}

#[derive(Debug, Copy, Clone)]
enum CommandResult {
    External { pid: Pid },
    Internal { status: ExitStatus },
    // break command
    Break,
    // continue command
    Continue,
    // return command
    Return,
    // The command is not executed because of `noexec`.
    NoExec,
}

#[derive(Debug)]
pub struct Frame {
    vars: HashMap<String, Arc<Variable>>,
}

impl Frame {
    pub fn new() -> Frame {
        Frame {
            vars: HashMap::new(),
        }
    }

    pub fn set(&mut self, key: &str, value: Variable) {
        self.vars.insert(key.into(), Arc::new(value));
    }

    pub fn remove(&mut self, key: &str) -> Option<Arc<Variable>> {
        self.vars.remove(key.into())
    }

    pub fn get<'a, 'b>(&'a self, key: &'b str) -> Option<Arc<Variable>> {
        self.vars.get(key).cloned()
    }
}

pub struct Env {
    shell_pgid: Pid,
    interactive: bool,
    shell_termios: Option<Termios>,

    last_status: i32,
    /// Global scope.
    global: Frame,
    /// Local scopes (variables declared with `local').
    frames: Vec<Frame>,
    exported: HashSet<String>,
    aliases: HashMap<String, Vec<Word>>,

    // Shell options.
    pub errexit: bool,
    pub nounset: bool,
    pub noexec: bool,

    /// Key is the job ID.
    jobs: HashMap<usize, Arc<Job>>,
    last_fore_job: Option<Arc<Job>>,
    /// Key is a pid.
    states: HashMap<Pid, ProcessState>,
    pid_job_mapping: HashMap<Pid, Arc<Job>>,
}

impl Env {
    pub fn new(shell_pgid: Pid, interactive: bool) -> Env {
        let shell_termios = if interactive {
            Some(tcgetattr(0 /* stdin */).expect("failed to tcgetattr"))
        } else {
            None
        };

        Env {
            shell_pgid,
            interactive,
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
            states: HashMap::new(),
            pid_job_mapping: HashMap::new(),
            last_fore_job: None,
        }
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
        self.frames.len() == 0
    }

    #[inline]
    pub fn current_frame(&self) -> &Frame {
        self.frames.last().unwrap_or(&self.global)
    }

    #[inline]
    pub fn current_frame_mut(&mut self) -> &mut Frame {
        self.frames.last_mut().unwrap_or(&mut self.global)
    }

    pub fn set(&mut self, key: &str, value: Value, is_local: bool) {
        let frame = if is_local {
            self.current_frame_mut()
        } else {
            &mut self.global
        };

        frame.set(key, Variable::new(value, is_local));
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

    pub fn get<'a, 'b>(&'a self, key: &'b str) -> Option<Arc<Variable>> {
        if let Some(var) = self.current_frame().get(key) {
            Some(var)
        } else {
            self.global.get(key)
        }
    }

    #[inline]
    pub fn get_str<'a, 'b>(&'a self, key: &'a str) -> Option<String> {
        match self.get(key) {
            Some(var) => {
                match var.value {
                    Value::String(ref s) => Some(s.clone()),
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

    pub fn add_alias(&mut self, name: &str, words: Vec<Word>) {
        self.aliases.insert(name.to_string(), words);
    }

    pub fn lookup_alias(&self, alias: &str) -> Option<Vec<Word>> {
        self.aliases.get(&alias.to_string()).cloned()
    }

    #[inline]
    pub fn jobs(&self) -> Vec<Arc<Job>> {
        let mut jobs = Vec::new();
        for job in self.jobs.values() {
            jobs.push(job.clone());
        }

        jobs
    }

    pub fn alloc_job_id(&mut self) -> usize {
        let mut id = 1;
        while self.jobs.contains_key(&id) {
            id += 1;
        }

        id
    }

    #[inline]
    pub fn last_fore_job(&self) -> Option<Arc<Job>> {
        self.last_fore_job.as_ref().map(|job| job.clone())
    }

    pub fn continue_job(&mut self, job_id: usize, background: bool) {
        // Mark all stopped processes as running.
        let job = self.jobs.get(&job_id).unwrap().clone();
        for proc in &job.processes {
            match self.get_process_state(*proc).unwrap() {
                &ProcessState::Stopped(_) => {
                    self.set_process_state(*proc, ProcessState::Running);
                },
                _ => (),
            }
        }

        if background {
            self.run_in_background(job_id, true);
        } else {
            self.run_in_foreground(job_id, true);
        }
    }

    pub fn run_in_foreground(&mut self, job_id: usize, sigcont: bool) -> ProcessState {
        let job = self.jobs.get(&job_id).unwrap().clone();
        self.last_fore_job = Some(job.clone());

        // Put the job into the foreground.
        tcsetpgrp(0 /* stdin */, job.pgid).expect("failed to tcsetpgrp");

        if sigcont {
            if let Some(ref termios) = *job.termios.borrow() {
                tcsetattr(0 /* stdout */, TCSADRAIN, termios).expect("failed to tcsetattr");
            }
            kill_process_group(job.pgid, Signal::SIGCONT).expect("failed to kill(SIGCONT)");
            trace!("sent sigcont");
        }

        // Wait for the job to exit.
        let status = self.wait_for_job(job.clone());

        // Go back into the shell.
        job.termios.replace(Some(tcgetattr(0 /* stdin */).expect("failed to tcgetattr")));
        let termios = self.shell_termios.as_ref().unwrap();
        tcsetpgrp(0 /* stdin */, self.shell_pgid).expect("failed to tcsetpgrp");
        tcsetattr(0 /* stdin */, TCSADRAIN, termios).expect("failed to tcgetattr");

        status
    }

    pub fn run_in_background(&mut self, job_id: usize, sigcont: bool) {
        let job = self.jobs.get(&job_id).unwrap().clone();
        if sigcont {
            kill_process_group(job.pgid, Signal::SIGCONT).expect("failed to kill(SIGCONT)");
        }
    }

    /// Waits for all processes in the job to exit. Note that the job will be
    /// deleted from `Env` if the process has exited.
    pub fn wait_for_job(&mut self, job: Arc<Job>) -> ProcessState {
        loop {
            if job.completed(self) || job.stopped(self) {
                break;
            }

            self.wait_for_any_process();
        }

        // Get the exit status of the last process.
        let state = self.get_process_state(*job.processes.iter().last().unwrap()).cloned();

        match state {
            Some(ProcessState::Completed(_)) => {
                // Remove the job and processes from the list.
                for proc in &*job.processes {
                    self.pid_job_mapping.remove(&proc);
                }
                self.jobs.remove(&job.id).unwrap();

                if let Some(ref last_job) = self.last_fore_job {
                    if job.id == last_job.id {
                        self.last_fore_job = None;
                    }
                }

                state.unwrap()
            },
            Some(ProcessState::Stopped(_)) => {
                eprintln!("nsh: [{}] Stopped: {}", job.id, job.cmd);
                state.unwrap()
            },
            _ => unreachable!(),
        }
    }

    /// Updates the process state.
    fn set_process_state(&mut self, pid: Pid, state: ProcessState) {
        self.states.insert(pid, state);
    }

    /// Returns the process state.
    fn get_process_state(&self, pid: Pid) -> Option<&ProcessState> {
        self.states.get(&pid)
    }

    /// Waits for an *any* process, i.e. `waitpid(-1)` and updates
    /// the process state recorded in the `Env`.
    pub fn wait_for_any_process(&mut self) -> Pid {
        let result = waitpid(None, Some(WaitPidFlag::WUNTRACED));
        let (pid, state) = match result {
            Ok(WaitStatus::Exited(pid, status)) => {
                trace!("nsh: pid={} status={}", pid, status);
                (pid, ProcessState::Completed(status))
            },
            Ok(WaitStatus::Signaled(pid, _signal, _)) => {
                // The `pid` process has been killed by `_signal`.
                (pid, ProcessState::Completed(-1))
            },
            Ok(WaitStatus::Stopped(pid, _signal)) => {
                (pid, ProcessState::Stopped(pid))
            },
            status => {
                // TODO:
                panic!("unexpected waitpid event: {:?}", status);
            }
        };

        self.set_process_state(pid, state);
        pid
    }

    fn exec_external_command(&mut self, ctx: &Context, argv: Vec<String>, redirects: &[parser::Redirection]) -> CommandResult {
        let mut fds = Vec::new();
        for r in redirects {
            match r.target {
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
                    let filepath = self.evaluate_word_into_string(wfilepath);
                    if let Ok(file) = options.open(&filepath) {
                        fds.push((file.into_raw_fd(), r.fd as RawFd))
                    } else {
                        warn!("nsh: failed to open file: `{}'", filepath);
                        return CommandResult::Internal { status: ExitStatus::ExitedWith(1) };
                    }
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
        let argv0 = match lookup_external_command(&argv[0]) {
            Some(argv0) => CString::new(argv0).unwrap(),
            None => {
                eprintln!("nsh: command not found `{}'", argv[0]);
                return CommandResult::Internal { status: ExitStatus::ExitedWith(1) };
            }
        };

        // Spawn a child.
        match fork().expect("failed to fork") {
            ForkResult::Parent { child } => {
                CommandResult::External { pid: child }
            },
            ForkResult::Child => {
                // FIXME: CString::new() internally calls memchr(); it could be non-negligible cost.
                let mut args = Vec::new();
                for arg in argv {
                    args.push(CString::new(arg).unwrap());
                }

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
                        std::env::set_var(name, var.value());
                    }
                }

                // TODO: inherit exported variables
                execv(&argv0, &args).expect("failed to exec");
                unreachable!();
            }
        }
    }

    pub fn run_internal_command(
        &mut self,
        argv: &[String],
        stdin: RawFd,
        stdout: RawFd,
        stderr: RawFd
    ) -> Result<ExitStatus, InternalCommandError> {

        let mut ctx = InternalCommandContext {
            argv,
            env: self,
            stdin: FdFile::new(stdin),
            stdout: FdFile::new(stdout),
            stderr: FdFile::new(stderr),
        };

        let cmd = argv[0].as_str();
        match INTERNAL_COMMANDS.get(cmd) {
            Some(func) => Ok(func(&mut ctx)),
            _ => Err(InternalCommandError::NotFound),
        }
    }

    fn eval_simple_command(&mut self, ctx: &Context, argv: Vec<String>, redirects: &[parser::Redirection]) -> CommandResult {
        if argv.is_empty() {
            // `argv` is empty. For example bash accepts `> foo.txt`; it creates an empty file
            // named "foo.txt".
            return CommandResult::Internal { status: ExitStatus::ExitedWith(0) };
        }

        // Functions
        if let Some(ref var) = self.get(argv[0].as_str()) {
            match var.as_ref().value {
                Value::Function(ref body) => {
                    self.enter_frame();

                    // Set $1, $2, ...
                    for (i, arg) in argv.iter().enumerate() {
                        self.set(&i.to_string(), Value::String(arg.clone()), true);
                    }

                    let result = match self.run_command(&body, ctx) {
                        CommandResult::Return => CommandResult::Internal { status: ExitStatus::ExitedWith(0) },
                        result => result,
                    };

                    self.leave_frame();
                    return result;
                },
                // The variable is not a function. `argv[0]` is am internal or
                // external command.
                _ => (),
            }
        }

        // Internal commands
        match self.run_internal_command(&argv, ctx.stdin, ctx.stdout, ctx.stderr) {
            Ok(status) => {
                return CommandResult::Internal { status };
            }
            Err(InternalCommandError::NotFound) => (), /* Try external command. */
        }

        // External commands
        self.exec_external_command(&ctx, argv, redirects)
    }

    fn run_command(&mut self, command: &parser::Command, ctx: &Context) -> CommandResult {
        if self.noexec {
            return CommandResult::NoExec;
        }

        trace!("run_command: {:?}", command);
        match command {
            parser::Command::SimpleCommand { argv: ref_wargv, redirects, ..} => {
                // FIXME: refactor
                let Word(ref spans) = ref_wargv[0];
                let wargv = if !spans.is_empty() {
                    match spans[0] {
                        Span::Literal(ref lit) => {
                            if let Some(alias_argv) = self.lookup_alias(lit.as_str()) {
                                let mut new_wargv = Vec::new();
                                new_wargv.extend(alias_argv);
                                new_wargv.extend(ref_wargv.iter().skip(1).cloned());
                                new_wargv
                            } else {
                                ref_wargv.clone()
                            }
                        }
                        _ => ref_wargv.clone(),
                    }
                } else {
                    ref_wargv.clone()
                };

                let argv = self.evaluate_words(&wargv);
                if argv.is_empty() {
                    return CommandResult::Internal { status: ExitStatus::ExitedWith(0) };
                }

                self.eval_simple_command(ctx, argv, redirects)
            }
            parser::Command::Assignment { assignments } => {
                for assign in assignments {
                    let value = self.evaluate_initializer(&assign.initializer);
                    self.set(&assign.name, value, false)
                }
                CommandResult::Internal { status: ExitStatus::ExitedWith(0) }
            },
            parser::Command::LocalDef { declarations } => {
                if self.in_global_frame() {
                    eprintln!("nsh: local variable can only be used in a function");
                    CommandResult::Internal { status: ExitStatus::ExitedWith(1) }
                } else {
                    for decl in declarations {
                        match decl {
                            LocalDeclaration::Assignment(Assignment { name, initializer, .. }) => {
                                let value = self.evaluate_initializer(&initializer);
                                self.set(&name, value, true)
                            },
                            LocalDeclaration::Name(name) =>  {
                                self.set(name, Value::String("".to_string()), true)
                            }
                        }
                    }
                    CommandResult::Internal { status: ExitStatus::ExitedWith(0) }
                }
            }
            parser::Command::FunctionDef { name, body } => {
                self.set(&name, Value::Function(body.clone()), true);
                CommandResult::Internal { status: ExitStatus::ExitedWith(0) }
            }
            parser::Command::If {
                condition,
                then_part,
                ..
            } => {
                // TODO: else, elif
                let result = self.run_terms(condition, ctx.stdin, ctx.stdout, ctx.stderr);
                if result == ExitStatus::ExitedWith(0) {
                    CommandResult::Internal {
                        status: self.run_terms(then_part, ctx.stdin, ctx.stdout, ctx.stderr),
                    }
                } else {
                    CommandResult::Internal { status: ExitStatus::ExitedWith(0) }
                }
            }
            parser::Command::Group { terms } => CommandResult::Internal {
                status: self.run_terms(terms, ctx.stdin, ctx.stdout, ctx.stderr),
            },
            parser::Command::For { var_name, words, body } => {
                for word in words {
                    let value = self.evaluate_word_into_string(word);
                    self.set(&var_name, Value::String(value), false);

                    let result = self.run_terms(body, ctx.stdin, ctx.stdout, ctx.stderr);
                    match result {
                        ExitStatus::Break => break,
                        ExitStatus::Continue => (),
                        ExitStatus::Return => return CommandResult::Internal { status: result },
                        _ => (),
                    }
                }

                CommandResult::Internal { status: ExitStatus::ExitedWith(0) }
            },
            parser::Command::Return => {
                CommandResult::Return
            }
            parser::Command::Break => {
                CommandResult::Break
            }
            parser::Command::Continue => {
                CommandResult::Continue
            }
            parser::Command::Case {..} => {
                // TODO:
                unimplemented!();
            }
        }
    }


    fn evaluate_expr(&self, expr: &Expr) -> i32 {
        match expr {
            Expr::Expr(sub_expr) => self.evaluate_expr(sub_expr),
            Expr::Literal(value) => *value,
            Expr::Parameter { name } => {
                if let Some(var) = self.get(name) {
                    match &var.value {
                        Value::String(s) => s.parse().unwrap_or(0),
                        _ => 0,
                    }
                } else {
                    0
                }
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
        }
    }

    fn expand_param(&mut self, name: &str, op: &ExpansionOp) -> String {
        match name {
            "?" => {
                return self.last_status.to_string();
            },
            _ => {
                if let Some(var) = self.get(name) {
                    // $<name> is defined and contains a string value.
                    let value = var.value().to_string();
                    return match op {
                        ExpansionOp::Length => value.len().to_string(),
                        _ => value,
                    };
                }
            }
        }

        // $<name> is not defined.
        match op {
            ExpansionOp::Length => {
                if self.nounset {
                    eprintln!("nsh: undefined variable `{}'", name);
                    std::process::exit(1);
                }

                "0".to_owned()
            },
            ExpansionOp::GetOrEmpty => {
                if self.nounset {
                    eprintln!("nsh: undefined variable `{}'", name);
                    std::process::exit(1);
                }

                "".to_owned()
            },
            ExpansionOp::GetOrDefault(word) => self.evaluate_word_into_string(word),
            ExpansionOp::GetOrDefaultAndAssign(word) => {
                let content = self.evaluate_word_into_string(word);
                self.set(name, Value::String(content.clone()), false);
                content
            }
            _ => panic!("TODO:"),
        }
    }


    fn evaluate_word_into_vec(&mut self, word: &Word, ifs: &str) -> Vec<String> {
        let mut words = Vec::new();
        for span in &word.0 {
            let (frag, expand) = match span {
                Span::Literal(s) => (s.clone(), false),
                Span::Parameter { name, op, quoted } => {
                    (self.expand_param(name, op), !quoted)
                },
                Span::ArrayParameter { name, index, quoted } => {
                    let index = self.evaluate_expr(index);
                    let frag = if index < 0 {
                        // TODO: return Err instead.
                        eprintln!("nsh: {}: the index must be larger than or equals 0", name);
                        "".to_owned()
                    } else {
                        self
                            .get(name)
                            .map(|v| v.value_at(index as usize).to_string())
                            .unwrap_or_else(|| "".to_owned())
                    };

                    (frag, !quoted)
                },
                Span::ArithExpr { expr } => {
                    (self.evaluate_expr(expr).to_string(), false)
                },
                Span::Tilde(user) => {
                    if user.is_some() {
                        // TODO: e.g. ~mike, ~chandler/venus
                        unimplemented!();
                    }

                    (dirs::home_dir().unwrap().to_str().unwrap().to_owned(), false)
                },
                Span::Command { body, quoted } => {
                    let stdout = self.exec_in_subshell(body);
                    // TODO: support binary output
                    let s = std::str::from_utf8(&stdout).unwrap_or("").to_string();
                    (s.trim_end_matches('\n').to_string(), !quoted)
                },
                Span::AnyChar => {
                    ("?".into(), false)
                },
                Span::AnyString => {
                    ("*".into(), false)
                },
            };

            // Expand `a${foo}b` into words: `a1` `2` `3b`, where `$foo=123`.
            if !frag.is_empty() {
                if expand {
                    for word in frag.split(|c| ifs.contains(c)) {
                        words.push(word.to_string());
                    }
                } else {
                    words.push(frag);
                }
            }
        }

        let mut glob_expanded = Vec::new();
        for word in words {
            // FIXME: Support file path which contains '*'.
            if word.contains('*') || word.contains('?') {
                glob_expanded.extend(expand_glob(&word))
            } else {
                glob_expanded.push(word);
            };
        }

        glob_expanded
    }

    fn evaluate_word_into_string(&mut self, word: &Word) -> String {
        let ifs = self.get_str("IFS").unwrap_or_else(|| "\t ".to_owned());
        self.evaluate_word_into_vec(word, &ifs).join(" ")
    }

    fn evaluate_words(&mut self, words: &[Word]) -> Vec<String> {
        let mut evaluated = Vec::new();
        let ifs = self.get_str("IFS").unwrap_or_else(|| "\t ".to_owned());
        for word in words {
            evaluated.extend(self.evaluate_word_into_vec(word, &ifs));
        }

        evaluated
    }

    fn evaluate_initializer(&mut self, initializer: &Initializer) -> Value {
        match initializer {
            Initializer::String(ref word) => Value::String(self.evaluate_word_into_string(word)),
            Initializer::Array(ref words) =>  {
                let mut elems = Vec::new();
                for word in words {
                    elems.push(self.evaluate_word_into_string(word));
                }
                Value::Array(elems)
            }
        }
    }

    pub fn exec_in_subshell(&mut self, terms: &[parser::Term]) -> Vec<u8> {
        let (pipe_out, pipe_in) = pipe().unwrap();
        match fork().expect("failed to fork") {
            ForkResult::Parent { child } => {
                waitpid(child, None).ok();
                let mut stdout = Vec::new();
                close(pipe_in).ok();
                unsafe { File::from_raw_fd(pipe_out) }.read_to_end(&mut stdout).ok();
                stdout
            },
            ForkResult::Child => {
                let stdin = 0;
                let stdout = pipe_in;
                let stderr = 2;
                let status = match self.run_terms(terms, stdin, stdout, stderr) {
                    ExitStatus::ExitedWith(status) => status,
                    _ => 0,
                };

                std::process::exit(status);
            }
        }
    }

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

                last_status = self.run_pipeline(pipeline, stdin, stdout, stderr, term.background);
            }
        }
        last_status
    }

    fn run_pipeline(
        &mut self,
        pipeline: &parser::Pipeline,
        pipeline_stdin: RawFd,
        pipeline_stdout: RawFd,
        stderr: RawFd,
        background: bool,
    ) -> ExitStatus {
        // Invoke commands in a pipeline.
        let mut last_command_result = None;
        let mut iter = pipeline.commands.iter().peekable();
        let mut childs = Vec::new();
        let mut stdin = pipeline_stdin;
        let mut pgid = None;
        while let Some(command) = iter.next() {
            let stdout;
            let pipes = if iter.peek().is_some() {
                // There is a next command in the pipeline (e.g. date in
                // `date | hexdump`). Create and connect a pipe.
                let (pipe_out, pipe_in) = pipe().unwrap();
                stdout = pipe_in;
                Some((pipe_out, pipe_in))
            } else {
                // The last command in the pipeline.
                stdout = pipeline_stdout;
                None
            };

            let ctx = Context { stdin, stdout, stderr, pgid, background, interactive: self.interactive };
            let result = self.run_command(command, &ctx);
            match result {
                CommandResult::External { pid } => {
                    if pgid.is_none() {
                        // The first child (the process group leader) pid is used for pgid.
                        pgid = Some(pid);
                    }

                    if self.interactive {
                        setpgid(pid, pgid.unwrap()).expect("failed to setpgid");
                    }

                    childs.push(pid);
                },
                _ => ()
            }

            if let Some((pipe_out, pipe_in)) = pipes {
                stdin = pipe_out;
                // `pipe_in` is used by a child process and is no longer needed.
                close(pipe_in).expect("failed to close pipe_in");
            }

            last_command_result = Some(result);
        }

        // Wait for the last command in the pipeline.
        // FIXME: needs refactoring: last_status should be immutable
        let mut last_status = ExitStatus::ExitedWith(0);
        match last_command_result {
            None => {
                trace!("nothing to execute");
                last_status = ExitStatus::ExitedWith(0);
            }
            Some(CommandResult::Internal { status }) => {
                last_status = status;
                if let ExitStatus::ExitedWith(status) = status {
                    self.last_status = status;
                }
            }
            Some(CommandResult::External { .. }) => {
                // Create a new job.
                let cmd_name = String::new(); // TODO:
                let job = Arc::new(Job::new(self.alloc_job_id(), pgid.unwrap(), cmd_name, childs.clone()));
                for child in childs {
                    self.set_process_state(child, ProcessState::Running);
                    self.pid_job_mapping.insert(child, job.clone());
                }
                self.jobs.insert(job.id, job.clone());

                if !self.interactive {
                    last_status = match self.wait_for_job(job.clone()) {
                        ProcessState::Completed(status) => {
                            ExitStatus::ExitedWith(status)
                        },
                        ProcessState::Stopped(_) => {
                            ExitStatus::Background(pgid.unwrap())
                        },
                        _ => unreachable!(),
                    }
                } else if background {
                    self.run_in_background(job.id, false);
                    last_status = ExitStatus::Background(pgid.unwrap());
                } else {
                    last_status = match self.run_in_foreground(job.id, false) {
                        ProcessState::Completed(status) => {
                            ExitStatus::ExitedWith(status)
                        },
                        ProcessState::Stopped(_) => {
                            ExitStatus::Background(pgid.unwrap())
                        },
                        _ => unreachable!(),
                    };
                }
            },
            Some(CommandResult::Break) => {
                return ExitStatus::Break;
            }
            Some(CommandResult::Continue) => {
                return ExitStatus::Continue;
            }
            Some(CommandResult::Return) => {
                return ExitStatus::Return;
            }
            Some(CommandResult::NoExec) => (),
        }

        if self.errexit {
            if let ExitStatus::ExitedWith(status) = last_status {
                if status != 0 {
                    std::process::exit(status);
                }
            }
        }

        last_status
    }

    pub fn exec(&mut self, ast: &Ast) -> ExitStatus {
        trace!("ast: {:#?}", ast);

        // Inherit shell's stdin/stdout/stderr.
        let stdin = 0;
        let stdout = 1;
        let stderr = 2;
        self.run_terms(&ast.terms, stdin, stdout, stderr)
    }

    pub fn exec_file(&mut self, script_file: PathBuf) -> ExitStatus {
        let mut f = File::open(script_file).expect("failed to open a file");
        let mut script = String::new();
        f.read_to_string(&mut script)
            .expect("failed to load a file");

        self.exec_str(script.as_str())
    }

    pub fn exec_str(&mut self, script: &str) -> ExitStatus {
        match parser::parse_line(script) {
            Ok(cmd) => {
                self.exec(&cmd)
            }
            Err(parser::SyntaxError::Empty) => {
                // Just ignore.
                ExitStatus::ExitedWith(0)
            },
            Err(err) => {
                eprintln!("nsh: parse error: {:?}", err);
                ExitStatus::ExitedWith(-1)
            }
        }
    }
}


fn expand_glob(pattern: &str) -> Vec<String> {
    let mut words = Vec::new();
    for entry in glob(pattern).expect("failed to glob") {
        match entry {
            Ok(path) => {
                words.push(path.to_str().unwrap().to_string());
            },
            Err(e) => trace!("glob error: {:?}", e),
        }
    }

    if words.len() == 0 {
        // TODO: return Result and abort the command instead
        eprintln!("nsh: no glob matches");
        return vec![pattern.to_string()];
    }

    words
}



fn move_fd(src: RawFd, dst: RawFd) {
    if src != dst {
        dup2(src, dst).expect("failed to dup2");
        close(src).expect("failed to close");
    }
}


#[test]
fn test_expr() {
    let mut env = Env::new(getpid(), false);
    assert_eq!(
        env.evaluate_expr(&&Expr::Mul(BinaryExpr {
            lhs: Box::new(Expr::Literal(2)),
            rhs: Box::new(Expr::Add(BinaryExpr {
                lhs: Box::new(Expr::Literal(3)),
                rhs: Box::new(Expr::Literal(7)),
            })),
        })),
        2 * (3 + 7)
    );

    env.set("x", Value::String(3.to_string()), false);
    assert_eq!(
        env.evaluate_expr(&&Expr::Add(BinaryExpr {
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
