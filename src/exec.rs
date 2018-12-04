use crate::builtins::{InternalCommandContext, INTERNAL_COMMANDS, InternalCommandError};
use crate::parser::{
    self, Ast, ExpansionOp, RunIf, Expr, BinaryExpr, Span, Word, Initializer,
    LocalDeclaration, Assignment
};
use crate::path::{lookup_external_command,wait_for_path_loader};
use crate::variable::{Variable, Value};
use crate::utils::FdFile;
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
use std::io::{BufReader, BufWriter};
use std::os::unix::io::IntoRawFd;
use std::os::unix::io::FromRawFd;
use std::os::unix::io::RawFd;
use std::sync::Arc;
use std::cell::RefCell;
use glob::glob;
use globset;

fn kill_process_group(pgid: Pid, signal: Signal) -> Result<(), nix::Error> {
    kill(Pid::from_raw(-pgid.as_raw()), signal)
}

fn move_fd(src: RawFd, dst: RawFd) {
    if src != dst {
        dup2(src, dst).expect("failed to dup2");
        close(src).expect("failed to close");
    }
}

enum LiteralOrGlob {
    Literal(String),
    AnyString,
    AnyChar,
}

struct PatternWord {
    fragments: Vec<LiteralOrGlob>
}

impl PatternWord {
    pub fn new(fragments: Vec<LiteralOrGlob>) -> PatternWord {
        PatternWord {
            fragments
        }
    }

    pub fn into_string(self) -> String {
        let mut string = String::new();
        for frag in self.fragments {
            match frag {
                LiteralOrGlob::Literal(lit) => string += &lit,
                LiteralOrGlob::AnyChar => string.push('?'),
                LiteralOrGlob::AnyString => string.push('*'),
            }
        }

        string
    }

    pub fn expand_glob(self) -> Vec<String> {
        let includes_glob = self.fragments.iter().any(|frag| {
            match frag {
                LiteralOrGlob::AnyString => true,
                LiteralOrGlob::AnyChar => true,
                _ => false,
            }
        });

        let mut expanded_words = Vec::new();
        if includes_glob {
            let mut pattern = String::new();
            for frag in self.fragments {
                match frag {
                    LiteralOrGlob::Literal(lit) => {
                        pattern += lit
                            .replace("*", "[*]")
                            .replace("?", "[?]")
                            .as_str();
                    },
                    LiteralOrGlob::AnyChar => {
                        pattern.push('?');
                    },
                    LiteralOrGlob::AnyString => {
                        pattern.push('*');
                    },
                }
            }

            let mut paths = Vec::new();
            for entry in glob(&pattern).expect("failed to glob") {
                match entry {
                    Ok(path) => {
                        paths.push(path.to_str().unwrap().to_string());
                    },
                    Err(e) => trace!("glob error: {:?}", e),
                }
            }
            if paths.is_empty() {
                // FIXME: return error instead
                panic!("nsh: no matches");
            }

            expanded_words.extend(paths);
        } else {
            let mut s = String::new();
            for frag in self.fragments {
                if let LiteralOrGlob::Literal(lit) = frag {
                    s += &lit;
                }
            }

            expanded_words.push(s);
        }

        expanded_words
    }
}


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

#[derive(Debug, Copy, Clone)]
struct Context {
    stdin: RawFd,
    stdout: RawFd,
    stderr: RawFd,
    pgid: Option<Pid>,
    background: bool,
    interactive: bool,
}


#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum ProcessState {
    Running,
    /// Contains the exit status.
    Completed(i32),
    Stopped(Pid),
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
        self.vars.remove(key)
    }

    pub fn get(&self, key: &str) -> Option<Arc<Variable>> {
        self.vars.get(key).cloned()
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

pub struct Isolate {
    shell_pgid: Pid,
    interactive: bool,
    term_fd: RawFd,
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

    jobs: HashMap<JobId, Arc<Job>>,
    last_fore_job: Option<Arc<Job>>,
    states: HashMap<Pid, ProcessState>,
    pid_job_mapping: HashMap<Pid, Arc<Job>>,
}

unsafe impl Send for Isolate {}

impl Isolate {
    pub fn new(interactive: bool) -> Isolate {
        let shell_pgid = getpid();
        let shell_termios = if interactive {
            Some(tcgetattr(0 /* stdin */).expect("failed to tcgetattr"))
        } else {
            None
        };

        Isolate {
            shell_pgid,
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
            states: HashMap::new(),
            pid_job_mapping: HashMap::new(),
            last_fore_job: None,
        }
    }

    #[inline]
    pub fn interactive(&self) -> bool {
        self.interactive
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

    fn evaluate_expr(&self, expr: &Expr) -> i32 {
        match expr {
            Expr::Expr(sub_expr) => self.evaluate_expr(sub_expr),
            Expr::Literal(value) => *value,
            Expr::Parameter { name } => {
                if let Some(var) = self.get(name) {
                    match var.value() {
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
                    let value = var.as_str().to_string();
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
            ExpansionOp::GetOrDefault(word) => self.expand_word_into_string(word),
            ExpansionOp::GetOrDefaultAndAssign(word) => {
                let content = self.expand_word_into_string(word);
                self.set(name, Value::String(content.clone()), false);
                content
            }
            _ => panic!("TODO:"),
        }
    }

    fn expand_word_into_vec(&mut self, word: &Word, ifs: &str) -> Vec<PatternWord> {
        let mut words = Vec::new();
        let mut current_word = Vec::new();
        for span in word.spans() {
            let (frag, expand) = match span {
                Span::Literal(s) => {
                    (LiteralOrGlob::Literal(s.clone()), false)
                },
                Span::Parameter { name, op, quoted } => {
                    (LiteralOrGlob::Literal(self.expand_param(name, op)), !quoted)
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

                    (LiteralOrGlob::Literal(frag), !quoted)
                },
                Span::ArithExpr { expr } => {
                    let result = self.evaluate_expr(expr).to_string();
                    (LiteralOrGlob::Literal(result), false)
                },
                Span::Tilde(user) => {
                    if user.is_some() {
                        // TODO: e.g. ~mike, ~chandler/venus
                        unimplemented!();
                    }

                    let dir = dirs::home_dir().unwrap().to_str().unwrap().to_owned();
                    (LiteralOrGlob::Literal(dir), false)
                },
                Span::Command { body, quoted } => {
                    // TODO: support binary output
                    let raw_stdout = self.run_in_subshell(body);
                    let stdout = std::str::from_utf8(&raw_stdout)
                        .unwrap_or("")
                        .to_string()
                        .trim_end_matches('\n')
                        .to_owned();
                    (LiteralOrGlob::Literal(stdout), !quoted)
                },
                Span::AnyChar { quoted } if !*quoted => {
                    (LiteralOrGlob::AnyChar, false)
                },
                Span::AnyString { quoted } if !*quoted => {
                    (LiteralOrGlob::AnyString, false)
                },
                Span::AnyChar { .. } => {
                    (LiteralOrGlob::Literal("?".into()), false)
                },
                Span::AnyString { .. } => {
                    (LiteralOrGlob::Literal("*".into()), false)
                },
            };

            // Expand `a${foo}b` into words: `a1` `2` `3b`, where `$foo=123`.
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
        }

        if !current_word.is_empty() {
            words.push(PatternWord::new(current_word));
        }

        if words.is_empty() {
            vec![PatternWord::new(vec![LiteralOrGlob::Literal("".into())])]
        } else {
            words
        }
    }

    fn expand_word_into_string(&mut self, word: &Word) -> String {
        let ifs = self.get_str("IFS").unwrap_or_else(|| "\t ".to_owned());
        let ws: Vec<String> = self.expand_word_into_vec(word, &ifs)
            .into_iter()
            .map(|w| w.into_string())
            .collect();
        ws.join(" ")
    }

    fn expand_words(&mut self, words: &[Word]) -> Vec<String> {
        let mut evaluated = Vec::new();
        let ifs = self.get_str("IFS").unwrap_or_else(|| "\t ".to_owned());
        for word in words {
            let mut ws = Vec::new();
            for w in self.expand_word_into_vec(word, &ifs) {
                for f in w.expand_glob() {
                    ws.push(f);
                }
            }

            evaluated.extend(ws);
        }

        evaluated
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
    fn evaluate_initializer(&mut self, initializer: &Initializer) -> Value {
        match initializer {
            Initializer::String(ref word) => Value::String(self.expand_word_into_string(word)),
            Initializer::Array(ref words) =>  {
                let mut elems = Vec::new();
                for word in words {
                    elems.push(self.expand_word_into_string(word));
                }
                Value::Array(elems)
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
        if sigcont {
            kill_process_group(job.pgid, Signal::SIGCONT).expect("failed to kill(SIGCONT)");
        }
    }

    /// Waits for all processes in the job to exit. Note that the job will be
    /// deleted from `Isolate` if the process has exited.
    pub fn wait_for_job(&mut self, job: &Arc<Job>) -> ProcessState {
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

    /// Waits for an *any* process, i.e. `waitpid(-1)` and updates
    /// the process state recorded in the `Isolate`.
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

    /// Updates the process state.
    fn set_process_state(&mut self, pid: Pid, state: ProcessState) {
        self.states.insert(pid, state);
    }

    /// Returns the process state.
    fn get_process_state(&self, pid: Pid) -> Option<&ProcessState> {
        self.states.get(&pid)
    }

    /// Spawn a child process and execute a command.
    fn run_external_command(
        &mut self,
        ctx: &Context,
        argv: Vec<String>,
        redirects: &[parser::Redirection],
        assignments: &[parser::Assignment]
    ) -> ExitStatus {
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
                    let filepath = self.expand_word_into_string(wfilepath);
                    if let Ok(file) = options.open(&filepath) {
                        fds.push((file.into_raw_fd(), r.fd as RawFd))
                    } else {
                        warn!("nsh: failed to open file: `{}'", filepath);
                        return ExitStatus::ExitedWith(1);
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
                return ExitStatus::ExitedWith(1);
            }
        };

        // Spawn a child.
        match fork().expect("failed to fork") {
            ForkResult::Parent { child } => {
                ExitStatus::Running(child)
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
                        std::env::set_var(name, var.as_str());
                    }
                }

                // Load assignments.
                for assignment in assignments {
                    let value = self.evaluate_initializer(&assignment.initializer);
                    match value {
                        Value::String(s) => std::env::set_var(&assignment.name, s),
                        Value::Array(_) => {
                            eprintln!("nsh: Array assignments in a command is not supported.");
                            std::process::exit(1);
                        },
                        Value::Function(_) => (),
                    }
                }

                // TODO: inherit exported variables
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
    ) -> Result<ExitStatus, InternalCommandError> {

        let mut opened_fds = Vec::new();
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
                    let filepath = self.expand_word_into_string(wfilepath);
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
                        warn!("nsh: failed to open file: `{}'", filepath);
                        return Err(InternalCommandError::BadRedirection);
                    }
                }
            }
        }

        let mut ctx = InternalCommandContext {
            argv,
            isolate: self,
            stdin: BufReader::new(FdFile::new(stdin)),
            stdout: BufWriter::new(FdFile::new(stdout)),
            stderr: BufWriter::new(FdFile::new(stderr)),
        };

        let cmd = argv[0].as_str();
        let result = match INTERNAL_COMMANDS.get(cmd) {
            Some(func) => Ok(func(&mut ctx)),
            _ => Err(InternalCommandError::NotFound),
        };

        // Close redirections.
        for fd in opened_fds {
            close(fd).expect("failed to close");
        }

        result
    }

    fn expand_alias(&self, argv: &[Word]) -> Vec<Word> {
        if let Some(word) = argv.get(0) {
            if let Some(Span::Literal(lit)) = word.spans().get(0) {
                if let Some(alias) = self.lookup_alias(lit.as_str()) {
                    let mut new_argv = Vec::new();
                    new_argv.extend(alias);
                    new_argv.extend(argv.iter().skip(1).cloned());
                    return new_argv;
                }
            }
        }

        argv.to_vec()
    }

    fn run_simple_command(
        &mut self,
        ctx: &Context,
        argv: &[Word],
        redirects: &[parser::Redirection],
        assignments: &[parser::Assignment],
    ) -> ExitStatus {
        let argv = self.expand_words(&self.expand_alias(argv));

        if argv.is_empty() {
            // `argv` is empty. For example bash accepts `> foo.txt`; it creates an empty file
            // named "foo.txt".
            return ExitStatus::ExitedWith(0);
        }

        // Functions
        if let Some(ref var) = self.get(argv[0].as_str()) {
            if let Value::Function(ref body) = var.value() {
                self.enter_frame();

                // Set $1, $2, ...
                for (i, arg) in argv.iter().skip(1).enumerate() {
                    self.set(&(i + 1).to_string(), Value::String(arg.clone()), true);
                }

                let result = match self.run_command(&body, ctx) {
                    ExitStatus::Return => ExitStatus::ExitedWith(0),
                    result => result,
                };

                self.leave_frame();
                return result;
            }
        }

        // Internal commands
        match self.run_internal_command(&argv, ctx.stdin, ctx.stdout, ctx.stderr, redirects) {
            Ok(status) => return status,
            Err(InternalCommandError::BadRedirection) => return ExitStatus::ExitedWith(1),
            Err(InternalCommandError::NotFound) => (), /* Try external command. */
        }

        // External commands
        self.run_external_command(&ctx, argv, redirects, assignments)
    }

    fn run_local_command(&mut self, declarations: &[parser::LocalDeclaration]) -> ExitStatus {
        if self.in_global_frame() {
            eprintln!("nsh: local variable can only be used in a function");
            ExitStatus::ExitedWith(1)
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
            ExitStatus::ExitedWith(0)
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
    ) -> ExitStatus {
        // then
        let result = self.run_terms(condition, ctx.stdin, ctx.stdout, ctx.stderr);
        if result == ExitStatus::ExitedWith(0) {
            return self.run_terms(then_part, ctx.stdin, ctx.stdout, ctx.stderr);
        }

        // elif
        for elif in elif_parts {
            let result = self.run_terms(&elif.condition, ctx.stdin, ctx.stdout, ctx.stderr);
            if result == ExitStatus::ExitedWith(0) {
                return self.run_terms(then_part, ctx.stdin, ctx.stdout, ctx.stderr);
            }
        }

        // else
        if let Some(else_part) = else_part {
            return self.run_terms(else_part, ctx.stdin, ctx.stdout, ctx.stderr);
        }

        ExitStatus::ExitedWith(0)
    }

    fn match_pattern(&self, pattern: &str, text: &str) -> bool {
        match globset::GlobBuilder::new(pattern).build() {
            Ok(matcher) => {
                matcher.compile_matcher().is_match(text)
            },
            Err(err) => {
                // FIXME: return an Result instead
                panic!("failed to construct globset matcher: {}", err);
            }
        }
    }

    fn run_case_command(
        &mut self,
        ctx: &Context,
        word: &parser::Word,
        cases: &[parser::CaseItem],
    ) -> ExitStatus {

        let word = self.expand_word_into_string(word);
        for case in cases {
            for raw_pattern in &case.patterns {
                let mut pattern = String::new();
                for ws in self.expand_word_into_vec(raw_pattern, "") {
                    for w in ws.fragments {
                        match w {
                            LiteralOrGlob::Literal(lit) => {
                                pattern += lit
                                    .replace("*", "[*]")
                                    .replace("?", "[?]")
                                    .as_str();
                            },
                            LiteralOrGlob::AnyChar => {
                                pattern.push('?');
                            },
                            LiteralOrGlob::AnyString => {
                                pattern.push('*');
                            }
                        }
                    }
                }

                if self.match_pattern(&pattern, &word) {
                    return self.run_terms(&case.body, ctx.stdin, ctx.stdout, ctx.stderr);
                }
            }
        }

        ExitStatus::ExitedWith(0)
    }

    fn run_while_command(
        &mut self,
        ctx: &Context,
        condition: &[parser::Term],
        body: &[parser::Term],
    ) -> ExitStatus {

        let mut last_result = ExitStatus::ExitedWith(0);
        loop {
            let result = self.run_terms(condition, ctx.stdin, ctx.stdout, ctx.stderr);
            if result != ExitStatus::ExitedWith(0) {
                break;
            }

            last_result = self.run_terms(body, ctx.stdin, ctx.stdout, ctx.stderr);
        }

        last_result
    }

    fn run_for_command(&mut self, ctx: &Context, var_name: &str, words: &[Word], body: &[parser::Term]) -> ExitStatus {
        for word in words {
            let value = self.expand_word_into_string(word);
            self.set(&var_name, Value::String(value), false);

            let result = self.run_terms(body, ctx.stdin, ctx.stdout, ctx.stderr);
            match result {
                ExitStatus::Break => break,
                ExitStatus::Continue => (),
                ExitStatus::Return => return result,
                _ => (),
            }
        }

        ExitStatus::ExitedWith(0)
    }

    fn run_command(&mut self, command: &parser::Command, ctx: &Context) -> ExitStatus {
        if self.noexec {
            return ExitStatus::NoExec;
        }

        trace!("run_command: {:?}", command);
        match command {
            parser::Command::SimpleCommand { argv, redirects, assignments } => {
                self.run_simple_command(ctx, &argv, &redirects, &assignments)
            }
            parser::Command::If { condition, then_part, elif_parts, else_part, redirects } => {
                self.run_if_command(ctx, &condition, &then_part, &elif_parts, &else_part, &redirects)
            }
            parser::Command::While { condition, body } => {
                self.run_while_command(ctx, &condition, &body)
            }
            parser::Command::Case { word, cases } => {
                self.run_case_command(ctx, &word, &cases)
            }
            parser::Command::For { var_name, words, body } => {
                self.run_for_command(ctx, var_name, &words, &body)
            },
            parser::Command::LocalDef { declarations } => {
                self.run_local_command(&declarations)
            }
            parser::Command::FunctionDef { name, body } => {
                self.set(name, Value::Function(body.clone()), true);
                ExitStatus::ExitedWith(0)
            }
            parser::Command::Assignment { assignments } => {
                for assign in assignments {
                    let value = self.evaluate_initializer(&assign.initializer);
                    self.set(&assign.name, value, false)
                }
                ExitStatus::ExitedWith(0)
            },
            parser::Command::Group { terms } => {
                self.run_terms(terms, ctx.stdin, ctx.stdout, ctx.stderr)
            },
            parser::Command::Return => {
                ExitStatus::Return
            }
            parser::Command::Break => {
                ExitStatus::Break
            }
            parser::Command::Continue => {
                ExitStatus::Continue
            }
        }
    }

    /// Runs commands in a subshell (`$()`).
    pub fn run_in_subshell(&mut self, terms: &[parser::Term]) -> Vec<u8> {
        let (pipe_out, pipe_in) = pipe().unwrap();

        // Since child process has its own isolated address space, `RELOAD_WORK.wait()`
        // would block forever. Wait for the path loader before forking to make sure
        // that `RELOAD_WORK.wait()` does not block.
        wait_for_path_loader();

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

    // Creates a pipeline and runs commands.
    fn run_pipeline(
        &mut self,
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
                let (pipe_out, pipe_in) = pipe().unwrap();
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

            if let ExitStatus::Running(pid) = result {
                if pgid.is_none() {
                    // The first child (the process group leader) pid is used for pgid.
                    pgid = Some(pid);
                }

                if self.interactive {
                    setpgid(pid, pgid.unwrap()).expect("failed to setpgid");
                }

                childs.push(pid);
            }

            if let Some((pipe_out, pipe_in)) = pipes {
                stdin = pipe_out;
                // `pipe_in` is used by a child process and is no longer needed.
                close(pipe_in).expect("failed to close pipe_in");
            }

            last_result = Some(result);
        }

        // Wait for the last command in the pipeline.
        // FIXME: needs refactoring: last_status should be immutable
        let last_status = match last_result {
            Some(ExitStatus::ExitedWith(status)) => {
                self.last_status = status;
                ExitStatus::ExitedWith(status)
            },
            Some(ExitStatus::Running(_)) => {
                let cmd_name = String::new(); // TODO:
                let job = self.create_job(cmd_name, pgid.unwrap(), childs);

                if !self.interactive {
                    match self.wait_for_job(&job) {
                        ProcessState::Completed(status) => {
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

                last_status = self.run_pipeline(pipeline, stdin, stdout, stderr, term.background);
            }
        }
        last_status
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

#[test]
fn test_expr() {
    let config = Config {
        path: "$ ".into(),
        prompt: "/bin".into(),
    };

    let mut isolate = Isolate::new(config, getpid(), false);
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
