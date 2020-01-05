use crate::completion::CompSpec;
use crate::eval::eval;
use crate::parser;
use crate::path::reload_paths;
use crate::process::{ExitStatus, Job, JobId, ProcessState};
use crate::variable::{Frame, Value, Variable};
use nix;
use nix::sys::termios::{tcgetattr, Termios};
use nix::unistd::{getpid, Pid};
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::prelude::*;
use std::os::unix::io::RawFd;
use std::path::PathBuf;
use std::rc::Rc;

/// A isolated shell execution environment.
/// TODO: Make fields private.
pub struct Shell {
    pub shell_pgid: Pid,
    pub interactive: bool,
    /// $0
    pub script_name: String,
    pub term_fd: RawFd,
    pub shell_termios: Option<Termios>,

    /// `$?`
    pub last_status: i32,
    /// `$!`
    pub last_back_job: Option<Rc<Job>>,

    /// Global scope.
    pub global: Frame,
    /// Local scopes (variables declared with `local').
    pub frames: Vec<Frame>,
    pub exported: HashSet<String>,
    pub aliases: HashMap<String, String>,

    // Shell options.
    pub errexit: bool,
    pub nounset: bool,
    pub noexec: bool,

    pub jobs: HashMap<JobId, Rc<Job>>,
    pub background_jobs: HashSet<JobId>,
    pub last_fore_job: Option<Rc<Job>>,
    pub states: HashMap<Pid, ProcessState>,
    pub pid_job_mapping: HashMap<Pid, Rc<Job>>,

    // pushd(1) / popd(1) stack
    cd_stack: Vec<String>,

    compspecs: HashMap<String, CompSpec>,
}

impl Shell {
    pub fn new() -> Shell {
        Shell {
            shell_pgid: getpid(),
            script_name: "".to_owned(),
            interactive: false,
            term_fd: 0, /* stdin */
            shell_termios: None,
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
            compspecs: HashMap::new(),
        }
    }

    pub fn set_script_name(&mut self, name: &str) {
        self.script_name = name.to_owned();
    }

    pub fn set_interactive(&mut self, interactive: bool) {
        self.interactive = interactive;
        self.shell_termios = if interactive {
            Some(tcgetattr(0 /* stdin */).expect("failed to tcgetattr"))
        } else {
            None
        };
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

        if !is_local && key == "PATH" {
            // $PATH is being updated. Reload directories.
            if let Value::String(ref path) = value {
                reload_paths(path);
            }
        }

        frame.set(key, value);
    }

    pub fn remove(&mut self, key: &str) -> Option<Rc<Variable>> {
        if let Some(var) = self.current_frame_mut().remove(key) {
            return Some(var);
        }

        if let Some(var) = self.global.remove(key) {
            return Some(var);
        }

        None
    }

    pub fn get(&self, key: &str) -> Option<Rc<Variable>> {
        if let Some(var) = self.current_frame().get(key) {
            Some(var)
        } else {
            self.global.get(key)
        }
    }

    #[inline]
    pub fn get_str(&self, key: &str) -> Option<String> {
        match self.get(key) {
            Some(var) => match var.value() {
                Some(Value::String(ref s)) => Some(s.clone()),
                _ => None,
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

    #[inline]
    pub fn get_compspec<'a>(&'a self, command: &str) -> Option<&'a CompSpec> {
        self.compspecs.get(command)
    }

    #[inline]
    pub fn set_compspec(&mut self, command: &str, compspec: CompSpec) {
        self.compspecs.insert(command.to_owned(), compspec);
    }

    pub fn get_var_as_i32(&self, name: &str) -> Option<i32> {
        self.get(name).and_then(|var| match var.value() {
            Some(Value::String(s)) => s.parse().ok(),
            _ => None,
        })
    }

    #[inline]
    pub fn jobs(&self) -> Vec<Rc<Job>> {
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
    pub fn last_fore_job(&self) -> Option<Rc<Job>> {
        self.last_fore_job.as_ref().cloned()
    }

    pub fn find_job_by_id(&self, id: JobId) -> Option<Rc<Job>> {
        self.jobs.get(&id).cloned()
    }

    /// Parses and runs a shell script file.
    pub fn run_file(&mut self, script_file: PathBuf) -> ExitStatus {
        let mut f = File::open(script_file).expect("failed to open a file");
        let mut script = String::new();
        f.read_to_string(&mut script)
            .expect("failed to load a file");

        self.run_str(script.as_str())
    }

    /// Parses and runs a script. Stdin/stdout/stderr are 0, 1, 2, respectively.
    pub fn run_str(&mut self, script: &str) -> ExitStatus {
        // Inherit shell's stdin/stdout/stderr.
        let stdin = 0;
        let stdout = 1;
        let stderr = 2;
        self.run_str_with_stdio(script, stdin, stdout, stderr)
    }

    /// Parses and runs a script in the given context.
    pub fn run_str_with_stdio(
        &mut self,
        script: &str,
        stdin: RawFd,
        stdout: RawFd,
        stderr: RawFd,
    ) -> ExitStatus {
        match parser::parse(script) {
            Ok(ast) => eval(self, &ast, stdin, stdout, stderr),
            Err(parser::ParseError::Empty) => {
                // Just ignore.
                ExitStatus::ExitedWith(0)
            }
            Err(parser::ParseError::Fatal(err)) => {
                eprintln!("nsh: parse error: {}", err);
                ExitStatus::ExitedWith(-1)
            }
        }
    }
}
