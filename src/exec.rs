use crate::builtins::{run_internal_command, InternalCommandError};
use crate::parser::{self, Ast, ExpansionOp, RunIf, Expr, BinaryExpr, Span, Word, Initializer};
use crate::path::lookup_external_command;
use nix::sys::wait::{waitpid, WaitStatus};
use nix::unistd::{close, dup2, execv, fork, pipe, ForkResult, Pid};
use std::collections::{HashMap, HashSet};
use std::ffi::CString;
use std::fs::{File, OpenOptions};
use std::path::PathBuf;
use std::io::prelude::*;
use std::os::unix::io::IntoRawFd;
use std::os::unix::io::FromRawFd;
use std::os::unix::io::RawFd;
use std::sync::Arc;
use glob::glob;

#[derive(Debug)]
pub enum Value {
    String(String),
    Array(Vec<String>),
    Function(Box<parser::Command>),
}

#[derive(Debug)]
pub struct Variable {
    value: Value,
}

impl Variable {
    pub fn from_string(value: String) -> Variable {
        Variable {
            value: Value::String(value),
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
    pub fn value_at<'a>(&'a self, env: &Env, index: &Expr) -> &'a str {
        match &self.value {
            Value::Array(elems) => {
                let index = evaluate_expr(env, index);
                if index < 0 {
                    return "";
                }

                match elems.get(index as usize) {
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
    Break,
    Continue,
}

#[derive(Debug, Copy, Clone)]
enum CommandResult {
    External { pid: Pid },
    Internal { status: ExitStatus },
    // break command
    Break,
    // continue command
    Continue,
}

#[derive(Debug)]
pub struct Env {
    prev: Option<Arc<Env>>,
    last_status: i32,
    vars: HashMap<String, Arc<Variable>>,
    exported: HashSet<String>,
    aliases: HashMap<String, Vec<Word>>,
}

impl Env {
    pub fn new() -> Env {
        Env {
            last_status: 0,
            vars: HashMap::new(),
            prev: None,
            exported: HashSet::new(),
            aliases: HashMap::new(),
        }
    }

    pub fn set(&mut self, key: &str, value: Variable) {
        self.vars.insert(key.into(), Arc::new(value));
    }

    pub fn get<'a, 'b>(&'a self, key: &'b str) -> Option<Arc<Variable>> {
        self.vars.get(key).cloned()
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
}

fn evaluate_expr(env: &Env, expr: &Expr) -> i32 {
    match expr {
        Expr::Expr(sub_expr) => evaluate_expr(env, sub_expr),
        Expr::Literal(value) => *value,
        Expr::Parameter { name } => {
            if let Some(var) = env.get(name) {
                match &var.value {
                    Value::String(s) => s.parse().unwrap_or(0),
                    _ => 0,
                }
            } else {
                0
            }
        },
        Expr::Add(BinaryExpr { lhs, rhs }) => {
            evaluate_expr(env, lhs) + evaluate_expr(env, rhs)
        },
        Expr::Sub(BinaryExpr { lhs, rhs }) => {
            evaluate_expr(env, lhs) - evaluate_expr(env, rhs)
        },
        Expr::Mul(BinaryExpr { lhs, rhs }) => {
            evaluate_expr(env, lhs) * evaluate_expr(env, rhs)
        },
        Expr::Div(BinaryExpr { lhs, rhs }) => {
            evaluate_expr(env, lhs) / evaluate_expr(env, rhs)
        },
    }
}

fn expand_param(env: &mut Env, name: &str, op: &ExpansionOp) -> String {
    match name {
        "?" => {
            return env.last_status.to_string();
        },
        _ => {
            if let Some(var) = env.get(name) {
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
        ExpansionOp::Length => "0".to_owned(),
        ExpansionOp::GetOrEmpty => "".to_owned(),
        ExpansionOp::GetOrDefault(word) => evaluate_word(env, word),
        ExpansionOp::GetOrDefaultAndAssign(word) => {
            let content = evaluate_word(env, word);
            env.set(name, Variable::from_string(content.clone()));
            content
        }
        _ => panic!("TODO:"),
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


fn evaluate_word(env: &mut Env, word: &Word) -> String {
    let mut buf = String::new();
    for span in &word.0 {
        match span {
            Span::Literal(s) => {
                buf += s;
            },
            Span::Parameter { name, op } => {
                buf += &expand_param(env, name, op);
            },
            Span::ArrayParameter { name, index } => {
                if let Some(var) = env.get(name) {
                    buf += var.value_at(env, index);
                }
            },
            Span::ArithExpr { expr } => {
                buf += &evaluate_expr(env, expr).to_string();
            },
            Span::Tilde(user) => {
                if user.is_some() {
                    // TODO: e.g. ~mike, ~chandler/venus
                    unimplemented!();
                }

                if let Some(home_dir) = dirs::home_dir() {
                    buf += home_dir.to_str().unwrap();
                } else {
                    // TODO: return an error instead
                    eprintln!("nsh: failed to get home directory");
                    buf += "./";
                }
            },
            Span::Command { body } => {
                let stdout = exec_in_subshell(env, body);
                // TODO: support binary output
                let s = std::str::from_utf8(&stdout).unwrap_or("").to_string();
                buf += &s.trim_end_matches('\n');
            },
            Span::AnyChar => {
                buf.push('?');
            },
            Span::AnyString => {
                buf.push('*');
            },
        }
    }

    buf
}

fn evaluate_words(env: &mut Env, words: &[Word]) -> Vec<String> {
    let mut evaluated = Vec::new();
    for word in words {
        let s = evaluate_word(env, word);
        if !s.is_empty() {
            // FIXME: Support file path which contains '*'.
            if s.contains('*') || s.contains('?') {
                evaluated.extend(expand_glob(&s));
            } else {
                evaluated.push(s);
            }
        }
    }

    evaluated
}

fn move_fd(src: RawFd, dst: RawFd) {
    if src != dst {
        dup2(src, dst).expect("failed to dup2");
        close(src).expect("failed to close");
    }
}

fn exec_command(env: &Env, argv: Vec<String>, fds: Vec<(RawFd, RawFd)>) -> Result<Pid, ()> {
    let argv0 = match lookup_external_command(&argv[0]) {
        Some(argv0) => CString::new(argv0).unwrap(),
        None => {
            eprintln!("nsh: command not found `{}'", argv[0]);
            return Err(());
        }
    };

    match fork().expect("failed to fork") {
        ForkResult::Parent { child } => Ok(child),
        ForkResult::Child => {
            // FIXME: CString::new() internally calls memchr(); it could be non-negligible cost.
            let mut args = Vec::new();
            for arg in argv {
                args.push(CString::new(arg).unwrap());
            }

            // Initialize stdin/stdout/stderr and redirections.
            for (src, dst) in fds {
                move_fd(src, dst);
            }

            // Set exported variables.
            for name in env.exported_names() {
                if let Some(var) = env.get(name) {
                    std::env::set_var(name, var.value());
                }
            }

            // TODO: inherit exported variables
            execv(&argv0, &args).expect("failed to exec");
            unreachable!();
        }
    }
}

pub fn exec_in_subshell(env: &mut Env, terms: &[parser::Term]) -> Vec<u8> {
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
            let status = match run_terms(env, terms, stdin, stdout, stderr) {
                ExitStatus::ExitedWith(status) => status,
                _ => 0,
            };

            std::process::exit(status);
        }
    }
}

pub fn run_terms(
    env: &mut Env,
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
                (_, RunIf::Always) => (),
                _ => continue,
            }

            last_status = run_pipeline(env, pipeline, stdin, stdout, stderr);
        }
    }
    last_status
}

fn run_pipeline(
    env: &mut Env,
    pipeline: &parser::Pipeline,
    pipeline_stdin: RawFd,
    pipeline_stdout: RawFd,
    stderr: RawFd,
) -> ExitStatus {
    // Invoke commands in a pipeline.
    let mut last_command_result = None;
    let mut iter = pipeline.commands.iter().peekable();
    let mut childs = Vec::new();
    let mut stdin = pipeline_stdin;
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

        last_command_result = Some(run_command(env, command, stdin, stdout, stderr));
        if let CommandResult::External { pid } = last_command_result.unwrap() {
            childs.push(pid);
        }

        if let Some((pipe_out, pipe_in)) = pipes {
            stdin = pipe_out;
            // `pipe_in` is used by a child process and is no longer needed.
            close(pipe_in).expect("failed to close pipe_in");
        }
    }

    // Wait for the last command in the pipeline.
    let mut last_status = ExitStatus::ExitedWith(0);
    match last_command_result {
        None => {
            trace!("nothing to execute");
            last_status = ExitStatus::ExitedWith(0);
            env.last_status = 0;
        }
        Some(CommandResult::Internal { status }) => {
            last_status = status;
            if let ExitStatus::ExitedWith(status) = status {
                env.last_status = status;
            }
        }
        Some(CommandResult::External { pid: last_pid }) => {
            for child in childs {
                let status = match waitpid(Pid::from_raw(-1), None).expect("faied to waitpid") {
                    WaitStatus::Exited(pid, status) => {
                        trace!("nsh: pid={} status={}", pid, status);
                        status
                    }
                    WaitStatus::Signaled(pid, status, _) => {
                        println!("nsh: pid={} signal={:#?}", pid, status);
                        -1
                    }
                    _ => {
                        // TODO:
                        panic!("unexpected waitpid event");
                    }
                };

                if child == last_pid {
                    last_status = ExitStatus::ExitedWith(status);
                    env.last_status = status;
                }
            }
        },
        Some(CommandResult::Break) => {
            return ExitStatus::Break;
        }
        Some(CommandResult::Continue) => {
            return ExitStatus::Continue;
        }
    }

    last_status
}

fn run_command(
    env: &mut Env,
    command: &parser::Command,
    stdin: RawFd,
    stdout: RawFd,
    stderr: RawFd,
) -> CommandResult {
    trace!("run_command: {:?}", command);
    match command {
        parser::Command::SimpleCommand {
            argv: ref ref_wargv,
            ref redirects,
            ..
        } => {
            if ref_wargv.is_empty() {
                // `argv` is empty. For example bash accepts `> foo.txt`; it creates an empty file
                // named "foo.txt".
                return CommandResult::Internal { status: ExitStatus::ExitedWith(0) };
            }

            // FIXME: refactor
            let Word(ref spans) = ref_wargv[0];
            let wargv = if !spans.is_empty() {
                match spans[0] {
                    Span::Literal(ref lit) => {
                        if let Some(alias_argv) = env.lookup_alias(lit.as_str()) {
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

            let argv = evaluate_words(env, &wargv);
            if argv.is_empty() {
                return CommandResult::Internal { status: ExitStatus::ExitedWith(0) };
            }

            // Functions
            if let Some(var) = env.get(argv[0].as_str()) {
                if let Variable {
                    value: Value::Function(body),
                } = var.as_ref()
                {
                    return run_command(env, &body, stdin, stdout, stderr);
                }
            }

            // Internal commands
            match run_internal_command(env, &argv, stdin, stdout, stderr) {
                Ok(status) => {
                    return CommandResult::Internal { status };
                }
                Err(InternalCommandError::NotFound) => (), /* Try external command. */
            }

            // External commands
            let mut fds = Vec::new();
            for r in redirects {
                match r.target {
                    parser::RedirectionType::File(ref wfilepath) => {
                        let mut options = OpenOptions::new();
                        match &r.direction {
                            parser::RedirectionDirection::Input => options.read(true),
                            parser::RedirectionDirection::Output => {
                                options.write(true).create(true)
                            }
                            parser::RedirectionDirection::Append => {
                                options.write(true).append(true)
                            }
                        };

                        trace!("redirection: options={:?}", options);
                        let filepath = evaluate_word(env, wfilepath);
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
                fds.push((stdin, 0));
            }
            if !fds.iter().any(|(_, dst)| *dst == 1) {
                fds.push((stdout, 1));
            }
            if !fds.iter().any(|(_, dst)| *dst == 2) {
                fds.push((stderr, 2));
            }

            match exec_command(env, argv, fds) {
                Ok(pid) => CommandResult::External { pid },
                Err(_) => CommandResult::Internal { status: ExitStatus::ExitedWith(-1) },
            }
        }
        parser::Command::Assignment { assignments } => {
            for assign in assignments {
                let value = match assign.initializer {
                    Initializer::String(ref word) =>  {
                        Variable {
                            value: Value::String(evaluate_word(env, word)),
                        }
                    },
                    Initializer::Array(ref words) =>  {
                        let mut elems = Vec::new();
                        for word in words {
                            elems.push(evaluate_word(env, word));
                        }

                        Variable {
                            value: Value::Array(elems),
                        }
                    }
                };

                env.set(&assign.name, value)
            }
            CommandResult::Internal { status: ExitStatus::ExitedWith(0) }
        }
        parser::Command::FunctionDef { name, body } => {
            let value = Variable {
                value: Value::Function(body.clone()),
            };
            env.set(&name, value);
            CommandResult::Internal { status: ExitStatus::ExitedWith(0) }
        }
        parser::Command::If {
            condition,
            then_part,
            ..
        } => {
            // TODO: else, elif
            let result = run_terms(env, condition, stdin, stdout, stderr);
            if result == ExitStatus::ExitedWith(0) {
                CommandResult::Internal {
                    status: run_terms(env, then_part, stdin, stdout, stderr),
                }
            } else {
                CommandResult::Internal { status: ExitStatus::ExitedWith(0) }
            }
        }
        parser::Command::Group { terms } => CommandResult::Internal {
            status: run_terms(env, terms, stdin, stdout, stderr),
        },
        parser::Command::For { var_name, words, body } => {
            for word in words {
                let var = Variable::from_string(evaluate_word(env, word));
                env.set(&var_name, var);

                let result = run_terms(env, body, stdin, stdout, stderr);
                match result {
                    ExitStatus::Break => break,
                    ExitStatus::Continue => (),
                    _ => (),
                }
            }

            CommandResult::Internal { status: ExitStatus::ExitedWith(0) }
        },
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

pub fn exec(env: &mut Env, ast: &Ast) -> ExitStatus {
    trace!("ast: {:#?}", ast);

    // Inherit shell's stdin/stdout/stderr.
    let stdin = 0;
    let stdout = 1;
    let stderr = 2;
    run_terms(env, &ast.terms, stdin, stdout, stderr)
}

pub fn exec_file(env: &mut Env, script_file: PathBuf) -> ExitStatus {
    let mut f = File::open(script_file).expect("failed to open a file");
    let mut script = String::new();
    f.read_to_string(&mut script)
        .expect("failed to load a file");

    exec_str(env, script.as_str())
}

pub fn exec_str(env: &mut Env, script: &str) -> ExitStatus {
    match parser::parse_line(script) {
        Ok(cmd) => {
            exec(env, &cmd)
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

#[test]
fn test_expr() {
    let mut env = Env::new();
    assert_eq!(
        evaluate_expr(&env, &Expr::Mul(BinaryExpr {
            lhs: Box::new(Expr::Literal(2)),
            rhs: Box::new(Expr::Add(BinaryExpr {
                lhs: Box::new(Expr::Literal(3)),
                rhs: Box::new(Expr::Literal(7)),
            })),
        })),
        2 * (3 + 7)
    );

    env.set("x", Variable::from_string(3.to_string()));
    assert_eq!(
        evaluate_expr(&env, &Expr::Add(BinaryExpr {
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
