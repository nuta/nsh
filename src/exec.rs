use crate::alias::lookup_alias;
use crate::builtins::{run_internal_command, InternalCommandError};
use crate::parser::{self, Ast, ExpansionOp, RunIf, Span, Word};
use crate::path::lookup_external_command;
use nix::sys::wait::{waitpid, WaitStatus};
use nix::unistd::{close, dup2, execv, fork, pipe, ForkResult, Pid};
use std::collections::HashMap;
use std::ffi::CString;
use std::fs::OpenOptions;
use std::os::unix::io::IntoRawFd;
use std::os::unix::io::RawFd;
use std::sync::{Arc, Mutex};

#[derive(Debug)]
pub enum Value {
    String(String),
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
}

#[derive(Debug)]
pub struct Scope {
    vars: HashMap<String, Arc<Variable>>,
    prev: Option<Arc<Scope>>,
}

#[derive(Debug)]
pub struct ExecEnv {
    scope: Scope,
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

lazy_static! {
    static ref CONTEXT: Mutex<ExecEnv> = Mutex::new(ExecEnv {
        scope: Scope {
            vars: HashMap::new(),
            prev: None,
        },
    });
}

pub fn set_var(scope: &mut Scope, key: &str, value: Variable) {
    scope.vars.insert(key.into(), Arc::new(value));
}

pub fn get_var<'a, 'b>(scope: &'a Scope, key: &'b str) -> Option<Arc<Variable>> {
    scope.vars.get(key).cloned()
}

fn expand_param(scope: &mut Scope, name: &str, op: &ExpansionOp) -> String {
    if let Some(var) = get_var(&scope, name) {
        let string_value = match var.value {
            Value::String(ref s) => s.clone(),
            _ => panic!("TODO: cannot expand"),
        };

        // $<name> is defined and contains a string value.
        return match op {
            ExpansionOp::Length => string_value.len().to_string(),
            _ => string_value,
        };
    }

    // $<name> is not defined.
    match op {
        ExpansionOp::Length => "0".to_owned(),
        ExpansionOp::GetOrEmpty => "".to_owned(),
        ExpansionOp::GetOrDefault(word) => evaluate_word(scope, word),
        ExpansionOp::GetOrDefaultAndAssign(word) => {
            let value = evaluate_word(scope, word);
            set_var(
                scope,
                name,
                Variable {
                    value: Value::String(value.clone()),
                },
            );
            value
        }
        _ => panic!("TODO:"),
    }
}

fn evaluate_span(scope: &mut Scope, span: &Span) -> String {
    match span {
        Span::Literal(s) => s.clone(),
        Span::Parameter { name, op } => expand_param(scope, name, op),
        _ => panic!("TODO:"),
    }
}

fn evaluate_word(scope: &mut Scope, word: &Word) -> String {
    let mut s = String::new();
    for span in &word.0 {
        s += evaluate_span(scope, &span).as_str();
    }

    s
}

fn evaluate_words(scope: &mut Scope, words: &[Word]) -> Vec<String> {
    let mut evaluated = Vec::new();
    for word in words {
        let s = evaluate_word(scope, word);
        if !s.is_empty() {
            evaluated.push(s);
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

fn exec_command(argv: Vec<String>, fds: Vec<(RawFd, RawFd)>) -> Result<Pid, ()> {
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

            // TODO: inherit exported variables
            execv(&argv0, &args).expect("failed to exec");
            unreachable!();
        }
    }
}

pub fn run_terms(
    scope: &mut Scope,
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

            last_status = run_pipeline(scope, pipeline, stdin, stdout, stderr);
        }
    }
    last_status
}

fn run_pipeline(
    scope: &mut Scope,
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

        last_command_result = Some(run_command(scope, command, stdin, stdout, stderr));
        if let CommandResult::External { pid } = last_command_result.unwrap() {
            childs.push(pid);
        }

        if let Some((pipe_out, pipe_in)) = pipes {
            stdin = pipe_out;
            // `pipe_in` is used by a child process and is no longer needed.
            close(pipe_in).expect("failed to close pipe_in");
        }
    }

    let mut last_status = ExitStatus::ExitedWith(0);
    match last_command_result {
        None => {
            trace!("nothing to execute");
            last_status = ExitStatus::ExitedWith(0);
        }
        Some(CommandResult::Internal { status }) => {
            last_status = status;
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
    scope: &mut Scope,
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

            // FIXME:
            let Word(ref spans) = ref_wargv[0];
            let wargv = if !spans.is_empty() {
                match spans[0] {
                    Span::Literal(ref lit) => {
                        if let Some(alias_argv) = lookup_alias(lit.as_str()) {
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

            let argv = evaluate_words(scope, &wargv);
            if argv.is_empty() {
                return CommandResult::Internal { status: ExitStatus::ExitedWith(0) };
            }

            // Functions
            if let Some(var) = get_var(scope, argv[0].as_str()) {
                if let Variable {
                    value: Value::Function(body),
                } = var.as_ref()
                {
                    return run_command(scope, &body, stdin, stdout, stderr);
                }
            }

            // Internal commands
            match run_internal_command(scope, argv[0].as_str(), &argv, stdin, stdout, stderr) {
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
                        let filepath = evaluate_word(scope, wfilepath);
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
            if fds.iter().any(|(_, dst)| *dst == 0) {
                fds.push((stdin, 0));
            }
            if fds.iter().any(|(_, dst)| *dst == 1) {
                fds.push((stdout, 1));
            }
            if fds.iter().any(|(_, dst)| *dst == 2) {
                fds.push((stderr, 2));
            }

            match exec_command(argv, fds) {
                Ok(pid) => CommandResult::External { pid },
                Err(_) => CommandResult::Internal { status: ExitStatus::ExitedWith(-1) },
            }
        }
        parser::Command::Assignment { assignments } => {
            for (name, value) in assignments {
                let value = Variable {
                    value: Value::String(evaluate_word(scope, value)),
                };

                set_var(scope, name.as_str(), value)
            }
            CommandResult::Internal { status: ExitStatus::ExitedWith(0) }
        }
        parser::Command::FunctionDef { name, body } => {
            let value = Variable {
                value: Value::Function(body.clone()),
            };
            set_var(scope, name.as_str(), value);
            CommandResult::Internal { status: ExitStatus::ExitedWith(0) }
        }
        parser::Command::If {
            condition,
            then_part,
            ..
        } => {
            // TODO: else, elif
            let result = run_terms(scope, condition, stdin, stdout, stderr);
            if result == ExitStatus::ExitedWith(0) {
                CommandResult::Internal {
                    status: run_terms(scope, then_part, stdin, stdout, stderr),
                }
            } else {
                CommandResult::Internal { status: ExitStatus::ExitedWith(0) }
            }
        }
        parser::Command::Group { terms } => CommandResult::Internal {
            status: run_terms(scope, terms, stdin, stdout, stderr),
        },
        parser::Command::For { var_name, words, body } => {
            for word in words {
                let var = Variable::from_string(evaluate_word(scope, word));
                set_var(scope, &var_name, var);

                let result = run_terms(scope, body, stdin, stdout, stderr);
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

pub fn exec(ast: &Ast) {
    trace!("ast: {:#?}", ast);
    let scope = &mut CONTEXT.lock().unwrap().scope;

    // Inherit shell's stdin/stdout/stderr.
    let stdin = 0;
    let stdout = 1;
    let stderr = 2;
    run_terms(scope, &ast.terms, stdin, stdout, stderr);
}
