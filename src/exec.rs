use nix::unistd::{pipe, fork, ForkResult, execv, dup2, close, Pid};
use nix::sys::wait::{waitpid, WaitStatus};
use parser::{self, Ast, ExpansionOp, RunIf, Span, Word};
use std::collections::HashMap;
use std::os::unix::io::RawFd;
use std::ffi::CString;
use std::sync::{Arc, Mutex};
use path_loader::lookup_external_command;

#[derive(Debug)]
pub enum Value {
    String(String),
    Function(Box<parser::Command>),
}

#[derive(Debug)]
pub struct Variable {
    value: Value,
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

pub type ExitStatus = i32;

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
    scope.vars.get(key.into()).map(|var| var.clone())
}

fn expand_param(scope: &mut Scope, name: &String, op: &ExpansionOp) -> String {
    if let Some(var) = get_var(&scope, name.as_str()) {
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

fn evaluate_words(scope: &mut Scope, words: &Vec<Word>) -> Vec<String> {
    let mut evaluated = Vec::new();
    for word in words {
        let s = evaluate_word(scope, word);
        if s.len() > 0 {
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

fn exec_command(
    argv: Vec<String>,
    stdin: RawFd,
    stdout: RawFd,
    stderr: RawFd,
) -> Result<Pid, ()> {
    let argv0 = match lookup_external_command(&argv[0]) {
        Some(argv0) => CString::new(argv0).unwrap(),
        None => {
            println!("nsh: command not found: `{}'", argv[0]);
            return Err(());
        }
    };

    match fork().expect("failed to fork") {
        ForkResult::Parent { child } => {
            return Ok(child);
        },
        ForkResult::Child => {
            // FIXME: CString::new() internally calls memchr(); it could be non-negligible cost.
            let mut args = Vec::new();
            for arg in argv {
                args.push(CString::new(arg).unwrap());
            }

            // Prepare child's stdin/stdout/stderr.
            move_fd(stdin, 0);
            move_fd(stdout, 1);
            move_fd(stderr, 2);

            // TODO: inherit exported variables
            execv(&argv0, &args).expect("failed to exec");
            unreachable!();
        },
    }
}

fn run_terms(
    scope: &mut Scope,
    terms: &Vec<parser::Term>,
    stdin: RawFd,
    stdout: RawFd,
    stderr: RawFd,
) -> ExitStatus {
    let mut last_status = 0;
    for term in terms {
        for pipeline in &term.pipelines {
            // Should we execute the pipline?
            match (last_status == 0, &pipeline.run_if) {
                (true, RunIf::Success) => (),
                (false, RunIf::Failure) => (),
                (_, RunIf::Always) => (),
                _ => return last_status,
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
    stderr: RawFd
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

    let mut last_status = 0;
    match last_command_result {
        None => {
            trace!("nothing to execute");
            last_status = 0;
        },
        Some(CommandResult::Internal { exited_with }) => {
            last_status = exited_with;
        },
        Some(CommandResult::External { pid: last_pid }) => {
            for child in childs {
                let status = match waitpid(Pid::from_raw(-1), None).expect("faied to waitpid") {
                    WaitStatus::Exited(pid, status) => {
                        trace!("nsh: pid={} status={}", pid, status);
                        status
                    },
                    WaitStatus::Signaled(pid, status, _) => {
                        println!("nsh: pid={} signal={:#?}", pid, status);
                        -1
                    },
                    _ => {
                        // TODO:
                        panic!("unexpected waitpid event");
                    }
                };

                if child == last_pid {
                    last_status = status;
                }
            }
        }
    }

    last_status
}

#[derive(Debug, Copy, Clone)]
enum CommandResult {
    External { pid: Pid },
    Internal { exited_with: i32 },
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
        parser::Command::SimpleCommand { argv: wargv, .. } => {
            let argv = evaluate_words(scope, wargv);
            if argv.len() == 0 {
                // `argv` is empty. For example bash accepts `> foo.txt`; it creates an empty file
                // named "foo.txt".
                return CommandResult::Internal { exited_with: 0 };
            }

            // Functions
            if let Some(var) = get_var(scope, argv.get(0).unwrap()) {
                if let Variable { value: Value::Function(body) } = var.as_ref() {
                    return run_command(scope, &body, stdin, stdout, stderr);
                }
            }

            // External commands
            return match exec_command(argv, stdin, stdout, stderr) {
                Ok(pid) => CommandResult::External { pid },
                Err(_) => CommandResult::Internal { exited_with: -1 },
            };
        }
        parser::Command::Assignment { assignments } => {
            for (name, value) in assignments {
                let value = Variable {
                    value: Value::String(evaluate_word(scope, value)),
                };

                set_var(scope, name.as_str(), value)
            }
            CommandResult::Internal { exited_with: 0 }
        }
        parser::Command::FunctionDef { name, body } => {
            let value = Variable {
                value: Value::Function(body.clone()),
            };
            set_var(scope, name.as_str(), value);
            CommandResult::Internal { exited_with: 0 }
        },
        parser::Command::If { condition, then_part, .. } => {
            // TODO: else, elif
            if run_terms(scope, condition, stdin, stdout, stderr) == 0 {
                    CommandResult::Internal {
                        exited_with: run_terms(scope, then_part, stdin, stdout, stderr)
                    }
            } else {
                CommandResult::Internal { exited_with: 0 }
            }
        },
        parser::Command::Group { terms } => {
            CommandResult::Internal {
                exited_with: run_terms(scope, terms, stdin, stdout, stderr)
            }
        },
        _ => panic!("TODO:"),
    }
}

pub fn exec(ast: Ast) {
    trace!("ast: {:#?}", ast);
    let scope = &mut CONTEXT.lock().unwrap().scope;

    // Inherit shell's stdin/stdout/stderr.
    let stdin = 0;
    let stdout = 1;
    let stderr = 2;
    run_terms(scope, &ast.terms, stdin, stdout, stderr);
}
