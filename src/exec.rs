use nix::unistd::pipe;
use parser::{self, Ast, ExpansionOp, RunIf, Span, Word};
use std::collections::HashMap;
use std::io;
use std::os::unix::io::FromRawFd;
use std::process::{self, ExitStatus, Stdio};
use std::sync::{Arc, Mutex};

#[derive(Debug)]
pub enum VariableValue {
    String(String),
    Function(Box<parser::Command>),
}

#[derive(Debug)]
pub struct Variable {
    value: VariableValue,
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
            VariableValue::String(ref s) => s.clone(),
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
                    value: VariableValue::String(value.clone()),
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

fn exec_command(
    argv: Vec<String>,
    stdin: Stdio,
    stdout: Stdio,
    stderr: Stdio,
) -> io::Result<ExitStatus> {
    process::Command::new(argv[0].clone())
        .args(argv.into_iter().skip(1))
        .stdin(stdin)
        .stdout(stdout)
        .stderr(stderr)
        .status()
}

pub struct CommandResult {
    status: i32,
}

fn run_terms(
    scope: &mut Scope,
    terms: &Vec<parser::Term>,
    mut stdin: Stdio,
    mut stdout: Stdio,
    mut stderr: Stdio,
) -> CommandResult {
    let mut last_status = 0;
    for term in terms {
        for pipeline in &term.pipelines {
            // Should we execute the pipline?
            match (last_status == 0, &pipeline.run_if) {
                (true, RunIf::Success) => (),
                (false, RunIf::Failure) => (),
                (_, RunIf::Always) => (),
                _ => continue,
            }

            // Invoke commands in a pipeline.
            let mut iter = pipeline.commands.iter().peekable();
            while let Some(command) = iter.next() {
                let next_stdin = if iter.peek().is_some() {
                    // There is a next command in the pipeline (e.g. date in
                    // `date | hexdump`). Create and connect a pipe.
                    let (pipe_out, pipe_in) = pipe().unwrap();
                    stdout = unsafe { Stdio::from_raw_fd(pipe_in) };
                    Some(unsafe { Stdio::from_raw_fd(pipe_out) })
                } else {
                    None
                };

                let r = run_command(scope, command, stdin, stdout, stderr);
                last_status = r.status;

                stdin = Stdio::inherit();;
                stderr = Stdio::inherit();
                stdout = Stdio::inherit();;
                if let Some(next) = next_stdin {
                    stdin = next;
                }
            }
        }
    }

    CommandResult {
        status: last_status,
    }
}

fn run_command(
    scope: &mut Scope,
    command: &parser::Command,
    stdin: Stdio,
    stdout: Stdio,
    stderr: Stdio,
) -> CommandResult {
    trace!("run_command: {:?}", command);
    match command {
        parser::Command::SimpleCommand { argv: wargv, .. } => {
            let argv = evaluate_words(scope, wargv);
            if argv.len() == 0 {
                // `argv` is empty. For example bash accepts `> foo.txt`; it creates an empty file
                // named "foo.txt".
                return CommandResult { status: 0 };
            }

            // Functions
            if let Some(var) = get_var(scope, argv.get(0).unwrap()) {
                if let Variable {
                    value: VariableValue::Function(body),
                } = var.as_ref()
                {
                    return run_command(scope, &body, stdin, stdout, stderr);
                }
            }

            // External commands
            let status = match exec_command(argv, stdin, stdout, stderr) {
                Ok(status) => status.code().unwrap(),
                Err(_) => {
                    println!("nsh: failed to execute");
                    -1
                }
            };

            CommandResult { status }
        }
        parser::Command::Assignment { assignments } => {
            for (name, value) in assignments {
                let value = Variable {
                    value: VariableValue::String(evaluate_word(scope, value)),
                };

                set_var(scope, name.as_str(), value)
            }
            CommandResult { status: 0 }
        }
        parser::Command::FunctionDef { name, body } => {
            set_var(
                scope,
                name.as_str(),
                Variable {
                    value: VariableValue::Function(body.clone()),
                },
            );
            CommandResult { status: 0 }
        },
        parser::Command::Group { terms } => {
            run_terms(scope, terms, stdin, stdout, stderr)
        },
        _ => panic!("TODO:"),
    }
}

pub fn exec(ast: Ast) {
    trace!("ast: {:#?}", ast);
    let scope = &mut CONTEXT.lock().unwrap().scope;
    let stdin = Stdio::inherit();
    let stdout = Stdio::inherit();
    let stderr = Stdio::inherit();
    run_terms(scope, &ast.terms, stdin, stdout, stderr);
}
