use parser::{self, ExpansionOp, Ast, Word, Span, RunIf};
use std::collections::HashMap;
use std::io;
use std::process::{self, ExitStatus, Stdio};
use std::sync::{Arc, Mutex};
use std::os::unix::io::{FromRawFd};
use nix::unistd::pipe;

#[derive(Debug)]
pub enum VariableValue {
    String(String),
}

#[derive(Debug)]
pub struct Variable {
    value: VariableValue,
}

#[derive(Debug)]
pub struct Scope {
    vars: HashMap<String, Variable>,
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
    scope.vars.insert(key.into(), value);
}

pub fn get_var<'a, 'b>(scope: &'a Scope, key: &'b str) -> Option<&'a Variable> {
    scope.vars.get(key.into())
}

fn expand_param(scope: &mut Scope, name: &String, op: &ExpansionOp) -> String {
        if let Some(var) = get_var(&scope, name.as_str()) {
            let string_value = match var.value {
                VariableValue::String(ref s) => s.clone(),
            };

            // $<name> is defined and contains a string value.
            return match op {
                ExpansionOp::Length => { string_value.len().to_string() },
                _ => string_value,
            };
        }

        // $<name> is not defined.
        match op {
            ExpansionOp::Length => { "0".to_owned() },
            ExpansionOp::GetOrEmpty => { "".to_owned() },
            ExpansionOp::GetOrDefault(word) => { evaluate_word(scope, word) },
            ExpansionOp::GetOrDefaultAndAssign(word) => {
                let value = evaluate_word(scope, word);
                set_var(scope, name, Variable {
                    value: VariableValue::String(value.clone())
                });
                value
            },
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

fn run_command(scope: &mut Scope, command: &parser::Command, stdin: Stdio, stdout: Stdio, stderr: Stdio) -> CommandResult {
    trace!("run_command: {:?}", command);
    match command {
        parser::Command::SimpleCommand { argv: wargv, .. } => {
            let argv2 = evaluate_words(scope, wargv);
            let status = match exec_command(argv2, stdin, stdout, stderr) {
                Ok(status) => status.code().unwrap(),
                Err(_) => {
                    println!("nsh: failed to execute");
                    -1
                },
            };
            CommandResult { status }
        },
        parser::Command::Assignment { assignments } => {
            for (name, value) in assignments {
                let value = Variable {
                    value: VariableValue::String(evaluate_word(scope, value))
                };

                set_var(scope, name.as_str(), value)
            }
            CommandResult { status: 0 }
        }
        _ => panic!("TODO:"),
    }
}

pub fn exec(ast: Ast) -> i32 {
    trace!("exec: {:#?}", ast);
    let scope = &mut CONTEXT.lock().unwrap().scope;
    let mut last_status = 0;
    for term in ast.terms {
        for pipeline in term.pipelines {
            // Should we execute the pipline?
            match (last_status == 0, pipeline.run_if) {
                (true, RunIf::Success) => (),
                (false, RunIf::Failure) => (),
                (_, RunIf::Always) => (),
                _ => continue,
            }

            // Invoke commands in a pipeline.
            let mut stdin  = Stdio::inherit();
            let mut stdout = Stdio::inherit();
            let mut stderr = Stdio::inherit();
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

    0
}
