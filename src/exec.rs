use parser::{self, ExpansionOp, Ast, Word, Span};
use std::collections::HashMap;
use std::io;
use std::process::{self, ExitStatus, Stdio};
use std::sync::{Arc, Mutex};

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

fn expand_param(scope: &Scope, name: String, op: ExpansionOp) -> String {
    match op {
        ExpansionOp::GetOrEmpty => {
            if let Some(var) = get_var(&scope, name.as_str()) {
                match var.value {
                    VariableValue::String(ref s) => s.clone(),
                }
            } else {
                "".to_owned()
            }
        },
        _ => panic!("TODO:"),
    }
}

fn evaluate_span(scope: &Scope, span: Span) -> String {
    match span {
        Span::Literal(s) => s,
        Span::Parameter { name, op } => expand_param(scope, name, op),
        _ => panic!("TODO:"),
    }
}

fn evaluate_word(scope: &Scope, word: Word) -> String {
    let mut s = String::new();
    for span in word.0 {
        s += evaluate_span(scope, span).as_str();
    }
    s
}

fn evaluate_words(scope: &Scope, words: Vec<Word>) -> Vec<String> {
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

fn run_command(scope: &mut Scope, command: parser::Command) -> i32 {
    trace!("run_command: {:?}", command);
    match command {
        parser::Command::SimpleCommand { argv: wargv, .. } => {
            let argv2 = evaluate_words(scope, wargv);
            match exec_command(argv2, Stdio::inherit(), Stdio::inherit(), Stdio::inherit()) {
                Ok(status) => status.code().unwrap(),
                Err(_) => {
                    println!("nsh: failed to execute");
                    -1
                },
            }
        },
        parser::Command::Assignment { assignments } => {
            for (name, value) in assignments {
                let value = Variable {
                    value: VariableValue::String(evaluate_word(scope, value))
                };

                set_var(scope, name.as_str(), value)
            }
            0
        }
        _ => panic!("TODO:"),
    }
}

pub fn exec(ast: Ast) -> i32 {
    trace!("exec: {:#?}", ast);
    let scope = &mut CONTEXT.lock().unwrap().scope;
    let mut result = -128;
    for term in ast.terms {
        for pipeline in term.pipelines {
            for command in pipeline.commands {
                result = run_command(scope, command);
            }
        }
    }

    result
}
