use parser::{self, ExpansionOp, Ast, Word, Span};
use std::collections::HashMap;
use std::io;
use std::process::{self, ExitStatus, Stdio};
use std::sync::{Arc, Mutex};

#[derive(Debug)]
pub struct ExecEnv {
    vars: HashMap<String, Arc<String>>,
}

lazy_static! {
    static ref EXEC_ENV: Mutex<ExecEnv> = Mutex::new(ExecEnv {
        vars: HashMap::new(),
    });
}

pub fn set_var(key: &str, value: &str) {
    EXEC_ENV
        .lock()
        .unwrap()
        .vars
        .insert(key.into(), Arc::new(value.into()))
        .unwrap();
}

pub fn get_var<'a>(key: &str) -> Option<Arc<String>> {
    EXEC_ENV
        .lock()
        .unwrap()
        .vars
        .get(key.into())
        .map(|v| v.clone())
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

fn expand_param(name: String, op: ExpansionOp) -> Arc<String> {
    match op {
        ExpansionOp::GetOrEmpty => {
            get_var(name.as_str()).unwrap_or(Arc::new("".to_owned()))
        },
        ExpansionOp::Length => {
            let value = get_var(name.as_str()).unwrap_or(Arc::new("".to_owned()));
            Arc::new(value.len().to_string())
        },
        _ => panic!("TODO:"),
    }
}

fn evaluate_span(span: Span) -> Arc<String> {
    match span {
        Span::Literal(s) => Arc::new(s),
        Span::Parameter { name, op } => expand_param(name, op),
        _ => panic!("TODO:"),
    }
}

fn evaluate_word(word: Word) -> String {
    let mut s = String::new();
    for span in word.0 {
        s += evaluate_span(span).as_str();
    }
    s
}

fn evaluate_words(words: Vec<Word>) -> Vec<String> {
    let mut evaluated = Vec::new();
    for word in words {
        let s = evaluate_word(word);
        if s.len() > 0 {
            evaluated.push(s);
        }
    }

    evaluated
}

fn run_command(command: parser::Command) -> i32 {
    match command {
        parser::Command::SimpleCommand { argv: wargv, .. } => {
            let argv2 = evaluate_words(wargv);
            match exec_command(argv2, Stdio::inherit(), Stdio::inherit(), Stdio::inherit()) {
                Ok(status) => status.code().unwrap(),
                Err(_) => {
                    println!("nsh: failed to execute");
                    -1
                },
            }
        },
        _ => panic!("TODO:"),
    }
}

pub fn exec(ast: Ast) -> i32 {
    trace!("exec: {:#?}", ast);
    let mut result = -128;
    for term in ast.terms {
        for pipeline in term.pipelines {
            for command in pipeline.commands {
                result = run_command(command);
            }
        }
    }

    result
}
