use parser::{self, Ast};
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

pub fn exec(tree: Ast) -> i32 {
    trace!("exec: {:?}", tree);
    let mut result = -128;
    // TODO:
    /*
    for pipeline in tree.pipelines {
        for command in pipeline.commands {
            match command {
                parser::Command::SimpleCommand { argv, .. } => {
                    result = match exec_command(argv, Stdio::inherit(), Stdio::inherit(), Stdio::inherit()) {
                        Ok(status) => status.code().unwrap(),
                        Err(_) => {
                            println!("nsh: failed to execute");
                            -1
                        },
                    }
                }
            }
        }
    }
    */

    result
}
