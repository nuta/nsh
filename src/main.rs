mod shell;

use shell::Shell;

pub fn main() {
    let shell = Shell::new();
    shell.run("echo hello");
}
