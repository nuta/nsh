use nsh::shell::Shell;

pub fn main() {
    let shell = Shell::new();
    if let Err(error) = shell.run("echo hello") {
        eprintln!("nsh: {error}");
        std::process::exit(1);
    }
}
