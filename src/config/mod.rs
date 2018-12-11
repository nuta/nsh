mod server;

#[derive(Debug, Deserialize)]
pub struct Config {
    // `$PROMPT`.
    #[serde(default = "default_prompt")]
    pub prompt: String,

    /// `$PATH`. Separated by `:`.
    #[serde(default = "default_path")]
    pub path: String,

    /// Startup script (known as `.bashrc` in Bash).
    #[serde(default = "default_empty")]
    pub rc: String,
}

fn default_prompt() -> String {
    "\\c{cyan}\\c{bold}\\u@\\h:\\c{reset} \\W\\n$\\c{reset} ".to_owned()
}

fn default_path() -> String {
    "/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/sbin".to_owned()
}

fn default_empty() -> String {
    String::new()
}

pub fn main() {
    server::main();
}
