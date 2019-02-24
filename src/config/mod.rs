use dirs;
use failure;
use std::path::Path;
use std::io::{BufReader, BufRead};

type Result<T> = std::result::Result<T, failure::Error>;
mod server;

#[derive(Debug)]
pub struct Config {
    pub prompt: String,
    pub path: String,
    /// Startup script.
    pub rc: String,
}

const DEFAULT_PROMPT: &'static str = "\\{cyan}\\{bold}\\{current_dir} $\\{reset} ";
const DEFAULT_PATH: &'static str = "/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/sbin";

pub fn default_config() -> Config {
    Config {
        prompt: DEFAULT_PROMPT.to_owned(),
        path: DEFAULT_PATH.to_owned(),
        rc: "".to_owned(),
    }
}


pub fn load_nshrc() -> Result<Config> {
    let home_dir = dirs::home_dir().unwrap();
    let nshrc_path = Path::new(&home_dir).join(".nshrc");
    let file = std::fs::File::open(nshrc_path)?;

    let mut prompt = None;
    let mut path = None;
    let mut rc = String::new();
    for line in BufReader::new(file).lines() {
        let line = line?;
        rc += &line;
        rc.push('\n');

        if !line.starts_with("### ") {
            break;
        }

        let var = line[4..].to_owned();
        let mut cols = var.splitn(2, "=");
        if let Some(name) = cols.next() {
            let content = cols.next().unwrap_or("").to_owned();
            match name {
                "PROMPT" => {
                    prompt = Some(content);
                },
                "PATH" => {
                    path = Some(content);
                },
                _ => (),
            }
        }
    }

    Ok(Config {
        prompt: prompt.unwrap_or_else(|| DEFAULT_PROMPT.to_owned()),
        path: path.unwrap_or_else(|| DEFAULT_PATH.to_owned()),
        rc,
    })
}

pub fn main() {
    server::main();
}
