//! A diagnosis program to report/fix a bug easier.
use termion::color;
use termion::style;

macro_rules! print_title {
    ($title:expr) => {
        println!(
            "{}{}{}{}{}",
            style::Reset,
            style::Bold,
            style::Underline,
            $title,
            style::Reset
        );
    };
}

macro_rules! print_env {
    ($key:expr) => {
        let value = std::env::var($key).unwrap_or_else(|_| "".to_owned());
        println!(
            "{}{}={}{}{}",
            style::Reset,
            $key,
            color::Fg(color::Green),
            value,
            style::Reset
        );
    };
}

pub fn main() {
    print_title!("Build Environment");
    println!("{}", env!("RUSTC_VERSION"));
    println!("{}", env!("CARGO_VERSION"));
    println!();

    print_title!("nsh");
    println!("version: {}", env!("CARGO_PKG_VERSION"));
    println!();

    print_title!("Terminal");
    print_env!("TERM_PROGRAM");
    print_env!("TERM_PROGRAM_VERSION");
    print_env!("TERM");
    print_env!("COLORTERM");
    print_env!("SHELL");
    print_env!("SHLVL");
    print_env!("TMUX");
    println!();
}
