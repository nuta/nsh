lazy_static! {
    pub static ref COLORS_ENABLED: bool = {
        use std::os::unix::io::AsRawFd;
        nix::unistd::isatty(std::io::stdout().as_raw_fd()).unwrap_or(false)
    };
}

#[macro_export]
macro_rules! print_err {
    () => { eprintln!(""); };
    ($fmt:expr) => {
        if *crate::macros::COLORS_ENABLED {
            eprintln!(concat!("{}{}nsh: ", $fmt, "{}"),
                ::crossterm::style::SetAttribute(::crossterm::style::Attribute::Bold),
                ::crossterm::style::SetForegroundColor(::crossterm::style::Color::Yellow),
                ::crossterm::style::SetAttribute(::crossterm::style::Attribute::Reset));
        } else {
            eprintln!(concat!("nsh: ", $fmt));
        }
    };
    () => { eprintln!(""); };
    ($fmt:expr, $($arg:tt)*) => {
        if *crate::macros::COLORS_ENABLED {
            eprintln!(concat!("{}{}nsh: ", $fmt, "{}"),
                ::crossterm::style::SetAttribute(::crossterm::style::Attribute::Bold),
                ::crossterm::style::SetForegroundColor(::crossterm::style::Color::Yellow),
                $($arg)*,
                ::crossterm::style::SetAttribute(::crossterm::style::Attribute::Reset));
        } else {
            eprintln!(concat!("nsh: ", $fmt), $($arg)*);
        }
    };
}
