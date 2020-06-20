#[macro_export]
macro_rules! print_err {
    () => { eprintln!(""); };
    ($fmt:expr) => {
        eprintln!(concat!("{}{}nsh: ", $fmt, "{}"),
            ::termion::style::Bold,
            ::termion::color::Fg(::termion::color::Yellow),
            ::termion::style::Reset);
    };
    () => { eprintln!(""); };
    ($fmt:expr, $($arg:tt)*) => {
        eprintln!(concat!("{}{}nsh: ", $fmt, "{}"),
            ::termion::style::Bold,
            ::termion::color::Fg(::termion::color::Yellow),
            $($arg)*,
            ::termion::style::Reset);
    };
}
