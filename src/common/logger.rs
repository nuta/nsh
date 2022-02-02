use backtrace::Backtrace;
use log::Level;

use crate::dirs::log_file_path;

pub fn install_logger(name: &str) {
    fern::Dispatch::new()
        .format(|out, message, record| {
            out.finish(format_args!(
                "{}[{}:{}] {}{}\x1b[0m",
                match record.level() {
                    Level::Error => "\x1b[1;31m",
                    Level::Warn => "\x1b[1;33m",
                    _ => "\x1b[34m",
                },
                record.file().unwrap_or_else(|| record.target()),
                record.line().unwrap_or(0),
                match record.level() {
                    Level::Error => "\x1b[1;31m",
                    Level::Warn => "\x1b[1;33m",
                    _ => "\x1b[0m",
                },
                message
            ))
        })
        .level(if cfg!(debug_assertions) {
            log::LevelFilter::Trace
        } else {
            log::LevelFilter::Info
        })
        .chain(fern::log_file(log_file_path(name)).unwrap())
        .apply()
        .expect("failed to initialize the logger");

    std::panic::set_hook(Box::new(|info| {
        error!("{}", info);
        prettify_backtrace(backtrace::Backtrace::new());
    }));
}

pub fn prettify_backtrace(backtrace: Backtrace) {
    for (i, frame) in backtrace.frames().iter().enumerate() {
        for symbol in frame.symbols() {
            if let Some(path) = symbol.filename() {
                let filename = path.to_str().unwrap_or("(non-utf8 path)");
                if filename.contains("/.rustup/")
                    || filename.contains("/.cargo/")
                    || filename.starts_with("/rustc/")
                {
                    continue;
                }

                error!(
                    "    #{} {}:{}, col {}",
                    i,
                    filename,
                    symbol.lineno().unwrap_or(0),
                    symbol.colno().unwrap_or(0),
                );
            }
        }
    }
}

pub fn backtrace() {
    prettify_backtrace(backtrace::Backtrace::new());
}
