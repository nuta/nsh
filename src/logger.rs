
use std::sync::Mutex;
use std::fs::OpenOptions;
use std::io::prelude::*;
use log::{Record, Metadata, Level, LevelFilter};
use termion::style::{Bold, Reset};
use termion::color;

struct Logger {
    file: Mutex<std::fs::File>,
}

impl log::Log for Logger {
    fn enabled(&self, _metadata: &Metadata) -> bool {
        true
    }

    fn log(&self, record: &Record) {
        if self.enabled(record.metadata()) {
            let mut file = self.file.lock().unwrap();
            let mut level_color = String::new();
            match record.level() {
                Level::Info  => {
                    use std::fmt::Write;
                    write!(level_color, "{}", color::Fg(color::Blue)).ok();
                },
                Level::Error => {
                    use std::fmt::Write;
                    write!(level_color, "{}", color::Fg(color::LightRed)).ok();
                },
                Level::Warn => {
                    use std::fmt::Write;
                    write!(level_color, "{}", color::Fg(color::LightMagenta)).ok();
                },
                Level::Debug => {
                    use std::fmt::Write;
                    write!(level_color, "{}", color::Fg(color::Green)).ok();
                },
                _ => {
                    use std::fmt::Write;
                    write!(level_color, "{}", color::Fg(color::Reset)).ok();
                },
            }

            writeln!(
                file,
                "{}{}[{}]{} {}{}: {}",
                Bold,
                level_color,
                record.level(),
                color::Fg(color::LightBlack),
                record.target(),
                Reset,
                record.args()
            ).ok();
        }
    }

    fn flush(&self) {
        let mut file = self.file.lock().unwrap();
        file.flush().ok();
    }
}

lazy_static! {
    static ref GLOBAL_LOGGER: Logger = {
        let mut log_file_path = dirs::home_dir().unwrap();
        log_file_path.push(".nsh.log");

        let file = OpenOptions::new()
            .create(true)
            .truncate(false)
            .append(true)
            .open(log_file_path)
            .unwrap();

        Logger {
            file: Mutex::new(file),
        }
    };
}

pub fn init() {
    log::set_logger(&*GLOBAL_LOGGER).unwrap();
    log::set_max_level(LevelFilter::Trace);
}
