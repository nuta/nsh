use std::path::Path;
use std::collections::HashMap;
use std::io;
use std::os::unix::fs::PermissionsExt;

struct DirColorEntry {
    bold: bool,
    color: String,
}

pub struct DirColor {
    map: HashMap<String, DirColorEntry>,
}

impl DirColor {
    pub fn new() -> DirColor {
        DirColor {
            map: HashMap::new(),
        }
    }

    pub fn load(&mut self, dircolors: &str) {
        for part in dircolors.trim().split(':') {
            let mut columns = part.splitn(2, '=');
            match (columns.next(), columns.next()) {
                (Some(key), Some(value)) => {
                    let mut cols = value.split(';');
                    let (bold, color) = match cols.next() {
                        Some("01") => (true, cols.next()),
                        Some("05") => (false, cols.next()),
                        col @ _ => (false, col),
                    };

                    if let Some(color) = color {
                        let entry = DirColorEntry {
                            color: color.to_owned(),
                            bold,
                        };

                        self.map.insert(key.to_owned(), entry);
                    }
                }
                _ => {}
            }
        }
    }

    pub fn write<W: io::Write>(&self, buf: &mut W, path: &Path) -> io::Result<()> {
        let metadata = path.metadata()?;
        let key = if metadata.is_dir() {
            "di"
        } else if metadata.permissions().mode() == 0o777 {
            "ex"
        } else if let Some(ext) = path.extension() {
            ext.to_str().unwrap()
        } else {
            "no"
        };

        if let Some(e) = self.map.get(key) {
            use termion::style::Bold;
            use termion::color::{Fg, Red, Green, Yellow, Blue, Magenta, Cyan};

            if e.bold {
                write!(buf, "{}", Bold)?;
            }

            match e.color.as_str() {
                "31" => { write!(buf, "{}", Fg(Red))?; }
                "32" => { write!(buf, "{}", Fg(Green))?; }
                "33" => { write!(buf, "{}", Fg(Yellow))?; }
                "34" => { write!(buf, "{}", Fg(Blue))?; }
                "35" => { write!(buf, "{}", Fg(Magenta))?; }
                "36" => { write!(buf, "{}", Fg(Cyan))?; }
                _ => {}
            }
        }

        Ok(())
    }
}
