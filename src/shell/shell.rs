use nix::sys::termios::{tcsetattr, SetArg, Termios};
use nsh_parser::parser::Parser;

use crate::path_table::PathTable;

pub struct Shell {
    parser: Parser,
    path_table: PathTable,
    saved_termios: Termios,
}

impl Shell {
    pub fn path_table(&self) -> &PathTable {
        &self.path_table
    }

    pub fn restore_termios(&self) {
        tcsetattr(0, SetArg::TCSADRAIN, &self.saved_termios).expect("failed to tcsetattr");
    }
}
