use anyhow::Result;
use nix::sys::{
    signal::Signal,
    termios::{tcsetattr, SetArg, Termios},
};
use nsh_parser::parser::Parser;

use crate::{
    job::{Job, ProcessState},
    path::PathTable,
};

fn restore_terminal_attrs(termios: &Termios) -> Result<()> {
    tcsetattr(0, SetArg::TCSADRAIN, termios)?;
    Ok(())
}

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
        restore_terminal_attrs(&self.saved_termios);
    }

    pub fn run_in_foreground(&mut self, job: &Job, sigcont: bool) -> ProcessState {
        todo!()
        /*
        self.last_fore_job = Some(job.clone());
        self.background_jobs.remove(job);
        set_terminal_process_group(job.pgid());

        if sigcont {
            if let Some(ref termios) = job.termios() {
                restore_terminal_attrs(termios).expect("failed to tcsetattr");
            }

            kill_process_group(job.pgid(), Signal::SIGCONT).expect("failed to kill(SIGCONT)");
            trace!("sent sigcont");
        }

        // Wait for the job to exit or stop.
        let status = wait_for_job(shell, job);

        // Save the current terminal status.
        job.termios
            .replace(Some(tcgetattr(0).expect("failed to tcgetattr")));

        // Go back into the shell.
        set_terminal_process_group(self.shell_pgid);
        self.restore_termios();

        status
        */
    }
}
