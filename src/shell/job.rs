use std::fmt;

use nix::{
    sys::{
        termios::Termios,
        wait::{waitpid, WaitPidFlag, WaitStatus},
    },
    unistd::Pid,
};

use crate::shell::Shell;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum ProcessState {
    Running,
    /// Contains the exit status.
    Completed(i32),
    /// Suspended (Ctrl-Z).
    Stopped(Pid),
}

#[derive(Debug, Copy, Clone, PartialEq, Hash)]
pub struct JobId(usize);

impl JobId {
    pub fn new(id: usize) -> JobId {
        JobId(id)
    }
}

impl fmt::Display for JobId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

pub struct Job {
    id: JobId,
    pgid: Pid,
    cmd: String,
    // TODO: Remove entries in shell.states on destruction.
    processes: Vec<Pid>,
    termios: Option<Termios>,
}

impl Job {
    pub fn pgid(&self) -> Pid {
        self.pgid
    }

    pub fn termios(&self) -> Option<&Termios> {
        self.termios.as_ref()
    }

    /// Waits for all processes in the job to exit. Note that the job will be
    /// deleted from `shell` if the process has exited.
    pub fn wait_for_job(&self) -> ProcessState {
        todo!()
        /*
        loop {
            if job.completed(shell) || job.stopped(shell) {
                break;
            }

            wait_for_any_process(shell, false);
        }

        // Get the exit status of the last process.
        let state = shell
            .get_process_state(*job.processes.iter().last().unwrap())
            .cloned();

        match state {
            Some(ProcessState::Completed(_)) => {
                // Remove the job and processes from the list.
                destroy_job(shell, job);
                state.unwrap()
            }
            Some(ProcessState::Stopped(_)) => {
                print_err!("[{}] Stopped: {}", job.id, job.cmd);
                state.unwrap()
            }
            _ => unreachable!(),
        }
        */
    }
}
