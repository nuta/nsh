use std::thread;
use std::sync::{Arc, Mutex, Condvar};
use crossbeam_channel::{self, Sender, Receiver};

type Callback = fn() -> ();

#[derive(Debug, Clone)]
pub struct Work {
    callback: Callback,
    wait_lock: Arc<Mutex<bool>>,
    wait_cvar: Arc<Condvar>,
}

impl Work {
    pub fn new(callback: Callback) -> Work {
        Work {
            callback,
            wait_lock: Arc::new(Mutex::new(false)),
            wait_cvar: Arc::new(Condvar::new()),
        }
    }

    pub fn enqueue(&self) {
        add_work(self.clone());
    }

    pub fn wait(&self) {
        // TODO: support re-run
        let mut done = self.wait_lock.lock().unwrap();
        while !*done {
            done = self.wait_cvar.wait(done).unwrap();
        }
    }
}

lazy_static! {
    static ref WORKER_CH: (Sender<Work>, Receiver<Work>) = {
        crossbeam_channel::unbounded()
    };
}

pub fn add_work(work: Work) {
    let tx = &WORKER_CH.0;
    tx.send(work).unwrap();
}

pub fn worker_thread() {
    let rx = &WORKER_CH.1;
    loop {
        let work = rx.recv().unwrap();
        (work.callback)();

        // Notify that we have finished the work.
        let mut done = work.wait_lock.lock().unwrap();
        *done = true;
        work.wait_cvar.notify_one();

    }
}

pub fn start_worker_threads() {
    // TODO: determine the number of CPUs at runtime
    let num_threads = 4;

    for _ in 0..num_threads {
        thread::spawn(worker_thread);
    }
}
