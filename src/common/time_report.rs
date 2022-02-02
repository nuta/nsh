use std::time::Instant;

use once_cell::sync::OnceCell;

static ENABLED: OnceCell<bool> = OnceCell::new();

pub struct TimeReport {
    title: &'static str,
    started_at: Instant,
}

impl TimeReport {
    pub fn new(title: &'static str) -> TimeReport {
        TimeReport {
            title,
            started_at: Instant::now(),
        }
    }
}

impl Drop for TimeReport {
    fn drop(&mut self) {
        if !ENABLED
            .get_or_init(|| std::option_env!("TIME_REPORT").is_some() || cfg!(debug_assertions))
        {
            return;
        }

        info!("{} = {:?}", self.title, self.started_at.elapsed());
    }
}
