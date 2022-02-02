use std::fmt::Debug;

pub trait OopsExt: Sized {
    fn oops_with_reason(self, reason: &str);

    fn oops(self) {
        self.oops_with_reason("");
    }

    fn oops_with<F: FnOnce() -> String>(self, reason: F) {
        self.oops_with_reason(&reason())
    }
}

impl<T, E: Debug> OopsExt for std::result::Result<T, E> {
    fn oops_with_reason(self, reason: &str) {
        match self {
            Ok(_) => {}
            Err(err) if reason.is_empty() => {
                warn!("oops: {:?}", err);
                crate::logger::backtrace();
            }
            Err(err) => {
                warn!("oops: {}: {:?}", reason, err);
                crate::logger::backtrace();
            }
        }
    }
}
