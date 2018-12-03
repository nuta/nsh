use nix::unistd;
use std::io::{Read, Write};
use std::os::unix::io::RawFd;

pub fn get_env(name: &str, default: &str) -> String {
    std::env::var(name).unwrap_or_else(|_| default.to_string())
}

/// `File`-like object but does not close the `fd`.
pub struct FdFile {
    fd: RawFd,
}

impl FdFile {
    pub fn new(fd: RawFd) -> FdFile {
        FdFile { fd }
    }
}

impl Write for FdFile {
    #[inline]
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let len = unistd::write(self.fd, buf).expect("failed to write");
        Ok(len)
    }

    #[inline]
    fn flush(&mut self) -> std::io::Result<()> {
        unistd::fsync(self.fd).ok();
        Ok(())
    }
}

impl Read for FdFile {
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let len = unistd::read(self.fd, buf).expect("failed to read");
        Ok(len)
    }
}
