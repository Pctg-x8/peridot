use std::io::Result as IOResult;

use appkit::NSString;

pub struct NativeLogStream;
impl std::io::Write for &'_ NativeLogStream {
    fn write(&mut self, buf: &[u8]) -> IOResult<usize> {
        unsafe {
            let mut fmt =
                NSString::from_str(core::str::from_utf8_unchecked(buf)).expect("NSString");
            NSLog(&mut *fmt);
            Ok(buf.len())
        }
    }

    fn flush(&mut self) -> IOResult<()> {
        std::io::stderr().flush()
    }
}
impl<'a> tracing_subscriber::fmt::MakeWriter<'a> for NativeLogStream {
    type Writer = &'a Self;

    fn make_writer(&'a self) -> Self::Writer {
        self
    }
}

pub struct NSLogger;
impl log::Log for NSLogger {
    fn log(&self, record: &log::Record) {
        if self.enabled(record.metadata()) {
            unsafe {
                let mut fmt =
                    NSString::from_str(&format!("[{}] {}", record.level(), record.args()))
                        .expect("NSString");
                NSLog(&mut *fmt);
            }
        }
    }
    fn enabled(&self, metadata: &log::Metadata) -> bool {
        metadata.level() <= log::Level::Info
    }
    fn flush(&self) {}
}
extern "C" {
    fn NSLog(format: *mut NSString, ...);
}
