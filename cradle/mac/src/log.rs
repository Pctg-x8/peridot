use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt, Layer, Registry};

use crate::native_interface::nslog_utf8;

pub struct NativeLogStream;
impl std::io::Write for &'_ NativeLogStream {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let fmt = unsafe { core::str::from_utf8_unchecked(buf) };
        unsafe {
            nslog_utf8(fmt.as_ptr(), fmt.len());
        }

        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        std::io::stderr().flush()
    }
}
impl<'a> tracing_subscriber::fmt::MakeWriter<'a> for NativeLogStream {
    type Writer = &'a Self;

    fn make_writer(&'a self) -> Self::Writer {
        self
    }
}

pub fn init_logging() {
    Registry::default()
        .with(
            tracing_subscriber::fmt::layer()
                .pretty()
                .with_writer(crate::log::NativeLogStream)
                .with_filter(tracing_subscriber::filter::EnvFilter::from_default_env()),
        )
        .init();
}
