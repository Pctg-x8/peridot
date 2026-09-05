use windows::Win32::System::Diagnostics::Debug::OutputDebugStringW;
use windows_core::PCWSTR;

pub struct DebugOutputWriter;
impl<'a> tracing_subscriber::fmt::MakeWriter<'a> for DebugOutputWriter {
    type Writer = &'a Self;

    fn make_writer(&'a self) -> Self::Writer {
        self
    }
}
impl std::io::Write for &'_ DebugOutputWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let zero_terminated = unsafe {
            core::str::from_utf8_unchecked(buf)
                .encode_utf16()
                .chain(core::iter::once(0))
                .collect::<Vec<_>>()
        };

        unsafe {
            OutputDebugStringW(PCWSTR(zero_terminated.as_ptr()));
        }

        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}
