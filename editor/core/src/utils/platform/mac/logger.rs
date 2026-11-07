use std::cell::RefCell;

use tracing::{Level, Subscriber};
use tracing_subscriber::{Layer, fmt::FormatFields, registry::LookupSpan};

pub struct LogLayer;
impl<S: Subscriber + for<'a> LookupSpan<'a>> Layer<S> for LogLayer {
    fn on_event(&self, event: &tracing::Event<'_>, ctx: tracing_subscriber::layer::Context<'_, S>) {
        thread_local! {
            static BUF: RefCell<String> = RefCell::new(String::new());
        }

        BUF.with(|buf| {
            let mut buflock = buf.try_borrow_mut();
            let mut tmpbuf;
            let mut buf = match buflock.as_mut() {
                Ok(x) => &mut *x,
                Err(_) => {
                    tmpbuf = String::new();
                    &mut tmpbuf
                }
            };
            let current_thread = std::thread::current();
            let nowtime = time::OffsetDateTime::from(std::time::SystemTime::now());

            struct StringIoWrite<'a>(&'a mut String);
            impl std::io::Write for StringIoWrite<'_> {
                #[inline(always)]
                fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
                    self.0.push_str(unsafe { str::from_utf8_unchecked(buf) });
                    Ok(buf.len())
                }

                #[inline(always)]
                fn flush(&mut self) -> std::io::Result<()> {
                    Ok(())
                }
            }

            if let Err(_) = nowtime.format_into(
                &mut StringIoWrite(&mut buf),
                &time::format_description::well_known::Iso8601::DEFAULT,
            ) {
                unsafe {
                    crate::platform::mac::bridge::ni_log_err(
                        c"unable to format event".as_ptr().cast(),
                    );
                }
                return;
            }
            let mut writer = tracing_subscriber::fmt::format::Writer::new(&mut *buf);
            if let Err(_) = write!(
                writer,
                " [{}] {}: ",
                event.metadata().level(),
                event.metadata().target()
            ) {
                unsafe {
                    crate::platform::mac::bridge::ni_log_err(
                        c"unable to format event".as_ptr().cast(),
                    );
                }
                return;
            }
            if let Err(_) = tracing_subscriber::fmt::format::DefaultFields::new()
                .format_fields(writer.by_ref(), event)
            {
                unsafe {
                    crate::platform::mac::bridge::ni_log_err(
                        c"unable to format event".as_ptr().cast(),
                    );
                }
                return;
            }
            if let Err(_) = write!(
                writer,
                "\n  at {}:{} ",
                event.metadata().file().unwrap_or("<unknown file>"),
                event.metadata().line().unwrap_or(0)
            ) {
                unsafe {
                    crate::platform::mac::bridge::ni_log_err(
                        c"unable to format event".as_ptr().cast(),
                    );
                }
                return;
            }
            if let Err(_) = match current_thread.name() {
                Some(n) => write!(writer, "[{n}]"),
                None => write!(writer, "[ThreadID#{:?}]", current_thread.id()),
            } {
                unsafe {
                    crate::platform::mac::bridge::ni_log_err(
                        c"unable to format event".as_ptr().cast(),
                    );
                }
                return;
            }

            if let Some(scope) = ctx.event_scope(event) {
                for s in scope {
                    if let Err(_) = write!(
                        writer,
                        "\n  in {}:{} {}",
                        s.metadata().file().unwrap_or("<unknown file>"),
                        s.metadata().line().unwrap_or(0),
                        s.name()
                    ) {
                        unsafe {
                            crate::platform::mac::bridge::ni_log_err(
                                c"unable to format event".as_ptr().cast(),
                            );
                        }
                        return;
                    }
                }
            }

            if let Err(_) = write!(writer, "\0") {
                unsafe {
                    crate::platform::mac::bridge::ni_log_err(
                        c"unable to format event".as_ptr().cast(),
                    );
                }
                return;
            }

            match event.metadata().level() {
                &Level::ERROR => unsafe { crate::platform::mac::bridge::ni_log_err(buf.as_ptr()) },
                &Level::WARN => unsafe { crate::platform::mac::bridge::ni_log_warn(buf.as_ptr()) },
                &Level::INFO => unsafe { crate::platform::mac::bridge::ni_log_info(buf.as_ptr()) },
                &Level::DEBUG => unsafe {
                    crate::platform::mac::bridge::ni_log_debug(buf.as_ptr())
                },
                &Level::TRACE => unsafe {
                    crate::platform::mac::bridge::ni_log_trace(buf.as_ptr())
                },
            }

            buf.clear();
        })
    }
}
