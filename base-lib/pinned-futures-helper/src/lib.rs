use core::pin::Pin;
use futures_io::AsyncRead;

/// `AsyncReadExt::read_exact` with pinned reference
pub async fn read_exact_async_pinned(
    mut reader: Pin<&mut (impl AsyncRead + ?Sized)>,
    buf: &mut [core::mem::MaybeUninit<u8>],
) -> std::io::Result<()> {
    let mut ro = 0;
    while ro < buf.len() {
        let read = core::future::poll_fn(|cx| {
            reader.as_mut().poll_read(cx, unsafe {
                core::mem::transmute::<&mut [core::mem::MaybeUninit<_>], &mut [_]>(&mut buf[ro..])
            })
        })
        .await?;
        if read == 0 {
            return Err(std::io::ErrorKind::UnexpectedEof.into());
        }

        ro += read;
    }

    Ok(())
}
