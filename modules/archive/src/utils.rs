use std::io::{Error as IOError, ErrorKind, IoSlice, Result as IOResult, Write};

pub fn write_all_vectored(
    w: &mut (impl Write + ?Sized),
    mut buffers: &mut [IoSlice],
) -> IOResult<()> {
    // reduce empty ioslices
    IoSlice::advance_slices(&mut buffers, 0);

    while !buffers.is_empty() {
        match w.write_vectored(buffers) {
            Ok(0) => {
                return Err(IOError::new(
                    ErrorKind::WriteZero,
                    "Failed to write whole buffer",
                ));
            }
            Ok(n) => IoSlice::advance_slices(&mut buffers, n),
            Err(e) if e.kind() == ErrorKind::Interrupted => (),
            Err(e) => return Err(e),
        }
    }

    Ok(())
}

#[cfg(feature = "async-rt-async-std")]
pub async fn write_all_vectored_async<'b>(
    w: &'b mut (impl async_std::io::Write + Unpin + ?Sized),
    mut buffers: &'b mut [IoSlice<'b>],
) -> IOResult<()> {
    // reduce empty ioslices
    IoSlice::advance_slices(&mut buffers, 0);

    while !buffers.is_empty() {
        match async_std::io::WriteExt::write_vectored(w, buffers).await {
            Ok(0) => {
                return Err(IOError::new(
                    ErrorKind::WriteZero,
                    "Failed to write whole buffer",
                ));
            }
            Ok(n) => IoSlice::advance_slices(&mut buffers, n),
            Err(e) if e.kind() == ErrorKind::Interrupted => (),
            Err(e) => return Err(e),
        }
    }

    Ok(())
}

#[cfg(feature = "async-rt-async-std")]
struct ReadVFuture<'r, 'bs, 'b, R: 'r + ?Sized> {
    reader: &'r mut R,
    buffers: &'bs mut [std::io::IoSliceMut<'b>],
}
#[cfg(feature = "async-rt-async-std")]
impl<'r, 'bs, 'b, R: 'r + ?Sized> std::future::Future for ReadVFuture<'r, 'bs, 'b, R>
where
    R: async_std::io::Read + Unpin,
{
    type Output = IOResult<usize>;

    fn poll(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        let Self { reader, buffers } = &mut *self;

        async_std::io::Read::poll_read_vectored(std::pin::Pin::new(reader), cx, buffers)
    }
}
