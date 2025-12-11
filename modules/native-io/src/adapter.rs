//! Adapter structs for other crate's traits.

use pin_project::pin_project;
use std::io::{Read, Seek};

pub struct RandomBlobReadSeekAdapter<R> {
    inner: R,
    pos: u64,
}
impl<R> RandomBlobReadSeekAdapter<R> {
    #[inline(always)]
    pub const fn new(inner: R) -> Self {
        Self { inner, pos: 0 }
    }
}
impl<R> Read for RandomBlobReadSeekAdapter<R>
where
    R: crate::RandomReadBlob,
{
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let transferred = self.inner.read(self.pos, unsafe {
            core::mem::transmute::<&mut [_], &mut [core::mem::MaybeUninit<_>]>(buf)
        })?;
        self.pos += transferred as u64;

        Ok(transferred)
    }
}
impl<R> Seek for RandomBlobReadSeekAdapter<R>
where
    R: crate::BlobMetadata,
{
    #[inline]
    fn seek(&mut self, pos: std::io::SeekFrom) -> std::io::Result<u64> {
        let new_pos = match pos {
            std::io::SeekFrom::Start(x) => x,
            std::io::SeekFrom::Current(x) => (self.pos as i64 + x) as _,
            std::io::SeekFrom::End(x) => (self.inner.byte_length()? as i64 + x) as _,
        };
        self.pos = new_pos;
        Ok(new_pos)
    }
}

#[pin_project(project = RandomBlobAsyncReadSeekAdapterStateProjected)]
enum RandomBlobAsyncReadSeekAdapterState<'r, R: crate::RandomReadBlobAsync + 'r> {
    Idle,
    Reading(#[pin] R::ReadFuture<'r, 'static>),
    ReadingVec(#[pin] R::ReadVecFuture<'r, 'static, 'static>),
}

#[pin_project]
pub struct RandomBlobAsyncReadSeekAdapter<'r, R: crate::RandomReadBlobAsync + 'r> {
    inner: &'r R,
    pos: u64,
    #[pin]
    state: RandomBlobAsyncReadSeekAdapterState<'r, R>,
}
impl<'r, R: crate::RandomReadBlobAsync + 'r> RandomBlobAsyncReadSeekAdapter<'r, R> {
    #[inline(always)]
    pub const fn new(inner: &'r R) -> Self {
        Self::with_pos(inner, 0)
    }

    #[inline(always)]
    pub const fn with_pos(inner: &'r R, pos: u64) -> Self {
        Self {
            inner,
            pos,
            state: RandomBlobAsyncReadSeekAdapterState::Idle,
        }
    }
}
impl<'r, R: crate::RandomReadBlobAsync + 'r> futures_io::AsyncRead
    for RandomBlobAsyncReadSeekAdapter<'r, R>
{
    fn poll_read(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut [u8],
    ) -> std::task::Poll<std::io::Result<usize>> {
        let mut this = self.project();

        loop {
            match this.state.as_mut().project() {
                RandomBlobAsyncReadSeekAdapterStateProjected::Idle => {
                    this.state.set(RandomBlobAsyncReadSeekAdapterState::Reading(
                        this.inner.read_async(*this.pos, unsafe {
                            core::mem::transmute::<&mut [_], &mut [core::mem::MaybeUninit<_>]>(buf)
                        }),
                    ));
                }
                RandomBlobAsyncReadSeekAdapterStateProjected::Reading(f) => {
                    let r = std::task::ready!(f.poll(cx));
                    this.state.set(RandomBlobAsyncReadSeekAdapterState::Idle);
                    break std::task::Poll::Ready(r);
                }
                RandomBlobAsyncReadSeekAdapterStateProjected::ReadingVec(_) => {
                    panic!("poll_read called but poll_read_vectored is ongoing");
                }
            }
        }
    }

    fn poll_read_vectored(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        bufs: &mut [std::io::IoSliceMut<'_>],
    ) -> std::task::Poll<std::io::Result<usize>> {
        let mut this = self.project();

        loop {
            match this.state.as_mut().project() {
                RandomBlobAsyncReadSeekAdapterStateProjected::Idle => {
                    this.state
                        .set(RandomBlobAsyncReadSeekAdapterState::ReadingVec(
                            this.inner.readv_async(*this.pos, unsafe {
                                core::mem::transmute::<&mut [_], &mut [_]>(&mut *bufs)
                            }),
                        ));
                }
                RandomBlobAsyncReadSeekAdapterStateProjected::ReadingVec(f) => {
                    let r = std::task::ready!(f.poll(cx));
                    this.state.set(RandomBlobAsyncReadSeekAdapterState::Idle);
                    break std::task::Poll::Ready(r);
                }
                RandomBlobAsyncReadSeekAdapterStateProjected::Reading(_) => {
                    panic!("poll_read_vectored called but poll_read is ongoing");
                }
            }
        }
    }
}
impl<'r, R: crate::RandomReadBlobAsync + 'r> futures_io::AsyncSeek
    for RandomBlobAsyncReadSeekAdapter<'r, R>
{
    fn poll_seek(
        self: std::pin::Pin<&mut Self>,
        _cx: &mut std::task::Context<'_>,
        pos: std::io::SeekFrom,
    ) -> std::task::Poll<std::io::Result<u64>> {
        // これは同期的にできる
        let pref = self.project().pos;
        *pref = match pos {
            std::io::SeekFrom::Start(x) => x,
            std::io::SeekFrom::Current(x) => pref.checked_add_signed(x).ok_or_else(|| {
                std::io::Error::new(
                    std::io::ErrorKind::InvalidInput,
                    "resulting file pointer is out of range!",
                )
            })?,
            // TODO: ここ計算ただしくない(なんでこうなった？)のであとで直す ただBlobMetadata側の定義にも手を入れないといけないかも(Futureの具体型がとれない)
            std::io::SeekFrom::End(x) => pref.checked_add_signed(x).ok_or_else(|| {
                std::io::Error::new(
                    std::io::ErrorKind::InvalidInput,
                    "resulting file pointer is out of range!",
                )
            })?,
        };

        std::task::Poll::Ready(Ok(*pref))
    }
}
