//! Peridot Archive

use std::io::prelude::{BufRead, Read};
use std::io::Result as IOResult;
use std::io::{Cursor, IoSliceMut, Seek, SeekFrom};

mod entry;
mod entry_tree;
mod native_io;
mod utils;

use bitflags::bitflags;

pub use self::entry::AssetEntryHeadingPair;
mod write;
pub use self::write::ArchiveWrite;
mod read;
pub use self::read::{Archive, ArchiveAsync, ArchiveReadError};

#[repr(C)]
pub struct LinearPaired2u64(u64, u64);

bitflags! {
    pub struct ContentFlags : u8 {
        const EMPTY = 0;
        const ROOT_HASH_TREE_EXACT = 0x01;
    }
}

/// 展開後のサイズが値として入る。圧縮指定時には無視されるので適当な値を指定する
#[derive(Debug)]
pub enum CompressionMethod {
    None,
    Zlib(u64),
    Lz4(u64),
    Zstd11(u64),
}

pub enum WhereArchiveAsync {
    OnMemory(Vec<u8>),
    #[cfg(feature = "async-rt-async-std")]
    FromIO(async_std::io::BufReader<async_std::fs::File>),
}
impl WhereArchiveAsync {
    pub async fn on_memory(&mut self) -> IOResult<&[u8]> {
        let replace_buf = match self {
            #[cfg(feature = "async-rt-async-std")]
            Self::FromIO(ref mut r) => {
                let mut buf = Vec::new();
                async_std::io::ReadExt::read_to_end(r, &mut buf).await?;
                Some(buf)
            }
            Self::OnMemory(_) => None,
        };

        if let Some(b) = replace_buf {
            *self = Self::OnMemory(b);
        }

        match self {
            Self::OnMemory(ref b) => Ok(b),
            _ => unreachable!(),
        }
    }
}

pub enum EitherArchiveReaderAsync {
    OnMemory(Cursor<Vec<u8>>),
    #[cfg(feature = "async-rt-async-std")]
    IO(async_std::io::BufReader<async_std::fs::File>),
}
impl From<WhereArchiveAsync> for EitherArchiveReaderAsync {
    #[inline]
    fn from(value: WhereArchiveAsync) -> Self {
        match value {
            WhereArchiveAsync::OnMemory(v) => Self::OnMemory(Cursor::new(v)),
            #[cfg(feature = "async-rt-async-std")]
            WhereArchiveAsync::FromIO(v) => Self::IO(v),
        }
    }
}
impl From<EitherArchiveReaderAsync> for WhereArchiveAsync {
    #[inline]
    fn from(value: EitherArchiveReaderAsync) -> Self {
        match value {
            EitherArchiveReaderAsync::OnMemory(v) => Self::OnMemory(v.into_inner()),
            #[cfg(feature = "async-rt-async-std")]
            EitherArchiveReaderAsync::IO(v) => Self::FromIO(v),
        }
    }
}
#[cfg(feature = "async-rt-async-std")]
impl async_std::io::Read for EitherArchiveReaderAsync {
    #[inline]
    fn poll_read(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut [u8],
    ) -> std::task::Poll<IOResult<usize>> {
        match self.get_mut() {
            Self::IO(ref mut r) => async_std::io::Read::poll_read(std::pin::Pin::new(r), cx, buf),
            Self::OnMemory(ref mut c) => std::task::Poll::Ready(c.read(buf)),
        }
    }

    #[inline]
    fn poll_read_vectored(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        bufs: &mut [IoSliceMut<'_>],
    ) -> std::task::Poll<IOResult<usize>> {
        match self.get_mut() {
            Self::IO(ref mut r) => {
                async_std::io::Read::poll_read_vectored(std::pin::Pin::new(r), cx, bufs)
            }
            Self::OnMemory(ref mut c) => std::task::Poll::Ready(c.read_vectored(bufs)),
        }
    }
}
#[cfg(feature = "async-rt-async-std")]
impl async_std::io::BufRead for EitherArchiveReaderAsync {
    #[inline]
    fn poll_fill_buf(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<IOResult<&[u8]>> {
        match self.get_mut() {
            Self::IO(ref mut r) => async_std::io::BufRead::poll_fill_buf(std::pin::Pin::new(r), cx),
            Self::OnMemory(ref mut c) => std::task::Poll::Ready(c.fill_buf()),
        }
    }

    #[inline]
    fn consume(self: std::pin::Pin<&mut Self>, amt: usize) {
        match self.get_mut() {
            Self::IO(ref mut r) => async_std::io::BufRead::consume(std::pin::Pin::new(r), amt),
            Self::OnMemory(ref mut c) => c.consume(amt),
        }
    }
}
#[cfg(feature = "async-rt-async-std")]
impl async_std::io::Seek for EitherArchiveReaderAsync {
    #[inline]
    fn poll_seek(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        pos: SeekFrom,
    ) -> std::task::Poll<IOResult<u64>> {
        match self.get_mut() {
            Self::IO(ref mut r) => async_std::io::Seek::poll_seek(std::pin::Pin::new(r), cx, pos),
            Self::OnMemory(ref mut c) => std::task::Poll::Ready(c.seek(pos)),
        }
    }
}
