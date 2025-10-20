use std::{
    io::IoSliceMut,
    os::{fd::AsRawFd, unix::ffi::OsStrExt},
    path::Path,
    ptr::NonNull,
};

use crate::native_io::generic_unix::{UnixFile, UnixFileUnmapData};

// dispatch_io requires block abi: https://clang.llvm.org/docs/Block-ABI-Apple.html
pub type BlockLiteralFlags = core::ffi::c_int;
pub const BLOCK_IS_NOESCAPE: BlockLiteralFlags = 1 << 23;
pub const BLOCK_HAS_COPY_DISPOSE: BlockLiteralFlags = 1 << 25;
pub const BLOCK_HAS_CTOR: BlockLiteralFlags = 1 << 26;
pub const BLOCK_IS_GLOBAL: BlockLiteralFlags = 1 << 28;
pub const BLOCK_HAS_STRET: BlockLiteralFlags = 1 << 29;
pub const BLOCK_HAS_SIGNATURE: BlockLiteralFlags = 1 << 30;

// 具体的なliteral型は別途入る
pub type Block = *mut core::ffi::c_void;

unsafe extern "C" {
    static _NSConcreteStackBlock: core::ffi::c_void;
    static _NSConcreteGlobalBlock: core::ffi::c_void;
}

pub type dispatch_block_t = Block;

pub type dispatch_data_t = *mut core::ffi::c_void;

unsafe extern "C" {
    pub fn dispatch_data_create(
        buffer: *const core::ffi::c_void,
        size: usize,
        queue: dispatch_queue_t,
        destructor: dispatch_block_t,
    ) -> dispatch_data_t;
    pub fn dispatch_data_get_size(data: dispatch_data_t) -> usize;
}

pub type dispatch_queue_t = *mut core::ffi::c_void;

unsafe extern "C" {
    static _dispatch_main_q: core::ffi::c_void;
}

#[inline(always)]
pub const fn dispatch_get_main_queue() -> dispatch_queue_t {
    unsafe { (&_dispatch_main_q as *const core::ffi::c_void).cast_mut() }
}

pub type dispatch_fd_t = core::ffi::c_int;

pub type dispatch_io_t = *mut core::ffi::c_void;

pub type dispatch_io_type_t = core::ffi::c_ulong;
pub const DISPATCH_IO_STREAM: dispatch_io_type_t = 0;
pub const DISPATCH_IO_RANDOM: dispatch_io_type_t = 1;

pub type dispatch_io_handler_t = Block;

pub type dispatch_io_close_flags_t = core::ffi::c_long;
pub const DISPATCH_IO_STOP: dispatch_io_close_flags_t = 0x01;

unsafe extern "C" {
    pub fn dispatch_io_create(
        r#type: dispatch_io_type_t,
        fd: dispatch_fd_t,
        queue: dispatch_queue_t,
        cleanup_handler: Block,
    ) -> dispatch_io_t;
    pub fn dispatch_io_close(channel: dispatch_io_t, flags: dispatch_io_close_flags_t);

    pub fn dispatch_io_read(
        channel: dispatch_io_t,
        offset: libc::off_t,
        length: usize,
        queue: dispatch_queue_t,
        io_handler: dispatch_io_handler_t,
    );
    pub fn dispatch_io_write(
        channel: dispatch_io_t,
        offset: libc::off_t,
        data: dispatch_data_t,
        queue: dispatch_queue_t,
        io_handler: dispatch_io_handler_t,
    );
}

#[repr(transparent)]
pub struct DispatchQueue(core::ffi::c_void);
impl DispatchQueue {
    pub fn main() -> Option<&'static Self> {
        unsafe { dispatch_get_main_queue().cast::<Self>().as_ref() }
    }
}

pub trait Disposable {
    fn dispose(&mut self);
}

#[repr(transparent)]
pub struct OwnedNativeObjectPtr<T: Disposable>(NonNull<T>);
impl<T: Disposable> Drop for OwnedNativeObjectPtr<T> {
    #[inline(always)]
    fn drop(&mut self) {
        unsafe {
            self.0.as_mut().dispose();
        }
    }
}

#[repr(transparent)]
pub struct DispatchIO(core::ffi::c_void);
impl Disposable for DispatchIO {
    fn dispose(&mut self) {
        unsafe {
            dispatch_io_close(&mut self.0, DISPATCH_IO_STOP);
        }
    }
}
impl DispatchIO {
    pub fn new<F>(
        r#type: dispatch_io_type_t,
        fd: &(impl AsRawFd + ?Sized),
        queue: &DispatchQueue,
        cleanup_handler: F,
    ) -> Option<OwnedNativeObjectPtr<Self>>
    where
        F: FnMut(core::ffi::c_int),
    {
        #[repr(C)]
        struct BlockLit<F> {
            isa: *mut core::ffi::c_void,
            flags: core::ffi::c_int,
            reserved: core::ffi::c_int,
            invoke: Option<extern "C" fn(*mut BlockLit<F>, core::ffi::c_int)>,
            descriptor: *mut BlockDesc,
            f: F,
        }
        #[repr(C)]
        struct BlockDesc {
            reserved: core::ffi::c_long,
            size: core::ffi::c_long,
        }

        extern "C" fn invoke_impl<F>(lit: *mut BlockLit<F>, error: core::ffi::c_int)
        where
            F: FnMut(core::ffi::c_int),
        {
            unsafe {
                ((*lit).f)(error);
            }
        }

        let block_desc: &'static BlockDesc = &BlockDesc {
            reserved: 0,
            size: core::mem::size_of::<BlockLit<F>>() as _,
        };
        let cleanup_handler_block = BlockLit {
            isa: unsafe { &_NSConcreteStackBlock as *const _ as _ },
            flags: 0,
            reserved: 0,
            invoke: Some(invoke_impl::<F>),
            descriptor: block_desc as *const _ as _,
            f: cleanup_handler,
        };

        let p = unsafe {
            dispatch_io_create(
                r#type,
                fd.as_raw_fd(),
                &queue.0 as *const _ as _,
                &cleanup_handler_block as *const _ as _,
            )
        };
        NonNull::new(p.cast::<Self>()).map(OwnedNativeObjectPtr)
    }
}

pub struct NativeFileReader(UnixFile);
impl NativeFileReader {
    #[inline(always)]
    pub fn open(path: impl AsRef<Path>) -> std::io::Result<Self> {
        Ok(Self(UnixFile::open(
            &std::ffi::CString::new(path.as_ref().as_os_str().as_bytes())
                .expect("nul character in the path"),
            libc::O_RDONLY,
        )?))
    }
}
impl super::RandomReadBlob for NativeFileReader {
    #[inline(always)]
    fn read(&self, offs: u64, buf: &mut [std::mem::MaybeUninit<u8>]) -> std::io::Result<usize> {
        self.0.pread(offs as _, buf)
    }

    #[inline(always)]
    fn readv(&self, offs: u64, iovecs: &mut [IoSliceMut]) -> std::io::Result<usize> {
        self.0.preadv(offs as _, unsafe {
            core::mem::transmute::<&mut [IoSliceMut], &mut [_]>(iovecs)
        })
    }
}
impl super::MemoryMapBlob for NativeFileReader {
    type MemoryUnmapData = UnixFileUnmapData;

    #[inline(always)]
    fn mmap(
        &self,
        offs: u64,
        len: usize,
    ) -> std::io::Result<(*mut core::ffi::c_void, Self::MemoryUnmapData)> {
        let r = self
            .0
            .mmap(len, libc::PROT_READ, libc::MAP_PRIVATE, offs as _)?;
        Ok((unsafe { r.start_addr.byte_add(r.offset) }, r))
    }

    #[inline(always)]
    fn munmap(&self, data: Self::MemoryUnmapData) -> std::io::Result<()> {
        data.unmap()
    }
}

pub struct NativeFileAsyncReader(OwnedNativeObjectPtr<DispatchIO>);
unsafe impl Sync for NativeFileAsyncReader {}
unsafe impl Send for NativeFileAsyncReader {}
impl NativeFileAsyncReader {
    pub fn open(path: impl AsRef<Path>) -> std::io::Result<Self> {
        unimplemented!()
    }
}
impl super::RandomReadBlobAsync for NativeFileAsyncReader {
    type ReadFuture<'a, 'b>
        = core::future::Ready<std::io::Result<usize>>
    where
        Self: 'a;
    type ReadVecFuture<'a, 'b, 'bb>
        = core::future::Ready<std::io::Result<usize>>
    where
        Self: 'a,
        'bb: 'b;

    fn read_async<'a, 'b>(
        &'a self,
        offs: u64,
        buf: &'b mut [core::mem::MaybeUninit<u8>],
    ) -> Self::ReadFuture<'a, 'b> {
        unimplemented!()
    }

    fn readv_async<'a, 'b, 'bb>(
        &'a self,
        offs: u64,
        buf: &'b mut [std::io::IoSliceMut<'bb>],
    ) -> Self::ReadVecFuture<'a, 'b, 'bb> {
        unimplemented!()
    }
}
impl super::MemoryMapBlob for NativeFileAsyncReader {
    type MemoryUnmapData = ();

    fn mmap(
        &self,
        offs: u64,
        len: usize,
    ) -> std::io::Result<(*mut core::ffi::c_void, Self::MemoryUnmapData)> {
        unimplemented!()
    }

    fn munmap(&self, data: Self::MemoryUnmapData) -> std::io::Result<()> {
        unimplemented!()
    }
}
