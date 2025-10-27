use std::{
    cell::UnsafeCell,
    io::IoSliceMut,
    ops::{Deref, DerefMut},
    os::{
        fd::{AsRawFd, RawFd},
        unix::ffi::OsStrExt,
    },
    path::Path,
    ptr::NonNull,
    sync::atomic::{AtomicPtr, AtomicUsize},
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

#[repr(C)]
pub struct BlockBase<F> {
    isa: *mut core::ffi::c_void,
    flags: core::ffi::c_int,
    reserved: core::ffi::c_int,
    invoke: Option<F>,
}

#[allow(non_camel_case_types)]
pub type dispatch_block_t = Block;

#[allow(non_camel_case_types)]
pub type dispatch_data_t = *mut core::ffi::c_void;

unsafe extern "C" {
    pub fn dispatch_data_create(
        buffer: *const core::ffi::c_void,
        size: usize,
        queue: dispatch_queue_t,
        destructor: dispatch_block_t,
    ) -> dispatch_data_t;
    pub fn dispatch_data_get_size(data: dispatch_data_t) -> usize;
    pub fn dispatch_data_apply(data: dispatch_data_t, applier: Block) -> bool;
}

pub unsafe fn dispatch_data_apply_wrapper<F>(data: dispatch_data_t, applier: F) -> bool
where
    F: FnMut(dispatch_data_t, usize, *const core::ffi::c_void, usize) -> bool,
{
    #[repr(C)]
    struct BlockLit<F> {
        base: BlockBase<
            extern "C" fn(
                *mut BlockLit<F>,
                dispatch_data_t,
                usize,
                *const core::ffi::c_void,
                usize,
            ) -> bool,
        >,
        descriptor: *mut BlockDesc,
        f: F,
    }
    #[repr(C)]
    struct BlockDesc {
        reserved: core::ffi::c_long,
        size: core::ffi::c_long,
    }

    extern "C" fn invoke_impl<F>(
        lit: *mut BlockLit<F>,
        region: dispatch_data_t,
        offset: usize,
        buffer: *const core::ffi::c_void,
        size: usize,
    ) -> bool
    where
        F: FnMut(dispatch_data_t, usize, *const core::ffi::c_void, usize) -> bool,
    {
        unsafe { ((*lit).f)(region, offset, buffer, size) }
    }

    let block_desc: &'static BlockDesc = &BlockDesc {
        reserved: 0,
        size: core::mem::size_of::<BlockLit<F>>() as _,
    };

    unsafe {
        dispatch_data_apply(
            data,
            &BlockLit {
                base: BlockBase {
                    isa: &_NSConcreteStackBlock as *const _ as _,
                    flags: 0,
                    reserved: 0,
                    invoke: Some(invoke_impl::<F>),
                },
                descriptor: block_desc as *const _ as _,
                f: applier,
            } as *const _ as _,
        )
    }
}

#[allow(non_camel_case_types)]
pub type dispatch_queue_t = *mut core::ffi::c_void;

unsafe extern "C" {
    static _dispatch_main_q: core::ffi::c_void;
}

#[inline(always)]
pub const fn dispatch_get_main_queue() -> dispatch_queue_t {
    unsafe { (&_dispatch_main_q as *const core::ffi::c_void).cast_mut() }
}

#[allow(non_camel_case_types)]
pub type dispatch_fd_t = core::ffi::c_int;

#[allow(non_camel_case_types)]
pub type dispatch_io_t = *mut core::ffi::c_void;

#[allow(non_camel_case_types)]
pub type dispatch_io_type_t = core::ffi::c_ulong;
pub const DISPATCH_IO_STREAM: dispatch_io_type_t = 0;
pub const DISPATCH_IO_RANDOM: dispatch_io_type_t = 1;

#[allow(non_camel_case_types)]
pub type dispatch_io_handler_t = Block;

#[allow(non_camel_case_types)]
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
impl<T: Disposable> Deref for OwnedNativeObjectPtr<T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref() }
    }
}
impl<T: Disposable> DerefMut for OwnedNativeObjectPtr<T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.as_mut() }
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
        F: FnMut(core::ffi::c_int) + Sync + Send,
    {
        #[repr(C)]
        struct BlockLit<F> {
            base: BlockBase<extern "C" fn(*mut BlockLit<F>, core::ffi::c_int)>,
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

        let p = unsafe {
            dispatch_io_create(
                r#type,
                fd.as_raw_fd(),
                &queue.0 as *const _ as _,
                &BlockLit {
                    base: BlockBase {
                        isa: &_NSConcreteStackBlock as *const _ as _,
                        flags: 0,
                        reserved: 0,
                        invoke: Some(invoke_impl::<F>),
                    },
                    descriptor: block_desc as *const _ as _,
                    f: cleanup_handler,
                } as *const _ as _,
            )
        };
        NonNull::new(p.cast::<Self>()).map(OwnedNativeObjectPtr)
    }

    pub fn read<F>(&self, offset: libc::off_t, length: usize, queue: &DispatchQueue, io_handler: F)
    where
        F: FnMut(bool, dispatch_data_t, core::ffi::c_int) + Sync + Send,
    {
        #[repr(C)]
        struct BlockLit<F> {
            base:
                BlockBase<extern "C" fn(*mut BlockLit<F>, bool, dispatch_data_t, core::ffi::c_int)>,
            descriptor: *mut BlockDesc,
            f: F,
        }
        #[repr(C)]
        struct BlockDesc {
            reserved: core::ffi::c_long,
            size: core::ffi::c_long,
        }

        extern "C" fn invoke_impl<F>(
            lit: *mut BlockLit<F>,
            done: bool,
            data: dispatch_data_t,
            error: core::ffi::c_int,
        ) where
            F: FnMut(bool, dispatch_data_t, core::ffi::c_int),
        {
            unsafe { ((*lit).f)(done, data, error) }
        }

        let block_desc: &'static BlockDesc = &BlockDesc {
            reserved: 0,
            size: core::mem::size_of::<BlockLit<F>>() as _,
        };

        unsafe {
            dispatch_io_read(
                self as *const _ as _,
                offset,
                length,
                queue as *const _ as _,
                &BlockLit {
                    base: BlockBase {
                        isa: &_NSConcreteStackBlock as *const _ as _,
                        flags: 0,
                        reserved: 0,
                        invoke: Some(invoke_impl::<F>),
                    },
                    descriptor: block_desc as *const _ as _,
                    f: io_handler,
                } as *const _ as _,
            )
        }
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
        Ok((r.data_addr(), r))
    }

    #[inline(always)]
    fn munmap(&self, data: Self::MemoryUnmapData) -> std::io::Result<()> {
        data.unmap()
    }
}

pub struct NativeFileAsyncReader {
    dch: OwnedNativeObjectPtr<DispatchIO>,
    fd: RawFd,
}
unsafe impl Sync for NativeFileAsyncReader {}
unsafe impl Send for NativeFileAsyncReader {}
impl NativeFileAsyncReader {
    pub fn open(path: impl AsRef<Path>) -> std::io::Result<Self> {
        let f = UnixFile::open(
            &std::ffi::CString::new(path.as_ref().as_os_str().as_bytes())
                .expect("nul character in the path"),
            libc::O_RDONLY,
        )?;
        let dio = DispatchIO::new(
            DISPATCH_IO_RANDOM,
            &f,
            DispatchQueue::main().expect("no main dispatch queue"),
            |_error| {
                let _ = unsafe { libc::close(f.as_raw_fd()) };
            },
        )
        .ok_or(std::io::Error::other("dispatch_io_create failed"))?;
        // DispatchIOでcloseするのでこっちではforget
        let fd = f.as_raw_fd();
        core::mem::forget(f);

        Ok(Self { dch: dio, fd })
    }
}
impl super::RandomReadBlobAsync for NativeFileAsyncReader {
    type ReadFuture<'a, 'b>
        = NativeFileReadFuture<'a, 'b>
    where
        Self: 'a;
    type ReadVecFuture<'a, 'b, 'bb>
        = NativeFileReadFuture<'a, 'b>
    where
        Self: 'a,
        'bb: 'b;

    #[inline(always)]
    fn read_async<'a, 'b>(
        &'a self,
        offs: u64,
        buf: &'b mut [core::mem::MaybeUninit<u8>],
    ) -> Self::ReadFuture<'a, 'b> {
        NativeFileReadFuture {
            f: self,
            pos: offs,
            buf,
            state: UnsafeCell::new(NativeFileReadState::Init),
        }
    }

    #[inline(always)]
    fn readv_async<'a, 'b, 'bb>(
        &'a self,
        offs: u64,
        buf: &'b mut [std::io::IoSliceMut<'bb>],
    ) -> Self::ReadVecFuture<'a, 'b, 'bb> {
        // TODO: 頑張れば一回のreadで複数iovec処理できそうではあるけどちょっと考慮すべき点が色々あって面倒なのでサポートなし扱いにする
        NativeFileReadFuture {
            f: self,
            pos: offs,
            buf: buf.first_mut().map_or(&mut [], |x| unsafe {
                core::mem::transmute::<&mut [_], &mut [core::mem::MaybeUninit<_>]>(x)
            }),
            state: UnsafeCell::new(NativeFileReadState::Init),
        }
    }
}
impl super::MemoryMapBlob for NativeFileAsyncReader {
    type MemoryUnmapData = UnixFileUnmapData;

    #[inline(always)]
    fn mmap(
        &self,
        offs: u64,
        len: usize,
    ) -> std::io::Result<(*mut core::ffi::c_void, Self::MemoryUnmapData)> {
        let p = unsafe {
            UnixFile::mmap_raw(self.fd, len, libc::PROT_READ, libc::MAP_PRIVATE, offs as _)?
        };

        Ok((p.data_addr(), p))
    }

    #[inline(always)]
    fn munmap(&self, data: Self::MemoryUnmapData) -> std::io::Result<()> {
        data.unmap()
    }
}

#[derive(Clone, Copy)]
enum NativeFileReadState {
    Init,
    Pending,
    CompleteSuccess(usize),
    CompleteFailure(core::ffi::c_int),
}

pub struct NativeFileReadFuture<'a, 'b> {
    f: &'a NativeFileAsyncReader,
    pos: u64,
    buf: &'b mut [core::mem::MaybeUninit<u8>],
    state: UnsafeCell<NativeFileReadState>,
}
impl<'a, 'b> core::future::Future for NativeFileReadFuture<'a, 'b> {
    type Output = std::io::Result<usize>;

    fn poll(
        self: core::pin::Pin<&mut Self>,
        cx: &mut core::task::Context<'_>,
    ) -> core::task::Poll<Self::Output> {
        let this = self.get_mut();

        match unsafe { *this.state.get() } {
            NativeFileReadState::Init => {
                *this.state.get_mut() = NativeFileReadState::Pending;
                this.f.dch.read(
                    this.pos as _,
                    this.buf.len(),
                    DispatchQueue::main().expect("no main queue"),
                    {
                        let state_sink: AtomicPtr<UnsafeCell<NativeFileReadState>> =
                            AtomicPtr::new(&this.state as *const _ as _);
                        let waker = cx.waker().clone();
                        let buf_head_ptr = AtomicPtr::new(this.buf.as_mut_ptr());
                        let transferred_accum = AtomicUsize::new(0);

                        move |done, data, error| {
                            if error != 0 {
                                // err
                                unsafe {
                                    *(*state_sink.load(core::sync::atomic::Ordering::Acquire))
                                        .get_mut() = NativeFileReadState::CompleteFailure(error);
                                }
                                waker.wake_by_ref();
                                return;
                            }

                            let transferred = unsafe { dispatch_data_get_size(data) };
                            let copy_offs = transferred_accum
                                .fetch_add(transferred, core::sync::atomic::Ordering::AcqRel);
                            let buf_head_ptr = unsafe {
                                buf_head_ptr
                                    .load(core::sync::atomic::Ordering::Acquire)
                                    .add(copy_offs)
                            };
                            unsafe {
                                dispatch_data_apply_wrapper(data, |_, offs, buf, size| {
                                    core::ptr::copy_nonoverlapping(
                                        buf as *const u8,
                                        buf_head_ptr.add(offs) as *mut u8,
                                        size,
                                    );
                                    true
                                });
                            }

                            if done {
                                unsafe {
                                    *(*state_sink.load(core::sync::atomic::Ordering::Acquire))
                                        .get_mut() = NativeFileReadState::CompleteSuccess(
                                        copy_offs + transferred,
                                    );
                                }
                                waker.wake_by_ref();
                            }
                        }
                    },
                );

                core::task::Poll::Pending
            }
            NativeFileReadState::Pending => core::task::Poll::Pending,
            NativeFileReadState::CompleteSuccess(s) => core::task::Poll::Ready(Ok(s)),
            NativeFileReadState::CompleteFailure(e) => {
                core::task::Poll::Ready(Err(std::io::Error::from_raw_os_error(e)))
            }
        }
    }
}
