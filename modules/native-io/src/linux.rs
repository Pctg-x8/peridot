use std::{
    cell::Cell,
    os::fd::{AsRawFd, RawFd},
    path::Path,
    sync::{Arc, atomic::AtomicU32},
};

use crate::generic_unix::{UnixFile, UnixFileUnmapData};

#[repr(transparent)]
pub struct File(RawFd);
impl Drop for File {
    #[inline]
    fn drop(&mut self) {
        let _ = unsafe { libc::close(self.0) };
    }
}
impl File {
    #[inline]
    pub fn open(pathname: &core::ffi::CStr, flags: core::ffi::c_int) -> std::io::Result<Self> {
        let fd = unsafe { libc::open(pathname.as_ptr(), flags) };
        if fd < 0 {
            Err(std::io::Error::last_os_error())
        } else {
            Ok(Self(fd))
        }
    }

    #[inline]
    pub fn lseek64(&self, offset: libc::off64_t, whence: core::ffi::c_int) -> std::io::Result<u64> {
        let r = unsafe { libc::lseek64(self.0, offset, whence) };
        if r < 0 {
            Err(std::io::Error::last_os_error())
        } else {
            Ok(r.cast_unsigned())
        }
    }
}

#[repr(transparent)]
pub struct NativeFileBlobRandomReader(UnixFile);
impl NativeFileBlobRandomReader {
    #[inline(always)]
    pub fn open(name: impl AsRef<Path>) -> std::io::Result<Self> {
        Ok(Self(UnixFile::open(
            &std::ffi::CString::new(name.as_ref().to_str().expect("invalid utf-8 sequence"))
                .expect("invalid for cstr"),
            libc::O_CLOEXEC,
        )?))
    }
}
impl super::BlobMetadata for NativeFileBlobRandomReader {
    fn byte_length(&self) -> std::io::Result<u64> {
        let mut stat = core::mem::MaybeUninit::uninit();
        self.0.stat64(&mut stat)?;
        let stat = unsafe { stat.assume_init_ref() };

        Ok(stat.st_size.cast_unsigned())
    }
}
impl super::RandomReadBlob for NativeFileBlobRandomReader {
    #[inline(always)]
    fn read(&self, pos: u64, buf: &mut [core::mem::MaybeUninit<u8>]) -> std::io::Result<usize> {
        self.0.pread(pos as _, buf)
    }

    #[inline(always)]
    fn readv(&self, pos: u64, buf: &mut [std::io::IoSliceMut]) -> std::io::Result<usize> {
        self.0.preadv(pos as _, unsafe {
            core::mem::transmute::<&mut [std::io::IoSliceMut], &mut [libc::iovec]>(buf)
        })
    }
}
impl super::MemoryMapBlob for NativeFileBlobRandomReader {
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

pub struct MemoryUnmapData {
    addr: *mut core::ffi::c_void,
    len: usize,
}

#[repr(transparent)]
pub struct NativeFileAsyncBlobRandomReader(File);
impl NativeFileAsyncBlobRandomReader {
    #[inline]
    pub fn open(name: impl AsRef<Path>) -> std::io::Result<Self> {
        Ok(Self(File::open(
            &std::ffi::CString::new(name.as_ref().to_str().expect("invalid utf-8 sequence"))
                .expect("invalid for cstr"),
            libc::O_CLOEXEC | libc::O_NONBLOCK,
        )?))
    }
}
impl super::BlobMetadataAsync for NativeFileAsyncBlobRandomReader {
    #[inline(always)]
    fn byte_length_async(&self) -> impl core::future::Future<Output = std::io::Result<u64>> {
        AsyncNativeFileByteLengthFuture {
            fd: self,
            buf: core::mem::MaybeUninit::uninit(),
            state: Arc::new(Cell::new(AsyncNativeFileReadState::Init)),
        }
    }
}
impl super::RandomReadBlobAsync for NativeFileAsyncBlobRandomReader {
    type ReadFuture<'a, 'b>
        = AsyncNativeFileReadFuture<'a, 'b>
    where
        Self: 'a;
    type ReadVecFuture<'a, 'b, 'b2>
        = AsyncNativeFileReadVecFuture<'a, 'b, 'b2>
    where
        Self: 'a,
        'b2: 'b;

    #[inline(always)]
    fn read_async<'a, 'b>(
        &'a self,
        pos: u64,
        buf: &'b mut [core::mem::MaybeUninit<u8>],
    ) -> Self::ReadFuture<'a, 'b> {
        AsyncNativeFileReadFuture {
            fd: self,
            pos,
            buf,
            state: Arc::new(Cell::new(AsyncNativeFileReadState::Init)),
        }
    }

    #[inline(always)]
    fn readv_async<'a, 'b, 'b2>(
        &'a self,
        pos: u64,
        buf: &'b mut [std::io::IoSliceMut<'b2>],
    ) -> Self::ReadVecFuture<'a, 'b, 'b2> {
        AsyncNativeFileReadVecFuture {
            fd: self,
            pos,
            iovecs: buf,
            state: Arc::new(Cell::new(AsyncNativeFileReadState::Init)),
        }
    }
}
impl super::MemoryMapBlob for NativeFileAsyncBlobRandomReader {
    type MemoryUnmapData = MemoryUnmapData;

    #[inline]
    fn mmap(
        &self,
        offs: u64,
        len: usize,
    ) -> std::io::Result<(*mut core::ffi::c_void, Self::MemoryUnmapData)> {
        let r = unsafe {
            libc::mmap(
                core::ptr::null_mut(),
                len,
                libc::PROT_READ,
                libc::MAP_PRIVATE,
                self.0.0,
                offs as _,
            )
        };
        if r == libc::MAP_FAILED {
            Err(std::io::Error::last_os_error())
        } else {
            Ok((r, MemoryUnmapData { addr: r, len }))
        }
    }

    #[inline]
    fn munmap(&self, data: Self::MemoryUnmapData) -> std::io::Result<()> {
        let r = unsafe { libc::munmap(data.addr, data.len) };
        if r < 0 {
            Err(std::io::Error::last_os_error())
        } else {
            Ok(())
        }
    }
}

#[derive(Clone, Copy)]
pub enum AsyncNativeFileReadState {
    Init,
    Pending,
    CompletedSuccess(usize),
    CompletedFailure(i32),
}

pub struct AsyncNativeFileReadFuture<'a, 'b> {
    fd: &'a NativeFileAsyncBlobRandomReader,
    pos: u64,
    buf: &'b mut [core::mem::MaybeUninit<u8>],
    state: Arc<Cell<AsyncNativeFileReadState>>,
}
impl<'a, 'b> Future for AsyncNativeFileReadFuture<'a, 'b> {
    type Output = std::io::Result<usize>;

    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        let this = self.get_mut();

        match this.state.get() {
            AsyncNativeFileReadState::Init => {
                // first call
                IoReactorHandle::current()
                    .expect("no reactor running")
                    .pusher
                    .push(|sqe| unsafe {
                        core::ptr::write_volatile(&mut sqe.fd, this.fd.0.0);
                        core::ptr::write_volatile(
                            &mut sqe.opcode,
                            linux_io_uring::ffi::IORING_OP_READ as _,
                        );
                        core::ptr::write_volatile(&mut sqe.union1.off, this.pos);
                        core::ptr::write_volatile(
                            &mut sqe.union2.addr,
                            this.buf.as_mut_ptr() as usize as _,
                        );
                        core::ptr::write_volatile(&mut sqe.len, this.buf.len() as _);
                        // TODO: 毎回mallocするのはちょっとやめたい気もする うまい感じのpoolつくれないか......
                        core::ptr::write_volatile(
                            &mut sqe.user_data,
                            Box::into_raw(Box::new(ReadFutureQueueData::Read {
                                state: Arc::downgrade(&this.state),
                                waker: cx.waker().clone(),
                            })) as usize as _,
                        );
                    });

                this.state.set(AsyncNativeFileReadState::Pending);
                core::task::Poll::Pending
            }
            AsyncNativeFileReadState::Pending => core::task::Poll::Pending,
            AsyncNativeFileReadState::CompletedSuccess(res) => core::task::Poll::Ready(Ok(res)),
            AsyncNativeFileReadState::CompletedFailure(e) => {
                core::task::Poll::Ready(Err(std::io::Error::from_raw_os_error(e)))
            }
        }
    }
}

pub struct AsyncNativeFileReadVecFuture<'a, 'b, 'b2> {
    fd: &'a NativeFileAsyncBlobRandomReader,
    pos: u64,
    iovecs: &'b mut [std::io::IoSliceMut<'b2>],
    state: Arc<Cell<AsyncNativeFileReadState>>,
}
impl<'a, 'b, 'b2> Future for AsyncNativeFileReadVecFuture<'a, 'b, 'b2> {
    type Output = std::io::Result<usize>;

    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        let this = self.get_mut();

        match this.state.get() {
            AsyncNativeFileReadState::Init => {
                // first call

                IoReactorHandle::current()
                    .expect("no reactor running")
                    .pusher
                    .push(|sqe| unsafe {
                        core::ptr::write_volatile(&mut sqe.fd, this.fd.0.0);
                        core::ptr::write_volatile(
                            &mut sqe.opcode,
                            linux_io_uring::ffi::IORING_OP_READV as _,
                        );
                        core::ptr::write_volatile(&mut sqe.union1.off, this.pos);
                        core::ptr::write_volatile(
                            &mut sqe.union2.addr,
                            this.iovecs.as_mut_ptr() as usize as _,
                        );
                        core::ptr::write_volatile(&mut sqe.len, this.iovecs.len() as _);
                        // TODO: 毎回mallocするのはちょっとやめたい気もする うまい感じのpoolつくれないか......
                        core::ptr::write_volatile(
                            &mut sqe.user_data,
                            Box::into_raw(Box::new(ReadFutureQueueData::Read {
                                state: Arc::downgrade(&this.state),
                                waker: cx.waker().clone(),
                            })) as usize as _,
                        );
                    });

                this.state.set(AsyncNativeFileReadState::Pending);
                core::task::Poll::Pending
            }
            AsyncNativeFileReadState::Pending => core::task::Poll::Pending,
            AsyncNativeFileReadState::CompletedSuccess(res) => core::task::Poll::Ready(Ok(res)),
            AsyncNativeFileReadState::CompletedFailure(e) => {
                core::task::Poll::Ready(Err(std::io::Error::from_raw_os_error(e)))
            }
        }
    }
}

const EMPTY_CSTR: &'static core::ffi::CStr =
    unsafe { core::ffi::CStr::from_bytes_with_nul_unchecked(&[0]) };

pub struct AsyncNativeFileByteLengthFuture<'a> {
    fd: &'a NativeFileAsyncBlobRandomReader,
    buf: core::mem::MaybeUninit<libc::statx>,
    state: Arc<Cell<AsyncNativeFileReadState>>,
}
impl<'a> Future for AsyncNativeFileByteLengthFuture<'a> {
    type Output = std::io::Result<u64>;

    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        let this = self.get_mut();

        match this.state.get() {
            AsyncNativeFileReadState::Init => {
                // first call
                IoReactorHandle::current()
                    .expect("no reactor running")
                    .pusher
                    .push(|sqe| unsafe {
                        core::ptr::write_volatile(&mut sqe.fd, this.fd.0.0);
                        core::ptr::write_volatile(
                            &mut sqe.opcode,
                            linux_io_uring::ffi::IORING_OP_STATX as _,
                        );
                        core::ptr::write_volatile(&mut sqe.union2.addr, EMPTY_CSTR.as_ptr() as _);
                        core::ptr::write_volatile(
                            &mut sqe.union3.statx_flags,
                            libc::AT_EMPTY_PATH as _,
                        );
                        core::ptr::write_volatile(&mut sqe.len, libc::STATX_SIZE);
                        core::ptr::write_volatile(
                            &mut sqe.union1.off,
                            this.buf.as_mut_ptr().addr() as _,
                        );
                        // TODO: 毎回mallocするのはちょっとやめたい気もする うまい感じのpoolつくれないか......
                        core::ptr::write_volatile(
                            &mut sqe.user_data,
                            Box::into_raw(Box::new(ReadFutureQueueData::Read {
                                state: Arc::downgrade(&this.state),
                                waker: cx.waker().clone(),
                            })) as usize as _,
                        );
                    });

                this.state.set(AsyncNativeFileReadState::Pending);
                core::task::Poll::Pending
            }
            AsyncNativeFileReadState::Pending => core::task::Poll::Pending,
            AsyncNativeFileReadState::CompletedSuccess(_) => {
                core::task::Poll::Ready(Ok(unsafe { this.buf.assume_init_ref().stx_size }))
            }
            AsyncNativeFileReadState::CompletedFailure(e) => {
                core::task::Poll::Ready(Err(std::io::Error::from_raw_os_error(e)))
            }
        }
    }
}

pub enum ReadFutureQueueData {
    Read {
        state: std::sync::Weak<Cell<AsyncNativeFileReadState>>,
        waker: core::task::Waker,
    },
}

#[derive(Clone)]
pub struct IoReactorHandle {
    pusher: SubmissionQueuePusher,
}
impl IoReactorHandle {
    #[inline]
    pub fn current() -> Option<Self> {
        IO_REACTOR_CURRENT_HANDLE.write().expect("poisoned").clone()
    }
}

static IO_REACTOR_CURRENT_HANDLE: std::sync::RwLock<Option<IoReactorHandle>> =
    std::sync::RwLock::new(None);

struct IoUringContext {
    uring: linux_io_uring::IoUring,
    sq_ptr: *mut core::ffi::c_void,
    sq_size: usize,
    // None for shared with sq
    cq: Option<(*mut core::ffi::c_void, usize)>,
    sring_tail_ptr: *mut u32,
    sring_mask_ptr: *mut u32,
    sring_array_head_ptr: *mut u32,
    cring_head_ptr: *mut u32,
    cring_tail_ptr: *mut u32,
    cring_mask_ptr: *mut u32,
    cqes_ptr: *mut linux_io_uring::ffi::io_uring_cqe,
    sqes: *mut linux_io_uring::ffi::io_uring_sqe,
    sqes_size: usize,
}
unsafe impl Sync for IoUringContext {}
unsafe impl Send for IoUringContext {}
impl Drop for IoUringContext {
    fn drop(&mut self) {
        let _ = unsafe { libc::munmap(self.sq_ptr, self.sq_size) };
        if let Some((ptr, size)) = self.cq.take() {
            let _ = unsafe { libc::munmap(ptr, size) };
        }
        let _ = unsafe { libc::munmap(self.sqes.cast(), self.sqes_size) };
    }
}
impl IoUringContext {
    fn new() -> Self {
        let mut params = linux_io_uring::ffi::io_uring_params {
            ..unsafe { core::mem::MaybeUninit::zeroed().assume_init() }
        };
        let uring = linux_io_uring::IoUring::new(32, &mut params).expect("IoUring::new");

        let is_shared_cq_sq = (params.features & linux_io_uring::ffi::IORING_FEAT_SINGLE_MMAP) != 0;
        let mut sring_size = params.sq_off.array as usize
            + params.sq_entries as usize * core::mem::size_of::<core::ffi::c_uint>();
        let mut cring_size = params.cq_off.cqes as usize
            + params.cq_entries as usize
                * core::mem::size_of::<linux_io_uring::ffi::io_uring_cqe>();
        if is_shared_cq_sq {
            // can be shared with sring and cring
            let ring_size = sring_size.max(cring_size);
            sring_size = ring_size;
            cring_size = ring_size;
        }
        let sq_ptr = unsafe {
            libc::mmap(
                core::ptr::null_mut(),
                sring_size,
                libc::PROT_READ | libc::PROT_WRITE,
                libc::MAP_SHARED | libc::MAP_POPULATE,
                uring.as_raw_fd(),
                linux_io_uring::ffi::IORING_OFF_SQ_RING as _,
            )
        };
        if sq_ptr == libc::MAP_FAILED {
            panic!("sq map failed");
        }

        let cq_ptr = if is_shared_cq_sq {
            sq_ptr
        } else {
            let p = unsafe {
                libc::mmap(
                    core::ptr::null_mut(),
                    cring_size,
                    libc::PROT_READ | libc::PROT_WRITE,
                    libc::MAP_SHARED | libc::MAP_POPULATE,
                    uring.as_raw_fd(),
                    linux_io_uring::ffi::IORING_OFF_CQ_RING as _,
                )
            };
            if p == libc::MAP_FAILED {
                panic!("cq map failed");
            }
            p
        };

        let sqes_size =
            params.sq_entries as usize * core::mem::size_of::<linux_io_uring::ffi::io_uring_sqe>();
        let sqes = unsafe {
            libc::mmap(
                core::ptr::null_mut(),
                sqes_size,
                libc::PROT_READ | libc::PROT_WRITE,
                libc::MAP_SHARED | libc::MAP_POPULATE,
                uring.as_raw_fd(),
                linux_io_uring::ffi::IORING_OFF_SQES as _,
            )
        };
        if sqes == libc::MAP_FAILED {
            panic!("sqes map failed");
        }

        let sring_tail = unsafe { sq_ptr.byte_add(params.sq_off.tail as _) };
        let sring_mask = unsafe { sq_ptr.byte_add(params.sq_off.ring_mask as _) };
        let sring_array = unsafe { sq_ptr.byte_add(params.sq_off.array as _) };
        let cring_head = unsafe { cq_ptr.byte_add(params.cq_off.head as _) };
        let cring_tail = unsafe { cq_ptr.byte_add(params.cq_off.tail as _) };
        let cring_mask = unsafe { cq_ptr.byte_add(params.cq_off.ring_mask as _) };
        let cqes = unsafe { cq_ptr.byte_add(params.cq_off.cqes as _) };

        Self {
            uring,
            sq_ptr,
            sq_size: sring_size,
            cq: if is_shared_cq_sq {
                None
            } else {
                Some((cq_ptr, cring_size))
            },
            sring_tail_ptr: sring_tail.cast(),
            sring_mask_ptr: sring_mask.cast(),
            sring_array_head_ptr: sring_array.cast(),
            cring_head_ptr: cring_head.cast(),
            cring_tail_ptr: cring_tail.cast(),
            cring_mask_ptr: cring_mask.cast(),
            cqes_ptr: cqes.cast(),
            sqes: sqes.cast(),
            sqes_size,
        }
    }

    #[inline(always)]
    const fn sring_tail(&self) -> &AtomicU32 {
        unsafe { AtomicU32::from_ptr(self.sring_tail_ptr) }
    }

    #[inline(always)]
    const fn cring_head(&self) -> &AtomicU32 {
        unsafe { AtomicU32::from_ptr(self.cring_head_ptr) }
    }

    #[inline(always)]
    const fn cring_tail(&self) -> &AtomicU32 {
        unsafe { AtomicU32::from_ptr(self.cring_tail_ptr) }
    }
}

struct CompletionQueueTaker {
    context: Arc<IoUringContext>,
}
impl CompletionQueueTaker {
    fn new(context: &Arc<IoUringContext>) -> Self {
        Self {
            context: context.clone(),
        }
    }

    fn try_take(&self, process: impl FnOnce(&linux_io_uring::ffi::io_uring_cqe)) -> bool {
        let head = unsafe { *self.context.cring_head_ptr };
        if head
            == self
                .context
                .cring_tail()
                .load(core::sync::atomic::Ordering::Acquire)
        {
            // empty
            return false;
        }

        let index = head & unsafe { *self.context.cring_mask_ptr };
        process(unsafe { &*self.context.cqes_ptr.add(index as _) });
        self.context
            .cring_head()
            .store(head + 1, core::sync::atomic::Ordering::Release);
        true
    }
}

#[derive(Clone)]
struct SubmissionQueuePusher {
    context: Arc<IoUringContext>,
}
impl SubmissionQueuePusher {
    fn new(context: &Arc<IoUringContext>) -> Self {
        Self {
            context: context.clone(),
        }
    }

    fn push(&self, describe_io: impl FnOnce(&mut linux_io_uring::ffi::io_uring_sqe)) {
        let tail = unsafe { *self.context.sring_tail_ptr };
        let index = tail & unsafe { *self.context.sring_mask_ptr };
        describe_io(unsafe { &mut *self.context.sqes.add(index as _) });
        unsafe {
            core::ptr::write_volatile(self.context.sring_array_head_ptr.add(index as _), index);
        }
        self.context
            .sring_tail()
            .store(tail + 1, core::sync::atomic::Ordering::Release);
        self.context
            .uring
            .enter(1, 0, 0, core::ptr::null_mut())
            .expect("io_uring_enter");
    }
}

pub struct IoReactorThread {
    join_handle: Option<std::thread::JoinHandle<()>>,
}
impl Drop for IoReactorThread {
    fn drop(&mut self) {
        let Some(join_handle) = self.join_handle.take() else {
            // already dropped?
            return;
        };
        let Some(reactor_handle) = IO_REACTOR_CURRENT_HANDLE.write().expect("poisoned").take()
        else {
            // already dropped?
            return;
        };

        reactor_handle.pusher.push(|sqe| {
            sqe.opcode = linux_io_uring::ffi::IORING_OP_MSG_RING as _;
            sqe.fd = reactor_handle.pusher.context.uring.as_raw_fd();
            sqe.len = 0;
            sqe.union1.off = 0;
        });
        join_handle.join().expect("err in IoReactorThread");
    }
}
impl IoReactorThread {
    pub fn spawn() -> Self {
        let uring = Arc::new(IoUringContext::new());
        let cq_taker = CompletionQueueTaker::new(&uring);

        *IO_REACTOR_CURRENT_HANDLE.write().expect("poisoned") = Some(IoReactorHandle {
            pusher: SubmissionQueuePusher::new(&uring),
        });

        let join_handle = std::thread::Builder::new()
            .name("Peridot NativeIO Reactor".into())
            .spawn({
                let uring = uring.clone();

                move || {
                    let mut terminated = false;
                    while !terminated {
                        uring
                            .uring
                            .enter(
                                0,
                                1,
                                linux_io_uring::ffi::IORING_ENTER_GETEVENTS,
                                core::ptr::null_mut(),
                            )
                            .expect("uring enter for wait events");
                        cq_taker.try_take(|cqe| {
                            // process cqe
                            let res = cqe.res;
                            let user_data = cqe.user_data;

                            if user_data == 0 {
                                // termination message
                                terminated = true;
                                return;
                            }

                            let queue_data: Box<ReadFutureQueueData> = unsafe {
                                Box::from_raw(core::ptr::with_exposed_provenance_mut(
                                    user_data as _,
                                ))
                            };
                            match &*queue_data {
                                ReadFutureQueueData::Read { state, waker } => {
                                    if let Some(st) = state.upgrade() {
                                        st.set(if res < 0 {
                                            AsyncNativeFileReadState::CompletedFailure(-res)
                                        } else {
                                            AsyncNativeFileReadState::CompletedSuccess(
                                                res.cast_unsigned() as _,
                                            )
                                        });
                                    }

                                    waker.wake_by_ref();
                                }
                            }
                        });
                    }
                }
            })
            .expect("Failed to spawn async fileio thread");

        Self {
            join_handle: Some(join_handle),
        }
    }
}
