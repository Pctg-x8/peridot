use std::{
    cell::Cell,
    os::fd::AsRawFd,
    sync::{
        Arc,
        atomic::{AtomicBool, AtomicU32},
    },
};

#[repr(transparent)]
pub struct File(std::os::unix::prelude::RawFd);
impl Drop for File {
    #[inline]
    fn drop(&mut self) {
        let _ = unsafe { libc::close(self.0) };
    }
}
impl File {
    pub fn open(pathname: &core::ffi::CStr, flags: core::ffi::c_int) -> std::io::Result<Self> {
        let fd = unsafe { libc::open(pathname.as_ptr(), flags) };
        if fd < 0 {
            Err(std::io::Error::last_os_error())
        } else {
            Ok(Self(fd))
        }
    }
}

#[repr(transparent)]
pub struct LinuxNativeFileReader(File);
impl LinuxNativeFileReader {
    #[inline]
    pub fn open(name: &(impl AsRef<std::path::Path> + ?Sized)) -> std::io::Result<Self> {
        let f = File::open(
            &std::ffi::CString::new(name.as_ref().to_str().expect("invalid utf-8 sequence"))
                .expect("invalid for cstr"),
            libc::O_CLOEXEC,
        )?;

        Ok(Self(f))
    }
}
impl super::NativeFileReader for LinuxNativeFileReader {
    #[inline]
    fn current_pointer_pos(&self) -> std::io::Result<u64> {
        let r = unsafe { libc::lseek64(self.0.0, 0, libc::SEEK_CUR) };
        if r < 0 {
            Err(std::io::Error::last_os_error())
        } else {
            Ok(r.cast_unsigned())
        }
    }

    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let r = unsafe { libc::read(self.0.0, buf.as_mut_ptr() as _, buf.len()) };
        if r < 0 {
            Err(std::io::Error::last_os_error())
        } else {
            Ok(r.cast_unsigned())
        }
    }

    #[inline]
    fn readv(&mut self, buf: &mut [std::io::IoSliceMut]) -> std::io::Result<usize> {
        let r = unsafe { libc::readv(self.0.0, buf.as_mut_ptr() as _, buf.len() as _) };
        if r < 0 {
            Err(std::io::Error::last_os_error())
        } else {
            Ok(r.cast_unsigned())
        }
    }

    #[inline]
    fn pread(&self, buf: &mut [u8], offs: u64) -> std::io::Result<usize> {
        let r = unsafe { libc::pread64(self.0.0, buf.as_mut_ptr() as _, buf.len(), offs as _) };
        if r < 0 {
            Err(std::io::Error::last_os_error())
        } else {
            Ok(r.cast_unsigned())
        }
    }
}
impl super::NativeFileMemoryMapProvider for LinuxNativeFileReader {
    type MemoryUnmapData = LinuxMemoryUnmapData;

    #[inline]
    fn mmap(
        &self,
        offs: u64,
        len: u64,
    ) -> std::io::Result<(*mut core::ffi::c_void, Self::MemoryUnmapData)> {
        let r = unsafe {
            libc::mmap(
                core::ptr::null_mut(),
                len as _,
                libc::PROT_READ,
                libc::MAP_PRIVATE,
                self.0.0,
                offs as _,
            )
        };
        if r == libc::MAP_FAILED {
            Err(std::io::Error::last_os_error())
        } else {
            Ok((
                r,
                LinuxMemoryUnmapData {
                    addr: r,
                    len: len as _,
                },
            ))
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

pub struct LinuxMemoryUnmapData {
    addr: *mut core::ffi::c_void,
    len: usize,
}

pub struct LinuxAsyncNativeFileReader {
    file: File,
    // io_uringはファイルポインタすすめてくれないらしいので自前で管理する
    readptr: u64,
}
impl LinuxAsyncNativeFileReader {
    #[inline]
    pub fn open(name: &(impl AsRef<std::path::Path> + ?Sized)) -> std::io::Result<Self> {
        let f = File::open(
            &std::ffi::CString::new(name.as_ref().to_str().expect("invalid utf-8 sequence"))
                .expect("invalid for cstr"),
            libc::O_CLOEXEC | libc::O_NONBLOCK,
        )?;

        Ok(Self {
            file: f,
            readptr: 0,
        })
    }
}
impl super::AsyncNativeFileReader for LinuxAsyncNativeFileReader {
    type ReadFuture<'a>
        = LinuxAsyncNativeFileReadFuture<'a>
    where
        Self: 'a;
    type PosReadFuture<'a, 'b>
        = core::future::Ready<std::io::Result<usize>>
    where
        Self: 'a;
    type ReadVecFuture<'a, 'b, 'b2>
        = core::future::Ready<std::io::Result<usize>>
    where
        Self: 'a,
        'b2: 'b;

    #[inline]
    fn current_pointer_pos(&self) -> std::io::Result<u64> {
        let r = unsafe { libc::lseek64(self.file.0, 0, libc::SEEK_CUR) };
        if r < 0 {
            Err(std::io::Error::last_os_error())
        } else {
            Ok(r.cast_unsigned())
        }
    }

    #[inline]
    fn read_async<'a>(&'a mut self, buf: &'a mut [u8]) -> Self::ReadFuture<'a> {
        LinuxAsyncNativeFileReadFuture {
            fd: self,
            buf,
            state: Arc::new(Cell::new(LinuxAsyncNativeFileReadState::Init)),
        }
    }

    #[inline]
    fn pread_async<'a, 'b>(&'a self, buf: &'b mut [u8], offs: u64) -> Self::PosReadFuture<'a, 'b> {
        println!("async pread");
        core::future::ready(Ok(0))
    }

    #[inline]
    fn readv_async<'a, 'b, 'b2>(
        &'a mut self,
        buf: &'b mut [std::io::IoSliceMut<'b2>],
    ) -> Self::ReadVecFuture<'a, 'b, 'b2> {
        println!("async readv");
        core::future::ready(Ok(0))
    }
}
impl super::NativeFileMemoryMapProvider for LinuxAsyncNativeFileReader {
    type MemoryUnmapData = LinuxMemoryUnmapData;

    #[inline]
    fn mmap(
        &self,
        offs: u64,
        len: u64,
    ) -> std::io::Result<(*mut core::ffi::c_void, Self::MemoryUnmapData)> {
        let r = unsafe {
            libc::mmap(
                core::ptr::null_mut(),
                len as _,
                libc::PROT_READ,
                libc::MAP_PRIVATE,
                self.file.0,
                offs as _,
            )
        };
        if r == libc::MAP_FAILED {
            Err(std::io::Error::last_os_error())
        } else {
            Ok((
                r,
                LinuxMemoryUnmapData {
                    addr: r,
                    len: len as _,
                },
            ))
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
pub enum LinuxAsyncNativeFileReadState {
    Init,
    Pending,
    CompletedSuccess(usize),
    CompletedFailure(i32),
}

pub struct LinuxAsyncNativeFileReadFuture<'a> {
    fd: &'a mut LinuxAsyncNativeFileReader,
    buf: &'a mut [u8],
    state: Arc<Cell<LinuxAsyncNativeFileReadState>>,
}
impl<'a> Future for LinuxAsyncNativeFileReadFuture<'a> {
    type Output = std::io::Result<usize>;

    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        let this = self.get_mut();

        match this.state.get() {
            LinuxAsyncNativeFileReadState::Init => {
                // first call
                let p = this.fd.readptr;

                LinuxIoReactorHandle::current()
                    .expect("no reactor running")
                    .pusher
                    .push(|sqe| unsafe {
                        core::ptr::write_volatile(&mut sqe.fd, this.fd.file.0);
                        core::ptr::write_volatile(
                            &mut sqe.opcode,
                            linux_io_uring::ffi::IORING_OP_READ as _,
                        );
                        core::ptr::write_volatile(&mut sqe.union1.off, p);
                        core::ptr::write_volatile(
                            &mut sqe.union2.addr,
                            this.buf.as_mut_ptr() as usize as _,
                        );
                        core::ptr::write_volatile(&mut sqe.len, this.buf.len() as _);
                        core::ptr::write_volatile(
                            &mut sqe.user_data,
                            Box::into_raw(Box::new(ReadFutureQueueData::Read {
                                state: Arc::downgrade(&this.state),
                                waker: cx.waker().clone(),
                            })) as usize as _,
                        );
                    });

                this.state.set(LinuxAsyncNativeFileReadState::Pending);
                core::task::Poll::Pending
            }
            LinuxAsyncNativeFileReadState::Pending => {
                // still pending
                this.state.set(LinuxAsyncNativeFileReadState::Pending);
                core::task::Poll::Pending
            }
            LinuxAsyncNativeFileReadState::CompletedSuccess(res) => {
                this.fd.readptr += res as u64;
                core::task::Poll::Ready(Ok(res))
            }
            LinuxAsyncNativeFileReadState::CompletedFailure(e) => {
                core::task::Poll::Ready(Err(std::io::Error::from_raw_os_error(e)))
            }
        }
    }
}

pub enum ReadFutureQueueData {
    Read {
        state: std::sync::Weak<Cell<LinuxAsyncNativeFileReadState>>,
        waker: core::task::Waker,
    },
}

#[derive(Clone)]
pub struct LinuxIoReactorHandle {
    pusher: SubmissionQueuePusher,
}
impl LinuxIoReactorHandle {
    #[inline]
    pub fn current() -> Option<Self> {
        LINUX_IO_REACTOR_CURRENT_HANDLE
            .write()
            .expect("poisoned")
            .clone()
    }
}

static LINUX_IO_REACTOR_CURRENT_HANDLE: std::sync::RwLock<Option<LinuxIoReactorHandle>> =
    std::sync::RwLock::new(None);

pub struct IoUringContext {
    uring: linux_io_uring::IoUring,
    params: linux_io_uring::ffi::io_uring_params,
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
    pub fn new() -> Self {
        let mut params = linux_io_uring::ffi::io_uring_params {
            ..unsafe { core::mem::MaybeUninit::zeroed().assume_init() }
        };
        let uring = linux_io_uring::IoUring::new(32, &mut params).expect("IoUring::new");

        let mut sring_size = params.sq_off.array as usize
            + params.sq_entries as usize * core::mem::size_of::<core::ffi::c_uint>();
        let mut cring_size = params.cq_off.cqes as usize
            + params.cq_entries as usize
                * core::mem::size_of::<linux_io_uring::ffi::io_uring_cqe>();
        if (params.features & linux_io_uring::ffi::IORING_FEAT_SINGLE_MMAP) != 0 {
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

        let is_shared_cq_sq = (params.features & linux_io_uring::ffi::IORING_FEAT_SINGLE_MMAP) != 0;
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
            params,
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
    pub const fn sring_tail(&self) -> &AtomicU32 {
        unsafe { AtomicU32::from_ptr(self.sring_tail_ptr) }
    }

    #[inline(always)]
    pub const fn cring_head(&self) -> &AtomicU32 {
        unsafe { AtomicU32::from_ptr(self.cring_head_ptr) }
    }

    #[inline(always)]
    pub const fn cring_tail(&self) -> &AtomicU32 {
        unsafe { AtomicU32::from_ptr(self.cring_tail_ptr) }
    }
}

pub struct CompletionQueueTaker {
    context: Arc<IoUringContext>,
}
impl CompletionQueueTaker {
    pub fn new(context: &Arc<IoUringContext>) -> Self {
        Self {
            context: context.clone(),
        }
    }

    pub fn try_take(&self, process: impl FnOnce(&linux_io_uring::ffi::io_uring_cqe)) -> bool {
        let head = self
            .context
            .cring_head()
            .load(core::sync::atomic::Ordering::Acquire);
        if head == unsafe { core::ptr::read_volatile(self.context.cring_tail_ptr) } {
            // empty
            return false;
        }

        let index = head & unsafe { core::ptr::read(self.context.cring_mask_ptr) };
        process(unsafe { &*self.context.cqes_ptr.add(index as _) });
        self.context
            .cring_head()
            .store(head + 1, core::sync::atomic::Ordering::Release);
        true
    }
}

#[derive(Clone)]
pub struct SubmissionQueuePusher {
    context: Arc<IoUringContext>,
}
impl SubmissionQueuePusher {
    pub fn new(context: &Arc<IoUringContext>) -> Self {
        Self {
            context: context.clone(),
        }
    }

    pub fn push(&self, describe_io: impl FnOnce(&mut linux_io_uring::ffi::io_uring_sqe)) {
        let tail = unsafe { core::ptr::read_volatile(self.context.sring_tail_ptr) };
        let index = tail & unsafe { core::ptr::read(self.context.sring_mask_ptr) };
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
    terminated: Arc<AtomicBool>,
}
impl Drop for IoReactorThread {
    fn drop(&mut self) {
        let Some(join_handle) = self.join_handle.take() else {
            // already dropped?
            return;
        };

        self.terminated
            .store(true, core::sync::atomic::Ordering::Release);
        join_handle.join().expect("err in IoReactorThread");
    }
}
impl IoReactorThread {
    pub fn spawn() -> Self {
        let uring = Arc::new(IoUringContext::new());
        let cq_taker = CompletionQueueTaker::new(&uring);

        *LINUX_IO_REACTOR_CURRENT_HANDLE.write().expect("poisoned") = Some(LinuxIoReactorHandle {
            pusher: SubmissionQueuePusher::new(&uring),
        });

        let terminated = Arc::new(AtomicBool::new(false));
        let join_handle = std::thread::Builder::new()
            .name("peridot-archiver Async FileIO".into())
            .spawn({
                let terminated = terminated.clone();

                move || {
                    while !terminated.load(core::sync::atomic::Ordering::Acquire) {
                        if !cq_taker.try_take(|cqe| {
                            // process cqe
                            // kernelで書かれたものを読むのでvolatileじゃないとだめそう(ふつうにreadしただけだと反応しない場合があった)
                            let res = unsafe { core::ptr::read_volatile(&cqe.res) };
                            let user_data = unsafe { core::ptr::read_volatile(&cqe.user_data) };

                            let queue_data: Box<ReadFutureQueueData> = unsafe {
                                Box::from_raw(core::ptr::with_exposed_provenance_mut(
                                    user_data as _,
                                ))
                            };
                            match &*queue_data {
                                ReadFutureQueueData::Read { state, waker } => {
                                    if let Some(st) = state.upgrade() {
                                        st.set(if res < 0 {
                                            LinuxAsyncNativeFileReadState::CompletedFailure(-res)
                                        } else {
                                            LinuxAsyncNativeFileReadState::CompletedSuccess(
                                                res.cast_unsigned() as _,
                                            )
                                        });
                                    }

                                    waker.wake_by_ref();
                                }
                            }
                        }) {
                            std::thread::yield_now();
                        }
                    }
                }
            })
            .expect("Failed to spawn async fileio thread");

        Self {
            join_handle: Some(join_handle),
            terminated,
        }
    }
}
