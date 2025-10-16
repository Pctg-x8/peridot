use core::ptr::NonNull;
use std::{
    cell::Cell,
    sync::{Arc, RwLock, atomic::AtomicBool},
};

use crossbeam_deque::{Injector, Worker};

#[repr(transparent)]
pub struct BundledAsset(NonNull<android::AAsset>);
impl Drop for BundledAsset {
    #[inline(always)]
    fn drop(&mut self) {
        unsafe {
            android::AAsset_close(self.0.as_ptr());
        }
    }
}
impl BundledAsset {
    pub fn seek64(&self, offset: libc::off64_t, whence: core::ffi::c_int) -> std::io::Result<u64> {
        let r = unsafe { android::AAsset_seek64(self.0.as_ptr(), offset, whence) };
        if r < 0 {
            Err(std::io::Error::last_os_error())
        } else {
            Ok(r.cast_unsigned())
        }
    }

    pub fn read(&self, buf: &mut [u8]) -> std::io::Result<usize> {
        let r =
            unsafe { android::AAsset_read(self.0.as_ptr(), buf.as_mut_ptr() as _, buf.len() as _) };
        if r < 0 {
            Err(std::io::Error::last_os_error())
        } else {
            Ok(r.cast_unsigned() as _)
        }
    }
}

#[repr(transparent)]
pub struct BundledAssetReader(BundledAsset);
impl super::NativeFileReader for BundledAssetReader {
    #[inline(always)]
    fn current_pointer_pos(&self) -> std::io::Result<u64> {
        self.0.seek64(0, libc::SEEK_CUR)
    }

    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        self.0.read(buf)
    }

    fn pread(&self, buf: &mut [u8], offs: u64) -> std::io::Result<usize> {
        // preadないのでseekしてからreadする
        let o0 = self.0.seek64(0, libc::SEEK_CUR)?;
        self.0.seek64(offs as _, libc::SEEK_SET)?;
        let r = self.0.read(buf)?;
        self.0.seek64(o0 as _, libc::SEEK_SET)?;
        Ok(r)
    }

    fn readv(&mut self, buf: &mut [std::io::IoSliceMut]) -> std::io::Result<usize> {
        // readvないのでふつうにreadを呼ぶ
        match buf.first_mut() {
            Some(b) => self.0.read(b),
            None => Ok(0),
        }
    }
}
impl super::NativeFileMemoryMapProvider for BundledAssetReader {
    type MemoryUnmapData = BundledAssetUnmapData;

    fn mmap(
        &self,
        offs: u64,
        len: u64,
    ) -> std::io::Result<(*mut core::ffi::c_void, Self::MemoryUnmapData)> {
        let mut start = core::mem::MaybeUninit::uninit();
        let mut length = core::mem::MaybeUninit::uninit();
        let fd = unsafe {
            android::AAsset_openFileDescriptor64(
                self.0.0.as_ptr(),
                start.as_mut_ptr(),
                length.as_mut_ptr(),
            )
        };
        assert!(fd >= 0, "opening asset file descriptor is not supported");

        let p = unsafe {
            libc::mmap(
                core::ptr::null_mut(),
                len as _,
                libc::PROT_READ,
                libc::MAP_PRIVATE,
                fd,
                start.assume_init() + offs as i64,
            )
        };
        if p == libc::MAP_FAILED {
            return Err(std::io::Error::last_os_error());
        }

        Ok((
            p,
            BundledAssetUnmapData {
                fd,
                ptr: p,
                len: len as _,
            },
        ))
    }

    fn munmap(&self, data: Self::MemoryUnmapData) -> std::io::Result<()> {
        let r = unsafe { libc::munmap(data.ptr, data.len) };
        if r < 0 {
            return Err(std::io::Error::last_os_error());
        }
        let r = unsafe { libc::close(data.fd) };
        if r < 0 {
            return Err(std::io::Error::last_os_error());
        }

        Ok(())
    }
}

pub struct BundledAssetAsyncReader {
    asset: BundledAsset,
}
impl super::AsyncNativeFileReader for BundledAssetAsyncReader {
    type ReadFuture<'a>
        = BundledAssetReadFuture<'a, 'a>
    where
        Self: 'a;
    type PosReadFuture<'a, 'b>
        = BundledAssetReadPosFuture<'a, 'b>
    where
        Self: 'a;
    type ReadVecFuture<'a, 'b, 'b2>
        = BundledAssetReadFuture<'a, 'b>
    where
        Self: 'a,
        'b2: 'b;

    #[inline(always)]
    fn current_pointer_pos(&self) -> std::io::Result<u64> {
        self.asset.seek64(0, libc::SEEK_CUR)
    }

    #[inline(always)]
    fn read_async<'a>(&'a mut self, buf: &'a mut [u8]) -> Self::ReadFuture<'a> {
        BundledAssetReadFuture {
            asset: &mut self.asset,
            buf,
            state: Arc::new(Cell::new(BundledAssetReadState::Init)),
        }
    }

    #[inline(always)]
    fn pread_async<'a, 'b>(&'a self, buf: &'b mut [u8], offs: u64) -> Self::PosReadFuture<'a, 'b> {
        // TODO: これスレッドセーフじゃないのでなにかしらロックとる必要がある
        BundledAssetReadPosFuture {
            asset: &self.asset,
            buf,
            offset: offs,
            state: Arc::new(Cell::new(BundledAssetReadState::Init)),
        }
    }

    #[inline(always)]
    fn readv_async<'a, 'b, 'b2>(
        &'a mut self,
        buf: &'b mut [std::io::IoSliceMut<'b2>],
    ) -> Self::ReadVecFuture<'a, 'b, 'b2> {
        BundledAssetReadFuture {
            asset: &mut self.asset,
            buf: buf.first_mut().map_or(&mut [], |x| x),
            state: Arc::new(Cell::new(BundledAssetReadState::Init)),
        }
    }
}
impl super::NativeFileMemoryMapProvider for BundledAssetAsyncReader {
    type MemoryUnmapData = BundledAssetUnmapData;

    fn mmap(
        &self,
        offs: u64,
        len: u64,
    ) -> std::io::Result<(*mut core::ffi::c_void, Self::MemoryUnmapData)> {
        let mut start = core::mem::MaybeUninit::uninit();
        let mut length = core::mem::MaybeUninit::uninit();
        let fd = unsafe {
            android::AAsset_openFileDescriptor64(
                self.asset.0.as_ptr(),
                start.as_mut_ptr(),
                length.as_mut_ptr(),
            )
        };
        assert!(fd >= 0, "opening asset file descriptor is not supported");

        let p = unsafe {
            libc::mmap(
                core::ptr::null_mut(),
                len as _,
                libc::PROT_READ,
                libc::MAP_PRIVATE,
                fd,
                start.assume_init() + offs as i64,
            )
        };
        if p == libc::MAP_FAILED {
            return Err(std::io::Error::last_os_error());
        }

        Ok((
            p,
            BundledAssetUnmapData {
                fd,
                ptr: p,
                len: len as _,
            },
        ))
    }

    fn munmap(&self, data: Self::MemoryUnmapData) -> std::io::Result<()> {
        let r = unsafe { libc::munmap(data.ptr, data.len) };
        if r < 0 {
            return Err(std::io::Error::last_os_error());
        }
        let r = unsafe { libc::close(data.fd) };
        if r < 0 {
            return Err(std::io::Error::last_os_error());
        }

        Ok(())
    }
}

pub struct BundledAssetUnmapData {
    fd: core::ffi::c_int,
    ptr: *mut core::ffi::c_void,
    len: usize,
}

#[derive(Clone, Copy)]
enum BundledAssetReadState {
    Init,
    Pending,
    CompleteSuccess(usize),
    CompleteFailed(i32),
}

pub struct BundledAssetReadFuture<'a, 'b> {
    asset: &'a mut BundledAsset,
    buf: &'b mut [u8],
    state: Arc<Cell<BundledAssetReadState>>,
}
impl<'a, 'b> Future for BundledAssetReadFuture<'a, 'b> {
    type Output = std::io::Result<usize>;

    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        let this = self.get_mut();

        match this.state.get() {
            BundledAssetReadState::Init => {
                IoWorkerHandle::current().expect("no worker running").post(
                    BackgroundTask::ReadAsset {
                        asset: this.asset.0,
                        ptr: this.buf.as_mut_ptr() as _,
                        len: this.buf.len(),
                        state_store: Arc::downgrade(&this.state),
                        waker: cx.waker().clone(),
                    },
                );

                this.state.set(BundledAssetReadState::Pending);
                core::task::Poll::Pending
            }
            BundledAssetReadState::Pending => core::task::Poll::Pending,
            BundledAssetReadState::CompleteSuccess(x) => core::task::Poll::Ready(Ok(x)),
            BundledAssetReadState::CompleteFailed(e) => {
                core::task::Poll::Ready(Err(std::io::Error::from_raw_os_error(e)))
            }
        }
    }
}

pub struct BundledAssetReadPosFuture<'a, 'b> {
    asset: &'a BundledAsset,
    buf: &'b mut [u8],
    offset: u64,
    state: Arc<Cell<BundledAssetReadState>>,
}
impl<'a, 'b> Future for BundledAssetReadPosFuture<'a, 'b> {
    type Output = std::io::Result<usize>;

    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        let this = self.get_mut();

        match this.state.get() {
            BundledAssetReadState::Init => {
                IoWorkerHandle::current().expect("no worker running").post(
                    BackgroundTask::ReadAssetPos {
                        asset: this.asset.0,
                        ptr: this.buf.as_mut_ptr() as _,
                        len: this.buf.len(),
                        offset: this.offset,
                        state_store: Arc::downgrade(&this.state),
                        waker: cx.waker().clone(),
                    },
                );

                this.state.set(BundledAssetReadState::Pending);
                core::task::Poll::Pending
            }
            BundledAssetReadState::Pending => core::task::Poll::Pending,
            BundledAssetReadState::CompleteSuccess(x) => core::task::Poll::Ready(Ok(x)),
            BundledAssetReadState::CompleteFailed(e) => {
                core::task::Poll::Ready(Err(std::io::Error::from_raw_os_error(e)))
            }
        }
    }
}

enum BackgroundTask {
    ReadAsset {
        asset: NonNull<android::AAsset>,
        ptr: *mut core::ffi::c_void,
        len: usize,
        state_store: std::sync::Weak<Cell<BundledAssetReadState>>,
        waker: core::task::Waker,
    },
    ReadAssetPos {
        asset: NonNull<android::AAsset>,
        ptr: *mut core::ffi::c_void,
        len: usize,
        offset: u64,
        state_store: std::sync::Weak<Cell<BundledAssetReadState>>,
        waker: core::task::Waker,
    },
}
unsafe impl Sync for BackgroundTask {}
unsafe impl Send for BackgroundTask {}

#[derive(Clone)]
struct IoWorkerHandle {
    injector: Arc<Injector<BackgroundTask>>,
    threads: std::sync::Weak<ThreadJoinHandles>,
}
impl IoWorkerHandle {
    fn current() -> Option<Self> {
        IO_WORKER_HANDLE_CURRENT.read().expect("poisoned").clone()
    }

    pub fn post(&self, task: BackgroundTask) {
        if let Some(threads) = self.threads.upgrade() {
            self.injector.push(task);
            for x in threads.0.iter() {
                x.thread().unpark();
            }
        }
    }
}

static IO_WORKER_HANDLE_CURRENT: RwLock<Option<IoWorkerHandle>> = RwLock::new(None);

#[repr(transparent)]
struct ThreadJoinHandles(Vec<std::thread::JoinHandle<()>>);
// hack: make_mutがclone要求するのでダミー実装でふさぐ weakをdissociateする方法があればこんなのいらないんだけど......
impl Clone for ThreadJoinHandles {
    fn clone(&self) -> Self {
        panic!(
            "*bug* join handles is not expected to be cloned(there are any other strong refs to this?)"
        );
    }
}

pub struct BackgroundIoWorkerPool {
    threads: Arc<ThreadJoinHandles>,
    termination: Arc<AtomicBool>,
}
impl Drop for BackgroundIoWorkerPool {
    fn drop(&mut self) {
        *IO_WORKER_HANDLE_CURRENT.write().expect("poisoned") = None;
        self.termination
            .store(true, core::sync::atomic::Ordering::Release);
        for t in Arc::make_mut(&mut self.threads).0.drain(..) {
            t.thread().unpark();
            t.join().expect("Join WorkerThread");
        }
    }
}
impl BackgroundIoWorkerPool {
    // いったん4並列にしておく
    const COUNT: usize = 4;

    pub fn spawn() -> Self {
        let injector = Arc::new(Injector::new());
        let workers = core::iter::repeat_with(Worker::new_fifo)
            .take(Self::COUNT)
            .collect::<Vec<_>>();
        let other_stealer_sets = (0..Self::COUNT)
            .map(|n| {
                workers
                    .iter()
                    .enumerate()
                    .filter(|&(nw, _)| n != nw)
                    .map(|(_, w)| w.stealer())
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();

        let termination = Arc::new(AtomicBool::new(false));
        let threads = workers
            .into_iter()
            .zip(other_stealer_sets.into_iter())
            .enumerate()
            .map(|(n, (w, other_stealers))| {
                std::thread::Builder::new()
                    .name(format!("peridot-archive Background IO Worker #{n}"))
                    .spawn({
                        let injector = injector.clone();
                        let termination = termination.clone();
                        move || {
                            while !termination.load(core::sync::atomic::Ordering::Acquire) {
                                let task = w.pop().or_else(|| {
                                    core::iter::repeat_with(|| {
                                        injector.steal_batch_and_pop(&w).or_else(|| {
                                            other_stealers.iter().map(|s| s.steal()).collect()
                                        })
                                    })
                                    .find(|x| !x.is_retry())
                                    .and_then(|x| x.success())
                                });
                                match task {
                                    Some(BackgroundTask::ReadAsset {
                                        asset,
                                        ptr,
                                        len,
                                        state_store,
                                        waker,
                                    }) => {
                                        if let Some(state_store) = state_store.upgrade() {
                                            let r = unsafe {
                                                android::AAsset_read(asset.as_ptr(), ptr, len)
                                            };
                                            state_store.set(if r < 0 {
                                                BundledAssetReadState::CompleteFailed(-r)
                                            } else {
                                                BundledAssetReadState::CompleteSuccess(
                                                    r.cast_unsigned() as _,
                                                )
                                            });
                                            waker.wake();
                                        }
                                    }
                                    Some(BackgroundTask::ReadAssetPos {
                                        asset,
                                        ptr,
                                        len,
                                        offset,
                                        state_store,
                                        waker,
                                    }) => {
                                        if let Some(state_store) = state_store.upgrade() {
                                            let r = unsafe {
                                                android::AAsset_seek64(
                                                    asset.as_ptr(),
                                                    0,
                                                    libc::SEEK_CUR,
                                                )
                                            };
                                            let rewind_pos = if r < 0 {
                                                state_store.set(
                                                    BundledAssetReadState::CompleteFailed(
                                                        std::io::Error::last_os_error()
                                                            .raw_os_error()
                                                            .unwrap_or(0),
                                                    ),
                                                );
                                                waker.wake();
                                                continue;
                                            } else {
                                                r.cast_unsigned()
                                            };

                                            let r = unsafe {
                                                android::AAsset_seek64(
                                                    asset.as_ptr(),
                                                    offset as _,
                                                    libc::SEEK_SET,
                                                )
                                            };
                                            if r < 0 {
                                                state_store.set(
                                                    BundledAssetReadState::CompleteFailed(
                                                        std::io::Error::last_os_error()
                                                            .raw_os_error()
                                                            .unwrap_or(0),
                                                    ),
                                                );
                                                waker.wake();
                                                continue;
                                            }

                                            let r = unsafe {
                                                android::AAsset_read(asset.as_ptr(), ptr, len)
                                            };
                                            if r < 0 {
                                                state_store.set(
                                                    BundledAssetReadState::CompleteFailed(
                                                        std::io::Error::last_os_error()
                                                            .raw_os_error()
                                                            .unwrap_or(0),
                                                    ),
                                                );
                                                waker.wake();
                                                continue;
                                            }
                                            let reads = r.cast_unsigned() as usize;

                                            let r = unsafe {
                                                android::AAsset_seek64(
                                                    asset.as_ptr(),
                                                    rewind_pos as _,
                                                    libc::SEEK_SET,
                                                )
                                            };
                                            if r < 0 {
                                                state_store.set(
                                                    BundledAssetReadState::CompleteFailed(
                                                        std::io::Error::last_os_error()
                                                            .raw_os_error()
                                                            .unwrap_or(0),
                                                    ),
                                                );
                                                waker.wake();
                                                continue;
                                            }

                                            state_store
                                                .set(BundledAssetReadState::CompleteSuccess(reads));
                                            waker.wake();
                                        }
                                    }
                                    None => std::thread::park(),
                                }
                            }
                        }
                    })
                    .expect("BackgroundIoWorkerPool spawn")
            })
            .collect::<Vec<_>>();
        let threads = Arc::new(ThreadJoinHandles(threads));

        *IO_WORKER_HANDLE_CURRENT.write().expect("poisoned") = Some(IoWorkerHandle {
            injector,
            threads: Arc::downgrade(&threads),
        });
        Self {
            threads,
            termination,
        }
    }
}
