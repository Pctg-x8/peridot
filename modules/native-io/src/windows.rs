use std::{
    io::Result as IOResult,
    os::windows::ffi::OsStrExt,
    path::Path,
    sync::{Arc, Mutex, RwLock},
};

use windows::{
    Win32::{
        Foundation::{
            CloseHandle, ERROR_HANDLE_EOF, ERROR_IO_INCOMPLETE, ERROR_IO_PENDING, GENERIC_READ,
            HANDLE, INVALID_HANDLE_VALUE,
        },
        Security::SECURITY_ATTRIBUTES,
        Storage::FileSystem::{
            CreateFileW, FILE_ATTRIBUTE_NORMAL, FILE_CREATION_DISPOSITION, FILE_FLAG_OVERLAPPED,
            FILE_FLAG_RANDOM_ACCESS, FILE_FLAGS_AND_ATTRIBUTES, FILE_SHARE_MODE, FILE_SHARE_READ,
            GetFileSizeEx, OPEN_EXISTING, ReadFile,
        },
        System::{
            IO::{
                CreateIoCompletionPort, GetOverlappedResult, GetQueuedCompletionStatusEx,
                OVERLAPPED, OVERLAPPED_0, OVERLAPPED_0_0, OVERLAPPED_ENTRY,
                PostQueuedCompletionStatus,
            },
            Memory::{
                CreateFileMappingW, FILE_MAP, FILE_MAP_READ, MEMORY_MAPPED_VIEW_ADDRESS,
                MapViewOfFile, PAGE_PROTECTION_FLAGS, PAGE_READONLY, UnmapViewOfFile,
            },
            SystemInformation::GetSystemInfo,
            Threading::INFINITE,
        },
    },
    core::{PCWSTR, Param},
};

#[repr(transparent)]
struct File(HANDLE);
impl Drop for File {
    #[inline(always)]
    fn drop(&mut self) {
        let _ = unsafe { CloseHandle(self.0) };
    }
}
impl File {
    #[inline(always)]
    const fn handle(&self) -> HANDLE {
        self.0
    }

    #[inline(always)]
    fn create(
        name: impl Param<PCWSTR>,
        desired_access: u32,
        share_mode: FILE_SHARE_MODE,
        security_attributes: Option<*const SECURITY_ATTRIBUTES>,
        creation_disposition: FILE_CREATION_DISPOSITION,
        flags_and_attributes: FILE_FLAGS_AND_ATTRIBUTES,
        template_file: Option<&File>,
    ) -> windows::core::Result<Self> {
        Ok(Self(unsafe {
            CreateFileW(
                name,
                desired_access,
                share_mode,
                security_attributes,
                creation_disposition,
                flags_and_attributes,
                template_file.map(|x| x.0),
            )?
        }))
    }

    /// # Safety
    /// concurrent read may require extra data race consideration(non-overlapped files may update their internal file pointer in operation)
    #[inline(always)]
    unsafe fn read(
        &self,
        buffer: Option<&mut [core::mem::MaybeUninit<u8>]>,
        read_byte_count: Option<&mut u32>,
        overlapped: Option<&mut OVERLAPPED>,
    ) -> windows::core::Result<()> {
        unsafe {
            ReadFile(
                self.0,
                core::mem::transmute::<Option<&mut [core::mem::MaybeUninit<_>]>, Option<&mut [_]>>(
                    buffer,
                ),
                read_byte_count.map(|x| x as *mut _),
                overlapped.map(|x| x as *mut _),
            )
        }
    }

    #[inline(always)]
    fn get_overlapped_result(
        &self,
        overlapped: &OVERLAPPED,
        wait: bool,
    ) -> windows::core::Result<u32> {
        let mut transferred_byte_count = 0;

        unsafe { GetOverlappedResult(self.0, overlapped, &mut transferred_byte_count, wait) }
            .map(move |_| transferred_byte_count)
    }
}

#[inline(always)]
const fn init_overlapped(offset: u64, event: HANDLE) -> OVERLAPPED {
    OVERLAPPED {
        Internal: 0,
        InternalHigh: 0,
        Anonymous: OVERLAPPED_0 {
            Anonymous: OVERLAPPED_0_0 {
                Offset: offset as u32,
                OffsetHigh: (offset >> 32) as u32,
            },
        },
        hEvent: event,
    }
}

#[repr(transparent)]
struct FileMapping(HANDLE);
impl FileMapping {
    #[inline(always)]
    fn new(
        file: &File,
        attributes: Option<*const SECURITY_ATTRIBUTES>,
        protect: PAGE_PROTECTION_FLAGS,
        maximum_size: u64,
        name: impl Param<PCWSTR>,
    ) -> windows::core::Result<Self> {
        Ok(Self(unsafe {
            CreateFileMappingW(
                file.0,
                attributes,
                protect,
                (maximum_size >> 32) as u32,
                maximum_size as u32,
                name,
            )?
        }))
    }

    #[inline(always)]
    fn map(
        &self,
        desired_access: FILE_MAP,
        offset: u64,
        byte_count: usize,
    ) -> MEMORY_MAPPED_VIEW_ADDRESS {
        unsafe {
            MapViewOfFile(
                self.0,
                desired_access,
                (offset >> 32) as u32,
                offset as u32,
                byte_count,
            )
        }
    }

    #[inline(always)]
    fn unmap(base_address: MEMORY_MAPPED_VIEW_ADDRESS) -> windows::core::Result<()> {
        unsafe { UnmapViewOfFile(base_address) }
    }
}

pub struct IoCompletionPort(HANDLE);
impl Drop for IoCompletionPort {
    #[inline(always)]
    fn drop(&mut self) {
        let _ = unsafe { CloseHandle(self.0) };
    }
}
unsafe impl Sync for IoCompletionPort {}
unsafe impl Send for IoCompletionPort {}
impl IoCompletionPort {
    #[inline(always)]
    pub fn new(num_threads: u32) -> windows::core::Result<Self> {
        Ok(Self(unsafe {
            CreateIoCompletionPort(INVALID_HANDLE_VALUE, None, 0, num_threads)?
        }))
    }

    #[inline(always)]
    pub fn add(&self, file_handle: HANDLE, key: usize) -> windows::core::Result<()> {
        unsafe { CreateIoCompletionPort(file_handle, Some(self.0), key, 0).map(drop) }
    }

    #[inline]
    pub fn wait<'r>(
        &self,
        result_sink: &'r mut [OVERLAPPED_ENTRY],
        timeout: Option<u32>,
        alertable: bool,
    ) -> windows::core::Result<&'r mut [OVERLAPPED_ENTRY]> {
        let mut result_count = 0;

        unsafe {
            GetQueuedCompletionStatusEx(
                self.0,
                result_sink,
                &mut result_count,
                timeout.unwrap_or(INFINITE),
                alertable,
            )?;
        }

        Ok(&mut result_sink[..result_count as usize])
    }

    #[inline(always)]
    pub fn post(
        &self,
        transferred_byte_count: u32,
        key: usize,
        overlapped: Option<*const OVERLAPPED>,
    ) -> windows::core::Result<()> {
        unsafe { PostQueuedCompletionStatus(self.0, transferred_byte_count, key, overlapped) }
    }
}

#[repr(transparent)]
pub struct NativeFileBlobRandomReader(File);
impl NativeFileBlobRandomReader {
    pub fn open(name: impl AsRef<Path>) -> std::io::Result<Self> {
        let path_wstr = name
            .as_ref()
            .as_os_str()
            .encode_wide()
            .chain(core::iter::once(0))
            .collect::<Vec<_>>();

        let h = File::create(
            PCWSTR::from_raw(path_wstr.as_ptr()),
            GENERIC_READ.0,
            FILE_SHARE_READ,
            None,
            OPEN_EXISTING,
            FILE_ATTRIBUTE_NORMAL | FILE_FLAG_RANDOM_ACCESS,
            None,
        )?;

        Ok(Self(h))
    }
}

pub struct MemoryUnmapData {
    _handle: FileMapping,
    base_addr: windows::Win32::System::Memory::MEMORY_MAPPED_VIEW_ADDRESS,
}
impl super::MemoryMapBlob for NativeFileBlobRandomReader {
    type MemoryUnmapData = MemoryUnmapData;

    fn mmap(
        &self,
        offs: u64,
        len: usize,
    ) -> std::io::Result<(*mut core::ffi::c_void, Self::MemoryUnmapData)> {
        // TODO: 必要そうならキャッシュする
        let mut sysinfo = core::mem::MaybeUninit::uninit();
        unsafe {
            GetSystemInfo(sysinfo.as_mut_ptr());
        }
        let page_size = unsafe { sysinfo.assume_init_ref().dwPageSize };

        // オフセットをページ境界にあわせる必要があるらしい
        let offset_aligned = (offs / page_size as u64) * page_size as u64;
        let offset_in_mapped_range = offs - offset_aligned;
        let len_extended = len as u64 + offset_in_mapped_range;

        let fm = FileMapping::new(&self.0, None, PAGE_READONLY, len_extended, None)?;
        let ptr = fm.map(FILE_MAP_READ, offset_aligned, 0);

        Ok((
            unsafe { ptr.Value.byte_add(offset_in_mapped_range as _) },
            MemoryUnmapData {
                _handle: fm,
                base_addr: ptr,
            },
        ))
    }

    #[inline]
    fn munmap(&self, data: Self::MemoryUnmapData) -> std::io::Result<()> {
        FileMapping::unmap(data.base_addr)?;

        Ok(())
    }
}
impl super::BlobMetadata for NativeFileBlobRandomReader {
    fn byte_length(&self) -> std::io::Result<u64> {
        let mut size = core::mem::MaybeUninit::uninit();
        unsafe {
            GetFileSizeEx(self.0.handle(), size.as_mut_ptr())?;
        }

        Ok(unsafe { size.assume_init().cast_unsigned() })
    }
}
impl super::RandomReadBlob for NativeFileBlobRandomReader {
    #[inline]
    fn read(&self, pos: u64, buf: &mut [core::mem::MaybeUninit<u8>]) -> std::io::Result<usize> {
        let mut read_bytes = 0;
        let r = unsafe {
            self.0.read(
                Some(buf),
                Some(&mut read_bytes),
                Some(&mut init_overlapped(pos, HANDLE(core::ptr::null_mut()))),
            )
        };
        match r {
            Ok(()) => Ok(read_bytes as _),
            // ERROR_HANDLE_EOFがUnexpectedEofのkindになってくれないらしいので手動で変換
            Err(e) if e.code() == ERROR_HANDLE_EOF.to_hresult() => {
                Err(std::io::Error::new(std::io::ErrorKind::UnexpectedEof, e))
            }
            Err(e) => Err(e.into()),
        }
    }

    // no native readv support for Windows
}

#[repr(transparent)]
pub struct NativeFileBlobAsyncRandomReader(File);
impl NativeFileBlobAsyncRandomReader {
    pub fn open(name: impl AsRef<Path>) -> std::io::Result<Self> {
        let path_wstr = name
            .as_ref()
            .as_os_str()
            .encode_wide()
            .chain(core::iter::once(0))
            .collect::<Vec<_>>();

        let h = File::create(
            PCWSTR::from_raw(path_wstr.as_ptr()),
            GENERIC_READ.0,
            FILE_SHARE_READ,
            None,
            OPEN_EXISTING,
            FILE_ATTRIBUTE_NORMAL | FILE_FLAG_RANDOM_ACCESS | FILE_FLAG_OVERLAPPED,
            None,
        )?;

        IoReactorHandle::current()
            .expect("no io reactor running")
            .iocp
            .add(h.handle(), IO_COMPLETION_KEY_GENERIC_IO)
            .expect("iocp add");

        Ok(Self(h))
    }
}

struct NativeFileBlobAsyncReadPendingState {
    overlapped: Box<OVERLAPPED>,
}
pub struct NativeFileBlobAsyncReadFuture<'a, 'b> {
    handle: &'a NativeFileBlobAsyncRandomReader,
    pos: u64,
    buf: &'b mut [core::mem::MaybeUninit<u8>],
    pending_state: Option<NativeFileBlobAsyncReadPendingState>,
}
impl<'a, 'b> core::future::Future for NativeFileBlobAsyncReadFuture<'a, 'b> {
    type Output = IOResult<usize>;

    fn poll(
        self: core::pin::Pin<&mut Self>,
        cx: &mut core::task::Context<'_>,
    ) -> core::task::Poll<Self::Output> {
        let this = self.get_mut();

        match this.pending_state {
            None => {
                let mut state = NativeFileBlobAsyncReadPendingState {
                    overlapped: Box::new(init_overlapped(this.pos, HANDLE(core::ptr::null_mut()))),
                };

                let mut transferred = 0;
                let r = unsafe {
                    this.handle.0.read(
                        Some(this.buf),
                        Some(&mut transferred),
                        Some(state.overlapped.as_mut()),
                    )
                };

                match r {
                    // completed synchronously
                    Ok(()) => core::task::Poll::Ready(Ok(transferred as _)),
                    Err(e) if e.code() == ERROR_IO_PENDING.to_hresult() => {
                        // working
                        IoReactorHandle::current()
                            .expect("no reactor running")
                            .request_register(OverlappedIoRegistrationRequest {
                                file: this.handle.0.handle(),
                                overlapped: state.overlapped.as_mut(),
                                waker: cx.waker().clone(),
                            })
                            .expect("Failed to register file handle to io reactor");
                        this.pending_state = Some(state);
                        core::task::Poll::Pending
                    }
                    // ERROR_HANDLE_EOFがUnexpectedEofのkindになってくれないらしいので手動で変換
                    Err(e) if e.code() == ERROR_HANDLE_EOF.to_hresult() => core::task::Poll::Ready(
                        Err(std::io::Error::new(std::io::ErrorKind::UnexpectedEof, e)),
                    ),
                    Err(e) => core::task::Poll::Ready(Err(e.into())),
                }
            }
            Some(ref mut state) => match this
                .handle
                .0
                .get_overlapped_result(state.overlapped.as_ref(), false)
            {
                Ok(transferred) => core::task::Poll::Ready(Ok(transferred as _)),
                Err(e)
                    if e.code() == ERROR_IO_PENDING.to_hresult()
                        || e.code() == ERROR_IO_INCOMPLETE.to_hresult() =>
                {
                    // still working
                    IoReactorHandle::current()
                        .expect("no reactor running")
                        .request_register(OverlappedIoRegistrationRequest {
                            file: this.handle.0.handle(),
                            overlapped: state.overlapped.as_mut(),
                            waker: cx.waker().clone(),
                        })
                        .expect("Failed to register file handle to io reactor");
                    core::task::Poll::Pending
                }
                // ERROR_HANDLE_EOFがUnexpectedEofのkindになってくれないらしいので手動で変換
                Err(e) if e.code() == ERROR_HANDLE_EOF.to_hresult() => core::task::Poll::Ready(
                    Err(std::io::Error::new(std::io::ErrorKind::UnexpectedEof, e)),
                ),
                Err(e) => core::task::Poll::Ready(Err(e.into())),
            },
        }
    }
}

pub struct NativeFileBlobAsyncReadVecFuture<'a, 'b, 'b2> {
    handle: &'a NativeFileBlobAsyncRandomReader,
    pos: u64,
    buf: &'b mut [std::io::IoSliceMut<'b2>],
    pending_state: Option<NativeFileBlobAsyncReadPendingState>,
}
impl<'a, 'b, 'b2> core::future::Future for NativeFileBlobAsyncReadVecFuture<'a, 'b, 'b2> {
    type Output = IOResult<usize>;

    fn poll(
        self: core::pin::Pin<&mut Self>,
        cx: &mut core::task::Context<'_>,
    ) -> core::task::Poll<Self::Output> {
        let this = self.get_mut();

        match this.pending_state {
            None => {
                let mut state = NativeFileBlobAsyncReadPendingState {
                    overlapped: Box::new(init_overlapped(this.pos, HANDLE(core::ptr::null_mut()))),
                };

                // windows has no actual vectored read support
                let mut transferred = 0;
                let r = unsafe {
                    this.handle.0.read(
                        Some(core::mem::transmute::<
                            &mut [_],
                            &mut [core::mem::MaybeUninit<_>],
                        >(&mut this.buf[0])),
                        Some(&mut transferred),
                        Some(state.overlapped.as_mut()),
                    )
                };

                match r {
                    // completed synchronously
                    Ok(()) => core::task::Poll::Ready(Ok(transferred as _)),
                    Err(e) if e.code() == ERROR_IO_PENDING.to_hresult() => {
                        // working
                        IoReactorHandle::current()
                            .expect("no reactor running")
                            .request_register(OverlappedIoRegistrationRequest {
                                file: this.handle.0.handle(),
                                overlapped: state.overlapped.as_mut(),
                                waker: cx.waker().clone(),
                            })
                            .expect("Failed to register file handle to io reactor");
                        this.pending_state = Some(state);
                        core::task::Poll::Pending
                    }
                    // ERROR_HANDLE_EOFがUnexpectedEofのkindになってくれないらしいので手動で変換
                    Err(e) if e.code() == ERROR_HANDLE_EOF.to_hresult() => core::task::Poll::Ready(
                        Err(std::io::Error::new(std::io::ErrorKind::UnexpectedEof, e)),
                    ),
                    Err(e) => core::task::Poll::Ready(Err(e.into())),
                }
            }
            Some(ref mut state) => match this
                .handle
                .0
                .get_overlapped_result(state.overlapped.as_ref(), false)
            {
                Ok(transferred) => core::task::Poll::Ready(Ok(transferred as _)),
                Err(e)
                    if e.code() == ERROR_IO_PENDING.to_hresult()
                        || e.code() == ERROR_IO_INCOMPLETE.to_hresult() =>
                {
                    // still working
                    IoReactorHandle::current()
                        .expect("no reactor running")
                        .request_register(OverlappedIoRegistrationRequest {
                            file: this.handle.0.handle(),
                            overlapped: state.overlapped.as_mut(),
                            waker: cx.waker().clone(),
                        })
                        .expect("Failed to register file handle to io reactor");
                    core::task::Poll::Pending
                }
                // ERROR_HANDLE_EOFがUnexpectedEofのkindになってくれないらしいので手動で変換
                Err(e) if e.code() == ERROR_HANDLE_EOF.to_hresult() => core::task::Poll::Ready(
                    Err(std::io::Error::new(std::io::ErrorKind::UnexpectedEof, e)),
                ),
                Err(e) => core::task::Poll::Ready(Err(e.into())),
            },
        }
    }
}

impl super::MemoryMapBlob for NativeFileBlobAsyncRandomReader {
    type MemoryUnmapData = MemoryUnmapData;

    fn mmap(
        &self,
        offs: u64,
        len: usize,
    ) -> std::io::Result<(*mut core::ffi::c_void, Self::MemoryUnmapData)> {
        // TODO: 必要そうならキャッシュする
        let mut sysinfo = core::mem::MaybeUninit::uninit();
        unsafe {
            GetSystemInfo(sysinfo.as_mut_ptr());
        }
        let page_size = unsafe { sysinfo.assume_init_ref().dwPageSize };

        // オフセットをページ境界にあわせる必要があるらしい
        let offset_aligned = (offs / page_size as u64) * page_size as u64;
        let offset_in_mapped_range = offs - offset_aligned;
        let len_extended = len as u64 + offset_in_mapped_range;

        let fm = FileMapping::new(&self.0, None, PAGE_READONLY, len_extended, None)?;
        let ptr = fm.map(FILE_MAP_READ, offset_aligned, 0);

        Ok((
            unsafe { ptr.Value.byte_add(offset_in_mapped_range as _) },
            MemoryUnmapData {
                _handle: fm,
                base_addr: ptr,
            },
        ))
    }

    #[inline]
    fn munmap(&self, data: Self::MemoryUnmapData) -> std::io::Result<()> {
        FileMapping::unmap(data.base_addr)?;

        Ok(())
    }
}
impl super::BlobMetadataAsync for NativeFileBlobAsyncRandomReader {
    #[inline(always)]
    fn byte_length_async(&self) -> impl core::future::Future<Output = std::io::Result<u64>> {
        async move {
            let mut size = core::mem::MaybeUninit::uninit();
            unsafe { GetFileSizeEx(self.0.handle(), size.as_mut_ptr())? };

            Ok(unsafe { size.assume_init().cast_unsigned() })
        }
    }
}
impl super::RandomReadBlobAsync for NativeFileBlobAsyncRandomReader {
    type ReadFuture<'a, 'b> = NativeFileBlobAsyncReadFuture<'a, 'b>;
    type ReadVecFuture<'a, 'b, 'b2>
        = NativeFileBlobAsyncReadVecFuture<'a, 'b, 'b2>
    where
        'b2: 'b;

    #[inline(always)]
    fn read_async<'a, 'b>(
        &'a self,
        pos: u64,
        buf: &'b mut [core::mem::MaybeUninit<u8>],
    ) -> Self::ReadFuture<'a, 'b> {
        NativeFileBlobAsyncReadFuture {
            handle: self,
            pos,
            buf,
            pending_state: None,
        }
    }

    #[inline(always)]
    fn readv_async<'a, 'b, 'b2>(
        &'a self,
        pos: u64,
        buf: &'b mut [std::io::IoSliceMut<'b2>],
    ) -> Self::ReadVecFuture<'a, 'b, 'b2> {
        NativeFileBlobAsyncReadVecFuture {
            handle: self,
            pos,
            buf,
            pending_state: None,
        }
    }
}

pub struct OverlappedIoRegistrationRequest {
    pub file: HANDLE,
    pub overlapped: *mut OVERLAPPED,
    pub waker: core::task::Waker,
}
unsafe impl Sync for OverlappedIoRegistrationRequest {}
unsafe impl Send for OverlappedIoRegistrationRequest {}

#[derive(Clone)]
pub struct IoReactorHandle {
    iocp: Arc<IoCompletionPort>,
    registration_requests: Arc<Mutex<Vec<OverlappedIoRegistrationRequest>>>,
}
impl IoReactorHandle {
    #[inline(always)]
    pub fn current() -> Option<Self> {
        IO_REACTOR_CURRENT_HANDLE.write().expect("poisoned").clone()
    }

    pub fn request_register(
        &self,
        req: OverlappedIoRegistrationRequest,
    ) -> windows::core::Result<()> {
        self.registration_requests
            .lock()
            .expect("Failed to lock request queue")
            .push(req);
        self.post_interrupt()
    }

    #[inline(always)]
    pub fn post_interrupt(&self) -> windows::core::Result<()> {
        self.iocp.post(0, IO_COMPLETION_KEY_INTERRUPT_REACTOR, None)
    }
}

static IO_REACTOR_CURRENT_HANDLE: RwLock<Option<IoReactorHandle>> = RwLock::new(None);

const IO_COMPLETION_KEY_GENERIC_IO: usize = 1;
const IO_COMPLETION_KEY_INTERRUPT_REACTOR: usize = 2;
const IO_COMPLETION_KEY_TERMINATE: usize = 3;

pub struct IoReactorThreadTerminator {
    port: Arc<IoCompletionPort>,
    thread: Option<std::thread::JoinHandle<()>>,
}
impl Drop for IoReactorThreadTerminator {
    #[inline(always)]
    fn drop(&mut self) {
        let Some(t) = self.thread.take() else {
            // already terminated?
            return;
        };

        let _ = self.port.post(0, IO_COMPLETION_KEY_TERMINATE, None);
        t.join().expect("join io reactor thread");
    }
}

pub fn spawn_io_reactor_thread() -> IoReactorThreadTerminator {
    use std::{collections::HashMap, sync::Mutex};

    let iocp = Arc::new(IoCompletionPort::new(0).expect("Failed to create io completion port"));
    let registration_requests = Arc::new(Mutex::new(Vec::new()));
    *IO_REACTOR_CURRENT_HANDLE.write().expect("poisoned") = Some(IoReactorHandle {
        iocp: iocp.clone(),
        registration_requests: registration_requests.clone(),
    });

    let thread = std::thread::Builder::new()
        .name("Windows IO Reactor".into())
        .spawn({
            let iocp = iocp.clone();

            move || {
                let mut waker_for_overlapped: HashMap<*mut OVERLAPPED, core::task::Waker> =
                    HashMap::new();

                loop {
                    let reqs = core::mem::replace(
                        &mut *registration_requests
                            .lock()
                            .expect("Failed to lock registration queue"),
                        Vec::new(),
                    );

                    for r in reqs {
                        waker_for_overlapped.insert(r.overlapped, r.waker);
                    }

                    let mut sink = [OVERLAPPED_ENTRY {
                        lpCompletionKey: 0,
                        lpOverlapped: core::ptr::null_mut(),
                        Internal: 0,
                        dwNumberOfBytesTransferred: 0,
                    }; 32];
                    let completions = iocp
                        .wait(&mut sink, None, false)
                        .expect("Failed to wait io completion");

                    for c in completions {
                        if c.lpCompletionKey == IO_COMPLETION_KEY_INTERRUPT_REACTOR {
                            // interruption(no action)
                            continue;
                        }

                        if c.lpCompletionKey == IO_COMPLETION_KEY_TERMINATE {
                            return;
                        }

                        if let Some(w) = waker_for_overlapped.remove(&c.lpOverlapped) {
                            w.wake();
                        }
                    }
                }
            }
        })
        .expect("spawning io reactor thread");

    IoReactorThreadTerminator {
        port: iocp,
        thread: Some(thread),
    }
}
