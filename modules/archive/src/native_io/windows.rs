use std::{
    io::Result as IOResult,
    os::windows::ffi::OsStrExt,
    sync::{Arc, Mutex},
};

#[repr(transparent)]
pub struct File(windows::Win32::Foundation::HANDLE);
impl Drop for File {
    #[inline]
    fn drop(&mut self) {
        let _ = unsafe { windows::Win32::Foundation::CloseHandle(self.0) };
    }
}
impl File {
    pub const fn handle(&self) -> windows::Win32::Foundation::HANDLE {
        self.0
    }

    #[inline]
    pub fn create(
        name: impl windows::core::Param<windows::core::PCWSTR>,
        desired_access: u32,
        share_mode: windows::Win32::Storage::FileSystem::FILE_SHARE_MODE,
        security_attributes: Option<*const windows::Win32::Security::SECURITY_ATTRIBUTES>,
        creation_disposition: windows::Win32::Storage::FileSystem::FILE_CREATION_DISPOSITION,
        flags_and_attributes: windows::Win32::Storage::FileSystem::FILE_FLAGS_AND_ATTRIBUTES,
        template_file: Option<&File>,
    ) -> windows::core::Result<Self> {
        Ok(Self(unsafe {
            windows::Win32::Storage::FileSystem::CreateFileW(
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

    #[inline]
    pub fn set_pointer(
        &mut self,
        distance: i64,
        new_file_pointer: Option<&mut i64>,
        move_method: windows::Win32::Storage::FileSystem::SET_FILE_POINTER_MOVE_METHOD,
    ) -> windows::core::Result<()> {
        unsafe {
            windows::Win32::Storage::FileSystem::SetFilePointerEx(
                self.0,
                distance,
                new_file_pointer.map(|x| x as *mut _),
                move_method,
            )
        }
    }

    // special case: no internal modification will be occured
    #[inline]
    pub fn get_current_pointer(&self) -> windows::core::Result<i64> {
        let mut ptr = 0;
        unsafe {
            windows::Win32::Storage::FileSystem::SetFilePointerEx(
                self.0,
                0,
                Some(&mut ptr),
                windows::Win32::Storage::FileSystem::FILE_CURRENT,
            )?;
        }

        Ok(ptr)
    }

    /// # Safety
    /// concurrent read may require extra data race consideration(non-overlapped files may update their internal file pointer in operation)
    #[inline]
    pub unsafe fn read(
        &self,
        buffer: Option<&mut [u8]>,
        read_byte_count: Option<&mut u32>,
        overlapped: Option<&mut windows::Win32::System::IO::OVERLAPPED>,
    ) -> windows::core::Result<()> {
        windows::Win32::Storage::FileSystem::ReadFile(
            self.0,
            buffer,
            read_byte_count.map(|x| x as *mut _),
            overlapped.map(|x| x as *mut _),
        )
    }

    pub fn get_overlapped_result(
        &self,
        overlapped: *const windows::Win32::System::IO::OVERLAPPED,
        wait: bool,
    ) -> windows::core::Result<u32> {
        let mut transferred_byte_count = 0;

        unsafe {
            windows::Win32::System::IO::GetOverlappedResult(
                self.0,
                overlapped,
                &mut transferred_byte_count,
                wait,
            )
            .map(move |_| transferred_byte_count)
        }
    }
}

const fn init_overlapped(
    offset: u64,
    event: windows::Win32::Foundation::HANDLE,
) -> windows::Win32::System::IO::OVERLAPPED {
    windows::Win32::System::IO::OVERLAPPED {
        Internal: 0,
        InternalHigh: 0,
        Anonymous: windows::Win32::System::IO::OVERLAPPED_0 {
            Anonymous: windows::Win32::System::IO::OVERLAPPED_0_0 {
                Offset: offset as u32,
                OffsetHigh: (offset >> 32) as u32,
            },
        },
        hEvent: event,
    }
}

#[repr(transparent)]
pub struct FileMapping(windows::Win32::Foundation::HANDLE);
impl FileMapping {
    pub const fn handle(&self) -> windows::Win32::Foundation::HANDLE {
        self.0
    }

    #[inline]
    pub fn new(
        file: &File,
        attributes: Option<*const windows::Win32::Security::SECURITY_ATTRIBUTES>,
        protect: windows::Win32::System::Memory::PAGE_PROTECTION_FLAGS,
        maximum_size: u64,
        name: impl windows::core::Param<windows::core::PCWSTR>,
    ) -> windows::core::Result<Self> {
        Ok(Self(unsafe {
            windows::Win32::System::Memory::CreateFileMappingW(
                file.0,
                attributes,
                protect,
                (maximum_size >> 32) as u32,
                maximum_size as u32,
                name,
            )?
        }))
    }

    #[inline]
    pub fn map(
        &self,
        desired_access: windows::Win32::System::Memory::FILE_MAP,
        offset: u64,
        byte_count: usize,
    ) -> windows::Win32::System::Memory::MEMORY_MAPPED_VIEW_ADDRESS {
        unsafe {
            windows::Win32::System::Memory::MapViewOfFile(
                self.0,
                desired_access,
                (offset >> 32) as u32,
                offset as u32,
                byte_count,
            )
        }
    }

    #[inline]
    pub fn unmap(
        base_address: windows::Win32::System::Memory::MEMORY_MAPPED_VIEW_ADDRESS,
    ) -> windows::core::Result<()> {
        unsafe { windows::Win32::System::Memory::UnmapViewOfFile(base_address) }
    }
}

pub struct IoCompletionPort(windows::Win32::Foundation::HANDLE);
impl Drop for IoCompletionPort {
    #[inline]
    fn drop(&mut self) {
        let _ = unsafe { windows::Win32::Foundation::CloseHandle(self.0) };
    }
}
unsafe impl Sync for IoCompletionPort {}
unsafe impl Send for IoCompletionPort {}
impl IoCompletionPort {
    #[inline]
    pub fn new(num_threads: u32) -> windows::core::Result<Self> {
        Ok(Self(unsafe {
            windows::Win32::System::IO::CreateIoCompletionPort(
                windows::Win32::Foundation::INVALID_HANDLE_VALUE,
                None,
                0,
                num_threads,
            )?
        }))
    }

    #[inline]
    pub fn add(
        &self,
        file_handle: windows::Win32::Foundation::HANDLE,
        key: usize,
    ) -> windows::core::Result<()> {
        unsafe {
            windows::Win32::System::IO::CreateIoCompletionPort(file_handle, Some(self.0), key, 0)
                .map(drop)
        }
    }

    #[inline]
    pub fn wait<'r>(
        &self,
        result_sink: &'r mut [windows::Win32::System::IO::OVERLAPPED_ENTRY],
        timeout: Option<u32>,
        alertable: bool,
    ) -> windows::core::Result<&'r mut [windows::Win32::System::IO::OVERLAPPED_ENTRY]> {
        let mut result_count = 0;

        unsafe {
            windows::Win32::System::IO::GetQueuedCompletionStatusEx(
                self.0,
                result_sink,
                &mut result_count,
                timeout.unwrap_or(windows::Win32::System::Threading::INFINITE),
                alertable,
            )?;
        }

        Ok(&mut result_sink[..result_count as usize])
    }

    #[inline]
    pub fn post(
        &self,
        transferred_byte_count: u32,
        key: usize,
        overlapped: Option<*const windows::Win32::System::IO::OVERLAPPED>,
    ) -> windows::core::Result<()> {
        unsafe {
            windows::Win32::System::IO::PostQueuedCompletionStatus(
                self.0,
                transferred_byte_count,
                key,
                overlapped,
            )
        }
    }
}

#[repr(transparent)]
pub struct WindowsNativeFileReader(File);
impl WindowsNativeFileReader {
    pub fn open(name: &(impl AsRef<std::path::Path> + ?Sized)) -> std::io::Result<Self> {
        let path_wstr = name
            .as_ref()
            .as_os_str()
            .encode_wide()
            .chain(core::iter::once(0))
            .collect::<Vec<_>>();

        let h = File::create(
            windows::core::PCWSTR::from_raw(path_wstr.as_ptr()),
            windows::Win32::Foundation::GENERIC_READ.0,
            windows::Win32::Storage::FileSystem::FILE_SHARE_READ,
            None,
            windows::Win32::Storage::FileSystem::OPEN_EXISTING,
            windows::Win32::Storage::FileSystem::FILE_ATTRIBUTE_NORMAL
                | windows::Win32::Storage::FileSystem::FILE_FLAG_RANDOM_ACCESS,
            None,
        )?;

        Ok(Self(h))
    }
}

pub struct WindowsMemoryUnmapData {
    _handle: FileMapping,
    base_addr: windows::Win32::System::Memory::MEMORY_MAPPED_VIEW_ADDRESS,
}
impl super::NativeFileMemoryMapProvider for WindowsNativeFileReader {
    type MemoryUnmapData = WindowsMemoryUnmapData;

    fn mmap(
        &self,
        offs: u64,
        len: u64,
    ) -> std::io::Result<(*mut core::ffi::c_void, Self::MemoryUnmapData)> {
        // TODO: 必要そうならキャッシュする
        let mut sysinfo = core::mem::MaybeUninit::uninit();
        unsafe {
            windows::Win32::System::SystemInformation::GetSystemInfo(sysinfo.as_mut_ptr());
        }
        let page_size = unsafe { sysinfo.assume_init_ref().dwPageSize };

        // オフセットをページ境界にあわせる必要があるらしい
        let offset_aligned = (offs / page_size as u64) * page_size as u64;
        let offset_in_mapped_range = offs - offset_aligned;
        let len_extended = len + offset_in_mapped_range;

        let fm = FileMapping::new(
            &self.0,
            None,
            windows::Win32::System::Memory::PAGE_READONLY,
            len_extended,
            None,
        )?;
        let ptr = fm.map(
            windows::Win32::System::Memory::FILE_MAP_READ,
            offset_aligned,
            0,
        );

        Ok((
            unsafe { ptr.Value.byte_add(offset_in_mapped_range as _) },
            WindowsMemoryUnmapData {
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
impl super::NativeFileReader for WindowsNativeFileReader {
    #[inline]
    fn current_pointer_pos(&self) -> std::io::Result<u64> {
        Ok(self.0.get_current_pointer()? as _)
    }

    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let mut read_bytes = 0;
        unsafe {
            self.0.read(Some(buf), Some(&mut read_bytes), None)?;
        }

        Ok(read_bytes as _)
    }

    #[inline]
    fn readv(&mut self, buf: &mut [std::io::IoSliceMut]) -> std::io::Result<usize> {
        // no support for windows
        self.read(&mut buf[0])
    }

    #[inline]
    fn pread(&self, buf: &mut [u8], offs: u64) -> std::io::Result<usize> {
        let mut read_bytes = 0;
        unsafe {
            self.0.read(
                Some(buf),
                Some(&mut read_bytes),
                Some(&mut init_overlapped(
                    offs,
                    windows::Win32::Foundation::HANDLE(core::ptr::null_mut()),
                )),
            )?;
        }

        Ok(read_bytes as _)
    }
}

#[repr(transparent)]
pub struct WindowsAsyncNativeFileReader(File);
impl WindowsAsyncNativeFileReader {
    pub fn open(name: &(impl AsRef<std::path::Path> + ?Sized)) -> std::io::Result<Self> {
        let path_wstr = name
            .as_ref()
            .as_os_str()
            .encode_wide()
            .chain(core::iter::once(0))
            .collect::<Vec<_>>();

        let h = File::create(
            windows::core::PCWSTR::from_raw(path_wstr.as_ptr()),
            windows::Win32::Foundation::GENERIC_READ.0,
            windows::Win32::Storage::FileSystem::FILE_SHARE_READ,
            None,
            windows::Win32::Storage::FileSystem::OPEN_EXISTING,
            windows::Win32::Storage::FileSystem::FILE_ATTRIBUTE_NORMAL
                | windows::Win32::Storage::FileSystem::FILE_FLAG_RANDOM_ACCESS
                | windows::Win32::Storage::FileSystem::FILE_FLAG_OVERLAPPED,
            None,
        )?;

        Ok(Self(h))
    }
}

struct WindowsNativeFileAsyncReadPendingState {
    overlapped: Box<windows::Win32::System::IO::OVERLAPPED>,
}
pub struct WindowsNativeFileAsyncReadFuture<'a> {
    handle: &'a mut WindowsAsyncNativeFileReader,
    buf: &'a mut [u8],
    pending_state: Option<WindowsNativeFileAsyncReadPendingState>,
}
impl<'a> core::future::Future for WindowsNativeFileAsyncReadFuture<'a> {
    type Output = IOResult<usize>;

    fn poll(
        self: core::pin::Pin<&mut Self>,
        cx: &mut core::task::Context<'_>,
    ) -> core::task::Poll<Self::Output> {
        let this = self.get_mut();

        match this.pending_state {
            None => {
                let current_pos = this.handle.0.get_current_pointer()?;

                let mut state = WindowsNativeFileAsyncReadPendingState {
                    overlapped: Box::new(init_overlapped(
                        current_pos as _,
                        windows::Win32::Foundation::INVALID_HANDLE_VALUE,
                    )),
                };

                let r = unsafe {
                    this.handle
                        .0
                        .read(Some(this.buf), None, Some(state.overlapped.as_mut()))
                };
                match r {
                    Err(e)
                        if e.code()
                            != windows::Win32::Foundation::ERROR_IO_PENDING.to_hresult() =>
                    {
                        return core::task::Poll::Ready(Err(e.into()));
                    }
                    _ => (),
                }

                WindowsIoReactorHandle::current()
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
            Some(ref mut state) => match this
                .handle
                .0
                .get_overlapped_result(state.overlapped.as_ref(), false)
            {
                Ok(transferred) => core::task::Poll::Ready(Ok(transferred as _)),
                Err(e)
                    if e.code() == windows::Win32::Foundation::ERROR_IO_PENDING.to_hresult()
                        || e.code()
                            == windows::Win32::Foundation::ERROR_IO_INCOMPLETE.to_hresult() =>
                {
                    // still working
                    WindowsIoReactorHandle::current()
                        .expect("no reactor running")
                        .request_register(OverlappedIoRegistrationRequest {
                            file: this.handle.0.handle(),
                            overlapped: state.overlapped.as_mut(),
                            waker: cx.waker().clone(),
                        })
                        .expect("Failed to register file handle to io reactor");
                    core::task::Poll::Pending
                }
                Err(e) => core::task::Poll::Ready(Err(e.into())),
            },
        }
    }
}

pub struct WindowsNativeFileAsyncReadVecFuture<'a, 'b, 'b2> {
    handle: &'a mut WindowsAsyncNativeFileReader,
    buf: &'b mut [std::io::IoSliceMut<'b2>],
    pending_state: Option<WindowsNativeFileAsyncReadPendingState>,
}
impl<'a, 'b, 'b2> core::future::Future for WindowsNativeFileAsyncReadVecFuture<'a, 'b, 'b2> {
    type Output = IOResult<usize>;

    fn poll(
        self: core::pin::Pin<&mut Self>,
        cx: &mut core::task::Context<'_>,
    ) -> core::task::Poll<Self::Output> {
        let this = self.get_mut();

        match this.pending_state {
            None => {
                let current_pos = this.handle.0.get_current_pointer()?;

                let mut state = WindowsNativeFileAsyncReadPendingState {
                    overlapped: Box::new(init_overlapped(
                        current_pos as _,
                        windows::Win32::Foundation::INVALID_HANDLE_VALUE,
                    )),
                };

                // windows has no actual vectored read support
                let r = unsafe {
                    this.handle.0.read(
                        Some(&mut this.buf[0]),
                        None,
                        Some(state.overlapped.as_mut()),
                    )
                };
                match r {
                    Err(e)
                        if e.code()
                            != windows::Win32::Foundation::ERROR_IO_PENDING.to_hresult() =>
                    {
                        return core::task::Poll::Ready(Err(e.into()));
                    }
                    _ => (),
                }

                WindowsIoReactorHandle::current()
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
            Some(ref mut state) => match this
                .handle
                .0
                .get_overlapped_result(state.overlapped.as_ref(), false)
            {
                Ok(transferred) => core::task::Poll::Ready(Ok(transferred as _)),
                Err(e)
                    if e.code() == windows::Win32::Foundation::ERROR_IO_PENDING.to_hresult()
                        || e.code()
                            == windows::Win32::Foundation::ERROR_IO_INCOMPLETE.to_hresult() =>
                {
                    // still working
                    WindowsIoReactorHandle::current()
                        .expect("no reactor running")
                        .request_register(OverlappedIoRegistrationRequest {
                            file: this.handle.0.handle(),
                            overlapped: state.overlapped.as_mut(),
                            waker: cx.waker().clone(),
                        })
                        .expect("Failed to register file handle to io reactor");
                    core::task::Poll::Pending
                }
                Err(e) => core::task::Poll::Ready(Err(e.into())),
            },
        }
    }
}

pub struct WindowsNativeFileAsyncPosReadFuture<'a, 'b> {
    handle: &'a WindowsAsyncNativeFileReader,
    buf: &'b mut [u8],
    offset: u64,
    pending_state: Option<WindowsNativeFileAsyncReadPendingState>,
}
impl<'a, 'b> core::future::Future for WindowsNativeFileAsyncPosReadFuture<'a, 'b> {
    type Output = IOResult<usize>;

    fn poll(
        self: core::pin::Pin<&mut Self>,
        cx: &mut core::task::Context<'_>,
    ) -> core::task::Poll<Self::Output> {
        let this = self.get_mut();

        match this.pending_state {
            None => {
                let mut state = WindowsNativeFileAsyncReadPendingState {
                    overlapped: Box::new(init_overlapped(
                        this.offset,
                        windows::Win32::Foundation::INVALID_HANDLE_VALUE,
                    )),
                };

                let r = unsafe {
                    this.handle
                        .0
                        .read(Some(this.buf), None, Some(state.overlapped.as_mut()))
                };
                match r {
                    Err(e)
                        if e.code()
                            != windows::Win32::Foundation::ERROR_IO_PENDING.to_hresult() =>
                    {
                        return core::task::Poll::Ready(Err(e.into()));
                    }
                    _ => (),
                }

                WindowsIoReactorHandle::current()
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
            Some(ref mut state) => match this
                .handle
                .0
                .get_overlapped_result(state.overlapped.as_ref(), false)
            {
                Ok(transferred) => core::task::Poll::Ready(Ok(transferred as _)),
                Err(e)
                    if e.code() == windows::Win32::Foundation::ERROR_IO_PENDING.to_hresult()
                        || e.code()
                            == windows::Win32::Foundation::ERROR_IO_INCOMPLETE.to_hresult() =>
                {
                    // still working
                    WindowsIoReactorHandle::current()
                        .expect("no reactor running")
                        .request_register(OverlappedIoRegistrationRequest {
                            file: this.handle.0.handle(),
                            overlapped: state.overlapped.as_mut(),
                            waker: cx.waker().clone(),
                        })
                        .expect("Failed to register file handle to io reactor");
                    core::task::Poll::Pending
                }
                Err(e) => core::task::Poll::Ready(Err(e.into())),
            },
        }
    }
}

impl super::NativeFileMemoryMapProvider for WindowsAsyncNativeFileReader {
    type MemoryUnmapData = WindowsMemoryUnmapData;

    fn mmap(
        &self,
        offs: u64,
        len: u64,
    ) -> std::io::Result<(*mut core::ffi::c_void, Self::MemoryUnmapData)> {
        // TODO: 必要そうならキャッシュする
        let mut sysinfo = core::mem::MaybeUninit::uninit();
        unsafe {
            windows::Win32::System::SystemInformation::GetSystemInfo(sysinfo.as_mut_ptr());
        }
        let page_size = unsafe { sysinfo.assume_init_ref().dwPageSize };

        // オフセットをページ境界にあわせる必要があるらしい
        let offset_aligned = (offs / page_size as u64) * page_size as u64;
        let offset_in_mapped_range = offs - offset_aligned;
        let len_extended = len + offset_in_mapped_range;

        let fm = FileMapping::new(
            &self.0,
            None,
            windows::Win32::System::Memory::PAGE_READONLY,
            len_extended,
            None,
        )?;
        let ptr = fm.map(
            windows::Win32::System::Memory::FILE_MAP_READ,
            offset_aligned,
            0,
        );

        Ok((
            unsafe { ptr.Value.byte_add(offset_in_mapped_range as _) },
            WindowsMemoryUnmapData {
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
impl super::AsyncNativeFileReader for WindowsAsyncNativeFileReader {
    type ReadFuture<'a> = WindowsNativeFileAsyncReadFuture<'a>;
    type ReadVecFuture<'a, 'b, 'b2>
        = WindowsNativeFileAsyncReadVecFuture<'a, 'b, 'b2>
    where
        'b2: 'b;
    type PosReadFuture<'a, 'b> = WindowsNativeFileAsyncPosReadFuture<'a, 'b>;

    #[inline]
    fn current_pointer_pos(&self) -> std::io::Result<u64> {
        Ok(self.0.get_current_pointer()? as _)
    }

    #[inline]
    fn read_async<'a>(&'a mut self, buf: &'a mut [u8]) -> Self::ReadFuture<'a> {
        WindowsNativeFileAsyncReadFuture {
            handle: self,
            buf,
            pending_state: None,
        }
    }

    #[inline]
    fn readv_async<'a, 'b, 'b2>(
        &'a mut self,
        buf: &'b mut [std::io::IoSliceMut<'b2>],
    ) -> Self::ReadVecFuture<'a, 'b, 'b2> {
        WindowsNativeFileAsyncReadVecFuture {
            handle: self,
            buf,
            pending_state: None,
        }
    }

    #[inline]
    fn pread_async<'a, 'b>(&'a self, buf: &'b mut [u8], offs: u64) -> Self::PosReadFuture<'a, 'b> {
        WindowsNativeFileAsyncPosReadFuture {
            handle: self,
            buf,
            offset: offs,
            pending_state: None,
        }
    }
}

pub struct OverlappedIoRegistrationRequest {
    pub file: windows::Win32::Foundation::HANDLE,
    pub overlapped: *mut windows::Win32::System::IO::OVERLAPPED,
    pub waker: core::task::Waker,
}
unsafe impl Sync for OverlappedIoRegistrationRequest {}
unsafe impl Send for OverlappedIoRegistrationRequest {}

#[derive(Clone)]
pub struct WindowsIoReactorHandle {
    iocp: Arc<IoCompletionPort>,
    registration_requests: Arc<Mutex<Vec<OverlappedIoRegistrationRequest>>>,
}
impl WindowsIoReactorHandle {
    #[inline]
    pub fn current() -> Option<WindowsIoReactorHandle> {
        WINDOWS_IO_REACTOR_CURRENT_HANDLE
            .write()
            .expect("poisoned")
            .clone()
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

    pub fn post_interrupt(&self) -> windows::core::Result<()> {
        self.iocp.post(0, IO_COMPLETION_KEY_INTERRUPT_REACTOR, None)
    }
}

static WINDOWS_IO_REACTOR_CURRENT_HANDLE: std::sync::RwLock<Option<WindowsIoReactorHandle>> =
    std::sync::RwLock::new(None);

const IO_COMPLETION_KEY_GENERIC_IO: usize = 0;
const IO_COMPLETION_KEY_INTERRUPT_REACTOR: usize = 1;
const IO_COMPLETION_KEY_TERMINATE: usize = 2;

pub struct IoReactorThreadTerminator(Arc<IoCompletionPort>);
impl Drop for IoReactorThreadTerminator {
    #[inline]
    fn drop(&mut self) {
        let _ = self.0.post(0, IO_COMPLETION_KEY_TERMINATE, None);
    }
}

#[cfg(feature = "async-rt-async-std")]
pub fn spawn_windows_io_reactor_thread(
) -> (async_std::task::JoinHandle<()>, IoReactorThreadTerminator) {
    use std::{collections::HashMap, sync::Mutex};

    let iocp = Arc::new(IoCompletionPort::new(0).expect("Failed to create io completion port"));
    let registration_requests = Arc::new(Mutex::new(Vec::new()));
    *WINDOWS_IO_REACTOR_CURRENT_HANDLE.write().expect("poisoned") = Some(WindowsIoReactorHandle {
        iocp: iocp.clone(),
        registration_requests: registration_requests.clone(),
    });
    let terminator = IoReactorThreadTerminator(iocp.clone());

    let thread = async_std::task::spawn_blocking(move || {
        let mut waker_for_overlapped: HashMap<
            *mut windows::Win32::System::IO::OVERLAPPED,
            core::task::Waker,
        > = HashMap::new();

        loop {
            let reqs = core::mem::replace(
                &mut *registration_requests
                    .lock()
                    .expect("Failed to lock registration queue"),
                Vec::new(),
            );

            for r in reqs {
                iocp.add(r.file, IO_COMPLETION_KEY_GENERIC_IO)
                    .expect("Failed to bind file handle");
                waker_for_overlapped.insert(r.overlapped, r.waker);
            }

            let mut sink = [windows::Win32::System::IO::OVERLAPPED_ENTRY {
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
    });

    (thread, terminator)
}
