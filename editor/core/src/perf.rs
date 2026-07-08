#[cfg(feature = "enable-profiling")]
use std::{
    collections::HashMap,
    io::{IoSlice, Seek, Write},
};

#[inline(always)]
#[cfg(feature = "enable-profiling")]
pub fn timestamp() -> i64 {
    #[cfg(windows)]
    let mut x = core::mem::MaybeUninit::uninit();
    #[cfg(windows)]
    unsafe {
        // never fails on winxp or later
        let _ = windows::Win32::System::Performance::QueryPerformanceCounter(x.as_mut_ptr());
    }
    #[cfg(windows)]
    unsafe {
        x.assume_init()
    }

    #[cfg(unix)]
    let mut x = core::mem::MaybeUninit::uninit();
    #[cfg(unix)]
    if unsafe { libc::clock_gettime(libc::CLOCK_MONOTONIC_RAW, x.as_mut_ptr()) } < 0 {
        tracing::error!(reason = %std::io::Error::last_os_error(), "clock_gettime failed");
        return 0;
    }
    #[cfg(unix)]
    unsafe {
        let x = x.assume_init();
        x.tv_nsec + x.tv_sec * 1_000_000_000
    }
}

#[cfg(feature = "enable-profiling")]
#[cfg(windows)]
pub static TIMESTAMP_FREQUENCY: std::sync::LazyLock<i64> = std::sync::LazyLock::new(|| {
    let mut x = core::mem::MaybeUninit::uninit();
    unsafe {
        // never fails on winxp or later
        let _ = windows::Win32::System::Performance::QueryPerformanceFrequency(x.as_mut_ptr());
    }
    unsafe { x.assume_init() }
});

// always const value(clock_gettime returns in nanosecs)
#[cfg(feature = "enable-profiling")]
#[cfg(unix)]
pub const TIMESTAMP_FREQUENCY: i64 = 1_000_000_000;

#[cfg(feature = "enable-profiling")]
#[cfg(unix)]
static mut SELF_STATM_FD: core::ffi::c_int = -1;

#[cfg(feature = "enable-profiling")]
#[derive(Debug)]
pub struct MemoryStats {
    pub total_resident_bytes: usize,
    pub total_reserved_bytes: usize,
    pub total_private_resident_bytes: usize,
}
#[cfg(feature = "enable-profiling")]
impl MemoryStats {
    pub fn fetch() -> Self {
        #[cfg(windows)]
        let mut stat = core::mem::MaybeUninit::<
            windows::Win32::System::ProcessStatus::PROCESS_MEMORY_COUNTERS_EX2,
        >::uninit();
        #[cfg(windows)]
        if let Err(e) = unsafe {
            windows::Win32::System::ProcessStatus::GetProcessMemoryInfo(
                windows::Win32::System::Threading::GetCurrentProcess(),
                stat.as_mut_ptr().cast(),
                core::mem::size_of::<
                    windows::Win32::System::ProcessStatus::PROCESS_MEMORY_COUNTERS_EX2,
                >() as _,
            )
        } {
            tracing::error!(reason = %e, "GetProcessMemoryInfo failed");
            return Self {
                total_resident_bytes: 0,
                total_reserved_bytes: 0,
                total_private_resident_bytes: 0,
            };
        }
        #[cfg(windows)]
        let stat = unsafe { stat.assume_init_ref() };
        #[cfg(windows)]
        return Self {
            total_resident_bytes: stat.WorkingSetSize,
            total_reserved_bytes: stat.PrivateUsage,
            total_private_resident_bytes: stat.PrivateWorkingSetSize,
        };

        #[cfg(target_os = "linux")]
        const BUFSIZE: usize = 64;
        #[cfg(target_os = "linux")]
        let mut buf = [core::mem::MaybeUninit::<u8>::uninit(); BUFSIZE];
        #[cfg(target_os = "linux")]
        let nread = unsafe { libc::pread(SELF_STATM_FD, buf.as_mut_ptr().cast(), BUFSIZE as _, 0) };
        #[cfg(target_os = "linux")]
        if nread < 0 {
            tracing::error!(reason = %std::io::Error::last_os_error(), "cannot read statm");
            return Self {
                total_resident_bytes: 0,
                total_reserved_bytes: 0,
                total_private_resident_bytes: 0,
            };
        }

        #[cfg(target_os = "linux")]
        let mut buf: &[u8] = unsafe {
            &core::mem::transmute::<&[_; BUFSIZE], &[_; BUFSIZE]>(&buf)[..nread as usize]
        };
        #[cfg(target_os = "linux")]
        let mut size = 0u64;
        #[cfg(target_os = "linux")]
        while let &[c, ref rest @ ..] = buf
            && c != b' '
        {
            size = size * 10 + (c - b'0') as u64;
            buf = rest;
        }
        #[cfg(target_os = "linux")]
        while let &[b' ', ref rest @ ..] = buf {
            buf = rest;
        }
        #[cfg(target_os = "linux")]
        let mut resident = 0u64;
        #[cfg(target_os = "linux")]
        while let &[c, ref rest @ ..] = buf
            && c != b' '
        {
            resident = resident * 10 + (c - b'0') as u64;
            buf = rest;
        }
        #[cfg(target_os = "linux")]
        while let &[b' ', ref rest @ ..] = buf {
            buf = rest;
        }
        #[cfg(target_os = "linux")]
        let mut resident_shared = 0u64;
        #[cfg(target_os = "linux")]
        while let &[c, ref rest @ ..] = buf
            && c != b' '
        {
            resident_shared = resident_shared * 10 + (c - b'0') as u64;
            buf = rest;
        }

        #[cfg(target_os = "linux")]
        let pagesize = unsafe { getpagesize() };

        #[cfg(target_os = "linux")]
        {
            Self {
                total_resident_bytes: (resident * pagesize as u64) as _,
                total_reserved_bytes: (size * pagesize as u64) as _,
                total_private_resident_bytes: ((resident - resident_shared) * pagesize as u64) as _,
            }
        }
    }
}

#[cfg(feature = "enable-profiling")]
#[cfg(unix)]
unsafe extern "C" {
    fn getpagesize() -> core::ffi::c_int;
}

/// Simple spin-lock based mutex
#[cfg(feature = "enable-profiling")]
pub struct Spinlocked<T> {
    value: core::cell::UnsafeCell<T>,
    lock: core::sync::atomic::AtomicBool,
}
#[cfg(feature = "enable-profiling")]
unsafe impl<T> Sync for Spinlocked<T> {}
#[cfg(feature = "enable-profiling")]
unsafe impl<T> Send for Spinlocked<T> {}
#[cfg(feature = "enable-profiling")]
impl<T> Spinlocked<T> {
    #[inline(always)]
    pub const fn new(value: T) -> Self {
        Self {
            value: core::cell::UnsafeCell::new(value),
            lock: core::sync::atomic::AtomicBool::new(false),
        }
    }

    #[inline(always)]
    pub const fn get_mut(&mut self) -> &mut T {
        self.value.get_mut()
    }

    #[inline(always)]
    pub fn lock<'a>(&'a self) -> SpinlockedGuard<'a, T> {
        self.acquire_lock();
        SpinlockedGuard { lock: self }
    }

    #[inline(always)]
    fn acquire_lock(&self) {
        while self
            .lock
            .compare_exchange(
                false,
                true,
                core::sync::atomic::Ordering::Relaxed,
                core::sync::atomic::Ordering::Relaxed,
            )
            .is_err()
        {
            core::hint::spin_loop();
        }
    }

    #[inline(always)]
    fn release_lock(&self) {
        self.lock
            .store(false, core::sync::atomic::Ordering::Relaxed);
    }
}

#[cfg(feature = "enable-profiling")]
#[repr(transparent)]
pub struct SpinlockedGuard<'a, T> {
    lock: &'a Spinlocked<T>,
}
#[cfg(feature = "enable-profiling")]
impl<'a, T> Drop for SpinlockedGuard<'a, T> {
    #[inline(always)]
    fn drop(&mut self) {
        self.lock.release_lock();
    }
}
#[cfg(feature = "enable-profiling")]
impl<'a, T> core::ops::Deref for SpinlockedGuard<'a, T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        unsafe { &*self.lock.value.get() }
    }
}
#[cfg(feature = "enable-profiling")]
impl<'a, T> core::ops::DerefMut for SpinlockedGuard<'a, T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.lock.value.get() }
    }
}

#[cfg(feature = "enable-profiling")]
pub struct Profiler {
    writer: Spinlocked<std::io::BufWriter<std::fs::File>>,
    marker_addr_to_name: Spinlocked<HashMap<usize, &'static str>>,
    section_last_id: core::sync::atomic::AtomicU64,
}
#[cfg(feature = "enable-profiling")]
impl Drop for Profiler {
    #[inline(always)]
    fn drop(&mut self) {
        // terminal marker
        self.writer
            .get_mut()
            .write_all(&[MarkerTag::Terminal as _])
            .expect("write");

        let marker_addr_to_name_start = self
            .writer
            .get_mut()
            .stream_position()
            .expect("file.stream_position");
        self.writer
            .get_mut()
            .write_all(&self.marker_addr_to_name.get_mut().len().to_ne_bytes())
            .expect("write");
        for (addr, name) in self.marker_addr_to_name.get_mut().drain() {
            Self::writeva(
                self.writer.get_mut(),
                &mut [
                    IoSlice::new(&addr.to_ne_bytes()),
                    IoSlice::new(name.as_bytes()),
                    IoSlice::new(&[0]),
                ],
            )
            .expect("write");
        }

        // footer
        Self::writeva(
            self.writer.get_mut(),
            &mut [IoSlice::new(&marker_addr_to_name_start.to_ne_bytes())],
        )
        .expect("write");
    }
}
#[cfg(feature = "enable-profiling")]
impl Profiler {
    fn new(target: std::fs::File) -> Self {
        let mut target = std::io::BufWriter::new(target);

        // header
        Self::writeva(
            &mut target,
            &mut [
                IoSlice::new(&0x0102u16.to_ne_bytes()),
                IoSlice::new(&[core::mem::size_of::<usize>() as u8]),
                IoSlice::new(&TIMESTAMP_FREQUENCY.to_ne_bytes()),
            ],
        )
        .expect("write");

        Self {
            writer: Spinlocked::new(target),
            marker_addr_to_name: Spinlocked::new(HashMap::new()),
            section_last_id: core::sync::atomic::AtomicU64::new(0),
        }
    }

    #[inline(always)]
    pub fn emit_event(&self, marker: &Event) {
        let ts = timestamp().to_ne_bytes();

        let marker_tag = MarkerTag::Event.to_ne_bytes();
        let marker_ident = core::ptr::from_ref(marker).addr().to_ne_bytes();
        let mut iovs = [
            IoSlice::new(&marker_tag),
            IoSlice::new(&ts),
            IoSlice::new(&marker_ident),
        ];

        self.marker_addr_to_name
            .lock()
            .insert(core::ptr::from_ref(marker).addr(), marker.name);
        let r = Self::writeva(&mut *self.writer.lock(), &mut iovs);
        if let Err(e) = r {
            tracing::warn!(reason = %e, "emit_marker fail");
        }
    }

    #[inline(always)]
    pub fn emit_section_begin(&self, marker: &Section) -> u64 {
        let ts = timestamp().to_ne_bytes();

        let marker_tag = MarkerTag::SectionBegin.to_ne_bytes();
        let marker_ident = core::ptr::from_ref(marker).addr().to_ne_bytes();
        let section_id = self
            .section_last_id
            .fetch_add(1, core::sync::atomic::Ordering::Relaxed);
        let section_id_bytes = section_id.to_ne_bytes();
        let data_type_tag = AuxDataTypeTag::None.to_ne_bytes();
        let mut iovs = [
            IoSlice::new(&marker_tag),
            IoSlice::new(&ts),
            IoSlice::new(&marker_ident),
            IoSlice::new(&section_id_bytes),
            IoSlice::new(&data_type_tag),
        ];

        self.marker_addr_to_name
            .lock()
            .insert(core::ptr::from_ref(marker).addr(), marker.name);
        let r = Self::writeva(&mut *self.writer.lock(), &mut iovs);
        if let Err(e) = r {
            tracing::warn!(reason = %e, "emit_section_begin fail");
        }

        section_id
    }

    #[inline(always)]
    pub fn emit_section_begin_with_str(&self, marker: &Section, s: &str) -> u64 {
        let ts = timestamp().to_ne_bytes();

        let marker_tag = MarkerTag::SectionBegin.to_ne_bytes();
        let marker_ident = core::ptr::from_ref(marker).addr().to_ne_bytes();
        let section_id = self
            .section_last_id
            .fetch_add(1, core::sync::atomic::Ordering::Relaxed);
        let section_id_bytes = section_id.to_ne_bytes();
        let data_type_tag = AuxDataTypeTag::String.to_ne_bytes();
        let data_type_tag2 = AuxDataTypeTag::None.to_ne_bytes();
        let mut iovs = [
            IoSlice::new(&marker_tag),
            IoSlice::new(&ts),
            IoSlice::new(&marker_ident),
            IoSlice::new(&section_id_bytes),
            IoSlice::new(&data_type_tag),
            IoSlice::new(s.as_bytes()),
            IoSlice::new(&[0]),
            IoSlice::new(&data_type_tag2),
        ];

        self.marker_addr_to_name
            .lock()
            .insert(core::ptr::from_ref(marker).addr(), marker.name);
        let r = Self::writeva(&mut *self.writer.lock(), &mut iovs);
        if let Err(e) = r {
            tracing::warn!(reason = %e, "emit_section_begin_with_fmt fail");
        }

        section_id
    }

    #[inline(always)]
    pub fn emit_section_end(&self, section_id: u64) {
        let ts = timestamp().to_ne_bytes();

        let marker_tag = MarkerTag::SectionEnd.to_ne_bytes();
        let section_id_bytes = section_id.to_ne_bytes();
        let mut iovs = [
            IoSlice::new(&marker_tag),
            IoSlice::new(&ts),
            IoSlice::new(&section_id_bytes),
        ];

        let r = Self::writeva(&mut *self.writer.lock(), &mut iovs);
        if let Err(e) = r {
            tracing::warn!(reason = %e, "emit_section_end fail");
        }
    }

    #[inline(always)]
    pub fn emit_memory_stats(&self) {
        let ts = timestamp().to_ne_bytes();
        let memstats = MemoryStats::fetch();

        let marker_tag = MarkerTag::MemoryStats.to_ne_bytes();
        let ms_total_resident_bytes = memstats.total_resident_bytes.to_ne_bytes();
        let ms_total_reserved_bytes = memstats.total_reserved_bytes.to_ne_bytes();
        let ms_total_private_resident_bytes = memstats.total_private_resident_bytes.to_ne_bytes();
        let mut iovs = [
            IoSlice::new(&marker_tag),
            IoSlice::new(&ts),
            IoSlice::new(&ms_total_resident_bytes),
            IoSlice::new(&ms_total_reserved_bytes),
            IoSlice::new(&ms_total_private_resident_bytes),
        ];

        let r = Self::writeva(&mut *self.writer.lock(), &mut iovs);
        if let Err(e) = r {
            tracing::warn!(reason = %e, "emit_memory_stats fail");
        }
    }

    #[inline(always)]
    fn writeva(w: &mut (impl Write + ?Sized), mut v: &mut [IoSlice]) -> std::io::Result<()> {
        while !v.is_empty() {
            let b = w.write_vectored(v)?;
            IoSlice::advance_slices(&mut v, b);
        }

        Ok(())
    }
}

#[repr(u8)]
enum MarkerTag {
    Terminal = 0,
    Event = 1,
    SectionBegin = 2,
    SectionEnd = 3,
    MemoryStats = 4,
}
impl MarkerTag {
    #[inline(always)]
    const fn to_ne_bytes(self) -> [u8; 1] {
        [self as _]
    }
}

#[repr(u16)]
enum AuxDataTypeTag {
    None = 0,
    String = 1,
}
impl AuxDataTypeTag {
    #[inline(always)]
    const fn to_ne_bytes(self) -> [u8; 2] {
        (self as u16).to_ne_bytes()
    }
}

#[cfg(feature = "enable-profiling")]
static mut PROFILER_INSTANCE: [u64; core::mem::size_of::<Profiler>() / 8] =
    [0u64; core::mem::size_of::<Profiler>() / 8];
#[cfg(feature = "enable-profiling")]
pub fn profiler() -> &'static Profiler {
    unsafe { &*(core::ptr::addr_of!(PROFILER_INSTANCE) as *const Profiler) }
}

pub fn init_profiler() {
    #[cfg(feature = "enable-profiling")]
    std::fs::create_dir_all(".profile-data").expect("create_dir_all");
    #[cfg(feature = "enable-profiling")]
    let file = std::fs::File::create(
        &time::OffsetDateTime::now_utc()
            .format(
                &[
                    time::format_description::BorrowedFormatItem::Literal(b".profile-data/"),
                    time::format_description::BorrowedFormatItem::Component(
                        time::format_description::Component::Year(
                            time::format_description::modifier::Year::default()
                                .with_padding(time::format_description::modifier::Padding::Zero)
                                .with_repr(time::format_description::modifier::YearRepr::Full),
                        ),
                    ),
                    time::format_description::BorrowedFormatItem::Component(
                        time::format_description::Component::Month(
                            time::format_description::modifier::Month::default()
                                .with_padding(time::format_description::modifier::Padding::Zero)
                                .with_repr(
                                    time::format_description::modifier::MonthRepr::Numerical,
                                ),
                        ),
                    ),
                    time::format_description::BorrowedFormatItem::Component(
                        time::format_description::Component::Day(
                            time::format_description::modifier::Day::default()
                                .with_padding(time::format_description::modifier::Padding::Zero),
                        ),
                    ),
                    time::format_description::BorrowedFormatItem::Literal(b"-"),
                    time::format_description::BorrowedFormatItem::Component(
                        time::format_description::Component::Hour(
                            time::format_description::modifier::Hour::default()
                                .with_padding(time::format_description::modifier::Padding::Zero),
                        ),
                    ),
                    time::format_description::BorrowedFormatItem::Component(
                        time::format_description::Component::Minute(
                            time::format_description::modifier::Minute::default()
                                .with_padding(time::format_description::modifier::Padding::Zero),
                        ),
                    ),
                    time::format_description::BorrowedFormatItem::Component(
                        time::format_description::Component::Second(
                            time::format_description::modifier::Second::default()
                                .with_padding(time::format_description::modifier::Padding::Zero),
                        ),
                    ),
                    time::format_description::BorrowedFormatItem::Literal(b".bin"),
                ][..],
            )
            .expect("time.format"),
    )
    .expect("file.create");

    #[cfg(feature = "enable-profiling")]
    #[cfg(unix)]
    {
        let fd = unsafe { libc::open(c"/proc/self/statm".as_ptr(), libc::O_RDONLY) };
        if fd < 0 {
            panic!("cannot open statm: {}", std::io::Error::last_os_error());
        }

        unsafe {
            SELF_STATM_FD = fd;
        }
    }

    #[cfg(feature = "enable-profiling")]
    unsafe {
        core::ptr::write(
            core::ptr::addr_of_mut!(PROFILER_INSTANCE).cast(),
            Profiler::new(file),
        )
    }
}

pub fn fini_profiler() {
    #[cfg(feature = "enable-profiling")]
    #[cfg(unix)]
    if unsafe { libc::close(SELF_STATM_FD) } < 0 {
        panic!("failed close statm: {}", std::io::Error::last_os_error());
    }

    #[cfg(feature = "enable-profiling")]
    unsafe {
        core::ptr::drop_in_place(core::ptr::addr_of_mut!(PROFILER_INSTANCE).cast::<Profiler>());
    }
}

#[cfg(feature = "enable-profiling")]
pub struct Event {
    name: &'static str,
}
#[cfg(feature = "enable-profiling")]
impl Event {
    pub const fn new(name: &'static str) -> Self {
        Self { name }
    }

    #[inline(always)]
    pub fn emit(&self) {
        profiler().emit_event(self);
    }
}

#[macro_export]
macro_rules! perf_event {
    ($vis: vis $varname: ident = $name: expr) => {
        #[cfg(feature = "enable-profiling")]
        $vis static $varname: $crate::perf::Event = $crate::perf::Event::new($name);
    };
}

#[macro_export]
macro_rules! perf_emit {
    ($marker: expr) => {
        #[cfg(feature = "enable-profiling")]
        $marker.emit();
    };
}

#[cfg(feature = "enable-profiling")]
pub struct Section {
    name: &'static str,
}
#[cfg(feature = "enable-profiling")]
impl Section {
    pub const fn new(name: &'static str) -> Self {
        Self { name }
    }
}

#[macro_export]
macro_rules! perf_section {
    ($vis: vis $varname: ident = $name: expr) => {
        #[cfg(feature = "enable-profiling")]
        $vis static $varname: $crate::perf::Section = $crate::perf::Section::new($name);
    };
}

#[macro_export]
macro_rules! perf_begin {
    ($section_id: ident = $marker: expr) => {
        #[cfg(feature = "enable-profiling")]
        let $section_id = $crate::perf::profiler().emit_section_begin(&$marker);
    };
    ($section_id: ident = $marker: expr, str $s: expr) => {
        #[cfg(feature = "enable-profiling")]
        let $section_id = $crate::perf::profiler().emit_section_begin_with_str(&$marker, $s);
    };
}

#[macro_export]
macro_rules! perf_end {
    ($section_id: expr) => {
        #[cfg(feature = "enable-profiling")]
        $crate::perf::profiler().emit_section_end($section_id);
    };
}

#[repr(transparent)]
pub struct SectionScope(#[cfg(feature = "enable-profiling")] pub u64);
impl Drop for SectionScope {
    #[inline(always)]
    fn drop(&mut self) {
        #[cfg(feature = "enable-profiling")]
        {
            profiler().emit_section_end(self.0);
        }
    }
}

#[macro_export]
macro_rules! perf_scope {
    ($name: ident = $marker: expr) => {
        #[cfg(feature = "enable-profiling")]
        let $name =
            $crate::perf::SectionScope($crate::perf::profiler().emit_section_begin(&$marker));
    };
    ($name: ident = $marker: expr, str $s: expr) => {
        #[cfg(feature = "enable-profiling")]
        let $name = $crate::perf::SectionScope(
            $crate::perf::profiler().emit_section_begin_with_str(&$marker, $s),
        );
    };
    (drop $name: ident) => {
        #[cfg(feature = "enable-profiling")]
        drop($name);
    };
    ($marker: expr) => {
        #[cfg(feature = "enable-profiling")]
        let _scope =
            $crate::perf::SectionScope($crate::perf::profiler().emit_section_begin(&$marker));
    };
    ($marker: expr, str $s: expr) => {
        #[cfg(feature = "enable-profiling")]
        let _scope = $crate::perf::SectionScope(
            $crate::perf::profiler().emit_section_begin_with_str(&$marker, $s),
        );
    };
}

#[macro_export]
macro_rules! perf_wrap {
    ($marker: expr; { $($xs: tt)* }) => {{
        $crate::perf_scope!($marker);
        $($xs)*
    }}
}

#[macro_export]
macro_rules! perf_sample_memory {
    () => {
        #[cfg(feature = "enable-profiling")]
        {
            $crate::perf::profiler().emit_memory_stats();
        }
    };
}
