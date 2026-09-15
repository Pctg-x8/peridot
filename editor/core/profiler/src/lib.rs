use std::{
    collections::HashMap,
    io::{IoSlice, Seek, Write},
};

use crate::spinlock::Spinlocked;

mod platform;
mod spinlock;

pub use profiler_attr::*;

#[repr(align(16))]
struct ProfilerInstanceMemRgn([u8; size_of::<Profiler>()]);
static mut PROFILER_INSTANCE: ProfilerInstanceMemRgn = ProfilerInstanceMemRgn([0; _]);
#[inline(always)]
pub const fn profiler() -> &'static Profiler {
    unsafe { &*(&raw const PROFILER_INSTANCE.0).cast::<Profiler>() }
}

pub fn init_profiler() {
    std::fs::create_dir_all(".profile-data").expect("create_dir_all");
    let file = std::fs::File::create(
        &time::OffsetDateTime::now_utc()
            .format(
                &[
                    time::format_description::BorrowedFormatItem::StringLiteral(".profile-data/"),
                    time::format_description::BorrowedFormatItem::Component(
                        time::format_description::Component::IsoYearFullStandardRange(
                            time::format_description::modifier::IsoYearFullStandardRange::default()
                                .with_padding(time::format_description::modifier::Padding::Zero),
                        ),
                    ),
                    time::format_description::BorrowedFormatItem::Component(
                        time::format_description::Component::MonthNumerical(
                            time::format_description::modifier::MonthNumerical::default()
                                .with_padding(time::format_description::modifier::Padding::Zero),
                        ),
                    ),
                    time::format_description::BorrowedFormatItem::Component(
                        time::format_description::Component::Day(
                            time::format_description::modifier::Day::default()
                                .with_padding(time::format_description::modifier::Padding::Zero),
                        ),
                    ),
                    time::format_description::BorrowedFormatItem::StringLiteral("-"),
                    time::format_description::BorrowedFormatItem::Component(
                        time::format_description::Component::Hour24(
                            time::format_description::modifier::Hour24::default()
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
                    time::format_description::BorrowedFormatItem::StringLiteral(".bin"),
                ][..],
            )
            .expect("time.format"),
    )
    .expect("file.create");

    #[cfg(target_os = "linux")]
    match self::platform::StatMFile::open() {
        Ok(x) => unsafe {
            SELF_STATM = x;
        },
        Err(e) => {
            panic!("cannot open statm: {e}");
        }
    }

    unsafe {
        (&raw mut PROFILER_INSTANCE.0)
            .cast::<Profiler>()
            .write(Profiler::new(file));
    }
}

pub fn fini_profiler() {
    #[cfg(target_os = "linux")]
    drop(unsafe {
        core::mem::replace(
            &mut *&raw mut SELF_STATM,
            self::platform::StatMFile::invalid(),
        )
    });

    unsafe {
        (&raw mut PROFILER_INSTANCE.0)
            .cast::<Profiler>()
            .drop_in_place();
    }
}

#[cfg(any(unix, windows))]
use self::platform::{TIMESTAMP_FREQUENCY, timestamp};

#[cfg(target_os = "linux")]
static mut SELF_STATM: platform::linux::StatMFile =
    unsafe { platform::linux::StatMFile::invalid() };

#[derive(Debug)]
pub struct MemoryStats {
    pub total_resident_bytes: usize,
    pub total_reserved_bytes: usize,
    pub total_private_resident_bytes: usize,
}
impl MemoryStats {
    pub fn fetch() -> Self {
        #[cfg(windows)]
        let stat = platform::windows::get_self_process_memory_info();
        #[cfg(windows)]
        return Self {
            total_resident_bytes: stat.WorkingSetSize,
            total_reserved_bytes: stat.PrivateUsage,
            total_private_resident_bytes: stat.PrivateWorkingSetSize,
        };

        #[cfg(target_os = "linux")]
        let statm = match unsafe { &*&raw const SELF_STATM }.read() {
            Ok(x) => x,
            Err(e) => {
                tracing::error!(reason = %e, "cannot read statm");
                return Self {
                    total_resident_bytes: 0,
                    total_reserved_bytes: 0,
                    total_private_resident_bytes: 0,
                };
            }
        };
        #[cfg(target_os = "linux")]
        let pagesize = self::platform::pagesize();

        #[cfg(target_os = "linux")]
        Self {
            total_resident_bytes: (statm.resident * pagesize as u64) as _,
            total_reserved_bytes: (statm.size * pagesize as u64) as _,
            total_private_resident_bytes: ((statm.resident - statm.shared) * pagesize as u64) as _,
        }
    }
}

pub struct Profiler {
    writer: Spinlocked<std::io::BufWriter<std::fs::File>>,
    marker_addr_to_name: Spinlocked<HashMap<usize, &'static str>>,
    section_last_id: core::sync::atomic::AtomicU64,
}
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
            writeva(
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
        writeva(
            self.writer.get_mut(),
            &mut [IoSlice::new(&marker_addr_to_name_start.to_ne_bytes())],
        )
        .expect("write");
    }
}
impl Profiler {
    fn new(target: std::fs::File) -> Self {
        let mut target = std::io::BufWriter::new(target);

        // header
        writeva(
            &mut target,
            &mut [
                IoSlice::new(&0x0102u16.to_ne_bytes()),
                IoSlice::new(&[size_of::<usize>() as u8]),
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
        let r = writeva(&mut *self.writer.lock(), &mut iovs);
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
        let r = writeva(&mut *self.writer.lock(), &mut iovs);
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
        let r = writeva(&mut *self.writer.lock(), &mut iovs);
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

        let r = writeva(&mut *self.writer.lock(), &mut iovs);
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

        let r = writeva(&mut *self.writer.lock(), &mut iovs);
        if let Err(e) = r {
            tracing::warn!(reason = %e, "emit_memory_stats fail");
        }
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

pub struct Event {
    name: &'static str,
}
impl Event {
    pub const fn new(name: &'static str) -> Self {
        Self { name }
    }
}

#[cfg(feature = "active")]
#[macro_export]
macro_rules! event {
    ($vis: vis $varname: ident = $name: expr) => {
        $vis static $varname: $crate::Event = $crate::Event::new($name);
    };
}

#[cfg(not(feature = "active"))]
#[macro_export]
macro_rules! event {
    ($($_: tt)*) => {};
}

#[cfg(feature = "active")]
#[macro_export]
macro_rules! emit {
    ($marker: expr) => {
        $crate::profiler().emit_event(&$marker);
    };
}

#[cfg(not(feature = "active"))]
#[macro_export]
macro_rules! emit {
    ($($_: tt)*) => {};
}

pub struct Section {
    name: &'static str,
}
impl Section {
    pub const fn new(name: &'static str) -> Self {
        Self { name }
    }
}

#[cfg(feature = "active")]
#[macro_export]
macro_rules! section {
    ($vis: vis $varname: ident = $name: expr) => {
        $vis static $varname: $crate::Section = $crate::Section::new($name);
    };
}

#[cfg(not(feature = "active"))]
#[macro_export]
macro_rules! section {
    ($($_: tt)*) => {};
}

#[cfg(feature = "active")]
#[macro_export]
macro_rules! begin {
    ($section_id: ident = $marker: expr) => {
        let $section_id = $crate::profiler().emit_section_begin(&$marker);
    };
    ($section_id: ident = $marker: expr, str $s: expr) => {
        let $section_id = $crate::profiler().emit_section_begin_with_str(&$marker, $s);
    };
}

#[cfg(not(feature = "active"))]
#[macro_export]
macro_rules! begin {
    ($($_: tt)*) => {};
}

#[cfg(feature = "active")]
#[macro_export]
macro_rules! end {
    ($section_id: expr) => {
        $crate::profiler().emit_section_end($section_id);
    };
}

#[cfg(not(feature = "active"))]
#[macro_export]
macro_rules! end {
    ($($_: tt)*) => {};
}

#[repr(transparent)]
pub struct SectionScope(pub u64);
impl Drop for SectionScope {
    #[inline(always)]
    fn drop(&mut self) {
        profiler().emit_section_end(self.0);
    }
}

#[cfg(feature = "active")]
#[macro_export]
macro_rules! scope {
    ($name: ident = $marker: expr) => {
        let $name = $crate::SectionScope($crate::profiler().emit_section_begin(&$marker));
    };
    ($name: ident = $marker: expr, str $s: expr) => {
        let $name =
            $crate::SectionScope($crate::profiler().emit_section_begin_with_str(&$marker, $s));
    };
    (drop $name: ident) => {
        drop($name);
    };
    ($marker: expr) => {
        let _scope = $crate::SectionScope($crate::profiler().emit_section_begin(&$marker));
    };
    ($marker: expr, str $s: expr) => {
        let _scope =
            $crate::SectionScope($crate::profiler().emit_section_begin_with_str(&$marker, $s));
    };
}

#[cfg(not(feature = "active"))]
#[macro_export]
macro_rules! scope {
    ($($_: tt)*) => {};
}

#[macro_export]
macro_rules! wrap {
    ($marker: expr, { $($xs: tt)* }) => {{
        $crate::scope!($marker);
        $($xs)*
    }};
    ($marker: expr, $x: expr) => {{
        $crate::scope!($marker);
        $x
    }};
}

#[cfg(feature = "active")]
#[macro_export]
macro_rules! sample_memory {
    () => {{
        $crate::profiler().emit_memory_stats();
    }};
}

#[cfg(not(feature = "active"))]
#[macro_export]
macro_rules! sample_memory {
    () => {};
}
