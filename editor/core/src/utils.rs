mod color;
pub use self::color::*;
mod coord;
pub use self::coord::*;
pub mod platform;
pub mod range_helper;

/// identity function
#[inline(always)]
pub const fn identity<T>(x: T) -> T {
    x
}

/// Safely comparable/equatable f32
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct SafeF32(f32);
impl Eq for SafeF32 {}
impl Ord for SafeF32 {
    #[inline(always)]
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        unsafe { self.0.partial_cmp(&other.0).unwrap_unchecked() }
    }
}
impl core::hash::Hash for SafeF32 {
    #[inline(always)]
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        f32::to_ne_bytes(self.0).hash(state)
    }
}
impl core::fmt::Display for SafeF32 {
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}
impl core::ops::Add for SafeF32 {
    type Output = Self;
    #[inline(always)]
    fn add(self, rhs: Self) -> Self {
        Self(self.0 + rhs.0)
    }
}
impl SafeF32 {
    pub const ZERO: Self = Self(0.0);

    pub const fn new(v: f32) -> Option<Self> {
        if v.is_nan() { None } else { Some(Self(v)) }
    }

    pub const unsafe fn new_unchecked(v: f32) -> Self {
        Self(v)
    }

    pub const fn value(&self) -> f32 {
        self.0
    }

    pub const fn max(self, other: Self) -> Self {
        Self(self.0.max(other.0))
    }
}

#[repr(transparent)]
pub struct UnsafeMainThreadOnlyOnceCell<T>(pub core::cell::OnceCell<T>);
unsafe impl<T> Sync for UnsafeMainThreadOnlyOnceCell<T> {}
unsafe impl<T> Send for UnsafeMainThreadOnlyOnceCell<T> {}

#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DummyDebug<T>(pub T);
impl<T> core::fmt::Debug for DummyDebug<T> {
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct(&format!("Hidden<{}>", std::any::type_name::<T>()))
            .finish_non_exhaustive()
    }
}

#[repr(transparent)]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NonCloneable<T>(pub T);
impl<T> Clone for NonCloneable<T> {
    fn clone(&self) -> Self {
        panic!("cannot clone this type: {}", std::any::type_name::<T>())
    }
}

#[repr(transparent)]
pub struct ByteLengthFormatter(pub usize);
impl core::fmt::Display for ByteLengthFormatter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (mut v, mut u) = (self.0 as f64, "bytes");
        if v >= 1000.0 {
            v /= 1024.0;
            u = "KB";
        }
        if v >= 1000.0 {
            v /= 1024.0;
            u = "MB";
        }
        if v >= 1000.0 {
            v /= 1024.0;
            u = "GB";
        }

        if u == "bytes" {
            write!(f, "{} bytes", self.0)
        } else {
            write!(f, "{v:.3} {u}")
        }
    }
}

/// round up to alignment
#[inline(always)]
pub const fn rup2(x: usize, a: usize) -> usize {
    (x + a - 1) & !(a - 1)
}

/// round up to alignment
#[inline(always)]
pub const fn rup2_u64(x: u64, a: u64) -> u64 {
    (x + a - 1) & !(a - 1)
}

#[inline(always)]
pub const fn range_from_len_u64(from: u64, len: u64) -> core::ops::Range<u64> {
    from..from + len
}

#[cfg(unix)]
#[inline(always)]
pub fn is_budou_cluster_char(c: char) -> bool {
    // 一部Commonにあるらしいので特別対応
    c as u32 == 0x30fc
        || c as u32 == 0xff70
        || peridot_tp_icu::get_script(c as _).is_ok_and(|s| {
            s == peridot_tp_icu::c::USCRIPT_HIRAGANA
                || s == peridot_tp_icu::c::USCRIPT_KATAKANA
                || s == peridot_tp_icu::c::USCRIPT_HAN
                || s == peridot_tp_icu::c::USCRIPT_THAI
        })
}
