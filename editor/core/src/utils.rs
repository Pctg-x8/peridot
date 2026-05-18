mod color;
pub use self::color::*;
mod coord;
pub use self::coord::*;
pub mod platform;

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

#[inline(always)]
pub const fn rup2(x: usize, a: usize) -> usize {
    (x + a - 1) & !(a - 1)
}

#[inline(always)]
pub const fn rup2_u64(x: u64, a: u64) -> u64 {
    (x + a - 1) & !(a - 1)
}
