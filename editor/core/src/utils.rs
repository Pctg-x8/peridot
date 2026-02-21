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
impl SafeF32 {
    pub const fn new(v: f32) -> Option<Self> {
        if v.is_nan() { None } else { Some(Self(v)) }
    }

    pub const unsafe fn new_unchecked(v: f32) -> Self {
        Self(v)
    }

    pub const fn value(&self) -> f32 {
        self.0
    }
}
