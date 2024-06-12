#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, PartialOrd)]
pub struct SafeF32(f32);
impl SafeF32 {
    #[inline(always)]
    pub fn new(value: f32) -> Self {
        assert!(!value.is_nan(), "NaN value is not allowed");

        Self(value)
    }

    #[inline(always)]
    pub const fn value(&self) -> f32 {
        self.0
    }
}
impl From<f32> for SafeF32 {
    fn from(value: f32) -> Self {
        Self::new(value)
    }
}
impl From<SafeF32> for f32 {
    fn from(value: SafeF32) -> Self {
        value.0
    }
}
impl core::cmp::Eq for SafeF32 {}
impl core::cmp::Ord for SafeF32 {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        unsafe { self.partial_cmp(other).unwrap_unchecked() }
    }
}
impl core::hash::Hash for SafeF32 {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.to_ne_bytes().hash(state)
    }
}

mod rect_ops;
pub use self::rect_ops::*;

mod safe_event;
pub use self::safe_event::*;
