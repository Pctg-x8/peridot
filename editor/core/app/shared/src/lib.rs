use core::any::TypeId;

mod coord;
pub use coord::*;
mod bitops;
pub use bitops::*;
mod color;
pub use color::*;
mod range_helper;
pub use range_helper::*;
mod text;
pub use text::*;

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
impl peridot_math::Zero for SafeF32 {
    const ZERO: Self = Self(0.0);
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

    pub const fn max(self, other: Self) -> Self {
        Self(self.0.max(other.0))
    }
}

/// round up to alignment, optimized for 2^n alignments
///
/// ## Examples
///
/// ```
/// assert_eq!(rup2(10, 4), 12);
/// assert_eq!(rup2(11, 4), 12);
/// assert_eq!(rup2(12, 4), 12);
/// assert_eq!(rup2(13, 4), 16);
/// assert_eq!(rup2(16, 4), 16);
/// ```
#[inline(always)]
pub const fn rup2(x: usize, a: usize) -> usize {
    (x + a - 1) & !(a - 1)
}

/// round up to alignment, optimized for 2^n alignments
///
/// ## Examples
///
/// ```
/// assert_eq!(rup2_u64(10, 4), 12);
/// assert_eq!(rup2_u64(11, 4), 12);
/// assert_eq!(rup2_u64(12, 4), 12);
/// assert_eq!(rup2_u64(13, 4), 16);
/// assert_eq!(rup2_u64(16, 4), 16);
/// ```
#[inline(always)]
pub const fn rup2_u64(x: u64, a: u64) -> u64 {
    (x + a - 1) & !(a - 1)
}

pub struct NonDropAnyTypeQueue {
    bytes: Vec<u8>,
    types: Vec<(&'static TypeId, usize)>,
}
impl NonDropAnyTypeQueue {
    pub fn new() -> Self {
        Self {
            bytes: Vec::new(),
            types: Vec::new(),
        }
    }

    pub const fn is_empty(&self) -> bool {
        self.types.is_empty()
    }

    pub fn clear(&mut self) {
        self.types.clear();
        self.bytes.clear();
    }

    pub fn push<T: 'static>(&mut self, feedback: T) {
        self.types
            .push((&const { TypeId::of::<T>() }, size_of::<T>()));
        let bytes_head = self.bytes.len();
        self.bytes
            .try_reserve(size_of::<T>())
            .expect("view_feedback_queue.push");
        unsafe {
            self.bytes.set_len(bytes_head + size_of::<T>());
            self.bytes
                .as_mut_ptr()
                .byte_add(bytes_head)
                .cast::<T>()
                .write_unaligned(feedback);
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (&'static TypeId, *const ())> + '_ {
        self.types.iter().scan(0, |offset, type_id| {
            let bytes = unsafe { self.bytes.as_ptr().byte_add(*offset) };
            *offset += type_id.1;
            Some((type_id.0, bytes.cast()))
        })
    }
}
