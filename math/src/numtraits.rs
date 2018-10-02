//! PeridotExtendedMathematics: Numeric Traits

/// Provides zero(additive identity)
pub trait Zero { const ZERO: Self; }
/// Provides one(multiple identity)
pub trait One { const ONE: Self; }

impl Zero for u8  { const ZERO: Self = 0; }
impl Zero for i8  { const ZERO: Self = 0; }
impl Zero for u16 { const ZERO: Self = 0; }
impl Zero for i16 { const ZERO: Self = 0; }
impl Zero for u32 { const ZERO: Self = 0; }
impl Zero for i32 { const ZERO: Self = 0; }
impl Zero for u64 { const ZERO: Self = 0; }
impl Zero for i64 { const ZERO: Self = 0; }
impl Zero for f32 { const ZERO: Self = 0.0; }
impl Zero for f64 { const ZERO: Self = 0.0; }
impl One for u8  { const ONE: Self = 1; }
impl One for i8  { const ONE: Self = 1; }
impl One for u16 { const ONE: Self = 1; }
impl One for i16 { const ONE: Self = 1; }
impl One for u32 { const ONE: Self = 1; }
impl One for i32 { const ONE: Self = 1; }
impl One for u64 { const ONE: Self = 1; }
impl One for i64 { const ONE: Self = 1; }
impl One for f32 { const ONE: Self = 1.0; }
impl One for f64 { const ONE: Self = 1.0; }
