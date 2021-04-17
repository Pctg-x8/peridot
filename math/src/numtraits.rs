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

/// Provides minimum value selector
pub trait Min<Other = Self> { type Output; fn min(self, other: Other) -> Self::Output; }
/// Provides maximum value selector
pub trait Max<Other = Self> { type Output; fn max(self, other: Other) -> Self::Output; }

impl Min for f32 { type Output = f32; fn min(self, other: f32) -> f32 { self.min(other) } }
impl Min for f64 { type Output = f64; fn min(self, other: f64) -> f64 { self.min(other) } }
impl Max for f32 { type Output = f32; fn max(self, other: f32) -> f32 { self.max(other) } }
impl Max for f64 { type Output = f64; fn max(self, other: f64) -> f64 { self.max(other) } }
