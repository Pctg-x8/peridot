//! Bit operation utils

use std::ops::{Add, BitAnd, Not, Sub};

use crate::One;

/// computes round up of `x` to the nearest multiple of `a`, specialized for `a` is a power of 2
#[inline(always)]
pub fn round_up_pow2n<T>(x: T, a: T) -> T
where
    T: BitAnd<T, Output = T>
        + Copy
        + Not<Output = T>
        + One
        + Sub<T, Output = T>
        + Add<T, Output = T>,
{
    let a1 = a - T::ONE;
    (x + a1) & !a1
}

/// computes round up of `x` to the nearest multiple of `a`, specialized for `a` is a power of 2 (u64)
#[inline(always)]
pub const fn round_up_pow2n_u64(x: u64, a: u64) -> u64 {
    (x + (a - 1)) & !(a - 1)
}
