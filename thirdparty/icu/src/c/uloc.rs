use core::ffi::*;

pub type ULocDataLocaleType = c_int;
pub const ULOC_ACTUAL_LOCALE: ULocDataLocaleType = 0;
pub const ULOC_VALID_LOCALE: ULocDataLocaleType = 1;
