use core::ffi::*;

pub type UErrorCode = c_int;
pub const U_ZERO_ERROR: UErrorCode = 0;

#[inline(always)]
pub const fn U_SUCCESS(code: UErrorCode) -> bool {
    code <= U_ZERO_ERROR
}

#[inline(always)]
pub const fn U_FAILURE(code: UErrorCode) -> bool {
    code > U_ZERO_ERROR
}

unsafe extern "C" {
    pub fn u_errorName(code: UErrorCode) -> *const c_char;
}
