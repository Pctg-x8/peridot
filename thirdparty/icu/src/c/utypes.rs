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

#[link(name = "icuuc", kind = "dylib")]
unsafe extern "C" {
    #[link_name = symbol_rename!(u_errorName)]
    pub fn u_errorName(code: UErrorCode) -> *const c_char;
}
