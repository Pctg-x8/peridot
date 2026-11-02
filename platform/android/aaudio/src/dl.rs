use core::ffi::*;

pub const RTLD_LOCAL: c_int = 0;

#[link(name = "dl")]
unsafe extern "C" {
    pub fn dlopen(file: *const c_char, flag: c_int) -> *mut c_void;
    pub fn dlclose(handle: *mut c_void) -> c_int;
    pub fn dlerror() -> *const c_char;
    pub fn dlsym(handle: *mut c_void, symbol: *const c_char) -> *mut c_void;
}

#[inline(always)]
pub fn maybe_error_msg<'a>() -> Option<&'a CStr> {
    match unsafe { dlerror() } {
        p if p.is_null() => None,
        p => Some(unsafe { CStr::from_ptr(p) }),
    }
}

/// # Safety
///
/// `dlerror` may be returns null(which produces invalid `CStr` reference)
#[inline(always)]
pub unsafe fn error_msg_unchecked<'a>() -> &'a CStr {
    unsafe { CStr::from_ptr(dlerror()) }
}

#[repr(transparent)]
pub struct Library(*mut core::ffi::c_void);
impl Drop for Library {
    #[inline(always)]
    fn drop(&mut self) {
        let r = unsafe { dlclose(self.0) };
        if r != 0 {
            let e = unsafe { error_msg_unchecked() };
            #[cfg(feature = "tracing")]
            tracing::error!(reason = ?e, "dlclose failed");
            #[cfg(not(feature = "tracing"))]
            eprintln!("dlclose failed: {e:?}");
        }
    }
}
impl Library {
    #[inline(always)]
    pub fn open(file: &CStr, flag: c_int) -> Option<Self> {
        let p = unsafe { dlopen(file.as_ptr(), flag) };
        if p.is_null() {
            let e = unsafe { error_msg_unchecked() };
            #[cfg(feature = "tracing")]
            tracing::error!(reason = ?e, "dlopen failed");
            #[cfg(not(feature = "tracing"))]
            eprintln!("dlopen failed: {e:?}");

            return None;
        }

        Some(Self(p))
    }

    /// # Safety
    ///
    /// direct call of the underlying system api(no checks provided)
    #[inline(always)]
    pub unsafe fn sym_raw(&self, sym: &CStr) -> *mut c_void {
        unsafe { dlsym(self.0, sym.as_ptr()) }
    }
}
