use core::ptr::NonNull;

pub mod ffi;

#[repr(transparent)]
pub struct Context(NonNull<ffi::spv_context_t>);
impl Drop for Context {
    #[inline(always)]
    fn drop(&mut self) {
        unsafe { ffi::spvContextDestroy(self.0.as_ptr()) }
    }
}
impl Context {
    pub fn new(env: ffi::spv_target_env) -> Self {
        unsafe { Self(NonNull::new_unchecked(ffi::spvContextCreate(env))) }
    }

    pub fn binary_to_text(
        &self,
        binary: &[u32],
        options: u32,
        diagnostic: Option<*mut *mut ffi::spv_diagnostic_t>,
    ) -> Result<Text, ffi::spv_result_t> {
        let mut p = core::mem::MaybeUninit::uninit();
        match unsafe {
            ffi::spvBinaryToText(
                self.0.as_ptr().cast_const(),
                binary.as_ptr(),
                binary.len(),
                options,
                p.as_mut_ptr(),
                diagnostic.unwrap_or_else(core::ptr::null_mut),
            )
        } {
            r if r == ffi::SPV_SUCCESS => {
                Ok(unsafe { Text(NonNull::new_unchecked(p.assume_init())) })
            }
            r => Err(r),
        }
    }
}

#[repr(transparent)]
pub struct Text(NonNull<ffi::spv_text_t>);
impl Drop for Text {
    #[inline(always)]
    fn drop(&mut self) {
        unsafe { ffi::spvTextDestroy(self.0.as_ptr()) }
    }
}
impl Text {
    #[inline(always)]
    pub const fn as_cstr(&self) -> &core::ffi::CStr {
        unsafe { core::ffi::CStr::from_ptr(self.0.as_ref().str) }
    }
}
