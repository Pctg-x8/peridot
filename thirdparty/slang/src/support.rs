use crate::IBlob;

#[repr(transparent)]
#[derive(Clone)]
pub struct CStrBlob<Inner: IBlob>(pub(crate) Inner);
impl<Inner: IBlob> core::fmt::Debug for CStrBlob<Inner> {
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_cstr().fmt(f)
    }
}
impl<Inner: IBlob> CStrBlob<Inner> {
    #[inline(always)]
    pub fn as_cstr(&self) -> &core::ffi::CStr {
        unsafe { core::ffi::CStr::from_ptr(self.0.get_buffer_pointer().cast()) }
    }

    #[inline(always)]
    pub fn into_inner(self) -> Inner {
        self.0
    }
}
