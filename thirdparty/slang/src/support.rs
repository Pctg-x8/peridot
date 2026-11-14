use core::{
    ffi::c_void,
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

use crate::{IBlob, IUnknown};

#[repr(transparent)]
pub struct Owned<T: IUnknown>(NonNull<T>);
impl<T: IUnknown> Drop for Owned<T> {
    #[inline(always)]
    fn drop(&mut self) {
        unsafe {
            T::release(self.0.as_ref());
        }
    }
}
impl<T: IUnknown> Clone for Owned<T> {
    #[inline(always)]
    fn clone(&self) -> Self {
        T::add_ref(unsafe { self.0.as_ref() });
        Self(self.0)
    }
}
impl<T: IUnknown> Deref for Owned<T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref() }
    }
}
impl<T: IUnknown> DerefMut for Owned<T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.as_mut() }
    }
}
impl<T: IUnknown> core::fmt::Debug for Owned<T> {
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple(&format!("Owned<{}>", core::any::type_name::<T>()))
            .field(&self.0)
            .finish()
    }
}
impl<T: IUnknown> Owned<T> {
    pub const unsafe fn from_ptr_unchecked(p: *mut c_void) -> Self {
        Self(unsafe { NonNull::new_unchecked(p).cast() })
    }

    pub const unsafe fn from_non_null_unchecked(p: NonNull<c_void>) -> Self {
        Self(p.cast())
    }

    pub const fn leak(self) -> NonNull<T> {
        let p = self.0;
        core::mem::forget(self);
        p
    }
}

#[repr(transparent)]
#[derive(Clone)]
pub struct CStrBlob<Inner: IBlob>(pub(crate) Owned<Inner>);
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
    pub fn into_inner(self) -> Owned<Inner> {
        self.0
    }
}
