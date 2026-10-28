//! libudev interop

pub mod ffi;

mod base;
pub use self::base::*;
mod device;
pub use self::device::Device;

use core::{
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

pub trait RefCounted {
    fn r#ref(&mut self);
    fn unref(&mut self);
}

#[repr(transparent)]
pub struct Owned<T: RefCounted>(NonNull<T>);
impl<T: RefCounted> Drop for Owned<T> {
    #[inline(always)]
    fn drop(&mut self) {
        unsafe { self.0.as_mut().unref() }
    }
}
impl<T: RefCounted> Clone for Owned<T> {
    #[inline(always)]
    fn clone(&self) -> Self {
        unsafe {
            (*self.0.as_ptr()).r#ref();
        }
        Self(self.0)
    }
}
impl<T: RefCounted> Deref for Owned<T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref() }
    }
}
impl<T: RefCounted> DerefMut for Owned<T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.as_mut() }
    }
}
impl<T: RefCounted> Owned<T> {
    #[inline(always)]
    pub unsafe fn unretain_from_ptr(p: *mut T) -> Option<Self> {
        NonNull::new(p).map(Self)
    }

    #[inline]
    pub unsafe fn retain_ptr(p: *mut T) -> Option<Self> {
        let mut p = NonNull::new(p)?;
        unsafe {
            p.as_mut().r#ref();
        }
        Some(Self(p))
    }

    #[inline(always)]
    pub const fn as_ptr(&self) -> *mut T {
        self.0.as_ptr()
    }
}
