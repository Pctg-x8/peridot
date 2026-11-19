use core::ops::{Deref, DerefMut};
use core::ptr::NonNull;

#[repr(C)]
pub(crate) struct FFIOpaqueStruct(
    [u8; 0],
    core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,
);

pub mod objc;
pub mod raw;

pub mod foundation;
pub mod graphics;
pub mod text;

#[cfg(target_pointer_width = "64")]
pub type NSInteger = core::ffi::c_long;
#[cfg(not(target_pointer_width = "64"))]
pub type NSInteger = core::ffi::c_int;

#[cfg(target_pointer_width = "64")]
pub type NSUInteger = core::ffi::c_ulong;
#[cfg(not(target_pointer_width = "64"))]
pub type NSUInteger = core::ffi::c_uint;

pub trait Object {
    fn as_typeref(&self) -> raw::CFTypeRef;

    #[inline(always)]
    fn as_any(&self) -> &AnyObject {
        unsafe { &*(self as *const Self as *const AnyObject) }
    }

    #[inline(always)]
    fn retain(&self) {
        unsafe {
            raw::CFRetain(self.as_typeref());
        }
    }

    #[inline(always)]
    unsafe fn release(&self) {
        unsafe {
            raw::CFRelease(self.as_typeref());
        }
    }

    #[inline(always)]
    fn retain_count(&self) -> raw::CFIndex {
        unsafe { raw::CFGetRetainCount(self.as_typeref()) }
    }
}
pub trait MutableObject: Object {}

#[repr(transparent)]
pub struct AnyObject(core::ffi::c_void);
impl Object for AnyObject {
    #[inline(always)]
    fn as_typeref(&self) -> raw::CFTypeRef {
        &self.0
    }
}

#[repr(transparent)]
pub struct Owned<T: Object>(core::ptr::NonNull<T>);
impl<T: Object> Drop for Owned<T> {
    #[inline(always)]
    fn drop(&mut self) {
        unsafe {
            tracing::trace!(
                target: "apple_sdk_port::drop_trace",
                type_name = core::any::type_name::<T>(),
                before_rc = self.0.as_ref().retain_count(),
                "release cf"
            );

            self.0.as_ref().release();
        }
    }
}
impl<T: Object> Clone for Owned<T> {
    #[inline(always)]
    fn clone(&self) -> Self {
        unsafe {
            self.0.as_ref().retain();
        }
        Self(self.0)
    }
}
impl<T: Object> core::ops::Deref for Owned<T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref() }
    }
}
impl<T: MutableObject> core::ops::DerefMut for Owned<T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.as_mut() }
    }
}
impl<T: Object + core::fmt::Debug> core::fmt::Debug for Owned<T> {
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        T::fmt(unsafe { self.0.as_ref() }, f)
    }
}
impl<T: Object> Owned<T> {
    #[inline(always)]
    pub const unsafe fn from_ptr_unchecked(ptr: *mut T) -> Self {
        Self(unsafe { core::ptr::NonNull::new_unchecked(ptr) })
    }

    #[inline(always)]
    pub const unsafe fn from_ptr(ptr: *mut T) -> Option<Self> {
        match core::ptr::NonNull::new(ptr) {
            Some(x) => Some(Self(x)),
            None => None,
        }
    }

    #[inline(always)]
    pub const fn as_ptr(&self) -> *mut T {
        self.0.as_ptr()
    }
}

#[repr(transparent)]
pub struct CFOwned<T>(NonNull<T>);
impl<T> Drop for CFOwned<T> {
    #[inline(always)]
    fn drop(&mut self) {
        unsafe {
            raw::CFRelease(self.0.as_ptr().cast());
        }
    }
}
impl<T> Deref for CFOwned<T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref() }
    }
}
impl<T> DerefMut for CFOwned<T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.as_mut() }
    }
}
impl<T> CFOwned<T> {
    #[inline(always)]
    pub const unsafe fn from_nonnull_ptr_unchecked(p: NonNull<T>) -> Self {
        Self(p)
    }

    #[inline(always)]
    pub const fn from_ptr_unchecked(p: *mut T) -> Self {
        unsafe { Self::from_nonnull_ptr_unchecked(NonNull::new_unchecked(p)) }
    }

    #[inline(always)]
    pub const fn from_ptr(p: *mut T) -> Option<Self> {
        match NonNull::new(p) {
            Some(x) => unsafe { Some(Self::from_nonnull_ptr_unchecked(x)) },
            None => None,
        }
    }

    #[inline(always)]
    pub const fn as_ptr(&self) -> *const T {
        self.0.as_ptr().cast_const()
    }

    #[inline(always)]
    pub const fn as_mut_ptr(&mut self) -> *mut T {
        self.0.as_ptr()
    }
}
