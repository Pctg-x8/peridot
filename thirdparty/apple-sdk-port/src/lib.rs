use core::ops::{Deref, DerefMut};
use core::ptr::NonNull;

use objc::{runtime::Object, *};

#[repr(C)]
pub(crate) struct FFIOpaqueStruct(
    [u8; 0],
    core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,
);

pub mod core_foundation;
pub use self::core_foundation::*;

pub mod core_graphics;
pub use self::core_graphics::*;

pub mod core_text;
pub use self::core_text::*;

pub mod foundation;
pub use self::foundation::*;

#[cfg(target_pointer_width = "64")]
pub type NSInteger = core::ffi::c_long;
#[cfg(not(target_pointer_width = "64"))]
pub type NSInteger = core::ffi::c_int;

#[cfg(target_pointer_width = "64")]
pub type NSUInteger = core::ffi::c_ulong;
#[cfg(not(target_pointer_width = "64"))]
pub type NSUInteger = core::ffi::c_uint;

#[repr(transparent)]
pub struct CFOwned<T>(NonNull<T>);
impl<T> Drop for CFOwned<T> {
    #[inline(always)]
    fn drop(&mut self) {
        unsafe {
            CFRelease(self.0.as_ptr().cast());
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

#[repr(transparent)]
pub struct Owned<T: NSObject>(NonNull<T>);
impl<T: NSObject> Drop for Owned<T> {
    #[inline(always)]
    fn drop(&mut self) {
        unsafe {
            self.0.as_ref().release();
        }
    }
}
impl<T: NSObject> Deref for Owned<T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref() }
    }
}
impl<T: NSObject> DerefMut for Owned<T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.as_mut() }
    }
}
impl<T: NSObject> Owned<T> {
    #[inline(always)]
    pub(crate) const unsafe fn from_nonnull_id_unchecked(p: NonNull<Object>) -> Self {
        Self(p.cast())
    }

    #[inline(always)]
    pub(crate) const unsafe fn from_id_unchecked(p: *mut Object) -> Self {
        unsafe { Self::from_nonnull_id_unchecked(core::ptr::NonNull::new_unchecked(p.cast())) }
    }

    #[inline(always)]
    pub(crate) const unsafe fn from_typed_id_unchecked(p: *mut T) -> Self {
        unsafe { Self(core::ptr::NonNull::new_unchecked(p)) }
    }
}

pub unsafe trait ObjcObject {
    fn as_id(&self) -> *mut Object;
    fn as_id_mut(&mut self) -> *mut Object;
}

pub unsafe trait NSObject: ObjcObject {
    #[inline(always)]
    fn retain(&self) -> &Self {
        let _: *mut Object = unsafe { msg_send![self.as_id(), retain] };
        self
    }

    #[inline(always)]
    fn release(&self) {
        unsafe { msg_send![self.as_id(), release] }
    }

    #[inline(always)]
    fn retain_count(&self) -> NSUInteger {
        unsafe { msg_send![self.as_id(), retainCount] }
    }
}

pub unsafe trait NSCopying: ObjcObject {
    #[inline(always)]
    fn copy_with_zone(&self, zone: *mut Object) -> *mut Object {
        unsafe { msg_send![self.as_id(), copyWithZone: zone] }
    }
}

pub unsafe trait NSMutableCopying: ObjcObject {
    #[inline(always)]
    fn mutable_copy_with_zone(&self, zone: *mut Object) -> *mut Object {
        unsafe { msg_send![self.as_id(), mutableCopyWithZone: zone] }
    }
}
