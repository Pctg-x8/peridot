use objc::*;

use crate::NSUInteger;
use core::{
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

mod foundation;
pub use self::foundation::*;

pub trait ObjcObject {
    fn as_id(&self) -> *const objc::runtime::Object;
    fn as_id_mut(&mut self) -> *mut objc::runtime::Object;
}

pub trait NSCopying: ObjcObject {}

pub trait NSMutableCopying: ObjcObject {}

pub trait NSObject: ObjcObject {
    #[inline(always)]
    fn perform_selector(&self, sel: objc::runtime::Sel) -> *mut objc::runtime::Object {
        unsafe { msg_send![self.as_id(), performSelector: sel] }
    }

    #[inline(always)]
    fn perform_selector_with_object(
        &self,
        sel: objc::runtime::Sel,
        object: *mut objc::runtime::Object,
    ) -> *mut objc::runtime::Object {
        unsafe { msg_send![self.as_id(), performSelector: sel withObject: object] }
    }

    #[inline(always)]
    fn perform_selector_with_object2(
        &self,
        sel: objc::runtime::Sel,
        object1: *mut objc::runtime::Object,
        object2: *mut objc::runtime::Object,
    ) -> *mut objc::runtime::Object {
        unsafe {
            msg_send![self.as_id(), performSelector: sel withObject: object1 withObject: object2]
        }
    }

    #[inline(always)]
    fn conforms_to_protocol(&self, protocol: *mut objc::runtime::Protocol) -> objc::runtime::BOOL {
        unsafe { msg_send![self.as_id(), conformsToProtocol: protocol] }
    }

    #[inline(always)]
    fn retain(&self) -> *mut objc::runtime::Object {
        unsafe { msg_send![self.as_id(), retain] }
    }

    #[inline(always)]
    unsafe fn release(&self) {
        unsafe { msg_send![self.as_id(), release] }
    }

    #[inline(always)]
    fn autorelease(&self) -> *mut objc::runtime::Object {
        unsafe { msg_send![self.as_id(), autorelease] }
    }

    #[inline(always)]
    fn retain_count(&self) -> NSUInteger {
        unsafe { msg_send![self.as_id(), retainCount] }
    }
}

#[repr(C)]
pub struct NSObjectOwned<T: NSObject>(NonNull<T>);
impl<T: NSObject> Drop for NSObjectOwned<T> {
    #[inline(always)]
    fn drop(&mut self) {
        unsafe { self.0.as_ref().release() }
    }
}
impl<T: NSObject> Clone for NSObjectOwned<T> {
    #[inline(always)]
    fn clone(&self) -> Self {
        unsafe { self.0.as_ref().retain() };
        Self(self.0)
    }
}
impl<T: NSObject> Deref for NSObjectOwned<T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref() }
    }
}
impl<T: NSObject> DerefMut for NSObjectOwned<T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.as_mut() }
    }
}
impl<T: NSObject> NSObjectOwned<T> {
    #[inline(always)]
    pub unsafe fn from_ptr_unretained_unchecked(p: *mut T) -> Self {
        Self(unsafe { NonNull::new_unchecked(p) })
    }

    #[inline(always)]
    pub unsafe fn from_id_unretained_unchecked(p: *mut objc::runtime::Object) -> Self {
        Self(unsafe { NonNull::new_unchecked(p.cast()) })
    }
}
