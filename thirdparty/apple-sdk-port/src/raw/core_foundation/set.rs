use crate::FFIOpaqueStruct;

use super::{Boolean, CFAllocatorRef, CFHashCode, CFIndex, CFStringRef, CFTypeID};
use core::ffi::*;

pub type CFSetRetainCallBack =
    extern "C" fn(allocator: CFAllocatorRef, value: *const c_void) -> *const c_void;
pub type CFSetReleaseCallBack = extern "C" fn(allocator: CFAllocatorRef, value: *const c_void);
pub type CFSetCopyDescriptionCallBack = extern "C" fn(value: *const c_void) -> CFStringRef;
pub type CFSetEqualCallBack =
    extern "C" fn(value1: *const c_void, value2: *const c_void) -> Boolean;
pub type CFSetHashCallBack = extern "C" fn(value: *const c_void) -> CFHashCode;

#[repr(C)]
pub struct CFSetCallBacks {
    pub version: CFIndex,
    pub retain: Option<CFSetRetainCallBack>,
    pub release: Option<CFSetReleaseCallBack>,
    pub copy_description: Option<CFSetCopyDescriptionCallBack>,
    pub equal: Option<CFSetEqualCallBack>,
    pub hash: Option<CFSetHashCallBack>,
}

#[repr(C)]
pub struct __CFSet(FFIOpaqueStruct);
pub type CFSetRef = *const __CFSet;
pub type CFMutableSetRef = *mut __CFSet;

unsafe extern "C" {
    pub static kCFTypeSetCallBacks: CFSetCallBacks;
    pub static kCFCopyStringSetCallBacks: CFSetCallBacks;

    pub fn CFSetGetTypeID() -> CFTypeID;

    pub fn CFSetCreate(
        allocator: CFAllocatorRef,
        values: *mut *const c_void,
        num_values: CFIndex,
        callbacks: *const CFSetCallBacks,
    ) -> CFSetRef;
    pub fn CFSetCreateCopy(allocator: CFAllocatorRef, set: CFSetRef) -> CFSetRef;
    pub fn CFSetCreateMutable(
        allocator: CFAllocatorRef,
        capacity: CFIndex,
        callbacks: *const CFSetCallBacks,
    ) -> CFMutableSetRef;
    pub fn CFSetCreateMutableCopy(
        allocator: CFAllocatorRef,
        capacity: CFIndex,
        set: CFSetRef,
    ) -> CFMutableSetRef;
    pub fn CFSetGetCount(set: CFSetRef) -> CFIndex;
    pub fn CFSetGetCountOfValue(set: CFSetRef, value: *const c_void) -> CFIndex;
    pub fn CFSetContainsValue(set: CFSetRef, value: *const c_void) -> Boolean;
    pub fn CFSetGetValue(set: CFSetRef, value: *const c_void) -> *const c_void;
    pub fn CFSetGetValueIfPresent(
        set: CFSetRef,
        candidate: *const c_void,
        value: *mut *const c_void,
    ) -> Boolean;
    pub fn CFSetGetValues(set: CFSetRef, values: *mut *const c_void);
    pub fn CFSetAddValue(set: CFMutableSetRef, value: *const c_void);
    pub fn CFSetReplaceValue(set: CFMutableSetRef, value: *const c_void);
    pub fn CFSetSetValue(set: CFMutableSetRef, value: *const c_void);
    pub fn CFSetRemoveValue(set: CFMutableSetRef, value: *const c_void);
    pub fn CFSetRemoveAllValues(set: CFMutableSetRef);
}
