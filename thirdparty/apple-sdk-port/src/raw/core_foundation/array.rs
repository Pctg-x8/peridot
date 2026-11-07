use crate::raw::Boolean;

use super::{CFAllocatorRef, CFIndex, CFRange, CFStringRef, CFTypeID};
use core::ffi::*;

pub type CFArrayRetainCallBack =
    extern "C" fn(allocator: CFAllocatorRef, value: *const c_void) -> *const c_void;
pub type CFArrayReleaseCallBack = extern "C" fn(allocator: CFAllocatorRef, value: *const c_void);
pub type CFArrayCopyDescriptionCallBack = extern "C" fn(value: *const c_void) -> CFStringRef;
pub type CFArrayEqualCallBack =
    extern "C" fn(value1: *const c_void, value2: *const c_void) -> Boolean;
#[repr(C)]
pub struct CFArrayCallBacks {
    pub version: CFIndex,
    pub retain: Option<CFArrayRetainCallBack>,
    pub release: Option<CFArrayReleaseCallBack>,
    pub copy_description: Option<CFArrayCopyDescriptionCallBack>,
    pub equal: Option<CFArrayEqualCallBack>,
}

#[repr(C)]
pub struct __CFArray(
    [u8; 0],
    core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,
);
pub type CFArrayRef = *const __CFArray;
pub type CFMutableArrayRef = *mut __CFArray;

unsafe extern "C" {
    pub static kCFTypeArrayCallBacks: CFArrayCallBacks;

    pub fn CFArrayGetTypeID() -> CFTypeID;

    pub fn CFArrayCreate(
        allocator: CFAllocatorRef,
        values: *mut *const c_void,
        num_values: CFIndex,
        callbacks: *const CFArrayCallBacks,
    ) -> CFArrayRef;
    pub fn CFArrayCreateCopy(allocator: CFAllocatorRef, array: CFArrayRef) -> CFArrayRef;
    pub fn CFArrayCreateMutable(
        allocator: CFAllocatorRef,
        capacity: CFIndex,
        callbacks: *const CFArrayCallBacks,
    ) -> CFMutableArrayRef;
    pub fn CFArrayCreateMutableCopy(
        allocator: CFAllocatorRef,
        capacity: CFIndex,
        array: CFArrayRef,
    ) -> CFMutableArrayRef;

    pub fn CFArrayGetCount(array: CFArrayRef) -> CFIndex;
    pub fn CFArrayGetCountOfValue(
        array: CFArrayRef,
        range: CFRange,
        value: *const c_void,
    ) -> CFIndex;
    pub fn CFArrayContainsValue(array: CFArrayRef, range: CFRange, value: *const c_void)
    -> Boolean;
    pub fn CFArrayGetValueAtIndex(array: CFArrayRef, index: CFIndex) -> *const c_void;
    pub fn CFArrayGetValues(array: CFArrayRef, range: CFRange, values: *mut *const c_void);
    pub fn CFArrayGetFirstIndexOfValue(
        array: CFArrayRef,
        range: CFRange,
        value: *const c_void,
    ) -> CFIndex;
    pub fn CFArrayGetLastIndexOfValue(
        array: CFArrayRef,
        range: CFRange,
        value: *const c_void,
    ) -> CFIndex;

    pub fn CFArrayAppendValue(array: CFMutableArrayRef, value: *const c_void);
    pub fn CFArrayInsertValueAtIndex(
        array: CFMutableArrayRef,
        index: CFIndex,
        value: *const c_void,
    );
    pub fn CFArraySetValueAtIndex(array: CFMutableArrayRef, index: CFIndex, value: *const c_void);
    pub fn CFArrayRemoveValueAtIndex(array: CFMutableArrayRef, index: CFIndex);
    pub fn CFArrayRemoveAllValues(array: CFMutableArrayRef);
    pub fn CFArrayReplaceValues(
        array: CFMutableArrayRef,
        range: CFRange,
        values: *mut *const c_void,
        new_count: CFIndex,
    );
    pub fn CFArrayExchangeValuesAtIndices(array: CFMutableArrayRef, idx1: CFIndex, idx2: CFIndex);
    pub fn CFArrayAppendArray(array: CFMutableArrayRef, other: CFArrayRef, other_range: CFRange);
}
