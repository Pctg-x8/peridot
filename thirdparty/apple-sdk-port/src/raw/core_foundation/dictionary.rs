use core::ffi::*;

use crate::raw::Boolean;

use super::{CFAllocatorRef, CFHashCode, CFIndex, CFStringRef, CFTypeID};

pub type CFDictionaryRetainCallBack =
    extern "C" fn(allocator: CFAllocatorRef, value: *const c_void) -> *const c_void;
pub type CFDictionaryReleaseCallBack =
    extern "C" fn(allocator: CFAllocatorRef, value: *const c_void);
pub type CFDictionaryCopyDescriptionCallBack = extern "C" fn(value: *const c_void) -> CFStringRef;
pub type CFDictionaryEqualCallBack =
    extern "C" fn(value1: *const c_void, value2: *const c_void) -> Boolean;
pub type CFDictionaryHashCallBack = extern "C" fn(value: *const c_void) -> CFHashCode;
#[repr(C)]
pub struct CFDictionaryKeyCallBacks {
    pub version: CFIndex,
    pub retain: Option<CFDictionaryRetainCallBack>,
    pub release: Option<CFDictionaryReleaseCallBack>,
    pub copy_description: Option<CFDictionaryCopyDescriptionCallBack>,
    pub equal: Option<CFDictionaryEqualCallBack>,
    pub hash: Option<CFDictionaryHashCallBack>,
}

#[repr(C)]
pub struct CFDictionaryValueCallBacks {
    pub version: CFIndex,
    pub retain: Option<CFDictionaryRetainCallBack>,
    pub release: Option<CFDictionaryReleaseCallBack>,
    pub copy_description: Option<CFDictionaryCopyDescriptionCallBack>,
    pub equal: Option<CFDictionaryEqualCallBack>,
}

#[repr(C)]
pub struct __CFDictionary(
    [u8; 0],
    core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,
);
pub type CFDictionaryRef = *const __CFDictionary;
pub type CFMutableDictionaryRef = *mut __CFDictionary;

unsafe extern "C" {
    pub static kCFTypeDictionaryKeyCallBacks: CFDictionaryKeyCallBacks;
    pub static kCFCopyStringDictionaryKeyCallBacks: CFDictionaryKeyCallBacks;
    pub static kCFTypeDictionaryValueCallBacks: CFDictionaryValueCallBacks;

    pub fn CFDictionaryGetTypeID() -> CFTypeID;

    pub fn CFDictionaryCreate(
        allocator: CFAllocatorRef,
        keys: *mut *const c_void,
        values: *mut *const c_void,
        num_values: CFIndex,
        key_callbacks: *const CFDictionaryKeyCallBacks,
        value_callbacks: *const CFDictionaryValueCallBacks,
    ) -> CFDictionaryRef;
    pub fn CFDictionaryCreateCopy(
        allocator: CFAllocatorRef,
        dict: CFDictionaryRef,
    ) -> CFDictionaryRef;
    pub fn CFDictionaryCreateMutable(
        allocator: CFAllocatorRef,
        capacity: CFIndex,
        key_callbacks: *const CFDictionaryKeyCallBacks,
        value_callbacks: *const CFDictionaryValueCallBacks,
    ) -> CFMutableDictionaryRef;
    pub fn CFDictionaryCreateMutableCopy(
        allocator: CFAllocatorRef,
        capacity: CFIndex,
        dict: CFDictionaryRef,
    ) -> CFMutableDictionaryRef;

    pub fn CFDictionaryGetCount(dict: CFDictionaryRef) -> CFIndex;
    pub fn CFDictionaryGetCountOfKey(dict: CFDictionaryRef, key: *const c_void) -> CFIndex;
    pub fn CFDictionaryGetCountOfValue(dict: CFDictionaryRef, value: *const c_void) -> CFIndex;
    pub fn CFDictionaryContainsKey(dict: CFDictionaryRef, key: *const c_void) -> Boolean;
    pub fn CFDictionaryContainsValue(dict: CFDictionaryRef, value: *const c_void) -> Boolean;
    pub fn CFDictionaryGetValue(dict: CFDictionaryRef, key: *const c_void) -> *const c_void;
    pub fn CFDictionaryGetValueIfPresent(
        dict: CFDictionaryRef,
        key: *const c_void,
        value: *mut *const c_void,
    ) -> Boolean;
    pub fn CFDictionaryGetKeysAndValues(
        dict: CFDictionaryRef,
        keys: *mut *const c_void,
        values: *mut *const c_void,
    );
    pub fn CFDictionaryAddValue(
        dict: CFMutableDictionaryRef,
        key: *const c_void,
        value: *const c_void,
    );
    pub fn CFDictionarySetValue(
        dict: CFMutableDictionaryRef,
        key: *const c_void,
        value: *const c_void,
    );
    pub fn CFDictionaryReplaceValue(
        dict: CFMutableDictionaryRef,
        key: *const c_void,
        value: *const c_void,
    );
    pub fn CFDictionaryRemoveValue(dict: CFMutableDictionaryRef, key: *const c_void);
    pub fn CFDictionaryRemoveAllValues(dict: CFMutableDictionaryRef);
}
