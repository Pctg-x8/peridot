#![allow(non_upper_case_globals)]

use core::ffi::*;

pub type CFAllocatorTypeID = c_ulonglong;

#[repr(C)]
#[allow(non_camel_case_types)]
pub struct _malloc_zone_t(
    [u8; 0],
    core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,
);

pub type UniChar = u16;

#[cfg(target_pointer_width = "64")]
pub type CFTypeID = c_ulonglong;
#[cfg(not(target_pointer_width = "64"))]
pub type CFTypeID = c_ulong;

#[cfg(target_pointer_width = "64")]
pub type CFOptionFlags = c_ulonglong;
#[cfg(not(target_pointer_width = "64"))]
pub type CFOptionFlags = c_ulong;

#[cfg(target_pointer_width = "64")]
pub type CFHashCode = c_ulonglong;
#[cfg(not(target_pointer_width = "64"))]
pub type CFHashCode = c_ulong;

#[cfg(target_pointer_width = "64")]
pub type CFIndex = c_longlong;
#[cfg(not(target_pointer_width = "64"))]
pub type CFIndex = c_long;

pub type CFTypeRef = *const c_void;
pub type CFPropertyListRef = CFTypeRef;

pub type CFComparisonResult = CFIndex;
pub const kCFCompareLessThan: CFComparisonResult = -1;
pub const kCFCompareEqualTo: CFComparisonResult = 0;
pub const kCFCompareGreaterThan: CFComparisonResult = 1;

pub type CFComparatorFunction = extern "C" fn(
    val1: *const c_void,
    val2: *const c_void,
    context: *mut c_void,
) -> CFComparisonResult;

pub const kCFNotFound: CFIndex = -1;

#[repr(C)]
pub struct CFRange {
    pub location: CFIndex,
    pub length: CFIndex,
}

#[repr(C)]
pub struct __CFNull(
    [u8; 0],
    core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,
);
pub type CFNullRef = *const __CFNull;

#[repr(C)]
pub struct __CFAllocator(
    [u8; 0],
    core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,
);
pub type CFAllocatorRef = *const __CFAllocator;

pub type CFAllocatorRetainCallBack = extern "C" fn(info: *const c_void) -> *const c_void;
pub type CFAllocatorReleaseCallBack = extern "C" fn(info: *const c_void);
pub type CFAllocatorCopyDescriptionCallBack = extern "C" fn(info: *const c_void) -> CFStringRef;
pub type CFAllocatorAllocateCallBack =
    extern "C" fn(alloc_size: CFIndex, hint: CFOptionFlags, info: *mut c_void) -> *mut c_void;
pub type CFAllocatorReallocateCallBack = extern "C" fn(
    ptr: *mut c_void,
    new_size: CFIndex,
    hint: CFOptionFlags,
    info: *mut c_void,
) -> *mut c_void;
pub type CFAllocatorDeallocateCallBack = extern "C" fn(ptr: *mut c_void, info: *mut c_void);
pub type CFAllocatorPreferredSizeCallBack =
    extern "C" fn(size: CFIndex, hint: CFOptionFlags, info: *mut c_void) -> CFIndex;

#[repr(C)]
pub struct CFAllocatorContext {
    pub version: CFIndex,
    pub info: *mut c_void,
    pub retain: Option<CFAllocatorRetainCallBack>,
    pub release: Option<CFAllocatorReleaseCallBack>,
    pub copy_description: Option<CFAllocatorCopyDescriptionCallBack>,
    pub allocate: Option<CFAllocatorAllocateCallBack>,
    pub reallocate: Option<CFAllocatorReallocateCallBack>,
    pub deallocate: Option<CFAllocatorDeallocateCallBack>,
    pub preferred_size: Option<CFAllocatorPreferredSizeCallBack>,
}

unsafe extern "C" {
    pub fn CFNullGetTypeID() -> CFTypeID;
    pub static kCFNull: CFNullRef;

    pub static kCFAllocatorDefault: CFAllocatorRef;
    pub static kCFAllocatorSystemDefault: CFAllocatorRef;
    pub static kCFAllocatorMalloc: CFAllocatorRef;
    pub static kCFAllocatorMallocZone: CFAllocatorRef;
    pub static kCFAllocatorNull: CFAllocatorRef;
    pub static kCFAllocatorUseContext: CFAllocatorRef;

    pub fn CFAllocatorGetTypeID() -> CFTypeID;
    pub fn CFAllocatorSetDefault(allocator: CFAllocatorRef);
    pub fn CFAllocatorGetDefault() -> CFAllocatorRef;
    pub fn CFAllocatorCreate(
        allocator: CFAllocatorRef,
        context: *mut CFAllocatorContext,
    ) -> CFAllocatorRef;
    pub fn CFAllocatorAllocateTyped(
        allocator: CFAllocatorRef,
        size: CFIndex,
        descriptor: CFAllocatorTypeID,
        hint: CFOptionFlags,
    ) -> *mut c_void;
    pub fn CFAllocatorReallocateTyped(
        allocator: CFAllocatorRef,
        ptr: *mut c_void,
        new_size: CFIndex,
        descriptor: CFAllocatorTypeID,
        hint: CFOptionFlags,
    ) -> *mut c_void;
    pub fn CFAllocatorAllocateBytes(
        allocator: CFAllocatorRef,
        size: CFIndex,
        hint: CFOptionFlags,
    ) -> *mut c_void;
    pub fn CFAllocatorReallocateBytes(
        allocator: CFAllocatorRef,
        ptr: *mut c_void,
        new_size: CFIndex,
        hint: CFOptionFlags,
    ) -> *mut c_void;
    pub fn CFAllocatorAllocate(
        allocator: CFAllocatorRef,
        size: CFIndex,
        hint: CFOptionFlags,
    ) -> *mut c_void;
    pub fn CFAllocatorReallocate(
        allocator: CFAllocatorRef,
        ptr: *mut c_void,
        new_size: CFIndex,
        hint: CFOptionFlags,
    ) -> *mut c_void;
    pub fn CFAllocatorDeallocate(allocator: CFAllocatorRef, ptr: *mut c_void);
    pub fn CFAllocatorGetPreferredSizeForSize(
        allocator: CFAllocatorRef,
        size: CFIndex,
        hint: CFOptionFlags,
    ) -> CFIndex;
    pub fn CFAllocatorGetContext(allocator: CFAllocatorRef, context: *mut CFAllocatorContext);

    pub fn CFGetTypeID(cf: CFTypeRef) -> CFTypeID;
    pub fn CFRetain(cf: CFTypeRef) -> CFTypeRef;
    pub fn CFRelease(cf: CFTypeRef);
    pub fn CFGetRetainCount(cf: CFTypeRef) -> CFIndex;
    pub fn CFEqual(cf1: CFTypeRef, cf2: CFTypeRef) -> Boolean;
    pub fn CFHash(cf: CFTypeRef) -> CFHashCode;
}

mod data;
use crate::raw::Boolean;

pub use self::data::*;

mod number;
pub use self::number::*;

mod string;
pub use self::string::*;

mod array;
pub use self::array::*;

mod dictionary;
pub use self::dictionary::*;

mod set;
pub use self::set::*;

mod attributed_string;
pub use self::attributed_string::*;

mod character_set;
pub use self::character_set::*;
