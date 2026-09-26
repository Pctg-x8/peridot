use crate::raw::Boolean;

use super::{CFAllocatorRef, CFComparisonResult, CFIndex, CFTypeID};
use core::ffi::*;

#[repr(C)]
pub struct __CFBoolean(
    [u8; 0],
    core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,
);
pub type CFBooleanRef = *const __CFBoolean;

pub type CFNumberType = CFIndex;
pub const kCFNumberSInt8Type: CFNumberType = 1;
pub const kCFNumberSInt16Type: CFNumberType = 2;
pub const kCFNumberSInt32Type: CFNumberType = 3;
pub const kCFNumberSInt64Type: CFNumberType = 4;
pub const kCFNumberFloat32Type: CFNumberType = 5;
pub const kCFNumberFloat64Type: CFNumberType = 6;
pub const kCFNumberCharType: CFNumberType = 7;
pub const kCFNumberShortType: CFNumberType = 8;
pub const kCFNumberIntType: CFNumberType = 9;
pub const kCFNumberLongType: CFNumberType = 10;
pub const kCFNumberLongLongType: CFNumberType = 11;
pub const kCFNumberFloatType: CFNumberType = 12;
pub const kCFNumberDoubleType: CFNumberType = 13;
pub const kCFNumberCFIndexType: CFNumberType = 14;
pub const kCFNumberNSIntegerType: CFNumberType = 15;
pub const kCFNumberCGFloatType: CFNumberType = 16;
pub const kCFNumberMaxType: CFNumberType = 16;

#[repr(C)]
pub struct __CFNumber(
    [u8; 0],
    core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,
);
pub type CFNumberRef = *const __CFNumber;

unsafe extern "C" {
    pub static kCFBooleanTrue: CFBooleanRef;
    pub static kCFBooleanFalse: CFBooleanRef;

    pub fn CFBooleanGetTypeID() -> CFTypeID;
    pub fn CFBooleanGetValue(boolean: CFBooleanRef) -> Boolean;

    pub static kCFNumberPositiveInfinity: CFNumberRef;
    pub static kCFNumberNegativeInfinity: CFNumberRef;
    pub static kCFNumberNaN: CFNumberRef;

    pub fn CFNumberGetTypeID() -> CFTypeID;

    pub fn CFNumberCreate(
        allocator: CFAllocatorRef,
        r#type: CFNumberType,
        value_ptr: *const c_void,
    ) -> CFNumberRef;
    pub fn CFNumberGetType(number: CFNumberRef) -> CFNumberType;
    pub fn CFNumberGetByteSize(number: CFNumberRef) -> CFIndex;
    pub fn CFNumberIsFloatType(number: CFNumberRef) -> Boolean;
    pub fn CFNumberGetValue(
        number: CFNumberRef,
        r#type: CFNumberType,
        value_ptr: *mut c_void,
    ) -> Boolean;
    pub fn CFNumberCompare(
        number: CFNumberRef,
        other: CFNumberRef,
        context: *mut c_void,
    ) -> CFComparisonResult;
}
