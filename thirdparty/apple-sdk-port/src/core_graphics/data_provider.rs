use crate::{CFDataRef, CFTypeID, FFIOpaqueStruct};
use core::ffi::*;
use std::os::unix::raw::off_t;

#[repr(C)]
pub struct CGDataProvider(FFIOpaqueStruct);
pub type CGDataProviderRef = *mut CGDataProvider;

pub type CGDataProviderGetBytesCallback =
    extern "C" fn(info: *mut c_void, buffer: *mut c_void, count: usize) -> usize;
pub type CGDataProviderSkipForwardCallback =
    extern "C" fn(info: *mut c_void, count: off_t) -> off_t;
pub type CGDataProviderRewindCallback = extern "C" fn(info: *mut c_void);
pub type CGDataProviderReleaseInfoCallback = extern "C" fn(info: *mut c_void);

#[repr(C)]
pub struct CGDataProviderSequentialCallbacks {
    pub version: c_uint,
    pub get_bytes: Option<CGDataProviderGetBytesCallback>,
    pub skip_forward: Option<CGDataProviderSkipForwardCallback>,
    pub rewind: Option<CGDataProviderRewindCallback>,
    pub release_info: Option<CGDataProviderReleaseInfoCallback>,
}

pub type CGDataProviderGetBytePointerCallback = extern "C" fn(info: *mut c_void) -> *const c_void;
pub type CGDataProviderReleaseBytePointerCallback =
    extern "C" fn(info: *mut c_void, pointer: *const c_void);
pub type CGDataProviderGetBytesAtPositionCallback =
    extern "C" fn(info: *mut c_void, buffer: *mut c_void, pos: off_t, cnt: usize) -> usize;

#[repr(C)]
pub struct CGDataProviderDirectCallbacks {
    pub version: c_uint,
    pub get_byte_pointer: Option<CGDataProviderGetBytePointerCallback>,
    pub release_byte_pointer: Option<CGDataProviderReleaseBytePointerCallback>,
    pub get_bytes_at_position: Option<CGDataProviderGetBytesAtPositionCallback>,
    pub release_info: Option<CGDataProviderReleaseInfoCallback>,
}

pub type CGDataProviderReleaseDataCallback =
    extern "C" fn(info: *mut c_void, data: *const c_void, size: usize);

unsafe extern "C" {
    pub fn CGDataProviderGetTypeID() -> CFTypeID;
    pub fn CGDataProviderCreateSequential(
        info: *mut c_void,
        callbacks: *const CGDataProviderSequentialCallbacks,
    ) -> CGDataProviderRef;
    pub fn CGDataProviderCreateDirect(
        info: *mut c_void,
        size: off_t,
        callbacks: *const CGDataProviderDirectCallbacks,
    ) -> CGDataProviderRef;
    pub fn CGDataProviderCreateWithData(
        info: *mut c_void,
        data: *const c_void,
        size: usize,
        release_data: Option<CGDataProviderReleaseDataCallback>,
    ) -> CGDataProviderRef;
    pub fn CGDataProviderCreateWithCFData(data: CFDataRef) -> CGDataProviderRef;
    pub fn CGDataProviderCreateWithFilename(filename: *const c_char) -> CGDataProviderRef;
    pub fn CGDataProviderRetain(provider: CGDataProviderRef) -> CGDataProviderRef;
    pub fn CGDataProviderRelease(provider: CGDataProviderRef);
    pub fn CGDataProviderCopyData(provider: CGDataProviderRef) -> CFDataRef;
    pub fn CGDataProviderGetInfo(provider: CGDataProviderRef) -> *mut c_void;
}
