use core::ffi::*;

use crate::{
    FFIOpaqueStruct,
    raw::{CFDictionaryRef, CFIndex, CFRange, CFTypeID, CGFloat, CGGlyph, CGPoint, CGSize},
};

#[repr(C)]
pub struct __CTRun(FFIOpaqueStruct);
pub type CTRunRef = *const __CTRun;

pub type CTRunStatus = u32;
pub const kCTRunStatusNoStatus: CTRunStatus = 0;
pub const kCTRunStatusRightToLeft: CTRunStatus = 1 << 0;
pub const kCTRunStatusNonMonotonic: CTRunStatus = 1 << 1;
pub const kCTRunStatusHasNonIdentityMatrix: CTRunStatus = 1 << 2;

unsafe extern "C" {
    pub fn CTRunGetTypeID() -> CFTypeID;

    pub fn CTRunGetGlyphCount(run: CTRunRef) -> CFIndex;
    pub fn CTRunGetAttributes(run: CTRunRef) -> CFDictionaryRef;
    pub fn CTRunGetStatus(run: CTRunRef) -> CTRunStatus;
    pub fn CTRunGetGlyphsPtr(run: CTRunRef) -> *const CGGlyph;
    pub fn CTRunGetPositionsPtr(run: CTRunRef) -> *const CGPoint;
    pub fn CTRunGetAdvancesPtr(run: CTRunRef) -> *const CGSize;
    pub fn CTRunGetStringIndicesPtr(run: CTRunRef) -> *const CFIndex;
    pub fn CTRunGetTypographicBounds(
        run: CTRunRef,
        range: CFRange,
        ascent: *mut CGFloat,
        descent: *mut CGFloat,
        leading: *mut CGFloat,
    ) -> c_double;
}
