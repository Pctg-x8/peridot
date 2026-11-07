#![allow(non_upper_case_globals)]

use crate::{
    FFIOpaqueStruct,
    raw::{CFArrayRef, CFDictionaryRef, CFRange, CFStringRef, CFTypeID, CGPathRef, CGPoint},
};

#[repr(C)]
pub struct __CTFrame(FFIOpaqueStruct);
pub type CTFrameRef = *const __CTFrame;

pub type CTFrameProgression = u32;
pub const kCTFrameProgressionTopToBottom: CTFrameProgression = 0;
pub const kCTFrameProgressionRightToLeft: CTFrameProgression = 1;
pub const kCTFrameProgressionLeftToRight: CTFrameProgression = 2;

pub type CTFramePathFillRule = u32;
pub const kCTFramePathFillEvenOdd: CTFramePathFillRule = 0;
pub const kCTFramePathFillWindingNumber: CTFramePathFillRule = 1;

unsafe extern "C" {
    pub fn CTFrameGetTypeID() -> CFTypeID;

    pub static kCTFrameProgressionAttributeName: CFStringRef;
    pub static kCTFramePathFillRuleAttributeName: CFStringRef;
    pub static kCTFramePathWidthAttributeName: CFStringRef;
    pub static kCTFrameClippingPathsAttributeName: CFStringRef;
    pub static kCTFramePathClippingPathAttributeName: CFStringRef;

    pub fn CTFrameGetStringRange(frame: CTFrameRef) -> CFRange;
    pub fn CTFrameGetVisibleStringRange(frame: CTFrameRef) -> CFRange;
    pub fn CTFrameGetPath(frame: CTFrameRef) -> CGPathRef;
    pub fn CTFrameGetFrameAttributes(frame: CTFrameRef) -> CFDictionaryRef;
    pub fn CTFrameGetLines(frame: CTFrameRef) -> CFArrayRef;
    pub fn CTFrameGetLineOrigins(frame: CTFrameRef, range: CFRange, origins: *mut CGPoint);
}
