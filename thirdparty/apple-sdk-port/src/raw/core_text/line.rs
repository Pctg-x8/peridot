use crate::{
    FFIOpaqueStruct,
    raw::{
        CFArrayRef, CFAttributedStringRef, CFIndex, CFOptionFlags, CFRange, CFTypeID, CGFloat,
        CGPoint, CGRect,
    },
};
use core::ffi::*;

#[repr(C)]
pub struct __CTLine(FFIOpaqueStruct);
pub type CTLineRef = *const __CTLine;

pub type CTLineBoundsOptions = CFOptionFlags;
pub const kCTLineBoundsExcludeTypographicLeading: CTLineBoundsOptions = 1 << 0;
pub const kCTLineBoundsExcludeTypographicShifts: CTLineBoundsOptions = 1 << 1;
pub const kCTLineBoundsUseHangingPunctuation: CTLineBoundsOptions = 1 << 2;
pub const kCTLineBoundsUseGlyphPathBounds: CTLineBoundsOptions = 1 << 3;
pub const kCTLineBoundsUseOpticalBounds: CTLineBoundsOptions = 1 << 4;
pub const kCTLineBoundsIncludeLanguageExtents: CTLineBoundsOptions = 1 << 5;

pub type CTLineTruncationType = u32;
pub const kCTLineTruncationStart: CTLineTruncationType = 0;
pub const kCTLineTruncationEnd: CTLineTruncationType = 1;
pub const kCTLineTruncationMiddle: CTLineTruncationType = 2;

unsafe extern "C" {
    pub fn CTLineGetTypeID() -> CFTypeID;

    pub fn CTLineCreateWithAttributedString(attr_string: CFAttributedStringRef) -> CTLineRef;
    pub fn CTLineCreateTruncatedLine(
        line: CTLineRef,
        width: c_double,
        truncation_type: CTLineTruncationType,
        truncation_token: CTLineRef,
    ) -> CTLineRef;
    pub fn CTLineCreateJustifiedLine(
        line: CTLineRef,
        justification_factor: CGFloat,
        justification_width: c_double,
    ) -> CTLineRef;
    pub fn CTLineGetGlyphCount(line: CTLineRef) -> CFIndex;
    pub fn CTLineGetGlyphRuns(line: CTLineRef) -> CFArrayRef;
    pub fn CTLineGetStringRange(line: CTLineRef) -> CFRange;
    pub fn CTLineGetPenOffsetForFlush(
        line: CTLineRef,
        flush_factor: CGFloat,
        flush_width: c_double,
    ) -> c_double;
    pub fn CTLineGetTypographicBounds(
        line: CTLineRef,
        ascent: *mut CGFloat,
        descent: *mut CGFloat,
        leading: *mut CGFloat,
    ) -> c_double;
    pub fn CTLineGetBoundsWithOptions(line: CTLineRef, options: CTLineBoundsOptions) -> CGRect;
    pub fn CTLineGetTrailingWhitespaceWidth(line: CTLineRef) -> c_double;
    pub fn CTLineGetStringIndexForPosition(line: CTLineRef, position: CGPoint) -> CFIndex;
    pub fn CTLineGetOffsetForStringIndex(
        line: CTLineRef,
        char_index: CFIndex,
        secondary_offset: *mut CGFloat,
    ) -> CGFloat;
}
