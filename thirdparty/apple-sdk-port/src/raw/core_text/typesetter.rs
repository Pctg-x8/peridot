use crate::{
    FFIOpaqueStruct,
    raw::{
        CFAttributedStringRef, CFDictionaryRef, CFIndex, CFRange, CFStringRef, CFTypeID, CTLineRef,
    },
};
use core::ffi::*;

#[repr(C)]
pub struct __CTTypesetter(FFIOpaqueStruct);
pub type CTTypesetterRef = *const __CTTypesetter;

unsafe extern "C" {
    pub fn CTTypesetterGetTypeID() -> CFTypeID;

    pub static kCTTypesetterOptionAllowUnboundedLayout: CFStringRef;
    pub static kCTTypesetterOptionForcedEmbeddingLevel: CFStringRef;

    pub fn CTTypesetterCreateWithAttributedString(string: CFAttributedStringRef)
    -> CTTypesetterRef;
    pub fn CTTypesetterCreateWithAttributedStringAndOptions(
        string: CFAttributedStringRef,
        options: CFDictionaryRef,
    ) -> CTTypesetterRef;
    pub fn CTTypesetterCreateLineWithOffset(
        typesetter: CTTypesetterRef,
        string_range: CFRange,
        offset: c_double,
    ) -> CTLineRef;
    pub fn CTTypesetterCreateLine(typesetter: CTTypesetterRef, string_range: CFRange) -> CTLineRef;
    pub fn CTTypesetterSuggestLineBreakWithOffset(
        typesetter: CTTypesetterRef,
        start_index: CFIndex,
        width: c_double,
        offset: c_double,
    ) -> CFIndex;
    pub fn CTTypesetterSuggestLineBreak(
        typesetter: CTTypesetterRef,
        start_index: CFIndex,
        width: c_double,
    ) -> CFIndex;
    pub fn CTTypesetterSuggestClusterBreakWithOffset(
        typesetter: CTTypesetterRef,
        start_index: CFIndex,
        width: c_double,
        offset: c_double,
    ) -> CFIndex;
    pub fn CTTypesetterSuggestClusterBreak(
        typesetter: CTTypesetterRef,
        start_index: CFIndex,
        width: c_double,
    ) -> CFIndex;
}
