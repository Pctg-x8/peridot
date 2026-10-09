use crate::{
    FFIOpaqueStruct,
    raw::{
        CFAttributedStringRef, CFDictionaryRef, CFRange, CFTypeID, CGPathRef, CGSize, CTFrameRef,
        CTTypesetterRef,
    },
};

#[repr(C)]
pub struct __CTFramesetter(FFIOpaqueStruct);
pub type CTFramesetterRef = *const __CTFramesetter;

unsafe extern "C" {
    pub fn CTramesetterGetTypeID() -> CFTypeID;

    pub fn CTFramesetterCreateWithTypesetter(typesetter: CTTypesetterRef) -> CTFramesetterRef;
    pub fn CTFramesetterCreateWithAttributedString(
        attr_string: CFAttributedStringRef,
    ) -> CTFramesetterRef;
    pub fn CTFramesetterCreateFrame(
        framesetter: CTFramesetterRef,
        string_range: CFRange,
        path: CGPathRef,
        frame_attributes: CFDictionaryRef,
    ) -> CTFrameRef;
    pub fn CTFramesetterGetTypesetter(framesetter: CTFramesetterRef) -> CTTypesetterRef;
    pub fn CTFramesetterSuggestFrameSizeWithConstraints(
        framesetter: CTFramesetterRef,
        string_range: CFRange,
        frame_attributes: CFDictionaryRef,
        constraints: CGSize,
        fit_range: *mut CFRange,
    ) -> CGSize;
}
