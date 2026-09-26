use crate::raw::Boolean;

use super::{
    CFAllocatorRef, CFDictionaryRef, CFIndex, CFMutableStringRef, CFRange, CFStringRef, CFTypeID,
    CFTypeRef,
};

#[repr(C)]
pub struct __CFAttributedString(
    [u8; 0],
    core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,
);
pub type CFAttributedStringRef = *const __CFAttributedString;
pub type CFMutableAttributedStringRef = *mut __CFAttributedString;

unsafe extern "C" {
    pub fn CFAttributedStringGetTypeID() -> CFTypeID;

    pub fn CFAttributedStringCreate(
        alloc: CFAllocatorRef,
        atr: CFStringRef,
        attributes: CFDictionaryRef,
    ) -> CFAttributedStringRef;
    pub fn CFAttributedStringCreateWithSubstring(
        alloc: CFAllocatorRef,
        astr: CFAttributedStringRef,
        range: CFRange,
    ) -> CFAttributedStringRef;
    pub fn CFAttributedStringCreateCopy(
        alloc: CFAllocatorRef,
        astr: CFAttributedStringRef,
    ) -> CFAttributedStringRef;
    pub fn CFAttributedStringGetString(astr: CFAttributedStringRef) -> CFStringRef;
    pub fn CFAttributedStringGetLength(astr: CFAttributedStringRef) -> CFIndex;
    pub fn CFAttributedStringGetAttributes(
        astr: CFAttributedStringRef,
        len: CFIndex,
        effective_range: *mut CFRange,
    ) -> CFDictionaryRef;
    pub fn CFAttributedStringGetAttribute(
        astr: CFAttributedStringRef,
        loc: CFIndex,
        attr_name: CFStringRef,
        effective_range: *mut CFRange,
    ) -> CFTypeRef;
    pub fn CFAttributedStringGetAttributesAndLongestEffectiveRange(
        astr: CFAttributedStringRef,
        loc: CFIndex,
        in_range: CFRange,
        longest_effective_range: *mut CFRange,
    ) -> CFDictionaryRef;
    pub fn CFAttributedStringGetAttributeAndLongestEffectiveRange(
        astr: CFAttributedStringRef,
        loc: CFIndex,
        attr_name: CFStringRef,
        in_range: CFRange,
        longest_effective_range: *mut CFRange,
    ) -> CFTypeRef;

    pub fn CFAttributedStringCreateMutableCopy(
        alloc: CFAllocatorRef,
        max_length: CFIndex,
        astr: CFAttributedStringRef,
    ) -> CFMutableAttributedStringRef;
    pub fn CFAttributedStringCreateMutable(
        alloc: CFAllocatorRef,
        max_length: CFIndex,
    ) -> CFMutableAttributedStringRef;
    pub fn CFAttributedStringReplaceString(
        astr: CFMutableAttributedStringRef,
        range: CFRange,
        replacement: CFStringRef,
    );
    pub fn CFAttributedStringGetMutableString(
        astr: CFMutableAttributedStringRef,
    ) -> CFMutableStringRef;
    pub fn CFAttributedStringSetAttributes(
        astr: CFMutableAttributedStringRef,
        range: CFRange,
        replacement: CFDictionaryRef,
        clear_other_attributes: Boolean,
    );
    pub fn CFAttributedStringSetAttribute(
        astr: CFMutableAttributedStringRef,
        range: CFRange,
        attr_name: CFStringRef,
        value: CFTypeRef,
    );
    pub fn CFAttributedStringRemoveAttribute(
        astr: CFMutableAttributedStringRef,
        range: CFRange,
        attr_name: CFStringRef,
    );
    pub fn CFAttributedStringReplaceAttributedString(
        astr: CFMutableAttributedStringRef,
        range: CFRange,
        replacement: CFAttributedStringRef,
    );
    pub fn CFAttributedStringBeginEditing(astr: CFMutableAttributedStringRef);
    pub fn CFAttributedStringEndEditing(astr: CFMutableAttributedStringRef);

    pub fn CFAttributedStringGetBidiLevelsAndResolvedDirections(
        astr: CFAttributedStringRef,
        range: CFRange,
        base_direction: i8,
        bidi_levels: *mut u8,
        base_directions: *mut u8,
    ) -> bool;
    pub fn CFAttributedStringGetStatisticalWritingDirections(
        astr: CFAttributedStringRef,
        range: CFRange,
        base_direction: i8,
        bidi_levels: *mut u8,
        base_directions: *mut u8,
    ) -> bool;
}
