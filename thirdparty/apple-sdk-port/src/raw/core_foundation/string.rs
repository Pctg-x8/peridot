use crate::raw::Boolean;

use super::{CFAllocatorRef, CFDataRef, CFDictionaryRef, CFIndex, CFRange, CFTypeID};
use core::ffi::*;

pub type CFStringEncoding = u32;

pub const kCFStringEncodingInvalidId: CFStringEncoding = 0xFFFFFFFF;
pub const kCFStringEncodingMacRoman: CFStringEncoding = 0;
pub const kCFStringEncodingWindowsLatin1: CFStringEncoding = 0x0500;
pub const kCFStringEncodingISOLatin1: CFStringEncoding = 0x0201;
pub const kCFStringEncodingNextStepLatin: CFStringEncoding = 0x0B01;
pub const kCFStringEncodingASCII: CFStringEncoding = 0x0600;
pub const kCFStringEncodingUnicode: CFStringEncoding = 0x0100;
pub const kCFStringEncodingUTF8: CFStringEncoding = 0x08000100;
pub const kCFStringEncodingNonLossyASCII: CFStringEncoding = 0x0BFF;
pub const kCFStringEncodingUTF16: CFStringEncoding = 0x0100;
pub const kCFStringEncodingUTF16BE: CFStringEncoding = 0x10000100;
pub const kCFStringEncodingUTF16LE: CFStringEncoding = 0x14000100;
pub const kCFStringEncodingUTF32: CFStringEncoding = 0x0c000100;
pub const kCFStringEncodingUTF32BE: CFStringEncoding = 0x18000100;
pub const kCFStringEncodingUTF32LE: CFStringEncoding = 0x1c000100;

#[repr(C)]
pub struct __CFString(
    [u8; 0],
    core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,
);
pub type CFStringRef = *const __CFString;
pub type CFMutableStringRef = *mut __CFString;

unsafe extern "C" {
    pub fn CFStringGetTypeID() -> CFTypeID;

    pub fn CFStringCreateWithCString(
        alloc: CFAllocatorRef,
        str: *const c_char,
        encoding: CFStringEncoding,
    ) -> CFStringRef;
    pub fn CFStringCreateWithBytes(
        alloc: CFAllocatorRef,
        bytes: *const u8,
        num_bytes: CFIndex,
        encoding: CFStringEncoding,
        is_external_representation: Boolean,
    ) -> CFStringRef;
    pub fn CFStringCreateWithCharacters(
        alloc: CFAllocatorRef,
        chars: *const u16,
        num_chars: CFIndex,
    ) -> CFStringRef;

    pub fn CFStringCreateWithPascalStringNoCopy(
        alloc: CFAllocatorRef,
        pstr: *const c_uchar,
        encoding: CFStringEncoding,
        contents_deallocator: CFAllocatorRef,
    ) -> CFStringRef;
    pub fn CFStringCreateWithCStringNoCopy(
        alloc: CFAllocatorRef,
        cstr: *const c_char,
        encoding: CFStringEncoding,
        contents_deallocator: CFAllocatorRef,
    ) -> CFStringRef;
    pub fn CFStringCreateWithBytesNoCopy(
        alloc: CFAllocatorRef,
        bytes: *const u8,
        num_bytes: CFIndex,
        encoding: CFStringEncoding,
        is_external_representation: Boolean,
        contents_deallocator: CFAllocatorRef,
    ) -> CFStringRef;
    pub fn CFStringCreateWithCharactersNoCopy(
        alloc: CFAllocatorRef,
        chars: *const u16,
        num_chars: CFIndex,
        contents_deallocator: CFAllocatorRef,
    ) -> CFStringRef;

    pub fn CFStringCreateWithSubstring(
        alloc: CFAllocatorRef,
        str: CFStringRef,
        range: CFRange,
    ) -> CFStringRef;
    pub fn CFStringCreateCopy(alloc: CFAllocatorRef, string: CFStringRef) -> CFStringRef;

    pub fn CFStringCreateWithFormat(
        alloc: CFAllocatorRef,
        format_options: CFDictionaryRef,
        format: CFStringRef,
        ...
    ) -> CFStringRef;

    pub fn CFStringCreateMutable(alloc: CFAllocatorRef, max_length: CFIndex) -> CFMutableStringRef;
    pub fn CFStringCreateMutableCopy(
        alloc: CFAllocatorRef,
        max_length: CFIndex,
        string: CFStringRef,
    ) -> CFMutableStringRef;

    pub fn CFStringCreateMutableWithExternalCharactersNoCopy(
        alloc: CFAllocatorRef,
        chars: *mut u16,
        num_chars: CFIndex,
        capacity: CFIndex,
        external_characters_allocator: CFAllocatorRef,
    ) -> CFMutableStringRef;

    pub fn CFStringGetLength(string: CFStringRef) -> CFIndex;
    pub fn CFStringGetCharacterAtIndex(string: CFStringRef, index: CFIndex) -> u16;
    pub fn CFStringGetCharacters(string: CFStringRef, range: CFRange, buffer: *mut u16);
    pub fn CFStringGetCString(
        string: CFStringRef,
        buffer: *mut c_char,
        buffer_size: CFIndex,
    ) -> Boolean;
    pub fn CFStringGetCStringPtr(string: CFStringRef, encoding: CFStringEncoding) -> *const c_char;
    pub fn CFStringGetCharactersPtr(string: CFStringRef) -> *const u16;
    pub fn CFStringGetBytes(
        string: CFStringRef,
        range: CFRange,
        encoding: CFStringEncoding,
        loss_byte: u8,
        is_external_representation: Boolean,
        buffer: *mut u8,
        max_buf_len: CFIndex,
        used_buf_len: *mut CFIndex,
    ) -> CFIndex;

    pub fn CFStringCreateFromExternalRepresentation(
        alloc: CFAllocatorRef,
        data: CFDataRef,
        encoding: CFStringEncoding,
    ) -> CFStringRef;
    pub fn CFStringCreateExternalRepresentation(
        alloc: CFAllocatorRef,
        string: CFStringRef,
        encoding: CFStringEncoding,
        loss_byte: u8,
    ) -> CFDataRef;
    pub fn CFStringGetSmallestEncoding(string: CFStringRef) -> CFStringEncoding;
    pub fn CFStringGetFastestEncoding(string: CFStringRef) -> CFStringEncoding;
    pub fn CFStringGetSystemEncoding() -> CFStringEncoding;
    pub fn CFStringGetMaximumSizeForEncoding(
        length: CFIndex,
        encoding: CFStringEncoding,
    ) -> CFIndex;

    pub fn CFStringGetFileSystemRepresentation(
        string: CFStringRef,
        buffer: *mut c_char,
        max_buf_len: CFIndex,
    ) -> Boolean;
    pub fn CFStringGetMaximumSizeOfFileSystemRepresentation(string: CFStringRef) -> CFIndex;
    pub fn CFStringCreateWithFileSystemRepresentation(
        alloc: CFAllocatorRef,
        buffer: *const c_char,
    ) -> CFStringRef;

    pub fn CFStringAppend(string: CFMutableStringRef, appendedString: CFStringRef);
    pub fn CFStringAppendCharacters(
        string: CFMutableStringRef,
        chars: *const u16,
        num_chars: CFIndex,
    );
    pub fn CFStringAppendPascalString(
        string: CFMutableStringRef,
        pstr: *const c_uchar,
        encoding: CFStringEncoding,
    );
    pub fn CFStringAppendCString(
        string: CFMutableStringRef,
        cstr: *const c_char,
        encoding: CFStringEncoding,
    );
    pub fn CFStringAppendFormat(
        string: CFMutableStringRef,
        format_options: CFDictionaryRef,
        format: CFStringRef,
        ...
    );
    pub fn CFStringInsert(string: CFMutableStringRef, index: CFIndex, inserted_str: CFStringRef);
    pub fn CFStringDelete(string: CFMutableStringRef, range: CFRange);
    pub fn CFStringReplace(string: CFMutableStringRef, range: CFRange, replacement: CFStringRef);
    pub fn CFStringReplaceAll(string: CFMutableStringRef, replacement: CFStringRef);
}
