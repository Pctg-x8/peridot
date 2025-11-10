#![allow(non_upper_case_globals)]
use std::mem::MaybeUninit;

use objc::{
    runtime::{BOOL, Object},
    *,
};

use crate::{NSCopying, NSObject, NSRange, NSUInteger, ObjcObject, Owned};

#[allow(non_camel_case_types)]
pub type unichar = core::ffi::c_ushort;

pub type NSStringEncoding = NSUInteger;
pub const NSASCIIStringEncoding: NSStringEncoding = 1;
pub const NSNEXTSTEPStringEncoding: NSStringEncoding = 2;
pub const NSJapaneseEUCStringEncoding: NSStringEncoding = 3;
pub const NSUTF8StringEncoding: NSStringEncoding = 4;
pub const NSISOLatin1StringEncoding: NSStringEncoding = 5;
pub const NSSymbolStringEncoding: NSStringEncoding = 6;
pub const NSNonLossyASCIIStringEncoding: NSStringEncoding = 7;
pub const NSShiftJISStringEncoding: NSStringEncoding = 8; /* kCFStringEncodingDOSJapanese */
pub const NSISOLatin2StringEncoding: NSStringEncoding = 9;
pub const NSUnicodeStringEncoding: NSStringEncoding = 10;
pub const NSWindowsCP1251StringEncoding: NSStringEncoding = 11; /* Cyrillic; same as AdobeStandardCyrillic */
pub const NSWindowsCP1252StringEncoding: NSStringEncoding = 12; /* WinLatin1 */
pub const NSWindowsCP1253StringEncoding: NSStringEncoding = 13; /* Greek */
pub const NSWindowsCP1254StringEncoding: NSStringEncoding = 14; /* Turkish */
pub const NSWindowsCP1250StringEncoding: NSStringEncoding = 15; /* WinLatin2 */
pub const NSISO2022JPStringEncoding: NSStringEncoding = 21; /* ISO 2022 Japanese encoding for e-mail */
pub const NSMacOSRomanStringEncoding: NSStringEncoding = 30;
pub const NSUTF16StringEncoding: NSStringEncoding = NSUnicodeStringEncoding; /* An alias for NSUnicodeStringEncoding */
pub const NSUTF16BigEndianStringEncoding: NSStringEncoding = 0x90000100; /* NSUTF16StringEncoding encoding with explicit endianness specified */
pub const NSUTF16LittleEndianStringEncoding: NSStringEncoding = 0x94000100; /* NSUTF16StringEncoding encoding with explicit endianness specified */
pub const NSUTF32StringEncoding: NSStringEncoding = 0x8c000100;
pub const NSUTF32BigEndianStringEncoding: NSStringEncoding = 0x98000100; /* NSUTF32StringEncoding encoding with explicit endianness specified */
pub const NSUTF32LittleEndianStringEncoding: NSStringEncoding = 0x9c000100;

pub unsafe trait NSString: NSObject + NSCopying {
    #[inline(always)]
    fn length(&self) -> NSUInteger {
        unsafe { msg_send![self.as_id(), length] }
    }

    #[inline(always)]
    fn utf8_string(&self) -> &core::ffi::CStr {
        unsafe { core::ffi::CStr::from_ptr(msg_send![self.as_id(), UTF8String]) }
    }

    #[inline(always)]
    fn fastest_encoding(&self) -> NSStringEncoding {
        unsafe { msg_send![self.as_id(), fastestEncoding] }
    }

    #[inline(always)]
    fn smallest_encoding(&self) -> NSStringEncoding {
        unsafe { msg_send![self.as_id(), smallestEncoding] }
    }

    #[inline(always)]
    fn get_c_string(
        &self,
        buffer: &mut [MaybeUninit<core::ffi::c_char>],
        encoding: NSStringEncoding,
    ) -> BOOL {
        unsafe {
            msg_send![
                self.as_id(),
                getCString: buffer.as_mut_ptr()
                maxLength: buffer.len() as NSUInteger
                encoding: encoding
            ]
        }
    }

    #[inline(always)]
    fn maximum_length_of_bytes_using_encoding(&self, encoding: NSStringEncoding) -> NSUInteger {
        unsafe { msg_send![self.as_id(), maximumLengthOfBytesUsingEncoding: encoding] }
    }

    #[inline(always)]
    fn length_of_bytes_using_encoding(&self, encoding: NSStringEncoding) -> NSUInteger {
        unsafe { msg_send![self.as_id(), lengthOfBytesUsingEncoding: encoding] }
    }
}

#[repr(transparent)]
pub struct NSStringObject(Object);
unsafe impl ObjcObject for NSStringObject {
    #[inline(always)]
    fn as_id(&self) -> *mut Object {
        &self.0 as *const Object as *mut Object
    }

    #[inline(always)]
    fn as_id_mut(&mut self) -> *mut Object {
        &mut self.0 as *mut Object
    }
}
unsafe impl NSObject for NSStringObject {}
unsafe impl NSCopying for NSStringObject {}
unsafe impl NSString for NSStringObject {}
impl NSStringObject {
    #[inline(always)]
    pub fn new_empty() -> Owned<Self> {
        unsafe { Owned::from_id_unchecked(msg_send![class!(NSString), string]) }
    }

    #[inline(always)]
    pub fn from_utf8(s: &core::ffi::CStr) -> Owned<Self> {
        unsafe {
            Owned::from_id_unchecked(msg_send![class!(NSString), stringWithUTF8String: s.as_ptr()])
        }
    }
}

pub unsafe trait NSMutableString: NSString {
    #[inline(always)]
    fn replace_characters(&mut self, range: NSRange, with: &(impl NSString + ?Sized)) {
        unsafe {
            msg_send![self.as_id_mut(), replaceCharactersInRange: range withString: with.as_id()]
        }
    }

    #[inline(always)]
    fn insert_string(&mut self, string: &(impl NSString + ?Sized), index: NSUInteger) {
        unsafe { msg_send![self.as_id_mut(), insertString: string.as_id() atIndex: index] }
    }

    #[inline(always)]
    fn delete_characters(&mut self, range: NSRange) {
        unsafe { msg_send![self.as_id_mut(), deleteCharactersInRange: range] }
    }

    #[inline(always)]
    fn append_string(&mut self, string: &(impl NSString + ?Sized)) {
        unsafe { msg_send![self.as_id_mut(), appendString: string.as_id()] }
    }

    #[inline(always)]
    fn set_string(&mut self, string: &(impl NSString + ?Sized)) {
        unsafe { msg_send![self.as_id_mut(), setString: string.as_id()] }
    }
}

#[repr(transparent)]
pub struct NSMutableStringObject(Object);
unsafe impl ObjcObject for NSMutableStringObject {
    #[inline(always)]
    fn as_id(&self) -> *mut Object {
        &self.0 as *const Object as *mut Object
    }

    #[inline(always)]
    fn as_id_mut(&mut self) -> *mut Object {
        &mut self.0 as *mut Object
    }
}
unsafe impl NSObject for NSMutableStringObject {}
unsafe impl NSCopying for NSMutableStringObject {}
unsafe impl NSString for NSMutableStringObject {}
unsafe impl NSMutableString for NSMutableStringObject {}
impl NSMutableStringObject {
    #[inline(always)]
    pub fn new_empty() -> Owned<Self> {
        unsafe { Owned::from_id_unchecked(msg_send![class!(NSMutableString), string]) }
    }

    #[inline(always)]
    pub fn from_utf8(s: &core::ffi::CStr) -> Owned<Self> {
        unsafe {
            Owned::from_id_unchecked(
                msg_send![class!(NSMutableString), stringWithUTF8String: s.as_ptr()],
            )
        }
    }

    #[inline(always)]
    pub fn with_capacity(capacity: NSUInteger) -> Owned<Self> {
        unsafe {
            Owned::from_id_unchecked(msg_send![
                class!(NSMutableString),
                stringWithCapacity: capacity
            ])
        }
    }
}
