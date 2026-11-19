#![allow(non_camel_case_types, non_upper_case_globals)]

use core::{ffi::*, mem::MaybeUninit};
use objc::*;

use crate::{
    NSInteger, NSUInteger,
    objc::{NSCopying, NSMutableCopying, NSObject, NSObjectOwned, ObjcObject},
};

pub type unichar = c_ushort;

pub type NSStringCompareOptions = NSUInteger;
pub const NSCaseInsensitiveSearch: NSStringCompareOptions = 1;
pub const NSLiteralSearch: NSStringCompareOptions = 2;
pub const NSBackwardsSearch: NSStringCompareOptions = 4;
pub const NSAnchoredSearch: NSStringCompareOptions = 8;
pub const NSNumericSearch: NSStringCompareOptions = 64;
pub const NSDiacriticInsensitiveSearch: NSStringCompareOptions = 128;
pub const NSWidthInsensitiveSearch: NSStringCompareOptions = 256;
pub const NSForcedOrderingSearch: NSStringCompareOptions = 512;
pub const NSRegularExpressionSearch: NSStringCompareOptions = 1024;

pub type NSStringEncoding = NSUInteger;
pub const NSASCIIStringEncoding: NSStringEncoding = 1;
pub const NSNEXTSTEPStringEncoding: NSStringEncoding = 2;
pub const NSJapaneseEUCStringEncoding: NSStringEncoding = 3;
pub const NSUTF8StringEncoding: NSStringEncoding = 4;
pub const NSISOLatin1StringEncoding: NSStringEncoding = 5;
pub const NSSymbolStringEncoding: NSStringEncoding = 6;
pub const NSNonLossyASCIIStringEncoding: NSStringEncoding = 7;
pub const NSShiftJISStringEncoding: NSStringEncoding = 8;
pub const NSISOLatin2StringEncoding: NSStringEncoding = 9;
pub const NSUnicodeStringEncoding: NSStringEncoding = 10;
pub const NSWindowsCP1251StringEncoding: NSStringEncoding = 11;
pub const NSWindowsCP1252StringEncoding: NSStringEncoding = 12;
pub const NSWindowsCP1253StringEncoding: NSStringEncoding = 13;
pub const NSWindowsCP1254StringEncoding: NSStringEncoding = 14;
pub const NSWindowsCP1250StringEncoding: NSStringEncoding = 15;
pub const NSISO2022JPStringEncoding: NSStringEncoding = 21;
pub const NSMacOSRomanStringEncoding: NSStringEncoding = 30;
pub const NSUTF16StringEncoding: NSStringEncoding = NSUnicodeStringEncoding;
pub const NSUTF16BigEndianStringEncoding: NSStringEncoding = 0x90000100;
pub const NSUTF16LittleEndianStringEncoding: NSStringEncoding = 0x94000100;
pub const NSUTF32StringEncoding: NSStringEncoding = 0x8c000100;
pub const NSUTF32BigEndianStringEncoding: NSStringEncoding = 0x98000100;
pub const NSUTF32LittleEndianStringEncoding: NSStringEncoding = 0x9c000100;

pub type NSStringEncodingConversionOptions = NSUInteger;
pub const NSStringEncodingConversionAllowLossy: NSStringEncodingConversionOptions = 1;
pub const NSStringEncodingConversionExternalRepresentation: NSStringEncodingConversionOptions = 2;

pub trait NSString: NSObject + NSCopying + NSMutableCopying {
    #[inline(always)]
    fn length(&self) -> NSUInteger {
        unsafe { msg_send![self.as_id(), length] }
    }

    #[inline(always)]
    fn utf8_string(&self) -> Option<&CStr> {
        let ptr: *const c_char = unsafe { msg_send![self.as_id(), UTF8String] };
        if ptr.is_null() {
            None
        } else {
            Some(unsafe { CStr::from_ptr(ptr) })
        }
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
    fn get_cstring(&self, buffer: &mut [MaybeUninit<c_char>], encoding: NSStringEncoding) -> bool {
        let r: objc::runtime::BOOL = unsafe {
            msg_send![self.as_id(), getCString: buffer.as_mut_ptr() maxLength: buffer.len() as NSUInteger encoding: encoding]
        };
        r
    }

    #[inline(always)]
    fn maximum_length_of_bytes_using_encoding(&self, enc: NSStringEncoding) -> NSUInteger {
        unsafe { msg_send![self.as_id(), maximumLengthOfBytesUsingEncoding: enc] }
    }

    #[inline(always)]
    fn length_of_bytes_using_encoding(&self, enc: NSStringEncoding) -> NSUInteger {
        unsafe { msg_send![self.as_id(), lengthOfBytesUsingEncoding: enc] }
    }
}

pub struct NSStringId(objc::runtime::Object);
impl ObjcObject for NSStringId {
    #[inline(always)]
    fn as_id(&self) -> *const objc::runtime::Object {
        &self.0
    }

    #[inline(always)]
    fn as_id_mut(&mut self) -> *mut objc::runtime::Object {
        &mut self.0
    }
}
impl NSObject for NSStringId {}
impl NSCopying for NSStringId {}
impl NSMutableCopying for NSStringId {}
impl NSString for NSStringId {}
impl NSStringId {
    #[inline(always)]
    pub fn from_utf8_string(null_terminated_cstring: &CStr) -> Result<NSObjectOwned<Self>, ()> {
        unsafe {
            Ok(NSObjectOwned::from_id_unretained_unchecked(
                msg_send![class!(NSString), stringWithUTF8String: null_terminated_cstring.as_ptr()],
            ))
        }
    }
}

pub trait NSError: NSObject + NSCopying {
    #[inline(always)]
    fn domain(&self) -> &NSStringId {
        unsafe {
            &*core::mem::transmute::<*const objc::runtime::Object, *const NSStringId>(msg_send![
                self.as_id(),
                domain
            ])
        }
    }

    #[inline(always)]
    fn code(&self) -> NSInteger {
        unsafe { msg_send![self.as_id(), code] }
    }

    #[inline(always)]
    fn localized_description(&self) -> &NSStringId {
        unsafe {
            &*core::mem::transmute::<*const objc::runtime::Object, *const NSStringId>(msg_send![
                self.as_id(),
                localizedDescription
            ])
        }
    }

    #[inline(always)]
    fn localized_failure_reason(&self) -> Option<&NSStringId> {
        let p: *const objc::runtime::Object =
            unsafe { msg_send![self.as_id(), localizedFailureReason] };
        if p.is_null() {
            None
        } else {
            Some(unsafe { &*core::mem::transmute::<_, *const NSStringId>(p) })
        }
    }

    #[inline(always)]
    fn localized_recovery_suggestion(&self) -> Option<&NSStringId> {
        let p: *const objc::runtime::Object =
            unsafe { msg_send![self.as_id(), localizedRecoverySuggestion] };
        if p.is_null() {
            None
        } else {
            Some(unsafe { &*core::mem::transmute::<_, *const NSStringId>(p) })
        }
    }

    #[inline(always)]
    fn localized_recovery_options(&self) -> Option<&NSStringId> {
        let p: *const objc::runtime::Object =
            unsafe { msg_send![self.as_id(), localizedRecoveryOptions] };
        if p.is_null() {
            None
        } else {
            Some(unsafe { &*core::mem::transmute::<_, *const NSStringId>(p) })
        }
    }
}

pub struct NSErrorId(objc::runtime::Object);
impl ObjcObject for NSErrorId {
    #[inline(always)]
    fn as_id(&self) -> *const objc::runtime::Object {
        &self.0
    }

    #[inline(always)]
    fn as_id_mut(&mut self) -> *mut objc::runtime::Object {
        &mut self.0
    }
}
impl NSObject for NSErrorId {}
impl NSCopying for NSErrorId {}
impl NSError for NSErrorId {}
