use std::ptr::NonNull;

use objc::{runtime::Object, *};

use crate::{
    NSCopying, NSDictionary, NSDictionaryObject, NSMutableStringObject, NSObject, NSRange,
    NSString, NSStringObject, NSUInteger, ObjcObject, Owned,
};

pub type NSAttributedStringKey = *mut NSStringObject;
pub type NSAttributedStringFormattingContextKey = *mut NSStringObject;

unsafe extern "C" {
    #[allow(improper_ctypes)]
    pub static NSInflectionConceptsKey: NSAttributedStringFormattingContextKey;
}

pub unsafe trait NSAttributedString: NSObject + NSCopying {
    #[inline(always)]
    fn string(&self) -> &NSStringObject {
        let p: *mut Object = unsafe { msg_send![self.as_id(), string] };
        unsafe { &*p.cast::<NSStringObject>() }
    }

    #[inline(always)]
    fn attributes_at_index(
        &self,
        index: NSUInteger,
        effective_range: Option<&NSRange>,
    ) -> &NSDictionaryObject<NSAttributedStringKey, *mut Object> {
        let p: *mut Object = unsafe {
            msg_send![self.as_id(), attributesAtIndex: index effectiveRange: effective_range.map_or_else(core::ptr::null_mut, |p| p as *const _ as *mut NSRange)]
        };
        unsafe { &*p.cast() }
    }

    #[inline(always)]
    fn length(&self) -> NSUInteger {
        unsafe { msg_send![self.as_id(), length] }
    }

    #[inline(always)]
    fn attribute(
        &self,
        attr_name: &(impl NSString + ?Sized),
        index: NSUInteger,
        effective_range: Option<&NSRange>,
    ) -> Option<NonNull<Object>> {
        NonNull::new(unsafe {
            msg_send![self.as_id(), attribute: attr_name atIndex: index effectiveRange: effective_range.map_or_else(core::ptr::null_mut, |p| p as *const _ as *mut NSRange)]
        })
    }
}

#[repr(transparent)]
pub struct NSAttributedStringObject(Object);
unsafe impl ObjcObject for NSAttributedStringObject {
    #[inline(always)]
    fn as_id(&self) -> *mut Object {
        &self.0 as *const Object as *mut Object
    }

    #[inline(always)]
    fn as_id_mut(&mut self) -> *mut Object {
        &mut self.0 as *mut Object
    }
}
unsafe impl NSObject for NSAttributedStringObject {}
unsafe impl NSCopying for NSAttributedStringObject {}
unsafe impl NSAttributedString for NSAttributedStringObject {}
impl NSAttributedStringObject {
    #[inline(always)]
    pub fn with_string(s: &(impl NSString + ?Sized)) -> Owned<Self> {
        let p: *mut Object = unsafe { msg_send![class!(NSAttributedString), alloc] };
        unsafe { Owned::from_id_unchecked(msg_send![p, initWithString: s.as_id()]) }
    }

    #[inline(always)]
    pub fn with_string_and_attributes(
        s: &(impl NSString + ?Sized),
        attributes: Option<&(impl NSDictionary<NSAttributedStringKey, *mut Object> + ?Sized)>,
    ) -> Owned<Self> {
        let p: *mut Object = unsafe { msg_send![class!(NSAttributedString), alloc] };
        unsafe {
            Owned::from_id_unchecked(msg_send![
                p,
                initWithString: s.as_id()
                attributes: attributes.map_or_else(core::ptr::null_mut, ObjcObject::as_id)
            ])
        }
    }
}

pub unsafe trait NSMutableAttributedString: NSAttributedString {
    #[inline(always)]
    fn replace_characters_in_range(&mut self, range: NSRange, string: &(impl NSString + ?Sized)) {
        unsafe {
            msg_send![
                self.as_id_mut(),
                replaceCharactersInRange: range
                withString: string.as_id()
            ]
        }
    }

    #[inline(always)]
    fn set_attributes(
        &mut self,
        attributes: Option<&(impl NSDictionary<NSAttributedStringKey, *mut Object> + ?Sized)>,
        range: NSRange,
    ) {
        unsafe {
            msg_send![
                self.as_id_mut(),
                setAttributes: attributes.map_or_else(core::ptr::null_mut, ObjcObject::as_id)
                range: range
            ]
        }
    }

    #[inline(always)]
    fn mutable_string(&mut self) -> &mut NSMutableStringObject {
        let p: *mut Object = unsafe { msg_send![self.as_id_mut(), mutableString] };
        unsafe { &mut *p.cast::<NSMutableStringObject>() }
    }

    #[inline(always)]
    fn add_attribute(
        &mut self,
        name: &(impl NSString + ?Sized),
        value: &(impl ObjcObject + ?Sized),
        range: NSRange,
    ) {
        unsafe {
            msg_send![
                self.as_id_mut(),
                addAttribute: name.as_id()
                value: value.as_id()
                range: range
            ]
        }
    }

    #[inline(always)]
    fn remove_attribute(&mut self, name: &(impl NSString + ?Sized), range: NSRange) {
        unsafe {
            msg_send![
                self.as_id_mut(),
                removeAttribute: name.as_id()
                range: range
            ]
        }
    }
}
