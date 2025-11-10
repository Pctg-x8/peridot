use crate::{NSStringObject, NSUInteger, Owned};

#[repr(C)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NSRange {
    pub location: NSUInteger,
    pub length: NSUInteger,
}
impl NSRange {
    #[inline(always)]
    pub const fn max(&self) -> NSUInteger {
        self.location + self.length
    }

    #[inline(always)]
    pub const fn location_in_range(&self, loc: NSUInteger) -> bool {
        !(loc < self.location) && (loc - self.location) < self.length
    }

    #[inline(always)]
    pub fn union(self, other: Self) -> Self {
        unsafe { NSUnionRange(self, other) }
    }

    #[inline(always)]
    pub fn intersection(self, other: Self) -> Self {
        unsafe { NSIntersectionRange(self, other) }
    }

    #[inline(always)]
    pub fn to_ns_string(self) -> Owned<NSStringObject> {
        unsafe { Owned::from_typed_id_unchecked(NSStringFromRange(self)) }
    }

    #[inline(always)]
    pub fn from_ns_string(string: &NSStringObject) -> Self {
        unsafe { NSRangeFromString(string as *const _ as _) }
    }
}

// TODO: NSValue extension for NSRange

pub type NSRangePointer = *mut NSRange;

unsafe extern "C" {
    pub fn NSUnionRange(range1: NSRange, range2: NSRange) -> NSRange;
    pub fn NSIntersectionRange(range1: NSRange, range2: NSRange) -> NSRange;
    #[allow(improper_ctypes)]
    pub fn NSStringFromRange(range: NSRange) -> *mut NSStringObject;
    #[allow(improper_ctypes)]
    pub fn NSRangeFromString(string: *mut NSStringObject) -> NSRange;
}
