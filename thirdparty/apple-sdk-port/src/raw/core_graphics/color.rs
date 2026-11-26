use core::ffi::*;

use crate::{
    FFIOpaqueStruct,
    raw::{CGColorSpaceRef, CGFloat},
};

#[repr(C)]
pub struct CGColor(FFIOpaqueStruct);
pub type CGColorRef = *mut CGColor;

unsafe extern "C" {
    pub fn CGColorCreate(space: CGColorSpaceRef, components: *const CGFloat) -> CGColorRef;
    pub fn CGColorCreateGenericGray(gray: CGFloat, alpha: CGFloat) -> CGColorRef;
    pub fn CGColorCreateGenericRGB(
        red: CGFloat,
        green: CGFloat,
        blue: CGFloat,
        alpha: CGFloat,
    ) -> CGColorRef;
    pub fn CGColorCreateGenericGrayGamma2_2(gray: CGFloat, alpha: CGFloat) -> CGColorRef;
    pub fn CGColorCreateSRGB(
        red: CGFloat,
        green: CGFloat,
        blue: CGFloat,
        alpha: CGFloat,
    ) -> CGColorRef;
    pub fn CGColorCreateWithContentHeadroom(
        headroom: c_float,
        space: CGColorSpaceRef,
        red: CGFloat,
        green: CGFloat,
        blue: CGFloat,
        alpha: CGFloat,
    ) -> CGColorRef;
}
