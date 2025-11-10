#![allow(non_upper_case_globals)]

use crate::{CGPoint, CGRect, FFIOpaqueStruct};
use core::ffi::*;

#[repr(C)]
pub struct CGPath(FFIOpaqueStruct);
pub type CGMutablePathRef = *mut CGPath;
pub type CGPathRef = *const CGPath;

pub type CGLineJoin = i32;
pub const kCGLineJoinMiter: CGLineJoin = 0;
pub const kCGLineJoinRound: CGLineJoin = 1;
pub const kCGLineJoinBevel: CGLineJoin = 2;

pub type CGLineCap = i32;
pub const kCGLineCapButt: CGLineCap = 0;
pub const kCGLineCapRound: CGLineCap = 1;
pub const kCGLineCapSquare: CGLineCap = 2;

pub type CGPathElementType = i32;
pub const kCGPathElementMoveToPoint: CGPathElementType = 0;
pub const kCGPathElementAddLineToPoint: CGPathElementType = 1;
pub const kCGPathElementAddQuadCurveToPoint: CGPathElementType = 2;
pub const kCGPathElementAddCurveToPoint: CGPathElementType = 3;
pub const kCGPathElementCloseSubpath: CGPathElementType = 4;

#[repr(C)]
pub struct CGPathElement {
    pub r#type: CGPathElementType,
    pub points: *mut CGPoint,
}

pub type CGPathApplierFunction = extern "C" fn(info: *mut c_void, element: *const CGPathElement);

unsafe extern "C" {
    pub fn CGPathGetBoundingBox(path: CGPathRef) -> CGRect;
    pub fn CGPathGetPathBoundingBox(path: CGPathRef) -> CGRect;
    pub fn CGPathApply(path: CGPathRef, info: *mut c_void, function: CGPathApplierFunction);
}
