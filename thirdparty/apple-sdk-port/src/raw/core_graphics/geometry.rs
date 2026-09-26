#![allow(non_upper_case_globals)]

use crate::raw::CGFloat;

#[repr(C)]
pub struct CGPoint {
    pub x: CGFloat,
    pub y: CGFloat,
}

#[repr(C)]
pub struct CGSize {
    pub width: CGFloat,
    pub height: CGFloat,
}

#[repr(C)]
pub struct CGVector {
    pub dx: CGFloat,
    pub dy: CGFloat,
}

#[repr(C)]
pub struct CGRect {
    pub origin: CGPoint,
    pub size: CGSize,
}

pub type CGRectEdge = u32;
pub const CGRectMinXEdge: CGRectEdge = 0;
pub const CGRectMinYEdge: CGRectEdge = 1;
pub const CGRectMaxXEdge: CGRectEdge = 2;
pub const CGRectMaxYEdge: CGRectEdge = 3;
