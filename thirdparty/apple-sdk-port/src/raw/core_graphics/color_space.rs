#![allow(non_upper_case_globals)]

use crate::{FFIOpaqueStruct, raw::CFStringRef};

#[repr(C)]
pub struct CGColorSpace(FFIOpaqueStruct);
pub type CGColorSpaceRef = *mut CGColorSpace;

pub type CGColorRenderingIntent = i32;
pub const kCGRenderingIntentDefault: CGColorRenderingIntent = 0;
pub const kCGRenderingIntentAbsoluteColorimetric: CGColorRenderingIntent = 1;
pub const kCGRenderingIntentRelativeColorimetric: CGColorRenderingIntent = 2;
pub const kCGRenderingIntentPerceptual: CGColorRenderingIntent = 3;
pub const kCGRenderingIntentSaturation: CGColorRenderingIntent = 4;

pub type CGColorSpaceModel = i32;
pub const kCGColorSpaceModelUnknown: CGColorSpaceModel = -1;
pub const kCGColorSpaceModelMonochrome: CGColorSpaceModel = 0;
pub const kCGColorSpaceModelRGB: CGColorSpaceModel = 1;
pub const kCGColorSpaceModelCMYK: CGColorSpaceModel = 2;
pub const kCGColorSpaceModelLab: CGColorSpaceModel = 3;
pub const kCGColorSpaceModelDeviceN: CGColorSpaceModel = 4;
pub const kCGColorSpaceModelIndexed: CGColorSpaceModel = 5;
pub const kCGColorSpaceModelPattern: CGColorSpaceModel = 6;
pub const kCGColorSpaceModelXYZ: CGColorSpaceModel = 7;

pub const CG_HDR_BT_2100: i32 = 1;

unsafe extern "C" {
    pub static kCGColorSpaceGenericGray: CFStringRef;
    pub static kCGColorSpaceGenericRGB: CFStringRef;
    pub static kCGColorSpaceGenericCMYK: CFStringRef;
    pub static kCGColorSpaceDisplayP3: CFStringRef;
    pub static kCGColorSpaceGenericRGBLinear: CFStringRef;
    pub static kCGColorSpaceAdobeRGB1998: CFStringRef;
    pub static kCGColorSpaceSRGB: CFStringRef;
    pub static kCGColorSpaceGenericGrayGamma2_2: CFStringRef;
    pub static kCGColorSpaceGenericXYZ: CFStringRef;
    pub static kCGColorSpaceGenericLab: CFStringRef;
    pub static kCGColorSpaceACESCGLinear: CFStringRef;
    pub static kCGColorSpaceITUE_700: CFStringRef;
    pub static kCGColorSpaceITUR_709_PQ: CFStringRef;
    pub static kCGColorSpaceITUR_709_HLG: CFStringRef;
}
