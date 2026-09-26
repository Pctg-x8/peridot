use crate::FFIOpaqueStruct;
use core::ffi::*;

#[repr(C)]
pub struct CGFont(FFIOpaqueStruct);
pub type CGFontRef = *mut CGFont;

pub type CGFontIndex = c_ushort;

pub type CGGlyph = CGFontIndex;
