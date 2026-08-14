#![allow(non_upper_case_globals)]

use crate::{FFIOpaqueStruct, raw::CFTypeID};

#[repr(C)]
pub struct __CTParagraphStyle(FFIOpaqueStruct);
pub type CTParagraphStyleRef = *const __CTParagraphStyle;

pub type CTTextAlignment = u8;
pub const kCTTextAlignmentLeft: CTTextAlignment = 0;
pub const kCTTextAlignmentRight: CTTextAlignment = 1;
pub const kCTTextAlignmentCenter: CTTextAlignment = 2;
pub const kCTTextAlignmentJustified: CTTextAlignment = 3;
pub const kCTTextAlignmentNatural: CTTextAlignment = 4;

pub type CTLineBreakMode = u8;
pub const kCTLineBreakByWordWrapping: CTLineBreakMode = 0;
pub const kCTLineBreakByCharWrapping: CTLineBreakMode = 1;
pub const kCTLineBreakByClipping: CTLineBreakMode = 2;
pub const kCTLineBreakByTruncatingHead: CTLineBreakMode = 3;
pub const kCTLineBreakByTruncatingTail: CTLineBreakMode = 4;
pub const kCTLineBreakByTruncatingMiddle: CTLineBreakMode = 5;

pub type CTWritingDirection = i8;
pub const kCTWritingDirectionNatural: CTWritingDirection = -1;
pub const kCTWritingDirectionLeftToRight: CTWritingDirection = 0;
pub const kCTWritingDirectionRightToLeft: CTWritingDirection = 1;

pub type CTParagraphStyleSpecifier = u32;
pub const kCTParagraphStyleSpecifierAlignment: CTParagraphStyleSpecifier = 0;
pub const kCTParagraphStyleSpecifierFirstLineHeadIndent: CTParagraphStyleSpecifier = 1;
pub const kCTParagraphStyleSpecifierHeadIndent: CTParagraphStyleSpecifier = 2;
pub const kCTParagraphStyleSpecifierTailIndent: CTParagraphStyleSpecifier = 3;
pub const kCTParagraphStyleSpecifierTabStop: CTParagraphStyleSpecifier = 4;
pub const kCTParagraphStyleSpecifierDefaultTabInterval: CTParagraphStyleSpecifier = 5;
pub const kCTParagraphStyleSpecifierLineBreakMode: CTParagraphStyleSpecifier = 6;
pub const kCTParagraphStyleSpecifierLineHeightMultiple: CTParagraphStyleSpecifier = 7;
pub const kCTParagraphStyleSpecifierMaximumLineHeight: CTParagraphStyleSpecifier = 8;
pub const kCTParagraphStyleSpecifierMinimumLineHeight: CTParagraphStyleSpecifier = 9;
pub const kCTParagraphStyleSpecifierParagraphSpacing: CTParagraphStyleSpecifier = 11;
pub const kCTParagraphStyleSpecifierParagraphSpacingBefore: CTParagraphStyleSpecifier = 12;
pub const kCTParagraphStyleSpecifierBaseWritingDirection: CTParagraphStyleSpecifier = 13;
pub const kCTParagraphStyleSpecifierMaximumLineSpacing: CTParagraphStyleSpecifier = 14;
pub const kCTParagraphStyleSpecifierMinimumLineSpacing: CTParagraphStyleSpecifier = 15;
pub const kCTParagraphStyleSpecifierLineSpacingAdjustment: CTParagraphStyleSpecifier = 16;
pub const kCTParagraphStyleSpecifierLineBoundsOptions: CTParagraphStyleSpecifier = 17;

#[repr(C)]
pub struct CTParagraphStyleSetting {
    pub spec: CTParagraphStyleSpecifier,
    pub value_size: usize,
    pub value: *const core::ffi::c_void,
}

unsafe extern "C" {
    pub fn CTParagraphStyleGetTypeID() -> CFTypeID;

    pub fn CTParagraphStyleCreate(
        settings: *const CTParagraphStyleSetting,
        settingCount: usize,
    ) -> CTParagraphStyleRef;
    pub fn CTParagraphStyleCreateCopy(paragraphStyle: CTParagraphStyleRef) -> CTParagraphStyleRef;
    pub fn CTParagraphStyleGetValueForSpecifier(
        paragraphStyle: CTParagraphStyleRef,
        spec: CTParagraphStyleSpecifier,
        valueBufferSize: usize,
        valueBuffer: *mut core::ffi::c_void,
    ) -> bool;
}
