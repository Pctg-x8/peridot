#![allow(non_upper_case_globals)]

use crate::CFStringRef;
use core::ffi::*;

pub const kCTFontClassMaskShift: c_int = 28;

pub type CTFontSymbolicTraits = u32;
pub const kCTFontTraitItalic: CTFontSymbolicTraits = 1 << 0;
pub const kCTFontTraitBold: CTFontSymbolicTraits = 1 << 1;
pub const kCTFontTraitExpanded: CTFontSymbolicTraits = 1 << 5;
pub const kCTFontTraitCondensed: CTFontSymbolicTraits = 1 << 6;
pub const kCTFontTraitMonoSpace: CTFontSymbolicTraits = 1 << 10;
pub const kCTFontTraitVertical: CTFontSymbolicTraits = 1 << 11;
pub const kCTFontTraitUIOptimized: CTFontSymbolicTraits = 1 << 12;
pub const kCTFontTraitColorGlyphs: CTFontSymbolicTraits = 1 << 13;
pub const kCTFontTraitComposite: CTFontSymbolicTraits = 1 << 14;
pub const kCTFontTraitClassMask: CTFontSymbolicTraits = 15 << kCTFontClassMaskShift;
pub const kCTFontItalicTrait: CTFontSymbolicTraits = kCTFontTraitItalic;
pub const kCTFontBoldTrait: CTFontSymbolicTraits = kCTFontTraitBold;
pub const kCTFontExpandedTrait: CTFontSymbolicTraits = kCTFontTraitExpanded;
pub const kCTFontCondensedTrait: CTFontSymbolicTraits = kCTFontTraitCondensed;
pub const kCTFontMonoSpaceTrait: CTFontSymbolicTraits = kCTFontTraitMonoSpace;
pub const kCTFontVerticalTrait: CTFontSymbolicTraits = kCTFontTraitVertical;
pub const kCTFontUIOptimizedTrait: CTFontSymbolicTraits = kCTFontTraitUIOptimized;
pub const kCTFontColorGlyphsTrait: CTFontSymbolicTraits = kCTFontTraitColorGlyphs;
pub const kCTFontCompositeTrait: CTFontSymbolicTraits = kCTFontTraitComposite;
pub const kCTFontClassMaskTrait: CTFontSymbolicTraits = kCTFontTraitClassMask;

pub type CTFontStylisticClass = u32;
pub const kCTFontClassUnknown: CTFontStylisticClass = 0 << kCTFontClassMaskShift;
pub const kCTFontClassOldStyleSerifs: CTFontStylisticClass = 1 << kCTFontClassMaskShift;
pub const kCTFontClassTransitionalSerifs: CTFontStylisticClass = 2 << kCTFontClassMaskShift;
pub const kCTFontClassModernSerifs: CTFontStylisticClass = 3 << kCTFontClassMaskShift;
pub const kCTFontClassClarendonSerifs: CTFontStylisticClass = 4 << kCTFontClassMaskShift;
pub const kCTFontClassSlabSerifs: CTFontStylisticClass = 5 << kCTFontClassMaskShift;
pub const kCTFontClassFreeformSerifs: CTFontStylisticClass = 7 << kCTFontClassMaskShift;
pub const kCTFontClassSansSerif: CTFontStylisticClass = 8 << kCTFontClassMaskShift;
pub const kCTFontClassOrnamentals: CTFontStylisticClass = 9 << kCTFontClassMaskShift;
pub const kCTFontClassScripts: CTFontStylisticClass = 10 << kCTFontClassMaskShift;
pub const kCTFontClassSymbolic: CTFontStylisticClass = 12 << kCTFontClassMaskShift;
pub const kCTFontUnknownClass: CTFontStylisticClass = kCTFontClassUnknown;
pub const kCTFontOldStyleSerifsClass: CTFontStylisticClass = kCTFontClassOldStyleSerifs;
pub const kCTFontTransitionalSerifsClass: CTFontStylisticClass = kCTFontClassTransitionalSerifs;
pub const kCTFontModernSerifsClass: CTFontStylisticClass = kCTFontClassModernSerifs;
pub const kCTFontClarendonSerifsClass: CTFontStylisticClass = kCTFontClassClarendonSerifs;
pub const kCTFontSlabSerifsClass: CTFontStylisticClass = kCTFontClassSlabSerifs;
pub const kCTFontFreeformSerifsClass: CTFontStylisticClass = kCTFontClassFreeformSerifs;
pub const kCTFontSansSerifClass: CTFontStylisticClass = kCTFontClassSansSerif;
pub const kCTFontOrnamentalsClass: CTFontStylisticClass = kCTFontClassOrnamentals;
pub const kCTFontScriptsClass: CTFontStylisticClass = kCTFontClassScripts;
pub const kCTFontSymbolicClass: CTFontStylisticClass = kCTFontClassSymbolic;

unsafe extern "system" {
    pub static kCTFontSymbolicTrait: CFStringRef;
    pub static kCTFontWeightTrait: CFStringRef;
    pub static kCTFontWidthTrait: CFStringRef;
    pub static kCTFontSlantTrait: CFStringRef;
}
