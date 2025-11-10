#![allow(non_upper_case_globals)]

use crate::{
    CFArrayRef, CFCharacterSetRef, CFDictionaryRef, CFIndex, CFOptionFlags, CFRange,
    CFStringEncoding, CFStringRef, CFTypeID, CFTypeRef, CGAffineTransform, CGFloat, CGFontRef,
    CGGlyph, CGPathRef, CGRect, CGSize, CTFontDescriptorRef, CTFontOrientation,
    CTFontSymbolicTraits, FFIOpaqueStruct, UniChar,
};
use core::ffi::*;

#[repr(C)]
pub struct __CTFont(FFIOpaqueStruct);
pub type CTFontRef = *const __CTFont;

pub type CTFontOptions = CFOptionFlags;
pub const kCTFontOptionsDefault: CTFontOptions = 0;
pub const kCTFontOptionsPreventAutoActivation: CTFontOptions = 1 << 0;
pub const kCTFontOptionsPreventAutoDownload: CTFontOptions = 1 << 1;
pub const kCTFontOptionsPreferSystemFont: CTFontOptions = 1 << 2;

pub type CTFontUIFontType = u32;
pub const kCTFontUIFontNone: CTFontUIFontType = (-1i32).cast_unsigned();
pub const kCTFontUIFontUser: CTFontUIFontType = 0;
pub const kCTFontUIFontUserFixedPitch: CTFontUIFontType = 1;
pub const kCTFontUIFontSystem: CTFontUIFontType = 2;
pub const kCTFontUIFontEmphasizedSystem: CTFontUIFontType = 3;
pub const kCTFontUIFontSmallSystem: CTFontUIFontType = 4;
pub const kCTFontUIFontSmallEmphasizedSystem: CTFontUIFontType = 5;
pub const kCTFontUIFontMiniSystem: CTFontUIFontType = 6;
pub const kCTFontUIFontMiniEmphasizedSystem: CTFontUIFontType = 7;
pub const kCTFontUIFontViews: CTFontUIFontType = 8;
pub const kCTFontUIFontApplication: CTFontUIFontType = 9;
pub const kCTFontUIFontLabel: CTFontUIFontType = 10;
pub const kCTFontUIFontMenuTitle: CTFontUIFontType = 11;
pub const kCTFontUIFontMenuItem: CTFontUIFontType = 12;
pub const kCTFontUIFontMenuItemMark: CTFontUIFontType = 13;
pub const kCTFontUIFontMenuItemCmdKey: CTFontUIFontType = 14;
pub const kCTFontUIFontWindowTitle: CTFontUIFontType = 15;
pub const kCTFontUIFontPushButton: CTFontUIFontType = 16;
pub const kCTFontUIFontUtilityWindowTitle: CTFontUIFontType = 17;
pub const kCTFontUIFontAlertHeader: CTFontUIFontType = 18;
pub const kCTFontUIFontSystemDetail: CTFontUIFontType = 19;
pub const kCTFontUIFontEmphasizedSystemDetail: CTFontUIFontType = 20;
pub const kCTFontUIFontToolbar: CTFontUIFontType = 21;
pub const kCTFontUIFontSmallToolbar: CTFontUIFontType = 22;
pub const kCTFontUIFontMessage: CTFontUIFontType = 23;
pub const kCTFontUIFontPalette: CTFontUIFontType = 24;
pub const kCTFontUIFontToolTip: CTFontUIFontType = 25;
pub const kCTFontUIFontControlContent: CTFontUIFontType = 26;

unsafe extern "C" {
    pub fn CTFontGetTypeID() -> CFTypeID;

    pub static kCTFontCopyrightNameKey: CFStringRef;
    pub static kCTFontFamilyNameKey: CFStringRef;
    pub static kCTFontSubFamilyNameKey: CFStringRef;
    pub static kCTFontStyleNameKey: CFStringRef;
    pub static kCTFontUniqueNameKey: CFStringRef;
    pub static kCTFontFullNameKey: CFStringRef;
    pub static kCTFontVersionNameKey: CFStringRef;
    pub static kCTFontPostScriptNameKey: CFStringRef;
    pub static kCTFontTrademarkNameKey: CFStringRef;
    pub static kCTFontManufacturerNameKey: CFStringRef;
    pub static kCTFontDesignerNameKey: CFStringRef;
    pub static kCTFontDescriptionNameKey: CFStringRef;
    pub static kCTFontVendorURLNameKey: CFStringRef;
    pub static kCTFontDesignerURLNameKey: CFStringRef;
    pub static kCTFontLicenseNameKey: CFStringRef;
    pub static kCTFontLicenseURLNameKey: CFStringRef;
    pub static kCTFontSampleTextNameKey: CFStringRef;
    pub static kCTFontPostScriptCIDNameKey: CFStringRef;

    pub fn CTFontCreateWithName(
        name: CFStringRef,
        size: CGFloat,
        matrix: *const CGAffineTransform,
    ) -> CTFontRef;
    pub fn CTFontCreateWithFontDescriptor(
        descriptor: CTFontDescriptorRef,
        size: CGFloat,
        matrix: *const CGAffineTransform,
    ) -> CTFontRef;
    pub fn CTFontCreateWithNameAndOptions(
        name: CFStringRef,
        size: CGFloat,
        matrix: *const CGAffineTransform,
        options: CTFontOptions,
    ) -> CTFontRef;
    pub fn CTFontCreateWithFontDescriptorAndOptions(
        descriptor: CTFontDescriptorRef,
        size: CGFloat,
        matrix: *const CGAffineTransform,
        options: CTFontOptions,
    ) -> CTFontRef;
    pub fn CTFontCreateUIFontForLanguage(
        uiType: CTFontUIFontType,
        size: CGFloat,
        language: CFStringRef,
    ) -> CTFontRef;
    pub fn CTFontCreateCopyWithAttributes(
        font: CTFontRef,
        size: CGFloat,
        matrix: *const CGAffineTransform,
        attributes: CTFontDescriptorRef,
    ) -> CTFontRef;
    pub fn CTFontCreateCopyWithSymbolicTraits(
        font: CTFontRef,
        size: CGFloat,
        matrix: *const CGAffineTransform,
        trait_value: CTFontSymbolicTraits,
        trait_mask: CTFontSymbolicTraits,
    ) -> CTFontRef;
    pub fn CTFontCreateCopyWithFamily(
        font: CTFontRef,
        size: CGFloat,
        matrix: *const CGAffineTransform,
        family: CFStringRef,
    ) -> CTFontRef;

    pub fn CTFontCreateForString(
        current_font: CTFontRef,
        string: CFStringRef,
        range: CFRange,
    ) -> CTFontRef;
    pub fn CTFontCreateForStringWithLanguage(
        current_font: CTFontRef,
        string: CFStringRef,
        range: CFRange,
        language: CFStringRef,
    ) -> CTFontRef;

    pub fn CTFontCopyFontDescriptor(font: CTFontRef) -> CTFontDescriptorRef;
    pub fn CTFontCopyAttribute(font: CTFontRef, attribute: CFStringRef) -> CFTypeRef;
    pub fn CTFontGetSize(font: CTFontRef) -> CGFloat;
    pub fn CTFontGetMatrix(font: CTFontRef) -> CGAffineTransform;
    pub fn CTFontGetSymbolicTraits(font: CTFontRef) -> CTFontSymbolicTraits;
    pub fn CTFontCopyTraits(font: CTFontRef) -> CFDictionaryRef;
    pub fn CTFontCopyDefaultCascadeListForLanguages(
        font: CTFontRef,
        language_pref_list: CFArrayRef,
    ) -> CFArrayRef;
    pub fn CTFontCopyPostScriptName(font: CTFontRef) -> CFStringRef;
    pub fn CTFontCopyFamilyName(font: CTFontRef) -> CFStringRef;
    pub fn CTFontCopyFullName(font: CTFontRef) -> CFStringRef;
    pub fn CTFontCopyDisplayName(font: CTFontRef) -> CFStringRef;
    pub fn CTFontCopyName(font: CTFontRef, name_key: CFStringRef) -> CFStringRef;
    pub fn CTFontCopyLocalizedName(
        font: CTFontRef,
        name_key: CFStringRef,
        actual_language: *mut CFStringRef,
    ) -> CFStringRef;

    pub fn CTFontCopyCharacterSet(font: CTFontRef) -> CFCharacterSetRef;
    pub fn CTFontGetStringEncoding(font: CTFontRef) -> CFStringEncoding;
    pub fn CTFontCopySupportedLanguages(font: CTFontRef) -> CFArrayRef;
    pub fn CTFontGetGlyphsForCharacters(
        font: CTFontRef,
        characters: *const UniChar,
        glyphs: *mut CGGlyph,
        count: CFIndex,
    ) -> bool;

    pub fn CTFontGetAscent(font: CTFontRef) -> CGFloat;
    pub fn CTFontGetDescent(font: CTFontRef) -> CGFloat;
    pub fn CTFontGetLeading(font: CTFontRef) -> CGFloat;
    pub fn CTFontGetUnitsPerEm(font: CTFontRef) -> c_uint;
    pub fn CTFontGetGlyphCount(font: CTFontRef) -> CFIndex;
    pub fn CTFontGetBoundingBox(font: CTFontRef) -> CGRect;
    pub fn CTFontGetUnderlinePosition(font: CTFontRef) -> CGFloat;
    pub fn CTFontGetUnderlineThickness(font: CTFontRef) -> CGFloat;
    pub fn CTFontGetSlantAngle(font: CTFontRef) -> CGFloat;
    pub fn CTFontGetCapHeight(font: CTFontRef) -> CGFloat;
    pub fn CTFontGetXHeight(font: CTFontRef) -> CGFloat;

    pub fn CTFontGetGlyphWithName(font: CTFontRef, glyph_name: CFStringRef) -> CGGlyph;
    pub fn CTFontCopyNameForGlyph(font: CTFontRef, glyph: CGGlyph) -> CFStringRef;
    pub fn CTFontGetBoundingRectsForGlyphs(
        font: CTFontRef,
        orientation: CTFontOrientation,
        glyphs: *const CGGlyph,
        bounding_rects: *mut CGRect,
        count: CFIndex,
    ) -> CGRect;
    pub fn CTFontGetOpticalBoundsForGlyphs(
        font: CTFontRef,
        glyphs: *const CGGlyph,
        bonuding_rects: *mut CGRect,
        count: CFIndex,
        options: CFOptionFlags,
    ) -> CGRect;
    pub fn CTFontGetAdvancesFotGlyphs(
        font: CTFontRef,
        orientation: CTFontOrientation,
        glyphs: *const CGGlyph,
        advances: *mut CGSize,
        count: CFIndex,
    ) -> c_double;
    pub fn CTFontGetVerticalTranslationsForGlyphs(
        font: CTFontRef,
        glyphs: *const CGGlyph,
        translations: *mut CGSize,
        count: CFIndex,
    );
    pub fn CTFontCreatePathForGlyph(
        font: CTFontRef,
        glyph: CGGlyph,
        matrix: *const CGAffineTransform,
    ) -> CGPathRef;

    pub static kCTFontVariationAxisIdentifierKey: CFStringRef;
    pub static kCTFontVariationAxisMinimumValueKey: CFStringRef;
    pub static kCTFontVariationAxisMaximumValueKey: CFStringRef;
    pub static kCTFontVariationAxisDefaultValueKey: CFStringRef;
    pub static kCTFontVariationAxisNameKey: CFStringRef;
    pub static kCTFontVariationAxisHiddenKey: CFStringRef;

    pub fn CTFontCopyVariationAxes(font: CTFontRef) -> CFArrayRef;
    pub fn CTFontCopyVariation(font: CTFontRef) -> CFDictionaryRef;

    pub static kCTFontOpenTypeFeatureTag: CFStringRef;
    pub static kCTFontOpenTypeFeatureValue: CFStringRef;
    pub static kCTFontFeatureTypeIdentifierKey: CFStringRef;
    pub static kCTFontFeatureTypeNameKey: CFStringRef;
    pub static kCTFontFeatureTypeExclusiveKey: CFStringRef;
    pub static kCTFontFeatureTypeSelectorsKey: CFStringRef;
    pub static kCTFontFeatureSelectorIdentifierKey: CFStringRef;
    pub static kCTFontFeatureSelectorNameKey: CFStringRef;
    pub static kCTFontFeatureSelectorDefaultKey: CFStringRef;
    pub static kCTFontFeatureSelectorSettingKey: CFStringRef;
    pub static kCTFontFeatureSampleTextKey: CFStringRef;
    pub static kCTFontFeatureTooltipTextKey: CFStringRef;

    pub fn CTFontCopyFeatures(font: CTFontRef) -> CFArrayRef;
    pub fn CTFontCopyFeatureSettings(font: CTFontRef) -> CFArrayRef;

    pub fn CTFontCopyGraphicsFont(
        font: CTFontRef,
        attributes: *mut CTFontDescriptorRef,
    ) -> CGFontRef;
    pub fn CTFontCreateWithGraphicsFont(
        graphics_font: CGFontRef,
        size: CGFloat,
        matrix: *const CGAffineTransform,
        attributes: CTFontDescriptorRef,
    ) -> CTFontRef;
}
