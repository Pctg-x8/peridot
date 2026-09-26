#![allow(non_upper_case_globals)]

use crate::{
    FFIOpaqueStruct,
    raw::{
        CFArrayRef, CFDictionaryRef, CFNumberRef, CFSetRef, CFStringRef, CFTypeID, CFTypeRef,
        CGFloat, CTFontSymbolicTraits,
    },
};
use core::ffi::*;

#[repr(C)]
pub struct __CTFontDescriptor(FFIOpaqueStruct);
pub type CTFontDescriptorRef = *const __CTFontDescriptor;

pub type CTFontOrientation = u32;
pub const kCTFontOrientationDefault: CTFontOrientation = 0;
pub const kCTFontOrientationHorizontal: CTFontOrientation = 1;
pub const kCTFontOrientationVertical: CTFontOrientation = 2;

pub type CTFontFormat = u32;
pub const kCTFontFormatUnrecognized: CTFontFormat = 0;
pub const kCTFontFormatOpenTypePostScript: CTFontFormat = 1;
pub const kCTFontFormatOpenTypeTrueType: CTFontFormat = 2;
pub const kCTFontFormatTrueType: CTFontFormat = 3;
pub const kCTFontFormatPostScript: CTFontFormat = 4;
pub const kCTFontFormatBitmap: CTFontFormat = 5;

pub type CTFontPriority = u32;
pub const kCTFontPrioritySystem: CTFontPriority = 10000;
pub const kCTFontPriorityNetwork: CTFontPriority = 20000;
pub const kCTFontPriorityComputer: CTFontPriority = 30000;
pub const kCTFontPriorityUser: CTFontPriority = 40000;
pub const kCTFontPriorityDynamic: CTFontPriority = 50000;
pub const kCTFontPriorityProcess: CTFontPriority = 60000;

pub type CTFontDescriptorMatchingState = u32;
pub const kCTFontDescriptorMatchingDidBegin: CTFontDescriptorMatchingState = 0;
pub const kCTFontDescriptorMatchingDidFinish: CTFontDescriptorMatchingState = 1;
pub const kCTFontDescriptorMatchingWillBeginQuerying: CTFontDescriptorMatchingState = 2;
pub const kCTFontDescriptorMatchingStalled: CTFontDescriptorMatchingState = 3;
pub const kCTFontDescriptorMatchingWillBeginDownloading: CTFontDescriptorMatchingState = 4;
pub const kCTFontDescriptorMatchingDownloading: CTFontDescriptorMatchingState = 5;
pub const kCTFontDescriptorMatchingDidFinishDownloading: CTFontDescriptorMatchingState = 6;
pub const kCTFontDescriptorMatchingDidMatch: CTFontDescriptorMatchingState = 7;
pub const kCTFontDescriptorMatchingDidFailWithError: CTFontDescriptorMatchingState = 8;

/// Blocks: `(state: CTFontDescriptorMatchingState, progress_parameter: CFDictionaryRef) -> bool``
pub type CTFontDescriptorProgressHandler = *mut c_void;

unsafe extern "C" {
    pub fn CTFontDescriptorGetTypeID() -> CFTypeID;

    pub static kCTFontURLAttribute: CFStringRef;
    pub static kCTFontNameAttribute: CFStringRef;
    pub static kCTFontDisplayNameAttribute: CFStringRef;
    pub static kCTFontFamilyNameAttribute: CFStringRef;
    pub static kCTFontStyleNameAttribute: CFStringRef;
    pub static kCTFontTraitsAttribute: CFStringRef;
    pub static kCTFontVariationAttribute: CFStringRef;
    pub static kCTFontVariationAxesAttribute: CFStringRef;
    pub static kCTFontSizeAttribute: CFStringRef;
    pub static kCTFontMatrixAttribute: CFStringRef;
    pub static kCTFontCascadeListAttribute: CFStringRef;
    pub static kCTFontCharacterSetAttribute: CFStringRef;
    pub static kCTFontLanguagesAttribute: CFStringRef;
    pub static kCTFontBaselineAdjustAttribute: CFStringRef;
    pub static kCTFontMacintoshEncodingsAttribute: CFStringRef;
    pub static kCTFontFeaturesAttribute: CFStringRef;
    pub static kCTFontFeatureSettingsAttribute: CFStringRef;
    pub static kCTFontFixedAdvanceAttribute: CFStringRef;
    pub static kCTFontOrientationAttribute: CFStringRef;
    pub static kCTFontFormatAttribute: CFStringRef;
    pub static kCTFontRegistrationScopeAttribute: CFStringRef;
    pub static kCTFontPriorityAttribute: CFStringRef;
    pub static kCTFontEnabledAttribute: CFStringRef;
    pub static kCTFontDownloadableAttribute: CFStringRef;
    pub static kCTFontDownloadedAttribute: CFStringRef;
    pub static kCTFontOpticalSizeAttribute: CFStringRef;

    pub fn CTFontDescriptorCreateWithNameAndSize(
        name: CFStringRef,
        size: CGFloat,
    ) -> CTFontDescriptorRef;
    pub fn CTFontDescriptorCreateWithAttributes(attributes: CFDictionaryRef)
    -> CTFontDescriptorRef;
    pub fn CTFontDescriptorCreateCopyWithAttributes(
        original: CTFontDescriptorRef,
        attributes: CFDictionaryRef,
    ) -> CTFontDescriptorRef;
    pub fn CTFontCescriptorCreateCopyWithFamily(
        original: CTFontDescriptorRef,
        family: CFStringRef,
    ) -> CTFontDescriptorRef;
    pub fn CTFontDescriptorCreateCopyWithSymbolicTraits(
        original: CTFontDescriptorRef,
        trait_value: CTFontSymbolicTraits,
        trait_mask: CTFontSymbolicTraits,
    ) -> CTFontDescriptorRef;
    pub fn CTFontDescriptorCreateCopyWithVariation(
        original: CTFontDescriptorRef,
        variation_identifier: CFNumberRef,
        variation_value: CGFloat,
    ) -> CTFontDescriptorRef;
    pub fn CTFontDescriptorCreateCopyWithFeature(
        original: CTFontDescriptorRef,
        feature_type_identifier: CFNumberRef,
        feature_selector_identifier: CFNumberRef,
    ) -> CTFontDescriptorRef;
    pub fn CTFontDescriptorCreateMatchingFontDescriptors(
        descriptor: CTFontDescriptorRef,
        mandatory_attributes: CFSetRef,
    ) -> CFArrayRef;
    pub fn CTFontDescriptorCreateMatchingFontDescriptor(
        descriptor: CTFontDescriptorRef,
        mandatory_attributes: CFSetRef,
    ) -> CTFontDescriptorRef;

    pub static kCTFontDescriptorMatchingSourceDescriptor: CFStringRef;
    pub static kCTFontDescriptorMatchingDescriptors: CFStringRef;
    pub static kCTFontDescriptorMatchingResult: CFStringRef;
    pub static kCTFontDescriptorMatchingPercentage: CFStringRef;
    pub static kCTFontDescriptorMatchingCurrentAssetSize: CFStringRef;
    pub static kCTFontDescriptorMatchingTotalDownloadedSize: CFStringRef;
    pub static kCTFontDescriptorMatchingTotalAssetSize: CFStringRef;
    pub static kCTFontDescriptorMatchingError: CFStringRef;

    pub fn CTFontDescriptorMatchFontDescriptorsWithProgressHandler(
        descriptors: CFArrayRef,
        mandatory_attributes: CFSetRef,
        progress_block: CTFontDescriptorProgressHandler,
    ) -> bool;

    pub fn CTFontDescriptorCopyAttributes(descriptor: CTFontDescriptorRef) -> CFDictionaryRef;
    pub fn CTFontDescriptorCopyAttribute(
        descriptor: CTFontDescriptorRef,
        attribute: CFStringRef,
    ) -> CFTypeRef;
    pub fn CTFontDescriptorCopyLocalizedAttribute(
        descriptor: CTFontDescriptorRef,
        attribute: CFStringRef,
        language: *mut CFStringRef,
    ) -> CFTypeRef;
}
