//! CoreText Font Provider impl

use apple_sdk_port::{
    kCFAllocatorDefault, kCFAllocatorNull, kCFNumberFloat32Type, kCFNumberSInt64Type,
    kCFStringEncodingUTF8, kCFTypeDictionaryKeyCallBacks, kCFTypeDictionaryValueCallBacks,
    kCTFontFamilyNameAttribute, kCTFontSymbolicTrait, kCTFontTraitItalic, kCTFontTraitsAttribute,
    kCTFontWeightTrait, CFDataCreate, CFDictionaryCreateMutable, CFDictionarySetValue,
    CFNumberCreate, CFOwned, CFStringCreateWithBytesNoCopy, CTFontCreateWithFontDescriptor,
    CTFontDescriptorCreateWithAttributes, CTFontManagerCreateFontDescriptorFromData,
};

use crate::{
    font::core_text::CoreTextFont, FontConstructionError, FontProperties, FontProvider,
    FontProviderConstruct, TTFBlob,
};

pub struct CoreTextFontProvider;
impl FontProviderConstruct for CoreTextFontProvider {
    fn new() -> Result<Self, FontConstructionError> {
        Ok(Self)
    }
}
impl FontProvider for CoreTextFontProvider {
    type Font = CoreTextFont;

    fn best_match(
        &mut self,
        family_name: &str,
        properties: &FontProperties,
        size: f32,
    ) -> Result<Self::Font, FontConstructionError> {
        let mut traits = unsafe {
            CFOwned::from_ptr(CFDictionaryCreateMutable(
                kCFAllocatorDefault,
                2,
                &kCFTypeDictionaryKeyCallBacks,
                &kCFTypeDictionaryValueCallBacks,
            ))
            .ok_or_else(|| {
                FontConstructionError::SysAPICallError("CFDictionaryCreateMutable(traits)")
            })?
        };
        let weight_num = unsafe {
            CFOwned::from_ptr(
                CFNumberCreate(
                    kCFAllocatorDefault,
                    kCFNumberFloat32Type,
                    (&properties.native_weight()) as *const _ as _,
                )
                .cast_mut(),
            )
            .ok_or_else(|| {
                FontConstructionError::SysAPICallError("CFNumberCreate(native_weight)")
            })?
        };
        let symbolic_traits = unsafe {
            // Note: swift-corelibs-foundationの実装を覗いた限りでは、どうやら符号なし指定ビット長の整数はより長いビット長の符号付き整数で表せばいいらしい
            CFOwned::from_ptr(
                CFNumberCreate(
                    kCFAllocatorDefault,
                    kCFNumberSInt64Type,
                    (&(if properties.italic {
                        kCTFontTraitItalic
                    } else {
                        0
                    } as u64)
                        .cast_signed()) as *const _ as _,
                )
                .cast_mut(),
            )
            .ok_or_else(|| {
                FontConstructionError::SysAPICallError("CFNumberCreate(symbolic_traits)")
            })?
        };
        unsafe {
            CFDictionarySetValue(
                traits.as_mut_ptr(),
                kCTFontWeightTrait.cast(),
                weight_num.as_ptr().cast(),
            );
        }
        unsafe {
            CFDictionarySetValue(
                traits.as_mut_ptr(),
                kCTFontSymbolicTrait.cast(),
                symbolic_traits.as_ptr().cast(),
            );
        }
        let mut attrs = unsafe {
            CFOwned::from_ptr(CFDictionaryCreateMutable(
                kCFAllocatorDefault,
                2,
                &kCFTypeDictionaryKeyCallBacks,
                &kCFTypeDictionaryValueCallBacks,
            ))
            .ok_or_else(|| {
                FontConstructionError::SysAPICallError("CFDictionaryCreateMutable(attrs)")
            })?
        };
        let family_name_nsstr = unsafe {
            CFOwned::from_ptr(
                CFStringCreateWithBytesNoCopy(
                    kCFAllocatorDefault,
                    family_name.as_bytes().as_ptr(),
                    family_name.len() as _,
                    kCFStringEncodingUTF8,
                    false as _,
                    kCFAllocatorNull,
                )
                .cast_mut(),
            )
            .ok_or_else(|| {
                FontConstructionError::SysAPICallError(
                    "CFStringCreateWithBytesNoCopy(family_name_nsstr)",
                )
            })?
        };
        unsafe {
            CFDictionarySetValue(
                attrs.as_mut_ptr(),
                kCTFontFamilyNameAttribute.cast(),
                family_name_nsstr.as_ptr().cast(),
            );
            CFDictionarySetValue(
                attrs.as_mut_ptr(),
                kCTFontTraitsAttribute.cast(),
                traits.as_ptr().cast(),
            );
        }
        let fd = unsafe {
            CFOwned::from_ptr(CTFontDescriptorCreateWithAttributes(attrs.as_ptr()).cast_mut())
                .ok_or_else(|| {
                    FontConstructionError::SysAPICallError("CTFontDescriptorCreateWithAttributes")
                })?
        };

        unsafe {
            CFOwned::from_ptr(
                CTFontCreateWithFontDescriptor(fd.as_ptr(), size as _, core::ptr::null())
                    .cast_mut(),
            )
            .ok_or_else(|| FontConstructionError::SysAPICallError("CTFont::from_font_descriptor"))
            .map(|x| CoreTextFont(x))
        }
    }

    fn load<NL: peridot::NativeLinker>(
        &self,
        e: &peridot::Engine<NL>,
        asset_path: &str,
        size: f32,
    ) -> Result<Self::Font, FontConstructionError> {
        let a: TTFBlob = e.load(asset_path)?;
        let d = unsafe {
            CFOwned::from_ptr(
                CFDataCreate(kCFAllocatorDefault, a.0.as_ptr(), a.0.len() as _).cast_mut(),
            )
            .ok_or_else(|| FontConstructionError::SysAPICallError("CFDataCreate"))?
        };
        let fd = unsafe {
            CFOwned::from_ptr(CTFontManagerCreateFontDescriptorFromData(d.as_ptr()).cast_mut())
                .ok_or_else(|| {
                    FontConstructionError::SysAPICallError(
                        "CTFontManagerCreateFontDescriptorFromData",
                    )
                })?
        };

        unsafe {
            CFOwned::from_ptr(
                CTFontCreateWithFontDescriptor(fd.as_ptr(), size as _, core::ptr::null())
                    .cast_mut(),
            )
            .ok_or_else(|| FontConstructionError::SysAPICallError("CTFont::from_font_descriptor"))
            .map(|x| CoreTextFont(x))
        }
    }
}
