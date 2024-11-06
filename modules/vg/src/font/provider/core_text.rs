//! CoreText Font Provider impl

use objc_ext::ObjcObject;

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
        &self,
        family_name: &str,
        properties: &FontProperties,
        size: f32,
    ) -> Result<Self::Font, FontConstructionError> {
        let traits = appkit::NSMutableDictionary::with_capacity(2).map_err(|_| {
            FontConstructionError::SysAPICallError("NSMutableDictionary::with_capacity")
        })?;
        let weight_num = appkit::NSNumber::from_float(properties.native_weight())
            .map_err(|_| FontConstructionError::SysAPICallError("NSNumber::from_float"))?;
        let symbolic_traits = appkit::NSNumber::from_uint(if properties.italic {
            appkit::CTFontSymbolicTraits::ItalicTrait as u32
        } else {
            0u32
        } as _)
        .map_err(|_| FontConstructionError::SysAPICallError("NSNumber::from_uint"))?;
        traits.set(
            AsRef::as_ref(unsafe { &*appkit::kCTFontWeightTrait }),
            weight_num.as_id(),
        );
        traits.set(
            AsRef::as_ref(unsafe { &*appkit::kCTFontSymbolicTrait }),
            symbolic_traits.as_id(),
        );
        let attrs = appkit::NSMutableDictionary::with_capacity(2).map_err(|_| {
            FontConstructionError::SysAPICallError("NSMutableDictionary::with_capacity")
        })?;
        let family_name_nsstr = appkit::NSString::from_str(family_name)
            .map_err(|_| FontConstructionError::SysAPICallError("NSString::from_str"))?;
        attrs.set(
            unsafe { &*appkit::kCTFontFamilyNameAttribute }.as_ref(),
            family_name_nsstr.as_id(),
        );
        attrs.set(
            unsafe { &*appkit::kCTFontTraitsAttribute }.as_ref(),
            traits.as_id(),
        );

        let fd =
            appkit::CTFontDescriptor::with_attributes(AsRef::as_ref(&**attrs)).map_err(|_| {
                FontConstructionError::SysAPICallError("CTFontDescriptor::with_attributes")
            })?;
        appkit::CTFont::from_font_descriptor(&fd, size as _, None)
            .map_err(|_| FontConstructionError::SysAPICallError("CTFont::from_font_descriptor"))
            .map(|x| CoreTextFont(x))
    }

    fn load<NL: peridot::NativeLinker>(
        &self,
        e: &peridot::Engine<NL>,
        asset_path: &str,
        size: f32,
    ) -> Result<Self::Font, FontConstructionError> {
        let a: TTFBlob = e.load(asset_path)?;
        let d = appkit::CFData::new(&a.0)
            .ok_or(FontConstructionError::SysAPICallError("CFData::new"))?;
        let fd = appkit::CTFontDescriptor::from_data(&d).ok_or(
            FontConstructionError::SysAPICallError("CTFontDescriptor::from_data"),
        )?;

        appkit::CTFont::from_font_descriptor(&fd, size as _, None)
            .map_err(|_| FontConstructionError::SysAPICallError("CTFont::from_font_descriptor"))
            .map(|x| CoreTextFont(x))
    }
}
