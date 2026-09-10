//! CoreText Font Provider impl

use apple_sdk_port::foundation;
use apple_sdk_port::raw::core_text::kCTFontTraitItalic;
use apple_sdk_port::text as native_text;

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
        let mut traits = foundation::MutableDictionary::new_generic_key_value(None, 2).ok_or(
            FontConstructionError::SysAPICallError("MutableDictionary::new(traits)"),
        )?;
        traits.set(
            native_text::font_weight_trait(),
            &*foundation::Number::new_f32(None, properties.native_weight()).ok_or(
                FontConstructionError::SysAPICallError("Number::new(native_weight)"),
            )?,
        );
        traits.set(
            native_text::font_symbolic_trait(),
            &*foundation::Number::new_u32(
                None,
                if properties.italic {
                    kCTFontTraitItalic
                } else {
                    0
                },
            )
            .ok_or(FontConstructionError::SysAPICallError(
                "Number::new(symbolic_traits)",
            ))?,
        );

        let mut attrs =
            foundation::MutableDictionary::<_, dyn apple_sdk_port::Object>::new_generic_key_value(
                None, 2,
            )
            .ok_or(FontConstructionError::SysAPICallError(
                "MutableDictionary::new(attrs)",
            ))?;
        attrs.set(
            native_text::FontDescriptor::family_name_attribute(),
            unsafe { &*foundation::String::from_str_no_copy(None, family_name) },
        );
        attrs.set(native_text::FontDescriptor::traits_attribute(), &*traits);

        let fd = native_text::FontDescriptor::from_attributes(&attrs).ok_or(
            FontConstructionError::SysAPICallError("FontDescriptor::from_attributes"),
        )?;

        native_text::Font::from_font_descriptor(&fd, size as _, None)
            .ok_or(FontConstructionError::SysAPICallError(
                "Font::from_font_descriptor",
            ))
            .map(CoreTextFont)
    }

    fn load<NL: peridot::NativeLinker>(
        &self,
        e: &peridot::Engine<NL>,
        asset_path: &str,
        size: f32,
    ) -> Result<Self::Font, FontConstructionError> {
        let a: TTFBlob = e.load(asset_path)?;
        let d = foundation::Data::new(None, &a.0)
            .ok_or(FontConstructionError::SysAPICallError("Data::new"))?;
        let fd = native_text::FontDescriptor::from_data(&d).ok_or(
            FontConstructionError::SysAPICallError("CTFontManagerCreateFontDescriptorFromData"),
        )?;

        native_text::Font::from_font_descriptor(&fd, size as _, None)
            .ok_or(FontConstructionError::SysAPICallError(
                "Font::from_font_descriptor",
            ))
            .map(CoreTextFont)
    }
}
