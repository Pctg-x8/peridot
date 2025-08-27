use cfg_if::cfg_if;

use crate::{FontConstructionError, FontProperties};

#[doc(hidden)]
pub trait FontProviderConstruct: Sized + FontProvider {
    /// Creates font provider
    fn new() -> Result<Self, FontConstructionError>;
}

/// Represents Font Provider(layered on DirectWrite FontCollection / Fontconfig).
///
/// To use this functionality, `DefaultFontProvider` is exported for some platforms.
pub trait FontProvider {
    /// Associated font type for this provider
    type Font: crate::Font;

    /// Create a best-matching font for family name and provided properties
    fn best_match(
        &mut self,
        family_name: &str,
        properties: &FontProperties,
        size: f32,
    ) -> Result<Self::Font, FontConstructionError>;

    /// Load a font from a specific asset
    fn load<NL: peridot::NativeLinker>(
        &self,
        e: &peridot::Engine<NL>,
        asset_path: &str,
        size: f32,
    ) -> Result<Self::Font, FontConstructionError>;
}

#[cfg(test)]
pub struct TestFont;
#[cfg(test)]
impl crate::Font for TestFont {
    type GlyphID = u32;

    fn advance_h(&self, _glyph: &Self::GlyphID) -> Result<f32, super::GlyphLoadingError> {
        Ok(0.0)
    }

    fn ascent(&self) -> f32 {
        0.0
    }

    fn bounds(
        &self,
        _glyph: &Self::GlyphID,
    ) -> Result<euclid::Rect<f32>, super::GlyphLoadingError> {
        Ok(euclid::Rect::zero())
    }

    fn glyph_id(&self, _c: char) -> Option<Self::GlyphID> {
        None
    }

    fn outline(
        &self,
        _glyph: &Self::GlyphID,
        _transform: &euclid::Transform2D<f32>,
        _builder: &mut impl lyon_path::builder::PathBuilder,
    ) -> Result<(), super::GlyphLoadingError> {
        Ok(())
    }

    fn set_em_size(&mut self, _size: f32) {}

    fn size(&self) -> f32 {
        0.0
    }

    fn units_per_em(&self) -> u32 {
        0
    }
}
#[cfg(test)]
pub struct DefaultFontProvider;
#[cfg(test)]
impl FontProvider for DefaultFontProvider {
    type Font = TestFont;

    fn best_match(
        &mut self,
        _family_name: &str,
        _properties: &FontProperties,
        _size: f32,
    ) -> Result<Self::Font, FontConstructionError> {
        Ok(TestFont)
    }

    fn load<NL: peridot::NativeLinker>(
        &self,
        _e: &peridot::Engine<NL>,
        _asset_path: &str,
        _size: f32,
    ) -> Result<Self::Font, FontConstructionError> {
        Ok(TestFont)
    }
}
#[cfg(test)]
impl FontProviderConstruct for DefaultFontProvider {
    fn new() -> Result<Self, FontConstructionError> {
        Ok(Self)
    }
}

cfg_if! {
    if #[cfg(all(windows, not(feature = "use-freetype")))] {
        // activate DirectWrite backend
        mod dwrite;
        pub use self::dwrite::*;
    }
}
#[cfg(feature = "use-freetype")]
cfg_if! {
    if #[cfg(feature = "use-fontconfig")] {
        // activate FreeType/Fontconfig backend
        mod fontconfig;
        pub use self::fontconfig::*;
    } else {
        // activate freetype-only backend
        mod freetype_only;
        pub use self::freetype_only::*;
    }
}
cfg_if! {
    if #[cfg(all(target_os = "macos", not(feature = "use-freetype")))] {
        // activate CoreText backend
        mod core_text;
        pub use self::core_text::*;
    }
}

#[cfg(not(test))]
cfg_if! {
    if #[cfg(feature = "use-freetype")] {
        #[cfg(feature = "use-fontconfig")]
        pub type DefaultFontProvider = FontconfigFontProvider;
        #[cfg(not(feature = "use-fontconfig"))]
        pub type DefaultFontProvider = FreetypeOnlyFontProvider;
    } else if #[cfg(windows)] {
        pub type DefaultFontProvider = DirectWriteFontProvider;
    } else if #[cfg(target_os = "macos")] {
        pub type DefaultFontProvider = CoreTextFontProvider;
    }
}
