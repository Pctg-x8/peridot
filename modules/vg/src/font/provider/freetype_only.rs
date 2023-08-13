//! Dummy Font Provider for freetype-only environments

use super::super::freetype;
use crate::{
    font::freetype::FreetypeFont, FontConstructionError, FontProvider, FontProviderConstruct,
    TTFBlob,
};

pub struct FreetypeOnlyFontProvider(freetype::System);
impl FontProviderConstruct for FreetypeOnlyFontProvider {
    fn new() -> Result<Self, FontConstructionError> {
        Ok(Self(freetype::System::new()))
    }
}
impl FontProvider for FreetypeOnlyFontProvider {
    type Font = FreetypeFont;

    fn best_match(
        &self,
        _family_name: &str,
        _properties: &crate::FontProperties,
        _size: f32,
    ) -> Result<Self::Font, crate::FontConstructionError> {
        Err(FontConstructionError::MatcherUnavailable)
    }

    fn load<NL: peridot::NativeLinker>(
        &self,
        e: &peridot::Engine<NL>,
        asset_path: &str,
        size: f32,
    ) -> Result<Self::Font, crate::FontConstructionError> {
        let a: TTFBlob = e.load(asset_path)?;
        let f = self
            .0
            .new_face_from_mem(&a.0, 0)
            .map_err(FontConstructionError::FT2)?;
        let face = self
            .0
            .new_face_group(vec![freetype::FaceGroupEntry::LoadedMem(f, a.0.into())]);
        face.set_size(size);

        Ok(FreetypeFont(face, size))
    }
}
