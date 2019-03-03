
use font_kit::{
    source::SystemSource, font::Font as UnderlyingHandle,
    error::{FontLoadingError, SelectionError, GlyphLoadingError}
};
pub use font_kit::{
    family_name::FamilyName, properties::Properties as FontProperties, hinting::HintingOptions,
    properties::{Style, Weight, Stretch}
};
use euclid::Rect;
use peridot_math::{Vector2, Vector2F32};
use lyon_path::builder::PathBuilder;

pub struct GlyphBound<T> { left: T, top: T, right: T, bottom: T }
impl<T: Copy> GlyphBound<T> {
    pub fn offset(&self) -> Vector2<T> { Vector2(self.left, self.top) }
    pub fn size(&self) -> Vector2<T> where T: std::ops::Sub<Output = T> {
        Vector2(self.right - self.left, self.bottom - self.top)
    }
}
#[repr(C)] #[derive(Clone, Debug)]
pub(crate) struct GlyphTransform { pub st: [f32; 4], pub ext: [f32; 2], pub pad: [f32; 2] }

#[derive(Debug)]
pub enum FontConstructionError { Selection(SelectionError), Loading(FontLoadingError) }
impl From<SelectionError> for FontConstructionError {
    fn from(v: SelectionError) -> Self { FontConstructionError::Selection(v) }
}
impl From<FontLoadingError> for FontConstructionError {
    fn from(v: FontLoadingError) -> Self { FontConstructionError::Loading(v) }
}
pub struct Font(UnderlyingHandle, f32);
impl Font {
    pub fn best_match(family_names: &[FamilyName], properties: &FontProperties, units_per_em: f32)
            -> Result<Self, FontConstructionError> {
        SystemSource::new().select_best_match(family_names, properties)?
            .load().map(|x| Font(x, units_per_em)).map_err(From::from)
    }
    pub fn set_em_size(&mut self, size: f32) { self.1 = size; }
    pub fn em_size(&self) -> f32 { self.1 }
    pub(crate) fn scale_value(&self) -> f32 { self.1 / self.units_per_em() as f32 }
    pub fn ascent(&self) -> f32 { self.0.metrics().ascent }
    pub fn baseline_offset(&self) -> f32 { self.0.metrics().ascent * self.scale_value() }

    pub(crate) fn glyph_id(&self, c: char) -> Option<u32> { self.0.glyph_for_char(c) }
    pub(crate) fn advance(&self, glyph: u32) -> Result<Vector2F32, GlyphLoadingError> {
        self.0.advance(glyph).map(|v| Vector2(v.x, v.y))
    }
    pub(crate) fn bounds(&self, glyph: u32) -> Result<Rect<f32>, GlyphLoadingError> {
        let r = self.0.typographic_bounds(glyph)?;
        Ok(Rect::new(r.origin, r.size))
    }
    pub(crate) fn outline<B: PathBuilder>(&self, glyph: u32, hint_opts: HintingOptions, builder: &mut B)
            -> Result<(), GlyphLoadingError> {
        self.0.outline(glyph, hint_opts, builder)
    }
    pub fn units_per_em(&self) -> u32 { self.0.metrics().units_per_em }

    pub fn full_name(&self) -> String { self.0.full_name() }
    pub fn properties(&self) -> FontProperties { self.0.properties() }
}
