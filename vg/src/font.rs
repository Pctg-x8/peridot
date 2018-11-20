
use font_kit::{
    source::SystemSource, font::Font as UnderlyingHandle, family_name::FamilyName, properties::Properties,
    hinting::HintingOptions,
    error::{FontLoadingError, SelectionError, GlyphLoadingError}
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

pub enum FontConstructionError { Selection(SelectionError), Loading(FontLoadingError) }
impl From<SelectionError> for FontConstructionError {
    fn from(v: SelectionError) -> Self { FontConstructionError::Selection(v) }
}
impl From<FontLoadingError> for FontConstructionError {
    fn from(v: FontLoadingError) -> Self { FontConstructionError::Loading(v) }
}
pub struct Font(UnderlyingHandle);
impl Font {
    pub fn best_match(family_names: &[FamilyName], properties: &Properties) -> Result<Self, FontConstructionError> {
        SystemSource::new().select_best_match(family_names, properties)?
            .load().map(Font).map_err(From::from)
    }

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
}
