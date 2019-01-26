
use font_kit::{
    source::SystemSource, font::Font as UnderlyingHandle,
    error::{FontLoadingError, SelectionError, GlyphLoadingError}
};
pub use font_kit::{family_name::FamilyName, properties::Properties as FontProperties, hinting::HintingOptions};
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
pub struct Font(UnderlyingHandle);
impl Font {
    pub fn best_match(family_names: &[FamilyName], properties: &FontProperties) -> Result<Self, FontConstructionError> {
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

    pub(crate) fn calc_text_render_offsets(&self, px_size: f32) -> GlyphTransform {
        let pixels_per_unit = px_size / self.0.metrics().units_per_em as f32;
        let mut stem_darkening_offset = embolden_amount(px_size, pixels_per_unit);
        let ascent = self.0.metrics().ascent;
        let sd_yscale = (ascent + stem_darkening_offset[1]) / (ascent * screen_multiplier());
        stem_darkening_offset[0] *= pixels_per_unit / 2.0f32.sqrt();
        stem_darkening_offset[1] *= sd_yscale * pixels_per_unit / 2.0f32.sqrt();
        GlyphTransform
        {
            st: [1.0, sd_yscale, stem_darkening_offset[0], stem_darkening_offset[1]], ext: [0.0; 2], pad: [0.0; 2]
        }
    }
}

#[cfg(not(target_os = "macos"))] fn screen_multiplier() -> f32 { 1.0 }
#[cfg(target_os = "macos")] fn screen_multiplier() -> f32 {
    extern crate appkit;
    appkit::NSScreen::main().backing_scale_factor() as _
}

fn compute_stem_darkening_amount(pixels_per_em: f32, pixels_per_unit: f32) -> [f32; 2]
{
    const LIMIT_SIZE: f32 = 72.0;
    let amounts: [f32; 2] = [(0.0121 * 2.0f32.sqrt()) * (2.0 / screen_multiplier()),
        (0.0121 * 1.25 * 2.0f32.sqrt()) * (2.0 / screen_multiplier())];

    if pixels_per_em <= LIMIT_SIZE
    {
        let scaled_amount = |a| f32::min(a * pixels_per_em, LIMIT_SIZE) / pixels_per_unit;
        [scaled_amount(amounts[0]), scaled_amount(amounts[1])]
    }
    else { [0.0; 2] }
}
#[cfg(feature = "StemDarkening")]
fn stem_darkening_amount(font_size: f32, pixels_per_unit: f32) -> [f32; 2]
{
    compute_stem_darkening_amount(font_size, pixels_per_unit)
}
#[cfg(not(feature = "StemDarkening"))]
fn stem_darkening_amount(_font_size: f32, _pixels_per_unit: f32) -> [f32; 2] { [0.0; 2] }
fn embolden_amount(font_size: f32, pixels_per_unit: f32) -> [f32; 2] {
    stem_darkening_amount(font_size, pixels_per_unit)
}
