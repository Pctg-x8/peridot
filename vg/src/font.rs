
use font_kit::{
    source::SystemSource, font::Font as UnderlyingHandle, family_name::FamilyName, properties::Properties,
    error::{FontLoadingError, SelectionError}
};

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
}
