//! fontconfig Font Provider impl

use peridot_tp_fontconfig as fc;

use crate::{
    font::freetype::FreetypeFont, FontConstructionError, FontProvider, FontProviderConstruct,
    TTFBlob,
};

use super::super::freetype;

pub struct FontconfigFontProvider {
    ft: freetype::System,
    fc: core::ptr::NonNull<fc::Config>,
}
impl FontProviderConstruct for FontconfigFontProvider {
    fn new() -> Result<Self, FontConstructionError> {
        unsafe {
            fc::init().expect("fontconfig init failed");
        }

        Ok(Self {
            ft: freetype::System::new(),
            fc: unsafe { fc::Config::current().expect("fontconfig not active") },
        })
    }
}
impl FontProvider for FontconfigFontProvider {
    type Font = FreetypeFont;

    fn best_match(
        &mut self,
        family_name: &str,
        properties: &crate::FontProperties,
        size: f32,
    ) -> Result<Self::Font, crate::FontConstructionError> {
        let c_family_name = std::ffi::CString::new(family_name).expect("FFI Conversion failure");
        let mut pat = fc::Pattern::new()
            .ok_or_else(|| FontConstructionError::SysAPICallError("FcPatternCreate"))?;
        pat.add(fc::Pattern::KEY_FAMILY, &c_family_name)
            .map_err(|_| FontConstructionError::SysAPICallError("FcPatternAdd"))?;
        pat.add(fc::Pattern::KEY_WEIGHT, &properties.weight)
            .map_err(|_| FontConstructionError::SysAPICallError("FcPatternAdd"))?;
        pat.add(fc::Pattern::KEY_SLANT, &properties.italic)
            .map_err(|_| FontConstructionError::SysAPICallError("FcPatternAdd"))?;
        pat.add(fc::Pattern::KEY_SIZE, &(size as core::ffi::c_double))
            .map_err(|_| FontConstructionError::SysAPICallError("FcPatternAdd"))?;
        unsafe {
            self.fc
                .as_mut()
                .substitute(&mut pat, fc::MatchKind::Pattern)
                .map_err(|_| FontConstructionError::SysAPICallError("FcConfigSubstitute"))?;
        }
        pat.default_substitute();
        let mut fonts = unsafe {
            fc::sort(self.fc.as_mut(), &mut pat, false, None)
                .map_err(|_| FontConstructionError::SysAPICallError("FcFontSort"))?
        };

        let group_desc = fonts
            .iter()
            .map(|f| {
                let face_index = f
                    .get::<core::ffi::c_int>(fc::Pattern::KEY_INDEX)
                    .map_err(|_| FontConstructionError::SysAPICallError("FcPatternGetInteger"))?
                    .ok_or_else(|| FontConstructionError::SysAPICallError("FcPatternGetInteger"))?;
                let font_path = f
                    .get::<&core::ffi::CStr>(fc::Pattern::KEY_FILE)
                    .map_err(|_| FontConstructionError::SysAPICallError("FcPatternGetString"))?
                    .ok_or_else(|| FontConstructionError::SysAPICallError("FcPatternGetString"))?;

                Ok(freetype::FaceGroupEntry::unloaded(
                    font_path,
                    face_index as _,
                ))
            })
            .collect::<Result<_, FontConstructionError>>()?;
        let face = self.ft.new_face_group(group_desc);
        face.set_size(size);

        Ok(FreetypeFont(face, size))
    }

    fn load<NL: peridot::NativeLinker>(
        &self,
        e: &peridot::Engine<NL>,
        asset_path: &str,
        size: f32,
    ) -> Result<Self::Font, crate::FontConstructionError> {
        let a: TTFBlob = e.load(asset_path)?;
        let f = self
            .ft
            .new_face_from_mem(&a.0, 0)
            .map_err(FontConstructionError::FT2)?;
        let face = self
            .ft
            .new_face_group(vec![freetype::FaceGroupEntry::LoadedMem(f, a.0.into())]);
        face.set_size(size);

        Ok(FreetypeFont(face, size))
    }
}
