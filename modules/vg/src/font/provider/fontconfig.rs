//! fontconfig Font Provider impl

use peridot_tp_fontconfig as fc;

use crate::{
    font::freetype::FreetypeFont, FontConstructionError, FontProperties, FontProvider,
    FontProviderConstruct, TTFBlob,
};

use super::super::freetype;

pub struct FontconfigFontProvider {
    ft: freetype::System,
    fc: core::ptr::NonNull<fc::Config>,
}
impl Drop for FontconfigFontProvider {
    #[inline(always)]
    fn drop(&mut self) {
        unsafe { fc::fini() }
    }
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
        let mut pat = Pattern(
            fc::Pattern::new()
                .ok_or_else(|| FontConstructionError::SysAPICallError("FcPatternCreate"))?,
        );
        pat.add_family_name(&c_family_name)
            .map_err(|_| FontConstructionError::SysAPICallError("FcPatternAdd.family"))?;
        pat.add_properties(properties)
            .map_err(|_| FontConstructionError::SysAPICallError("FcPatternAdd.properties"))?;
        pat.add_size(size)
            .map_err(|_| FontConstructionError::SysAPICallError("FcPatternAdd.size"))?;
        let fonts = pat.perform_match(unsafe { self.fc.as_mut() })?;

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
    ) -> Result<Self::Font, FontConstructionError> {
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

#[repr(transparent)]
pub struct Pattern(core::ptr::NonNull<fc::Pattern>);
impl Drop for Pattern {
    #[inline(always)]
    fn drop(&mut self) {
        unsafe { fc::Pattern::destroy(self.0.as_mut()) }
    }
}
impl Pattern {
    #[inline(always)]
    pub fn add_family_name(&mut self, family_name: &core::ffi::CStr) -> Result<(), ()> {
        unsafe { self.0.as_mut().add(fc::Pattern::KEY_FAMILY, family_name) }
    }

    #[inline(always)]
    pub fn add_properties(&mut self, props: &FontProperties) -> Result<(), ()> {
        unsafe {
            self.0
                .as_mut()
                .add(fc::Pattern::KEY_WEIGHT, &props.weight)?;
            self.0.as_mut().add(
                fc::Pattern::KEY_SLANT,
                &(if props.italic {
                    fc::raw::FC_SLANT_ITALIC
                } else {
                    0
                }),
            )?;
        }

        Ok(())
    }

    #[inline(always)]
    pub fn add_size(&mut self, size: f32) -> Result<(), ()> {
        unsafe {
            self.0
                .as_mut()
                .add(fc::Pattern::KEY_SIZE, &(size as core::ffi::c_double))
        }
    }

    pub fn perform_match(&mut self, fc: &mut fc::Config) -> Result<FontSet, FontConstructionError> {
        unsafe {
            fc.substitute(self.0.as_mut(), fc::MatchKind::Pattern)
                .map_err(|_| FontConstructionError::SysAPICallError("FcConfigSubstitute"))?;
            self.0.as_mut().default_substitute();

            Ok(FontSet(
                fc::sort(fc, self.0.as_mut(), false, None)
                    .map_err(|_| FontConstructionError::SysAPICallError("FcFontSort"))?,
            ))
        }
    }
}

#[repr(transparent)]
pub struct FontSet(core::ptr::NonNull<fc::FontSet>);
impl Drop for FontSet {
    #[inline(always)]
    fn drop(&mut self) {
        unsafe { fc::FontSet::destroy(self.0.as_mut()) }
    }
}
impl FontSet {
    #[inline(always)]
    pub fn iter(&self) -> impl Iterator<Item = &mut fc::Pattern> {
        unsafe {
            (*self.0.as_ptr())
                .fonts_slice()
                .into_iter()
                .map(|x| &mut *x.as_ptr())
        }
    }
}
