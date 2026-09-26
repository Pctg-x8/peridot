use crate::{
    Object, Owned,
    foundation::{Data, Dictionary, String},
    graphics::Path,
    raw::*,
};

pub const fn font_symbolic_trait() -> &'static String {
    unsafe { &*kCTFontSymbolicTrait.cast::<String>() }
}

pub const fn font_weight_trait() -> &'static String {
    unsafe { &*kCTFontWeightTrait.cast::<String>() }
}

pub const fn font_width_trait() -> &'static String {
    unsafe { &*kCTFontWidthTrait.cast::<String>() }
}

pub const fn font_slant_trait() -> &'static String {
    unsafe { &*kCTFontSlantTrait.cast::<String>() }
}

#[repr(transparent)]
pub struct FontDescriptor(__CTFontDescriptor);
impl Object for FontDescriptor {
    #[inline(always)]
    fn as_typeref(&self) -> CFTypeRef {
        &self.0 as *const _ as _
    }
}
impl FontDescriptor {
    #[inline(always)]
    pub fn from_attributes(attributes: &Dictionary<String, dyn Object>) -> Option<Owned<Self>> {
        unsafe {
            Owned::from_ptr(
                CTFontDescriptorCreateWithAttributes(attributes as *const _ as _) as *mut Self,
            )
        }
    }

    #[inline(always)]
    pub fn from_data(data: &Data) -> Option<Owned<Self>> {
        unsafe {
            Owned::from_ptr(
                CTFontManagerCreateFontDescriptorFromData(data as *const _ as _) as *mut Self,
            )
        }
    }

    pub const fn family_name_attribute() -> &'static String {
        unsafe { &*kCTFontFamilyNameAttribute.cast::<String>() }
    }

    pub const fn traits_attribute() -> &'static String {
        unsafe { &*kCTFontTraitsAttribute.cast::<String>() }
    }
}

#[repr(transparent)]
pub struct Font(__CTFont);
impl Object for Font {
    #[inline(always)]
    fn as_typeref(&self) -> CFTypeRef {
        &self.0 as *const _ as _
    }
}
impl Font {
    #[inline(always)]
    pub fn from_font_descriptor(
        descriptor: &FontDescriptor,
        size: CGFloat,
        matrix: Option<&CGAffineTransform>,
    ) -> Option<Owned<Self>> {
        unsafe {
            Owned::from_ptr(CTFontCreateWithFontDescriptor(
                descriptor as *const _ as _,
                size,
                matrix.map_or_else(core::ptr::null, |x| x as *const _),
            ) as *mut Self)
        }
    }

    #[inline(always)]
    pub fn clone_with_attributes(
        &self,
        size: CGFloat,
        matrix: Option<&CGAffineTransform>,
        attributes: Option<&FontDescriptor>,
    ) -> Option<Owned<Self>> {
        unsafe {
            Owned::from_ptr(CTFontCreateCopyWithAttributes(
                self as *const _ as _,
                size,
                matrix.map_or_else(core::ptr::null, |x| x as *const _),
                attributes.map_or_else(core::ptr::null, |x| x as *const _ as _),
            ) as *mut Self)
        }
    }

    #[inline(always)]
    pub fn size(&self) -> CGFloat {
        unsafe { CTFontGetSize(&self.0) }
    }

    #[inline(always)]
    pub fn matrix(&self) -> CGAffineTransform {
        unsafe { CTFontGetMatrix(&self.0) }
    }

    #[inline(always)]
    pub fn ascent(&self) -> CGFloat {
        unsafe { CTFontGetAscent(&self.0) }
    }

    #[inline(always)]
    pub fn descent(&self) -> CGFloat {
        unsafe { CTFontGetDescent(&self.0) }
    }

    #[inline(always)]
    pub fn units_per_em(&self) -> core::ffi::c_uint {
        unsafe { CTFontGetUnitsPerEm(&self.0) }
    }

    #[inline(always)]
    pub fn glyph_for_character(&self, character: UniChar) -> Option<core::num::NonZero<CGGlyph>> {
        let mut glyph = core::mem::MaybeUninit::uninit();
        let r = unsafe { CTFontGetGlyphsForCharacters(&self.0, &character, glyph.as_mut_ptr(), 1) };
        if !r {
            None
        } else {
            Some(unsafe { core::num::NonZero::new_unchecked(glyph.assume_init()) })
        }
    }

    #[inline(always)]
    pub fn glyphs_for_characters(
        &self,
        characters: &[UniChar],
        glyphs: &mut [core::mem::MaybeUninit<CGGlyph>],
    ) -> bool {
        debug_assert!(glyphs.len() >= characters.len());

        unsafe {
            CTFontGetGlyphsForCharacters(
                &self.0,
                characters.as_ptr(),
                glyphs.as_mut_ptr() as _,
                characters.len() as _,
            )
        }
    }

    #[inline(always)]
    pub fn advance_for_glyph(&self, orientation: FontOrientation, glyph: CGGlyph) -> CGSize {
        let mut adv = core::mem::MaybeUninit::uninit();
        unsafe {
            CTFontGetAdvancesForGlyphs(&self.0, orientation as _, &glyph, adv.as_mut_ptr(), 1);
        }

        unsafe { adv.assume_init() }
    }

    #[inline(always)]
    pub fn advances_for_glyphs(
        &self,
        orientation: FontOrientation,
        glyphs: &[CGGlyph],
        advances: &mut [core::mem::MaybeUninit<CGSize>],
    ) -> core::ffi::c_double {
        debug_assert!(advances.len() >= glyphs.len());

        unsafe {
            CTFontGetAdvancesForGlyphs(
                &self.0,
                orientation as _,
                glyphs.as_ptr(),
                advances.as_mut_ptr() as _,
                glyphs.len() as _,
            )
        }
    }

    #[inline(always)]
    pub fn bounding_rect_for_glyph(&self, orientation: FontOrientation, glyph: CGGlyph) -> CGRect {
        let mut rect = core::mem::MaybeUninit::uninit();
        unsafe {
            CTFontGetBoundingRectsForGlyphs(
                &self.0,
                orientation as _,
                &glyph,
                rect.as_mut_ptr(),
                1,
            );
        }

        unsafe { rect.assume_init() }
    }

    #[inline(always)]
    pub fn create_path_for_glyph(
        &self,
        glyph: CGGlyph,
        matrix: Option<&CGAffineTransform>,
    ) -> Option<Owned<Path>> {
        unsafe {
            Owned::from_ptr(CTFontCreatePathForGlyph(
                &self.0,
                glyph,
                matrix.map_or_else(core::ptr::null, |x| x as *const _),
            ) as *mut Path)
        }
    }
}

#[repr(u32)]
pub enum FontOrientation {
    Default = kCTFontOrientationDefault,
    Horizontal = kCTFontOrientationHorizontal,
    Vertical = kCTFontOrientationVertical,
}
