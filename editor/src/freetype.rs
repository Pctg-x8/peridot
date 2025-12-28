use std::{os::unix::ffi::OsStrExt, path::Path};

use freetype2::*;

#[repr(transparent)]
pub struct Library(FT_Library);
impl Drop for Library {
    #[inline(always)]
    fn drop(&mut self) {
        match unsafe { FT_Done_FreeType(self.0) } {
            0 => (),
            e => tracing::error!(code = e, "FT_Done_FreeType failed"),
        }
    }
}
unsafe impl Sync for Library {}
unsafe impl Send for Library {}
impl Library {
    #[inline]
    pub fn init() -> Result<Self, FT_Error> {
        let mut library = core::mem::MaybeUninit::uninit();
        match unsafe { FT_Init_FreeType(library.as_mut_ptr()) } {
            0 => Ok(Self(unsafe { library.assume_init() })),
            e => Err(e),
        }
    }
}

#[repr(transparent)]
pub struct Face(FT_Face);
unsafe impl Sync for Face {}
unsafe impl Send for Face {}
impl Drop for Face {
    #[inline(always)]
    fn drop(&mut self) {
        match unsafe { FT_Done_Face(self.0) } {
            0 => (),
            e => tracing::error!(code = e, "FT_Done_Face failed"),
        }
    }
}
impl Face {
    #[inline(always)]
    pub const fn as_native(&self) -> FT_Face {
        self.0
    }

    #[inline(always)]
    pub const fn units_per_em(&self) -> u16 {
        unsafe { (*self.0).units_per_em }
    }

    #[inline(always)]
    pub const fn ascender(&self) -> i16 {
        unsafe { (*self.0).ascender }
    }

    #[inline(always)]
    pub const fn ascender_real_per_em(&self) -> f64 {
        self.ascender() as f64 / self.units_per_em() as f64
    }

    #[inline]
    pub fn new(
        library: &Library,
        path: &core::ffi::CStr,
        face_index: FT_Long,
    ) -> Result<Self, FT_Error> {
        let mut face = core::mem::MaybeUninit::uninit();
        match unsafe { FT_New_Face(library.0, path.as_ptr(), face_index, face.as_mut_ptr()) } {
            0 => Ok(Self(unsafe { face.assume_init() })),
            e => Err(e),
        }
    }

    #[inline]
    pub fn set_char_size(&mut self, size: f32, resolution: FT_UInt) -> Result<(), FT_Error> {
        match unsafe { FT_Set_Char_Size(self.0, 0, (size * 64.0) as i32, 0, resolution) } {
            0 => Ok(()),
            e => Err(e),
        }
    }

    #[inline]
    pub fn load_glyph(
        &mut self,
        index: FT_UInt,
        flags: i32,
    ) -> Result<&mut GlyphSlotRec, FT_Error> {
        match unsafe { FT_Load_Glyph(self.0, index, flags) } {
            0 => Ok(unsafe { &mut *(*self.0).glyph.cast::<GlyphSlotRec>() }),
            e => Err(e),
        }
    }
}

#[repr(transparent)]
pub struct GlyphSlotRec(pub FT_GlyphSlotRec);
impl GlyphSlotRec {
    #[inline(always)]
    pub const fn outline_mut(&mut self) -> &mut Outline {
        unsafe { core::mem::transmute::<&mut FT_Outline, &mut Outline>(&mut self.0.outline) }
    }
}

#[repr(transparent)]
pub struct Outline(FT_Outline);
impl Outline {
    #[inline]
    pub fn decompose<F: OutlineFuncs>(
        &mut self,
        funcs: &mut F,
        shift: core::ffi::c_int,
        delta: FT_Pos,
    ) -> Result<(), FT_Error> {
        extern "system" fn move_to<F: OutlineFuncs>(
            to: *const FT_Vector,
            user: *mut core::ffi::c_void,
        ) -> core::ffi::c_int {
            match unsafe { F::move_to(&mut *user.cast::<F>(), &*to) } {
                Ok(_) => 0,
                Err(e) => e,
            }
        }
        extern "system" fn line_to<F: OutlineFuncs>(
            to: *const FT_Vector,
            user: *mut core::ffi::c_void,
        ) -> core::ffi::c_int {
            match unsafe { F::line_to(&mut *user.cast::<F>(), &*to) } {
                Ok(_) => 0,
                Err(e) => e,
            }
        }
        extern "system" fn conic_to<F: OutlineFuncs>(
            control: *const FT_Vector,
            to: *const FT_Vector,
            user: *mut core::ffi::c_void,
        ) -> core::ffi::c_int {
            match unsafe { F::conic_to(&mut *user.cast::<F>(), &*control, &*to) } {
                Ok(_) => 0,
                Err(e) => e,
            }
        }
        extern "system" fn cubic_to<F: OutlineFuncs>(
            control1: *const FT_Vector,
            control2: *const FT_Vector,
            to: *const FT_Vector,
            user: *mut core::ffi::c_void,
        ) -> core::ffi::c_int {
            match unsafe { F::cubic_to(&mut *user.cast::<F>(), &*control1, &*control2, &*to) } {
                Ok(_) => 0,
                Err(e) => e,
            }
        }

        match unsafe {
            outline::FT_Outline_Decompose(
                &mut self.0,
                &mut FT_Outline_Funcs {
                    move_to: move_to::<F>,
                    line_to: line_to::<F>,
                    conic_to: conic_to::<F>,
                    cubic_to: cubic_to::<F>,
                    shift,
                    delta,
                },
                funcs as *mut _ as _,
            )
        } {
            0 => Ok(()),
            e => Err(e),
        }
    }
}

pub trait OutlineFuncs {
    fn move_to(&mut self, to: &FT_Vector) -> Result<(), FT_Error>;
    fn line_to(&mut self, to: &FT_Vector) -> Result<(), FT_Error>;
    fn conic_to(&mut self, control: &FT_Vector, to: &FT_Vector) -> Result<(), FT_Error>;
    fn cubic_to(
        &mut self,
        control1: &FT_Vector,
        control2: &FT_Vector,
        to: &FT_Vector,
    ) -> Result<(), FT_Error>;
}
