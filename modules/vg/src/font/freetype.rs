//! FreeType and Fontconfig Loaders

use euclid::Rect;
use freetype2::outline::*;
use freetype2::*;
use lyon_path::builder::{FlatPathBuilder, PathBuilder};
use parking_lot::{
    MappedRwLockReadGuard, MappedRwLockWriteGuard, RwLock, RwLockReadGuard, RwLockWriteGuard,
};
use std::cell::Cell;
use std::ffi::{CStr, CString};
use std::sync::Arc;

use crate::{Font, GlyphLoadingError};

pub struct FreetypeFont(pub(crate) FaceGroup, pub(crate) f32);
impl Font for FreetypeFont {
    type GlyphID = (usize, u32);

    fn set_em_size(&mut self, size: f32) {
        self.1 = size;
        self.0.set_size(size);
    }
    fn size(&self) -> f32 {
        self.1
    }

    fn ascent(&self) -> f32 {
        self.0.ascender() as f32 * self.1 / self.units_per_em() as f32
    }
    fn units_per_em(&self) -> u32 {
        self.0.units_per_em() as _
    }

    fn glyph_id(&self, c: char) -> Option<Self::GlyphID> {
        self.0.char_index(c)
    }
    fn advance_h(&self, glyph: &Self::GlyphID) -> Result<f32, GlyphLoadingError> {
        self.0.get_mut(glyph.0).load_glyph(glyph.1)?;

        Ok(self.0.get(glyph.0).glyph_advance().x as f32 / 64.0)
    }
    fn bounds(&self, glyph: &Self::GlyphID) -> Result<Rect<f32>, GlyphLoadingError> {
        let mut fnt = self.0.get_mut(glyph.0);
        fnt.load_glyph(glyph.1)?;
        let m = fnt.glyph_metrics();

        Ok(Rect::new(
            euclid::point2(m.horiBearingX as f32 / 64.0, m.horiBearingY as f32 / 64.0),
            euclid::size2(m.width as f32 / 64.0, m.height as f32 / 64.0),
        ))
    }
    fn outline(
        &self,
        glyph: &Self::GlyphID,
        transform: &euclid::Transform2D<f32>,
        builder: &mut impl PathBuilder,
    ) -> Result<(), GlyphLoadingError> {
        self.0.get_mut(glyph.0).load_glyph(glyph.1)?;
        self.0
            .get_mut(glyph.0)
            .decompose_outline(transform, builder);

        Ok(())
    }
}

#[repr(transparent)]
pub struct UniqueSystem(FT_Library);
unsafe impl Sync for UniqueSystem {}
unsafe impl Send for UniqueSystem {}
impl UniqueSystem {
    #[inline(always)]
    pub fn new() -> Self {
        let mut obj = core::mem::MaybeUninit::uninit();
        unsafe {
            FT_Init_FreeType(obj.as_mut_ptr());
            Self(obj.assume_init())
        }
    }
}
impl Drop for UniqueSystem {
    #[inline(always)]
    fn drop(&mut self) {
        unsafe {
            FT_Done_FreeType(self.0);
        }
    }
}

#[repr(transparent)]
#[derive(Clone)]
pub struct System(Arc<RwLock<UniqueSystem>>);
impl System {
    #[inline(always)]
    pub fn new() -> Self {
        Self(Arc::new(RwLock::new(UniqueSystem::new())))
    }
}

pub enum FaceGroupEntry {
    Unloaded(CString, FT_Long),
    Loaded(Face),
    LoadedMem(Face, Arc<Vec<u8>>),
}
impl FaceGroupEntry {
    pub fn unloaded(path: &CStr, index: FT_Long) -> Self {
        Self::Unloaded(path.to_owned(), index)
    }

    pub const fn is_loaded(&self) -> bool {
        matches!(self, Self::Loaded(_) | Self::LoadedMem(_, _))
    }
}
pub struct FaceGroup {
    parent: System,
    faces: Vec<RwLock<FaceGroupEntry>>,
    current_size: Cell<f32>,
}
impl System {
    pub fn new_face_group(&self, entries: Vec<FaceGroupEntry>) -> FaceGroup {
        let faces = entries.into_iter().map(Into::into).collect();
        FaceGroup {
            parent: self.clone(),
            faces,
            current_size: Cell::new(0.0),
        }
    }
}
impl FaceGroup {
    pub fn get<'x>(&'x self, index: usize) -> MappedRwLockReadGuard<'x, Face> {
        if !self.faces[index].read().is_loaded() {
            let mut new_face = match &*self.faces[index].read() {
                FaceGroupEntry::Unloaded(p, x) => self.parent.new_face(p.as_ptr() as _, *x),
                _ => unreachable!(),
            };

            new_face.set_size(self.current_size.get());
            *self.faces[index].write() = FaceGroupEntry::Loaded(new_face);
        }

        RwLockReadGuard::map(self.faces[index].read(), |f| {
            if let FaceGroupEntry::Loaded(f) | FaceGroupEntry::LoadedMem(f, _) = f {
                f
            } else {
                unreachable!()
            }
        })
    }

    pub fn get_mut<'x>(&'x self, index: usize) -> MappedRwLockWriteGuard<'x, Face> {
        if !self.faces[index].read().is_loaded() {
            let mut new_face = match &*self.faces[index].read() {
                FaceGroupEntry::Unloaded(p, x) => self.parent.new_face(p.as_ptr() as _, *x),
                _ => unreachable!(),
            };

            new_face.set_size(self.current_size.get());
            *self.faces[index].write() = FaceGroupEntry::Loaded(new_face);
        }

        RwLockWriteGuard::map(self.faces[index].write(), |f| match f {
            FaceGroupEntry::Loaded(f) | FaceGroupEntry::LoadedMem(f, _) => f,
            _ => unreachable!(),
        })
    }

    pub fn set_size(&self, size: f32) {
        self.current_size.set(size);
        for e in &self.faces {
            let mut eb = e.write();
            if let FaceGroupEntry::Loaded(f) | FaceGroupEntry::LoadedMem(f, _) = &mut *eb {
                f.set_size(size);
            }
        }
    }

    pub fn units_per_em(&self) -> FT_UShort {
        self.get(0).units_per_em()
    }
    pub fn ascender(&self) -> FT_Short {
        self.get(0).ascender()
    }

    pub fn char_index(&self, c: char) -> Option<(usize, FT_UInt)> {
        for n in 0..self.faces.len() {
            let ci = self.get(n).char_index(c);
            if ci != 0 {
                return Some((n, ci));
            }
        }

        None
    }
}

pub struct Face {
    _parent: System,
    ptr: FT_Face,
}
impl System {
    pub fn new_face(&self, path: *const u8, face_index: FT_Long) -> Face {
        let us = self.0.write();

        let mut ptr = core::mem::MaybeUninit::uninit();
        unsafe {
            FT_New_Face(us.0, path as _, face_index, ptr.as_mut_ptr());
            Face {
                _parent: self.clone(),
                ptr: ptr.assume_init(),
            }
        }
    }

    pub fn new_face_from_mem(&self, mem: &[u8], face_index: FT_Long) -> Result<Face, FT_Error> {
        let us = self.0.write();

        let mut ptr = core::mem::MaybeUninit::uninit();
        unsafe {
            let r = FT_New_Memory_Face(
                us.0,
                mem.as_ptr(),
                mem.len() as _,
                face_index,
                ptr.as_mut_ptr(),
            );
            if r != 0 {
                Err(r)
            } else {
                Ok(Face {
                    _parent: self.clone(),
                    ptr: ptr.assume_init(),
                })
            }
        }
    }
}
impl Drop for Face {
    fn drop(&mut self) {
        let _us_lock = self._parent.0.write();

        unsafe {
            FT_Done_Face(self.ptr);
        }
    }
}
unsafe impl Sync for Face {}
unsafe impl Send for Face {}

impl Face {
    pub fn select_unicode(&mut self) {
        unsafe { FT_Select_Charmap(self.ptr, FT_ENCODING_UNICODE) };
    }

    pub fn set_size(&mut self, size: f32) {
        unsafe { FT_Set_Char_Size(self.ptr, (size * 64.0) as _, (size * 64.0) as _, 100, 100) };
    }

    pub fn units_per_em(&self) -> FT_UShort {
        unsafe { (*self.ptr).units_per_em }
    }

    pub fn ascender(&self) -> FT_Short {
        unsafe { (*self.ptr).ascender }
    }

    pub fn char_index(&self, c: char) -> FT_UInt {
        unsafe { FT_Get_Char_Index(self.ptr, c as _) }
    }

    pub fn load_glyph(&mut self, g: u32) -> Result<(), FT_Error> {
        let r = unsafe { FT_Load_Glyph(self.ptr, g, FT_LOAD_DEFAULT) };
        if r != 0 {
            Err(r)
        } else {
            Ok(())
        }
    }

    pub fn glyph_advance(&self) -> &FT_Vector {
        unsafe { &(*(*self.ptr).glyph).advance }
    }

    pub fn glyph_metrics(&self) -> &FT_Glyph_Metrics {
        unsafe { &(*(*self.ptr).glyph).metrics }
    }

    pub fn decompose_outline<B: PathBuilder>(
        &mut self,
        transform: &euclid::Transform2D<f32>,
        builder: &mut B,
    ) {
        let decomposer = FT_Outline_Funcs {
            move_to: outline_decompose_moveto::<B>,
            line_to: outline_decompose_lineto::<B>,
            conic_to: outline_decompose_conicto::<B>,
            cubic_to: outline_decompose_cubicto::<B>,
            shift: 0,
            delta: 0,
        };
        let mut ctx = OutlineContext { builder, transform };

        unsafe {
            FT_Outline_Decompose(
                &mut (*(*self.ptr).glyph).outline,
                &decomposer,
                &mut ctx as *mut _ as _,
            );
        }
    }
}

struct OutlineContext<'t, B: FlatPathBuilder> {
    builder: &'t mut B,
    transform: &'t euclid::Transform2D<f32>,
}

extern "system" fn outline_decompose_moveto<B: FlatPathBuilder>(
    to: *const FT_Vector,
    context: *mut libc::c_void,
) -> libc::c_int {
    let ctx = unsafe { &mut *(context as *mut OutlineContext<B>) };

    let vector = unsafe { &*to };
    ctx.builder
        .move_to(ctx.transform.transform_point(&euclid::point2(
            vector.x as f32 / 64.0,
            vector.y as f32 / 64.0,
        )));

    0
}
extern "system" fn outline_decompose_lineto<B: FlatPathBuilder>(
    to: *const FT_Vector,
    context: *mut libc::c_void,
) -> libc::c_int {
    let ctx = unsafe { &mut *(context as *mut OutlineContext<B>) };

    let vector = unsafe { &*to };
    ctx.builder
        .line_to(ctx.transform.transform_point(&euclid::point2(
            vector.x as f32 / 64.0,
            vector.y as f32 / 64.0,
        )));

    0
}
extern "system" fn outline_decompose_conicto<B: PathBuilder>(
    control: *const FT_Vector,
    to: *const FT_Vector,
    context: *mut libc::c_void,
) -> libc::c_int {
    let ctx = unsafe { &mut *(context as *mut OutlineContext<B>) };

    let cv = unsafe { &*control };
    let vector = unsafe { &*to };
    ctx.builder.quadratic_bezier_to(
        ctx.transform
            .transform_point(&euclid::point2(cv.x as f32 / 64.0, cv.y as f32 / 64.0)),
        ctx.transform.transform_point(&euclid::point2(
            vector.x as f32 / 64.0,
            vector.y as f32 / 64.0,
        )),
    );

    0
}
extern "system" fn outline_decompose_cubicto<B: PathBuilder>(
    control: *const FT_Vector,
    control2: *const FT_Vector,
    to: *const FT_Vector,
    context: *mut libc::c_void,
) -> libc::c_int {
    let ctx = unsafe { &mut *(context as *mut OutlineContext<B>) };

    let cv = unsafe { &*control };
    let cv2 = unsafe { &*control2 };
    let vector = unsafe { &*to };
    ctx.builder.cubic_bezier_to(
        ctx.transform
            .transform_point(&euclid::point2(cv.x as f32 / 64.0, cv.y as f32 / 64.0)),
        ctx.transform
            .transform_point(&euclid::point2(cv2.x as f32 / 64.0, cv2.y as f32 / 64.0)),
        ctx.transform.transform_point(&euclid::point2(
            vector.x as f32 / 64.0,
            vector.y as f32 / 64.0,
        )),
    );

    0
}
