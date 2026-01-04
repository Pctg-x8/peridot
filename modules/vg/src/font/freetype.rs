//! FreeType and Fontconfig Loaders

use euclid::Rect;
use lyon_path::builder::PathBuilder;
use parking_lot::{
    MappedRwLockReadGuard, MappedRwLockWriteGuard, RwLock, RwLockReadGuard, RwLockWriteGuard,
};
use peridot_tp_freetype::{self as ft, FractionalExt};
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
    fn outline<B: PathBuilder>(
        &self,
        glyph: &Self::GlyphID,
        transform: &euclid::Transform2D<f32>,
        builder: &mut B,
    ) -> Result<(), GlyphLoadingError> {
        self.0.get_mut(glyph.0).load_glyph(glyph.1)?;
        self.0
            .get_mut(glyph.0)
            .decompose_outline(transform, builder)?;

        Ok(())
    }
}

#[repr(transparent)]
pub struct UniqueSystem(ft::raw::FT_Library);
unsafe impl Sync for UniqueSystem {}
unsafe impl Send for UniqueSystem {}
impl UniqueSystem {
    #[inline(always)]
    pub fn new() -> Self {
        Self(ft::init_freetype().expect("Failed to initialize freetype2"))
    }
}
impl Drop for UniqueSystem {
    #[inline(always)]
    fn drop(&mut self) {
        if let Err(e) = unsafe { ft::done_freetype(self.0) } {
            tracing::error!(reason = ?e, "Failed to deinitialize freetype2");
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
    Unloaded(CString, ft::Long),
    Loaded(Face),
    LoadedMem(Face, #[allow(dead_code)] Arc<Vec<u8>>),
}
impl FaceGroupEntry {
    pub fn unloaded(path: &CStr, index: ft::Long) -> Self {
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
                FaceGroupEntry::Unloaded(p, x) => {
                    self.parent.new_face(p, *x).expect("Failed to load face")
                }
                _ => unreachable!(),
            };

            if let Err(e) = new_face.set_size(self.current_size.get()) {
                tracing::warn!(reason = ?e, "Failed to set face size");
            }
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
                FaceGroupEntry::Unloaded(p, x) => {
                    self.parent.new_face(p, *x).expect("Failed to load face")
                }
                _ => unreachable!(),
            };

            if let Err(e) = new_face.set_size(self.current_size.get()) {
                tracing::warn!(reason = ?e, "Failed to set face size");
            }
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
                if let Err(e) = f.set_size(size) {
                    tracing::warn!(reason = ?e, "Failed to set face size");
                }
            }
        }
    }

    pub fn units_per_em(&self) -> ft::raw::FT_UShort {
        self.get(0).units_per_em()
    }
    pub fn ascender(&self) -> ft::raw::FT_Short {
        self.get(0).ascender()
    }

    pub fn char_index(&self, c: char) -> Option<(usize, ft::raw::FT_UInt)> {
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
    ptr: ft::raw::FT_Face,
}
impl System {
    #[inline]
    pub fn new_face(&self, path: &core::ffi::CStr, face_index: ft::Long) -> ft::Result<Face> {
        let ptr = unsafe { ft::new_face(self.0.write().0, path, face_index)? };

        Ok(Face {
            _parent: self.clone(),
            ptr,
        })
    }

    #[inline]
    pub fn new_face_from_mem(&self, mem: &[u8], face_index: ft::Long) -> ft::Result<Face> {
        let ptr = unsafe { ft::new_memory_face(self.0.write().0, &mem, face_index)? };

        Ok(Face {
            _parent: self.clone(),
            ptr,
        })
    }
}
impl Drop for Face {
    fn drop(&mut self) {
        let _us_lock = self._parent.0.write();

        if let Err(e) = unsafe { ft::done_face(self.ptr) } {
            tracing::error!(reason = ?e, "cleanup freetype face failed");
        }
    }
}
unsafe impl Sync for Face {}
unsafe impl Send for Face {}
impl Face {
    #[inline(always)]
    pub fn set_size(&mut self, size: f32) -> ft::Result<()> {
        unsafe { ft::set_char_size(self.ptr, 0, size.to_f26dot6_lossy(), 0, 100) }
    }

    #[inline(always)]
    pub fn units_per_em(&self) -> ft::UShort {
        unsafe { (*self.ptr).units_per_em }
    }

    #[inline(always)]
    pub fn ascender(&self) -> ft::Short {
        unsafe { (*self.ptr).ascender }
    }

    #[inline(always)]
    pub fn char_index(&self, c: char) -> ft::UInt {
        unsafe { ft::char_index(self.ptr, c as _) }
    }

    #[inline(always)]
    pub fn load_glyph(&mut self, g: u32) -> ft::Result<()> {
        unsafe { ft::load_glyph(self.ptr, g, ft::LoadFlags::DEFAULT) }
    }

    pub fn glyph_advance(&self) -> &ft::Vector {
        unsafe { &(*(*self.ptr).glyph).advance }
    }

    pub fn glyph_metrics(&self) -> &ft::raw::FT_Glyph_Metrics {
        unsafe { &(*(*self.ptr).glyph).metrics }
    }

    #[inline(always)]
    pub fn decompose_outline<B: PathBuilder>(
        &mut self,
        transform: &euclid::Transform2D<f32>,
        builder: &mut B,
    ) -> ft::Result<()> {
        unsafe {
            ft::outline_decompose(
                &mut (*(*self.ptr).glyph).outline,
                &mut OutlineContext { builder, transform },
                0,
                0,
            )
        }
    }
}

struct OutlineContext<'t, B> {
    builder: &'t mut B,
    transform: &'t euclid::Transform2D<f32>,
}
impl<B: PathBuilder> ft::OutlineFuncs for OutlineContext<'_, B> {
    fn move_to(&mut self, to: &peridot_tp_freetype::Vector) {
        self.builder.move_to(
            self.transform
                .transform_point(&euclid::point2(to.x as f32 / 64.0, to.y as f32 / 64.0)),
        );
    }

    fn line_to(&mut self, to: &peridot_tp_freetype::Vector) {
        self.builder.line_to(
            self.transform
                .transform_point(&euclid::point2(to.x as f32 / 64.0, to.y as f32 / 64.0)),
        );
    }

    fn conic_to(
        &mut self,
        control: &peridot_tp_freetype::Vector,
        to: &peridot_tp_freetype::Vector,
    ) {
        self.builder.quadratic_bezier_to(
            self.transform.transform_point(&euclid::point2(
                control.x as f32 / 64.0,
                control.y as f32 / 64.0,
            )),
            self.transform
                .transform_point(&euclid::point2(to.x as f32 / 64.0, to.y as f32 / 64.0)),
        );
    }

    fn cubic_to(
        &mut self,
        control1: &peridot_tp_freetype::Vector,
        control2: &peridot_tp_freetype::Vector,
        to: &peridot_tp_freetype::Vector,
    ) {
        self.builder.cubic_bezier_to(
            self.transform.transform_point(&euclid::point2(
                control1.x as f32 / 64.0,
                control1.y as f32 / 64.0,
            )),
            self.transform.transform_point(&euclid::point2(
                control2.x as f32 / 64.0,
                control2.y as f32 / 64.0,
            )),
            self.transform
                .transform_point(&euclid::point2(to.x as f32 / 64.0, to.y as f32 / 64.0)),
        );
    }
}
