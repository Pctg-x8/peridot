//! FreeType and Fontconfig Loaders

use euclid::Rect;
use freetype2::outline::*;
use freetype2::*;
use lyon_path::builder::{FlatPathBuilder, PathBuilder};
use std::cell::{Cell, Ref, RefCell};
use std::ffi::{CStr, CString};
use std::rc::Rc;

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
        self.0.get(glyph.0).load_glyph(glyph.1)?;

        Ok(self.0.get(glyph.0).glyph_advance().x as f32 / 64.0)
    }
    fn bounds(&self, glyph: &Self::GlyphID) -> Result<Rect<f32>, GlyphLoadingError> {
        let fnt = self.0.get(glyph.0);
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
        self.0.get(glyph.0).load_glyph(glyph.1)?;
        self.0.get(glyph.0).decompose_outline(transform, builder);

        Ok(())
    }
}

#[repr(transparent)]
pub struct UniqueSystem(FT_Library);
impl UniqueSystem {
    pub fn new() -> Self {
        let mut obj = core::mem::MaybeUninit::uninit();
        unsafe {
            FT_Init_FreeType(obj.as_mut_ptr());
            UniqueSystem(obj.assume_init())
        }
    }
}
impl Drop for UniqueSystem {
    fn drop(&mut self) {
        unsafe {
            FT_Done_FreeType(self.0);
        }
    }
}

#[repr(transparent)]
#[derive(Clone)]
pub struct System(Rc<UniqueSystem>);
impl System {
    pub fn new() -> Self {
        System(UniqueSystem::new().into())
    }
}

pub enum FaceGroupEntry {
    Unloaded(CString, FT_Long),
    Loaded(Face),
    LoadedMem(Face, Rc<Vec<u8>>),
}
impl FaceGroupEntry {
    pub fn unloaded(path: &CStr, index: FT_Long) -> Self {
        Self::Unloaded(path.to_owned(), index)
    }

    pub const fn is_loaded(&self) -> bool {
        match self {
            Self::Loaded(_) | Self::LoadedMem(_, _) => true,
            _ => false,
        }
    }
}
pub struct FaceGroup {
    parent: System,
    faces: Vec<RefCell<FaceGroupEntry>>,
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
    pub fn get(&self, index: usize) -> Ref<Face> {
        if !self.faces[index].borrow().is_loaded() {
            let new_face = match &*self.faces[index].borrow() {
                FaceGroupEntry::Unloaded(p, x) => self.parent.new_face(p.as_ptr() as _, *x),
                _ => unreachable!(),
            };
            new_face.set_size(self.current_size.get());
            let bm = &self.faces[index];
            *bm.borrow_mut() = FaceGroupEntry::Loaded(new_face);
        }

        Ref::map(self.faces[index].borrow(), |f| {
            if let FaceGroupEntry::Loaded(f) | FaceGroupEntry::LoadedMem(f, _) = f {
                f
            } else {
                unreachable!()
            }
        })
    }

    pub fn set_size(&self, size: f32) {
        self.current_size.set(size);
        for e in &self.faces {
            let eb = e.borrow();
            if let &FaceGroupEntry::Loaded(ref f) | &FaceGroupEntry::LoadedMem(ref f, _) = &*eb {
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
        let mut ptr = core::mem::MaybeUninit::uninit();
        unsafe {
            FT_New_Face(self.0 .0, path as _, face_index, ptr.as_mut_ptr());
            Face {
                _parent: self.clone(),
                ptr: ptr.assume_init(),
            }
        }
    }

    pub fn new_face_from_mem(&self, mem: &[u8], face_index: FT_Long) -> Result<Face, FT_Error> {
        let mut ptr = core::mem::MaybeUninit::uninit();
        unsafe {
            let r = FT_New_Memory_Face(
                self.0 .0,
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
        unsafe {
            FT_Done_Face(self.ptr);
        }
    }
}

impl Face {
    pub fn select_unicode(&self) {
        unsafe { FT_Select_Charmap(self.ptr, FT_ENCODING_UNICODE) };
    }

    pub fn set_size(&self, size: f32) {
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

    pub fn load_glyph(&self, g: u32) -> Result<(), FT_Error> {
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
        &self,
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

    return 0;
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

    return 0;
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

    return 0;
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

    return 0;
}
