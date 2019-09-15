//! FreeType and Fontconfig Loaders

use freetype2::*;
use freetype2::outline::*;
use std::rc::Rc;
use std::mem::MaybeUninit;
use lyon_path::builder::{PathBuilder, FlatPathBuilder};
use std::cell::{Cell, RefCell, Ref};
use std::ffi::{CStr, CString};

pub struct UniqueSystem { ptr: FT_Library }
#[derive(Clone)]
pub struct System(Rc<UniqueSystem>);
impl UniqueSystem
{
	pub fn new() -> Self
	{
		let mut obj = MaybeUninit::uninit();
		unsafe
		{
			FT_Init_FreeType(obj.as_mut_ptr());
			UniqueSystem { ptr: obj.assume_init() }
		}
	}
}
impl Drop for UniqueSystem
{
	fn drop(&mut self)
	{
		unsafe { FT_Done_FreeType(self.ptr); }
	}
}
impl System
{
	pub fn new() -> Self { System(UniqueSystem::new().into()) }
}

pub enum FaceGroupEntry { Unloaded(CString, FT_Long), Loaded(Face) }
impl FaceGroupEntry
{
	pub fn is_loaded(&self) -> bool { match self { Self::Loaded(_) => true, _ => false } }
}
pub struct FaceGroup
{
	parent: System,
	faces: Vec<RefCell<FaceGroupEntry>>,
	current_size: Cell<f32>
}
impl System
{
	pub fn new_face_group(&self, entries: &[(*const i8, FT_Long)]) -> FaceGroup
	{
		let faces = entries.iter()
			.map(|&(p, x)| FaceGroupEntry::Unloaded(unsafe { CStr::from_ptr(p).to_owned() }, x).into())
			.collect();
		
		FaceGroup { parent: self.clone(), faces, current_size: Cell::new(0.0) }
	}
}
impl FaceGroup
{
	pub fn get(&self, index: usize) -> Ref<Face>
	{
		if !self.faces[index].borrow().is_loaded()
		{
			let new_face = if let FaceGroupEntry::Unloaded(p, x) = &*self.faces[index].borrow()
			{
				self.parent.new_face(p.as_ptr() as _, *x)
			}
			else { unreachable!() };
			new_face.set_size(self.current_size.get());
			let bm = &self.faces[index];
			*bm.borrow_mut() = FaceGroupEntry::Loaded(new_face);
		}

		Ref::map(self.faces[index].borrow(), |f| if let FaceGroupEntry::Loaded(f) = f { f } else { unreachable!() })
	}

	pub fn set_size(&self, size: f32)
	{
		self.current_size.set(size);
		for e in &self.faces
		{
			let eb = e.borrow();
			if let &FaceGroupEntry::Loaded(ref f) = &*eb { f.set_size(size); }
		}
	}

	pub fn units_per_em(&self) -> FT_UShort { self.get(0).units_per_em() }
	pub fn ascender(&self) -> FT_Short { self.get(0).ascender() }

	pub fn char_index(&self, c: char) -> Option<(usize, FT_UInt)>
	{
		for n in 0..self.faces.len()
		{
			let ci = self.get(n).char_index(c);
			if ci != 0 { return Some((n, ci)); }
		}
		
		None
	}
}

pub struct Face { _parent: System, ptr: FT_Face }
impl System
{
	pub fn new_face(&self, path: *const u8, face_index: FT_Long) -> Face
	{
		let mut ptr = MaybeUninit::uninit();
		unsafe
		{
			FT_New_Face(self.0.ptr, path as _, face_index, ptr.as_mut_ptr());
			Face { _parent: self.clone(), ptr: ptr.assume_init() }
		}
	}
}
impl Drop for Face
{
	fn drop(&mut self)
	{
		unsafe { FT_Done_Face(self.ptr); }
	}
}

impl Face
{
	pub fn select_unicode(&self)
	{
		unsafe { FT_Select_Charmap(self.ptr, FT_ENCODING_UNICODE) };
	}
	pub fn set_size(&self, size: f32)
	{
		unsafe { FT_Set_Char_Size(self.ptr, (size * 64.0) as _, (size * 64.0) as _, 100, 100) };
	}
	pub fn units_per_em(&self) -> FT_UShort { unsafe { (*self.ptr).units_per_em } }
	pub fn ascender(&self) -> FT_Short { unsafe { (*self.ptr).ascender } }

	pub fn char_index(&self, c: char) -> FT_UInt
	{
		unsafe { FT_Get_Char_Index(self.ptr, c as _) }
	}
	pub fn load_glyph(&self, g: u32) -> Result<(), FT_Error>
	{
		let r = unsafe { FT_Load_Glyph(self.ptr, g, FT_LOAD_DEFAULT) };
		if r != 0 { Err(r) } else { Ok(()) }
	}
	pub fn glyph_advance(&self) -> &FT_Vector
	{
		unsafe { &(*(*self.ptr).glyph).advance }
	}
	pub fn glyph_metrics(&self) -> &FT_Glyph_Metrics
	{
		unsafe { &(*(*self.ptr).glyph).metrics }
	}

	pub fn decompose_outline<B: PathBuilder>(&self, builder: &mut B)
	{
		let decomposer = FT_Outline_Funcs
		{
			move_to: outline_decompose_moveto::<B>,
			line_to: outline_decompose_lineto::<B>,
			conic_to: outline_decompose_conicto::<B>,
			cubic_to: outline_decompose_cubicto::<B>,
			shift: 0, delta: 0
		};

		unsafe { FT_Outline_Decompose(&mut (*(*self.ptr).glyph).outline, &decomposer, builder as *mut B as _); }
	}
}

extern "system"
fn outline_decompose_moveto<B: FlatPathBuilder>(to: *const FT_Vector, context: *mut libc::c_void) -> libc::c_int
{
	let builder = unsafe { &mut *(context as *mut B) };
	let vector = unsafe { &*to };
	builder.move_to(euclid::point2(vector.x as f32 / 64.0, vector.y as f32 / 64.0));

	return 0;
}
extern "system"
fn outline_decompose_lineto<B: FlatPathBuilder>(to: *const FT_Vector, context: *mut libc::c_void) -> libc::c_int
{
	let builder = unsafe { &mut *(context as *mut B) };
	let vector = unsafe { &*to };
	builder.line_to(euclid::point2(vector.x as f32 / 64.0, vector.y as f32 / 64.0));

	return 0;
}
extern "system"
fn outline_decompose_conicto<B: PathBuilder>(control: *const FT_Vector, to: *const FT_Vector,
	context: *mut libc::c_void) -> libc::c_int
{
	let builder = unsafe { &mut *(context as *mut B) };
	let cv = unsafe { &*control };
	let vector = unsafe { &*to };
	builder.quadratic_bezier_to(
		euclid::point2(cv.x as f32 / 64.0, cv.y as f32 / 64.0),
		euclid::point2(vector.x as f32 / 64.0, vector.y as f32 / 64.0)
	);

	return 0;
}
extern "system"
fn outline_decompose_cubicto<B: PathBuilder>(control: *const FT_Vector, control2: *const FT_Vector,
	to: *const FT_Vector, context: *mut libc::c_void) -> libc::c_int
{
	let builder = unsafe { &mut *(context as *mut B) };
	let cv = unsafe { &*control };
	let cv2 = unsafe { &*control2 };
	let vector = unsafe { &*to };
	builder.cubic_bezier_to(
		euclid::point2(cv.x as f32 / 64.0, cv.y as f32 / 64.0),
		euclid::point2(cv2.x as f32 / 64.0, cv2.y as f32 / 64.0),
		euclid::point2(vector.x as f32 / 64.0, vector.y as f32 / 64.0)
	);

	return 0;
}
