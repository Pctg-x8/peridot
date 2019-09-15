//! FreeType and Fontconfig Loaders

use freetype2::*;
use freetype2::outline::*;
use std::rc::Rc;
use std::mem::MaybeUninit;
use lyon_path::builder::{PathBuilder, FlatPathBuilder};

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
	pub fn units_per_em(&self) -> FT_UShort { unsafe { (*self.ptr).units_per_em } }
	pub fn ascender(&self) -> FT_Short { unsafe { (*self.ptr).ascender } }

	pub fn char_index(&self, c: char) -> FT_UInt { unsafe { FT_Get_Char_Index(self.ptr, c as _) } }
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
	builder.move_to(euclid::point2(vector.x as f32 * 64.0, vector.y as f32 * 64.0));

	return 0;
}
extern "system"
fn outline_decompose_lineto<B: FlatPathBuilder>(to: *const FT_Vector, context: *mut libc::c_void) -> libc::c_int
{
	let builder = unsafe { &mut *(context as *mut B) };
	let vector = unsafe { &*to };
	builder.line_to(euclid::point2(vector.x as f32 * 64.0, vector.y as f32 * 64.0));

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
		euclid::point2(cv.x as f32 * 64.0, cv.y as f32 * 64.0),
		euclid::point2(vector.x as f32 * 64.0, vector.y as f32 * 64.0)
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
		euclid::point2(cv.x as f32 * 64.0, cv.y as f32 * 64.0),
		euclid::point2(cv2.x as f32 * 64.0, cv2.y as f32 * 64.0),
		euclid::point2(vector.x as f32 * 64.0, vector.y as f32 * 64.0)
	);

	return 0;
}
