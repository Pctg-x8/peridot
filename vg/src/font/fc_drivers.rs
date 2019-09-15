//! FontConfig Driver

use fontconfig::*;
use std::ptr::NonNull;
use std::mem::MaybeUninit;
use std::ffi::{CStr, CString};

pub struct Config { ptr: NonNull<FcConfig> }
impl Config
{
	pub fn init() -> Self
	{
		unsafe
		{
			FcInit();

			Config { ptr: NonNull::new_unchecked(FcConfigGetCurrent()) }
		}
	}

	pub fn substitute_pattern(&self, pat: &mut Pattern)
	{
		unsafe { FcConfigSubstitute(self.ptr.as_ptr(), pat.ptr.as_ptr(), FcMatchPattern); }
	}
	pub fn match_font(&self, pat: &Pattern) -> Option<Pattern>
	{
		let ptr = unsafe { FcFontMatch(self.ptr.as_ptr(), pat.ptr.as_ptr(), std::ptr::null_mut()) };

		NonNull::new(ptr).map(|ptr| Pattern { ptr })
	}
}

pub struct Pattern { ptr: NonNull<FcPattern> }
impl Pattern
{
	/*pub fn parse_name(name: *const u8) -> Option<Self>
	{
		NonNull::new(unsafe { FcNameParse(name) }).map(|ptr| Pattern { ptr })
	}*/
	pub fn with_name_weight_style_size(name: *const u8, ot_weight: u32, italic: bool, size: f32) -> Option<Self>
	{
		let size_range = Range::new_double(size as _, size as _).expect("Failed to create SizeRange");
		let ptr = unsafe
		{
			FcPatternBuild(std::ptr::null_mut(),
				FC_FAMILY, FcTypeString, name,
				FC_WEIGHT, FcTypeInteger, FcWeightFromOpenType(ot_weight as _) as FcChar32,
				FC_SLANT, FcTypeInteger, if italic { FC_SLANT_ITALIC } else { 0 } as FcChar32,
				FC_SIZE, FcTypeRange, size_range.ptr,
				std::ptr::null::<FcChar8>())
		};

		NonNull::new(ptr).map(|ptr| Pattern { ptr })
	}

	pub fn default_substitute(&mut self) { unsafe { FcDefaultSubstitute(self.ptr.as_ptr()); } }

	pub fn get_filepath(&self) -> Option<&CStr>
	{
		let mut file = MaybeUninit::uninit();
		let fc_file = CString::new(FC_FILE).expect("invalid fc_file def");
		let res = unsafe { FcPatternGetString(self.ptr.as_ptr(), fc_file.as_ptr(), 0, file.as_mut_ptr()) };
		if res == FcResultMatch
		{
			Some(unsafe { CStr::from_ptr(file.assume_init() as *const _) })
		}
		else
		{
			None
		}
	}
	pub fn get_face_index(&self) -> Option<u32>
	{
		let mut index = MaybeUninit::uninit();
		let fc_index = CString::new(FC_INDEX).expect("invalid fc_index def");
		let res = unsafe { FcPatternGetInteger(self.ptr.as_ptr(), fc_index.as_ptr(), 0, index.as_mut_ptr()) };
		if res == FcResultMatch
		{
			Some(unsafe { index.assume_init() as _ })
		}
		else
		{
			None
		}
	}
}
impl Drop for Pattern
{
	fn drop(&mut self)
	{
		unsafe { FcPatternDestroy(self.ptr.as_ptr()); }
	}
}

pub struct Range { ptr: NonNull<FcRange> }
impl Range
{
	pub fn new_double(begin: f64, end: f64) -> Option<Self>
	{
		NonNull::new(unsafe { FcRangeCreateDouble(begin as _, end as _) }).map(|ptr| Range { ptr })
	}
}
impl Drop for Range
{
	fn drop(&mut self)
	{
		unsafe { FcRangeDestroy(self.ptr.as_ptr()); }
	}
}
