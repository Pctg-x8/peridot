use core::mem::MaybeUninit;

pub mod raw;

pub type Result<T> = core::result::Result<T, Error>;
#[repr(transparent)]
pub struct Error(pub(crate) raw::FT_Error);
impl core::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "FT_Error({})", self.0)
    }
}
#[inline(always)]
pub const fn ft_error_to_result(e: raw::FT_Error) -> Result<()> {
    if e == 0 { Ok(()) } else { Err(Error(e)) }
}

#[inline(always)]
pub fn init_freetype() -> Result<raw::FT_Library> {
    let mut inst = MaybeUninit::uninit();
    ft_error_to_result(unsafe { raw::FT_Init_FreeType(inst.as_mut_ptr()) })?;
    Ok(unsafe { inst.assume_init() })
}

#[inline(always)]
pub unsafe fn done_freetype(lib: raw::FT_Library) -> Result<()> {
    ft_error_to_result(unsafe { raw::FT_Done_FreeType(lib) })
}

#[inline(always)]
pub unsafe fn new_face(
    lib: raw::FT_Library,
    filepathname: &core::ffi::CStr,
    face_index: raw::FT_Long,
) -> Result<raw::FT_Face> {
    let mut inst = MaybeUninit::uninit();
    ft_error_to_result(unsafe {
        raw::FT_New_Face(lib, filepathname.as_ptr(), face_index, inst.as_mut_ptr())
    })?;
    Ok(unsafe { inst.assume_init() })
}

#[inline(always)]
pub unsafe fn new_memory_face(
    lib: raw::FT_Library,
    file_base: &[raw::FT_Byte],
    face_index: raw::FT_Long,
) -> Result<raw::FT_Face> {
    let mut inst = MaybeUninit::uninit();
    ft_error_to_result(unsafe {
        raw::FT_New_Memory_Face(
            lib,
            file_base.as_ptr(),
            file_base.len() as raw::FT_Long,
            face_index,
            inst.as_mut_ptr(),
        )
    })?;
    Ok(unsafe { inst.assume_init() })
}

#[inline(always)]
pub unsafe fn open_face(
    lib: raw::FT_Library,
    args: &raw::FT_Open_Args,
    face_index: raw::FT_Long,
) -> Result<raw::FT_Face> {
    let mut inst = MaybeUninit::uninit();
    ft_error_to_result(unsafe { raw::FT_Open_Face(lib, args, face_index, inst.as_mut_ptr()) })?;
    Ok(unsafe { inst.assume_init() })
}

#[inline(always)]
pub unsafe fn attach_file(face: raw::FT_Face, filepathname: &core::ffi::CStr) -> Result<()> {
    ft_error_to_result(unsafe { raw::FT_Attach_File(face, filepathname.as_ptr()) })
}

#[inline(always)]
pub unsafe fn attach_stream(face: raw::FT_Face, parameters: &raw::FT_Open_Args) -> Result<()> {
    ft_error_to_result(unsafe { raw::FT_Attach_Stream(face, parameters) })
}

#[inline(always)]
pub unsafe fn reference_face(face: raw::FT_Face) -> Result<()> {
    ft_error_to_result(unsafe { raw::FT_Reference_Face(face) })
}

#[inline(always)]
pub unsafe fn done_face(face: raw::FT_Face) -> Result<()> {
    ft_error_to_result(unsafe { raw::FT_Done_Face(face) })
}

#[inline(always)]
pub unsafe fn select_size(face: raw::FT_Face, strike_index: raw::FT_Int) -> Result<()> {
    ft_error_to_result(unsafe { raw::FT_Select_Size(face, strike_index) })
}

#[repr(u32)]
pub enum SizeRequestType {
    Nominal = raw::FT_SIZE_REQUEST_TYPE_NOMINAL,
    RealDim = raw::FT_SIZE_REQUEST_TYPE_REAL_DIM,
    BoundingBox = raw::FT_SIZE_REQUEST_TYPE_BBOX,
    Cell = raw::FT_SIZE_REQUEST_TYPE_CELL,
    Scales = raw::FT_SIZE_REQUEST_TYPE_SCALES,
}

#[inline(always)]
pub unsafe fn request_size(face: raw::FT_Face, req: raw::FT_Size_Request) -> Result<()> {
    ft_error_to_result(unsafe { raw::FT_Request_Size(face, req) })
}

#[inline(always)]
pub unsafe fn set_char_size(
    face: raw::FT_Face,
    char_width: raw::FT_F26Dot6,
    char_height: raw::FT_F26Dot6,
    horz_resolution: raw::FT_UInt,
    vert_resolution: raw::FT_UInt,
) -> Result<()> {
    ft_error_to_result(unsafe {
        raw::FT_Set_Char_Size(
            face,
            char_width,
            char_height,
            horz_resolution,
            vert_resolution,
        )
    })
}

#[inline(always)]
pub unsafe fn set_pixel_sizes(
    face: raw::FT_Face,
    pixel_width: raw::FT_UInt,
    pixel_height: raw::FT_UInt,
) -> Result<()> {
    ft_error_to_result(unsafe { raw::FT_Set_Pixel_Sizes(face, pixel_width, pixel_height) })
}

#[inline(always)]
pub unsafe fn load_glyph(
    face: raw::FT_Face,
    glyph_index: raw::FT_UInt,
    load_flags: LoadFlags,
) -> Result<()> {
    ft_error_to_result(unsafe { raw::FT_Load_Glyph(face, glyph_index, load_flags.bits()) })
}

#[inline(always)]
pub unsafe fn load_char(
    face: raw::FT_Face,
    char_code: raw::FT_ULong,
    load_flags: LoadFlags,
) -> Result<()> {
    ft_error_to_result(unsafe { raw::FT_Load_Char(face, char_code, load_flags.bits()) })
}

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy)]
    pub struct LoadFlags : i32 {
        const DEFAULT = raw::FT_LOAD_DEFAULT;
        const NO_SCALE = raw::FT_LOAD_NO_SCALE;
        const NO_HINTING = raw::FT_LOAD_NO_HINTING;
        const RENDER = raw::FT_LOAD_RENDER;
        const NO_BITMAP = raw::FT_LOAD_NO_BITMAP;
        const VERTICAL_LAYOUT = raw::FT_LOAD_VERTICAL_LAYOUT;
        const FORCE_AUTOHINT = raw::FT_LOAD_FORCE_AUTOHINT;
        const CROP_BITMAP = raw::FT_LOAD_CROP_BITMAP;
        const PEDANTIC = raw::FT_LOAD_PEDANTIC;
        const IGNORE_GLOBAL_ADVANCE_WIDTH = raw::FT_LOAD_IGNORE_GLOBAL_ADVANCE_WIDTH;
        const NO_RECURSE = raw::FT_LOAD_NO_RECURSE;
        const IGNORE_TRANSFORM = raw::FT_LOAD_IGNORE_TRANSFORM;
        const MONOCHROME = raw::FT_LOAD_MONOCHROME;
        const LINEAR_DESIGN = raw::FT_LOAD_LINEAR_DESIGN;
        const NO_AUTOHINT = raw::FT_LOAD_NO_AUTOHINT;
        const TARGET_NORMAL = raw::FT_LOAD_TARGET_NORMAL;
        const TARGET_LIGHT = raw::FT_LOAD_TARGET_LIGHT;
        const TARGET_MONO = raw::FT_LOAD_TARGET_MONO;
        const TARGET_LCD = raw::FT_LOAD_TARGET_LCD;
        const TARGET_LCD_V = raw::FT_LOAD_TARGET_LCD_V;
        const COLOR = raw::FT_LOAD_COLOR;
        const COMPUTE_METRICS = raw::FT_LOAD_COMPUTE_METRICS;
        const BITMAP_METRICS_ONLY = raw::FT_LOAD_BITMAP_METRICS_ONLY;
    }
}
impl LoadFlags {
    #[inline(always)]
    pub const fn target_mode(mode: RenderMode) -> Self {
        Self::from_bits_retain((mode as i32 & 15) << 16)
    }
}

#[inline(always)]
pub unsafe fn set_transform(
    face: raw::FT_Face,
    transform: Option<&mut raw::FT_Matrix>,
    delta: Option<&mut raw::FT_Vector>,
) {
    unsafe { raw::FT_Set_Transform(face, opt_pointer_mut(transform), opt_pointer_mut(delta)) }
}

#[inline(always)]
pub unsafe fn render_glyph(slot: raw::FT_GlyphSlot, render_mode: RenderMode) -> Result<()> {
    ft_error_to_result(unsafe { raw::FT_Render_Glyph(slot, render_mode as _) })
}

#[repr(i32)]
#[derive(Debug, Clone, Copy)]
pub enum RenderMode {
    Normal = raw::FT_RENDER_MODE_NORMAL,
    Light = raw::FT_RENDER_MODE_LIGHT,
    Mono = raw::FT_RENDER_MODE_MONO,
    Lcd = raw::FT_RENDER_MODE_LCD,
    LcdV = raw::FT_RENDER_MODE_LCD_V,
}

#[repr(u32)]
#[derive(Debug, Clone, Copy)]
pub enum KerningMode {
    Default = raw::FT_KERNING_DEFAULT,
    Unfitted = raw::FT_KERNING_UNFITTED,
    Unscaled = raw::FT_KERNING_UNSCALED,
}

#[inline(always)]
pub unsafe fn kerning(
    face: raw::FT_Face,
    left_glyph: raw::FT_UInt,
    right_glyph: raw::FT_UInt,
    kern_mode: KerningMode,
) -> Result<raw::FT_Vector> {
    let mut sink = MaybeUninit::uninit();
    ft_error_to_result(unsafe {
        raw::FT_Get_Kerning(
            face,
            left_glyph,
            right_glyph,
            kern_mode as _,
            sink.as_mut_ptr(),
        )
    })?;
    Ok(unsafe { sink.assume_init() })
}

#[inline(always)]
pub unsafe fn track_kerning(
    face: raw::FT_Face,
    point_size: raw::FT_Fixed,
    degree: raw::FT_Int,
) -> Result<raw::FT_Fixed> {
    let mut sink = MaybeUninit::uninit();
    ft_error_to_result(unsafe {
        raw::FT_Get_Track_Kerning(face, point_size, degree, sink.as_mut_ptr())
    })?;
    Ok(unsafe { sink.assume_init() })
}

#[inline(always)]
pub unsafe fn glyph_name(
    face: raw::FT_Face,
    glyph_index: raw::FT_UInt,
    buffer: raw::FT_Pointer,
    buffer_max: raw::FT_UInt,
) -> Result<()> {
    ft_error_to_result(unsafe { raw::FT_Get_Glyph_Name(face, glyph_index, buffer, buffer_max) })
}

#[inline(always)]
pub unsafe fn select_charmap(face: raw::FT_Face, encoding: raw::FT_Encoding) -> Result<()> {
    ft_error_to_result(unsafe { raw::FT_Select_Charmap(face, encoding) })
}

#[inline(always)]
pub unsafe fn set_charmap(face: raw::FT_Face, charmap: raw::FT_CharMap) -> Result<()> {
    ft_error_to_result(unsafe { raw::FT_Set_Charmap(face, charmap) })
}

pub trait FractionalExt {
    fn to_f26dot6_lossy(self) -> raw::FT_F26Dot6;
    fn from_f26dot6_lossy(v: raw::FT_F26Dot6) -> Self;
}
impl FractionalExt for f32 {
    fn to_f26dot6_lossy(self) -> raw::FT_F26Dot6 {
        (self * 64.0) as raw::FT_F26Dot6
    }
    fn from_f26dot6_lossy(v: raw::FT_F26Dot6) -> Self {
        v as f32 / 64.0
    }
}
impl FractionalExt for f64 {
    fn to_f26dot6_lossy(self) -> raw::FT_F26Dot6 {
        (self * 64.0) as raw::FT_F26Dot6
    }
    fn from_f26dot6_lossy(v: raw::FT_F26Dot6) -> Self {
        v as f64 / 64.0
    }
}

#[inline(always)]
const fn opt_pointer_mut<T>(p: Option<&mut T>) -> *mut T {
    match p {
        Some(x) => x,
        None => core::ptr::null_mut(),
    }
}
