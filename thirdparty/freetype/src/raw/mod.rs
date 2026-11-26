#![allow(non_camel_case_types, non_snake_case, non_upper_case_globals)]

/// https://doc.rust-lang.org/nomicon/ffi.html#representing-opaque-structs
macro_rules! ExternOpaqueStruct {
    ($v: vis struct $t: ident) => {
        #[repr(C)]
        $v struct $t {
            _data: [u8; 0],
            _marker: core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,
        }
    };
}

mod types;
pub use self::types::*;
mod system;
pub use self::system::*;
mod image;
pub use self::image::*;
mod advanc;
pub use self::advanc::*;
mod bbox;
pub use self::bbox::*;
mod bdf;
pub use self::bdf::*;
mod bitmap;
pub use self::bitmap::*;
mod bzip2;
pub use self::bzip2::*;
mod cache;
pub use self::cache::*;
mod cid;
pub use self::cid::*;
mod color;
pub use self::color::*;
mod driver;
pub use self::driver::*;
mod errors;
pub use self::errors::*;
mod fntfmt;
pub use self::fntfmt::*;
mod gasp;
pub use self::gasp::*;
mod glyph;
pub use self::glyph::*;
mod gxval;
pub use self::gxval::*;
mod gzip;
pub use self::gzip::*;
mod increm;
pub use self::increm::*;
mod lcdfil;
pub use self::lcdfil::*;
mod list;
pub use self::list::*;
mod modapi;
pub use self::modapi::*;
mod outline;
pub use self::outline::*;
mod params;
pub use self::params::*;

#[repr(C)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FT_Glyph_Metrics {
    pub width: FT_Pos,
    pub height: FT_Pos,
    pub horiBearingX: FT_Pos,
    pub horiBearingY: FT_Pos,
    pub horiAdvance: FT_Pos,
    pub vertBearingX: FT_Pos,
    pub vertBearingY: FT_Pos,
    pub vertAdvance: FT_Pos,
}

#[repr(C)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FT_Bitmap_Size {
    pub height: FT_Short,
    pub width: FT_Short,
    pub size: FT_Pos,
    pub x_ppem: FT_Pos,
    pub y_ppem: FT_Pos,
}

ExternOpaqueStruct!(pub struct FT_LibraryRec);
ExternOpaqueStruct!(pub struct FT_ModuleRec);
ExternOpaqueStruct!(pub struct FT_DriverRec);
ExternOpaqueStruct!(pub struct FT_RendererRec);
pub type FT_Library = *mut FT_LibraryRec;
pub type FT_Module = *mut FT_ModuleRec;
pub type FT_Driver = *mut FT_DriverRec;
pub type FT_Renderer = *mut FT_RendererRec;
pub type FT_Face = *mut FT_FaceRec;
pub type FT_Size = *mut FT_SizeRec;
pub type FT_GlyphSlot = *mut FT_GlyphSlotRec;
pub type FT_CharMap = *mut FT_CharMapRec;

macro_rules! FT_ENC_TAG {
    ($x1: expr, $x2: expr, $x3: expr, $x4: expr) => {
        (($x1 as u32) << 24) | (($x2 as u32) << 16) | (($x3 as u32) << 8) | ($x4 as u32)
    };
}
pub type FT_Encoding = u32;
pub const FT_ENCODING_NONE: FT_Encoding = FT_ENC_TAG!(0, 0, 0, 0);
pub const FT_ENCODING_MS_SYMBOL: FT_Encoding = FT_ENC_TAG!('s', 'y', 'm', 'b');
pub const FT_ENCODING_UNICODE: FT_Encoding = FT_ENC_TAG!('u', 'n', 'i', 'c');
pub const FT_ENCODING_SJIS: FT_Encoding = FT_ENC_TAG!('s', 'j', 'i', 's');
pub const FT_ENCODING_PRC: FT_Encoding = FT_ENC_TAG!('g', 'b', ' ', ' ');
pub const FT_ENCODING_BIG5: FT_Encoding = FT_ENC_TAG!('b', 'i', 'g', '5');
pub const FT_ENCODING_WANSUNG: FT_Encoding = FT_ENC_TAG!('y', 'a', 'n', 's');
pub const FT_ENCODING_JOHAB: FT_Encoding = FT_ENC_TAG!('j', 'o', 'h', 'a');
pub const FT_ENCODING_GB2312: FT_Encoding = FT_ENCODING_PRC;
pub const FT_ENCODING_MS_SJIS: FT_Encoding = FT_ENCODING_SJIS;
pub const FT_ENCODING_MS_GB2312: FT_Encoding = FT_ENCODING_PRC;
pub const FT_ENCODING_MS_BIG5: FT_Encoding = FT_ENCODING_BIG5;
pub const FT_ENCODING_MS_WANSUNG: FT_Encoding = FT_ENCODING_WANSUNG;
pub const FT_ENCODING_MS_JOHAB: FT_Encoding = FT_ENCODING_JOHAB;
pub const FT_ENCODING_STANDARD: FT_Encoding = FT_ENC_TAG!('A', 'D', 'O', 'B');
pub const FT_ENCODING_EXPERT: FT_Encoding = FT_ENC_TAG!('A', 'D', 'B', 'E');
pub const FT_ENCODING_CUSTOM: FT_Encoding = FT_ENC_TAG!('A', 'D', 'B', 'C');
pub const FT_ENCODING_LATIN_1: FT_Encoding = FT_ENC_TAG!('l', 'a', 't', '1');
pub const FT_ENCODING_LATIN_2: FT_Encoding = FT_ENC_TAG!('l', 'a', 't', '2');
pub const FT_ENCODING_APPLE_ROMAN: FT_Encoding = FT_ENC_TAG!('a', 'r', 'm', 'n');

#[repr(C)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FT_CharMapRec {
    pub face: FT_Face,
    pub encoding: FT_Encoding,
    pub platform_id: FT_UShort,
    pub encoding_id: FT_UShort,
}

#[repr(C)]
pub struct FT_FaceRec {
    pub num_faces: FT_Long,
    pub face_index: FT_Long,
    pub face_flags: FT_Long,
    pub style_flags: FT_Long,
    pub num_glyphs: FT_Long,
    pub family_name: *mut FT_String,
    pub style_name: *mut FT_String,
    pub num_fixed_sizes: FT_Int,
    pub available_sizes: *mut FT_Bitmap_Size,
    pub num_charmaps: FT_Int,
    pub charmaps: *mut FT_CharMap,
    pub generic: FT_Generic,
    pub bbox: FT_BBox,
    pub units_per_em: FT_UShort,
    pub ascender: FT_Short,
    pub descender: FT_Short,
    pub height: FT_Short,
    pub max_advance_width: FT_Short,
    pub max_advance_height: FT_Short,
    pub underline_position: FT_Short,
    pub underline_thickness: FT_Short,
    pub glyph: FT_GlyphSlot,
    pub size: FT_Size,
    pub charmap: FT_CharMap,
    driver: FT_Driver,
    memory: FT_Memory,
    stream: FT_Stream,
    sizes_list: FT_ListRec,
    autohint: FT_Generic,
    extensions: *mut core::ffi::c_void,
    internal: *mut core::ffi::c_void,
}

pub const FT_FACE_FLAG_SCALABLE: FT_Long = 1 << 0;
pub const FT_FACE_FLAG_FIXED_SIZES: FT_Long = 1 << 1;
pub const FT_FACE_FLAG_FIXED_WIDTH: FT_Long = 1 << 2;
pub const FT_FACE_FLAG_SFNT: FT_Long = 1 << 3;
pub const FT_FACE_FLAG_HORIZONTAL: FT_Long = 1 << 4;
pub const FT_FACE_FLAG_VERTICAL: FT_Long = 1 << 5;
pub const FT_FACE_FLAG_KERNING: FT_Long = 1 << 6;
pub const FT_FACE_FLAG_FAST_GLYPHS: FT_Long = 1 << 7;
pub const FT_FACE_FLAG_MULTIPLE_MASTERS: FT_Long = 1 << 8;
pub const FT_FACE_FLAG_GLYPH_NAMES: FT_Long = 1 << 9;
pub const FT_FACE_FLAG_EXTERNAL_STREAM: FT_Long = 1 << 10;
pub const FT_FACE_FLAG_HINTER: FT_Long = 1 << 11;
pub const FT_FACE_FLAG_CID_KEYED: FT_Long = 1 << 12;
pub const FT_FACE_FLAG_TRICKY: FT_Long = 1 << 13;
pub const FT_FACE_FLAG_COLOR: FT_Long = 1 << 14;
pub const FT_FACE_FLAG_VARIATION: FT_Long = 1 << 15;
impl FT_FaceRec {
    pub const fn flags_has_horizontal(&self) -> bool {
        (self.face_flags & FT_FACE_FLAG_HORIZONTAL) != 0
    }
    pub const fn flags_has_vertical(&self) -> bool {
        (self.face_flags & FT_FACE_FLAG_VERTICAL) != 0
    }
    pub const fn flags_has_kerning(&self) -> bool {
        (self.face_flags & FT_FACE_FLAG_KERNING) != 0
    }
    pub const fn flags_is_scalable(&self) -> bool {
        (self.face_flags & FT_FACE_FLAG_SCALABLE) != 0
    }
    pub const fn flags_is_sfnt(&self) -> bool {
        (self.face_flags & FT_FACE_FLAG_SFNT) != 0
    }
    pub const fn flags_is_fixed_width(&self) -> bool {
        (self.face_flags & FT_FACE_FLAG_FIXED_WIDTH) != 0
    }
    pub const fn flags_has_fixed_sizes(&self) -> bool {
        (self.face_flags & FT_FACE_FLAG_FIXED_SIZES) != 0
    }
    pub const fn flags_has_glyph_names(&self) -> bool {
        (self.face_flags & FT_FACE_FLAG_GLYPH_NAMES) != 0
    }
    pub const fn flags_has_multiple_masters(&self) -> bool {
        (self.face_flags & FT_FACE_FLAG_MULTIPLE_MASTERS) != 0
    }
    pub const fn is_named_instance(&self) -> bool {
        (self.face_index & 0x7fff_0000) != 0
    }
    pub const fn flags_is_variation(&self) -> bool {
        (self.face_flags & FT_FACE_FLAG_VARIATION) != 0
    }
    pub const fn flags_is_cid_keyed(&self) -> bool {
        (self.face_flags & FT_FACE_FLAG_CID_KEYED) != 0
    }
    pub const fn flags_is_tricky(&self) -> bool {
        (self.face_flags & FT_FACE_FLAG_TRICKY) != 0
    }
    pub const fn flags_has_color(&self) -> bool {
        (self.face_flags & FT_FACE_FLAG_COLOR) != 0
    }
}

pub const FT_STYLE_FLAG_ITALIC: FT_Long = 1 << 0;
pub const FT_STYLE_FLAG_BOLD: FT_Long = 1 << 1;

#[repr(C)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FT_Size_Metrics {
    pub x_ppem: FT_UShort,
    pub y_ppem: FT_UShort,
    pub x_scale: FT_Fixed,
    pub y_scale: FT_Fixed,
    pub ascender: FT_Pos,
    pub descender: FT_Pos,
    pub height: FT_Pos,
    pub max_advance: FT_Pos,
}
#[repr(C)]
pub struct FT_SizeRec {
    pub face: FT_Face,
    pub generic: FT_Generic,
    pub metrics: FT_Size_Metrics,
    internal: *mut core::ffi::c_void,
}

ExternOpaqueStruct!(pub struct FT_SubGlyphRec);
pub type FT_SubGlyph = *mut FT_SubGlyphRec;

#[repr(C)]
pub struct FT_GlyphSlotRec {
    pub library: FT_Library,
    pub face: FT_Face,
    pub next: FT_GlyphSlot,
    pub glyph_index: FT_UInt,
    pub generic: FT_Generic,
    pub metrics: FT_Glyph_Metrics,
    pub linearHoriAdvance: FT_Fixed,
    pub linearVertAdvance: FT_Fixed,
    pub advance: FT_Vector,
    pub format: FT_Glyph_Format,
    pub bitmap: FT_Bitmap,
    pub bitmap_left: FT_Int,
    pub bitmap_top: FT_Int,
    pub outline: FT_Outline,
    pub num_subglyphs: FT_UInt,
    pub subglyphs: FT_SubGlyph,
    pub control_data: *mut core::ffi::c_void,
    pub control_len: core::ffi::c_long,
    pub lsb_delta: FT_Pos,
    pub rsb_delta: FT_Pos,
    pub other: *mut core::ffi::c_void,
    internal: *mut core::ffi::c_void,
}

#[link(name = "freetype")]
unsafe extern "system" {
    pub fn FT_Init_FreeType(alibrary: *mut FT_Library) -> FT_Error;
    pub fn FT_Done_FreeType(library: FT_Library) -> FT_Error;
}

pub const FT_OPEN_MEMORY: FT_UInt = 0x01;
pub const FT_OPEN_STREAM: FT_UInt = 0x02;
pub const FT_OPEN_PATHNAME: FT_UInt = 0x04;
pub const FT_OPEN_DRIVER: FT_UInt = 0x08;
pub const FT_OPEN_PARAMS: FT_UInt = 0x10;

#[repr(C)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FT_Parameter {
    pub tag: FT_ULong,
    pub data: FT_Pointer,
}
#[repr(C)]
pub struct FT_Open_Args {
    pub flags: FT_UInt,
    pub memory_base: *const FT_Byte,
    pub memory_size: FT_Long,
    pub pathname: *mut FT_String,
    pub stream: FT_Stream,
    pub driver: FT_Module,
    pub num_params: FT_Int,
    pub params: *mut FT_Parameter,
}
unsafe extern "system" {
    pub fn FT_New_Face(
        library: FT_Library,
        filepathname: *const core::ffi::c_char,
        face_index: FT_Long,
        aface: *mut FT_Face,
    ) -> FT_Error;
    pub fn FT_New_Memory_Face(
        library: FT_Library,
        file_base: *const FT_Byte,
        file_size: FT_Long,
        face_index: FT_Long,
        aface: *mut FT_Face,
    ) -> FT_Error;
    pub fn FT_Open_Face(
        library: FT_Library,
        args: *const FT_Open_Args,
        face_index: FT_Long,
        aface: *mut FT_Face,
    ) -> FT_Error;
    pub fn FT_Attach_File(face: FT_Face, filepathname: *const core::ffi::c_char) -> FT_Error;
    pub fn FT_Attach_Stream(face: FT_Face, parameters: *const FT_Open_Args) -> FT_Error;
    pub fn FT_Reference_Face(face: FT_Face) -> FT_Error;
    pub fn FT_Done_Face(face: FT_Face) -> FT_Error;
    pub fn FT_Select_Size(face: FT_Face, strike_index: FT_Int) -> FT_Error;
}
pub type FT_Size_Request_Type = u32;
pub const FT_SIZE_REQUEST_TYPE_NOMINAL: FT_Size_Request_Type = 0;
pub const FT_SIZE_REQUEST_TYPE_REAL_DIM: FT_Size_Request_Type = 1;
pub const FT_SIZE_REQUEST_TYPE_BBOX: FT_Size_Request_Type = 2;
pub const FT_SIZE_REQUEST_TYPE_CELL: FT_Size_Request_Type = 3;
pub const FT_SIZE_REQUEST_TYPE_SCALES: FT_Size_Request_Type = 4;

#[repr(C)]
#[derive(Debug)]
pub struct FT_Size_RequestRec {
    pub type_: FT_Size_Request_Type,
    pub width: FT_Long,
    pub height: FT_Long,
    pub horiResolution: FT_UInt,
    pub vertResolution: FT_UInt,
}
pub type FT_Size_Request = *mut FT_Size_RequestRec;

unsafe extern "system" {
    pub fn FT_Request_Size(face: FT_Face, req: FT_Size_Request) -> FT_Error;
    pub fn FT_Set_Char_Size(
        face: FT_Face,
        char_width: FT_F26Dot6,
        char_height: FT_F26Dot6,
        horz_resolution: FT_UInt,
        vert_resolution: FT_UInt,
    ) -> FT_Error;
    pub fn FT_Set_Pixel_Sizes(
        face: FT_Face,
        pixel_width: FT_UInt,
        pixel_height: FT_UInt,
    ) -> FT_Error;
    pub fn FT_Load_Glyph(face: FT_Face, glyph_index: FT_UInt, load_flags: i32) -> FT_Error;
    pub fn FT_Load_Char(face: FT_Face, char_code: FT_ULong, load_flags: i32) -> FT_Error;
}

pub const FT_LOAD_DEFAULT: i32 = 0;
pub const FT_LOAD_NO_SCALE: i32 = 1 << 0;
pub const FT_LOAD_NO_HINTING: i32 = 1 << 1;
pub const FT_LOAD_RENDER: i32 = 1 << 2;
pub const FT_LOAD_NO_BITMAP: i32 = 1 << 3;
pub const FT_LOAD_VERTICAL_LAYOUT: i32 = 1 << 4;
pub const FT_LOAD_FORCE_AUTOHINT: i32 = 1 << 5;
pub const FT_LOAD_CROP_BITMAP: i32 = 1 << 6;
pub const FT_LOAD_PEDANTIC: i32 = 1 << 7;
pub const FT_LOAD_IGNORE_GLOBAL_ADVANCE_WIDTH: i32 = 1 << 9;
pub const FT_LOAD_NO_RECURSE: i32 = 1 << 10;
pub const FT_LOAD_IGNORE_TRANSFORM: i32 = 1 << 11;
pub const FT_LOAD_MONOCHROME: i32 = 1 << 12;
pub const FT_LOAD_LINEAR_DESIGN: i32 = 1 << 13;
pub const FT_LOAD_NO_AUTOHINT: i32 = 1 << 15;
/* bits 6-19 are used by `FT_LOAD_TARGET_***`  */
pub const FT_LOAD_COLOR: i32 = 1 << 20;
pub const FT_LOAD_COMPUTE_METRICS: i32 = 1 << 21;
pub const FT_LOAD_BITMAP_METRICS_ONLY: i32 = 1 << 22;

pub const FT_LOAD_TARGET_NORMAL: i32 = (FT_RENDER_MODE_NORMAL & 15) << 16;
pub const FT_LOAD_TARGET_LIGHT: i32 = (FT_RENDER_MODE_LIGHT & 15) << 16;
pub const FT_LOAD_TARGET_MONO: i32 = (FT_RENDER_MODE_MONO & 15) << 16;
pub const FT_LOAD_TARGET_LCD: i32 = (FT_RENDER_MODE_LCD & 15) << 16;
pub const FT_LOAD_TARGET_LCD_V: i32 = (FT_RENDER_MODE_LCD_V & 15) << 16;

#[macro_export]
macro_rules! FT_LOAD_TARGET_MODE {
    ($x: expr) => {
        ((x >> 16) & 15) as FT_Render_Mode
    };
}

unsafe extern "system" {
    pub fn FT_Set_Transform(face: FT_Face, matrix: *mut FT_Matrix, delta: *mut FT_Vector);
}

pub type FT_Render_Mode = i32;
pub const FT_RENDER_MODE_NORMAL: FT_Render_Mode = 0;
pub const FT_RENDER_MODE_LIGHT: FT_Render_Mode = 1;
pub const FT_RENDER_MODE_MONO: FT_Render_Mode = 2;
pub const FT_RENDER_MODE_LCD: FT_Render_Mode = 3;
pub const FT_RENDER_MODE_LCD_V: FT_Render_Mode = 4;

unsafe extern "system" {
    pub fn FT_Render_Glyph(slot: FT_GlyphSlot, render_mode: FT_Render_Mode) -> FT_Error;
}

pub type FT_Kerning_Mode = u32;
pub const FT_KERNING_DEFAULT: FT_Kerning_Mode = 0;
pub const FT_KERNING_UNFITTED: FT_Kerning_Mode = 1;
pub const FT_KERNING_UNSCALED: FT_Kerning_Mode = 2;

unsafe extern "system" {
    pub fn FT_Get_Kerning(
        face: FT_Face,
        left_glyph: FT_UInt,
        right_glyph: FT_UInt,
        kern_mode: FT_UInt,
        akerning: *mut FT_Vector,
    ) -> FT_Error;
    pub fn FT_Get_Track_Kerning(
        face: FT_Face,
        point_size: FT_Fixed,
        degree: FT_Int,
        akerning: *mut FT_Fixed,
    ) -> FT_Error;
    pub fn FT_Get_Glyph_Name(
        face: FT_Face,
        glyph_index: FT_UInt,
        buffer: FT_Pointer,
        buffer_max: FT_UInt,
    ) -> FT_Error;
    pub fn FT_Get_Postscript_Name(face: FT_Face) -> *const core::ffi::c_char;
    pub fn FT_Select_Charmap(face: FT_Face, encoding: FT_Encoding) -> FT_Error;
    pub fn FT_Set_Charmap(face: FT_Face, charmap: FT_CharMap) -> FT_Error;
    pub fn FT_Get_Charmap_Index(charmap: FT_CharMap) -> FT_Int;
    pub fn FT_Get_Char_Index(face: FT_Face, charcode: FT_ULong) -> FT_UInt;
    pub fn FT_Get_First_Char(face: FT_Face, agindex: *mut FT_UInt) -> FT_ULong;
    pub fn FT_Get_Next_Char(face: FT_Face, char_code: FT_ULong, agindex: *mut FT_UInt) -> FT_ULong;
    pub fn FT_Face_Properties(
        face: FT_Face,
        num_properties: FT_UInt,
        properties: *mut FT_Parameter,
    ) -> FT_Error;
    pub fn FT_Get_Name_Index(face: FT_Face, glyph_name: *const FT_String) -> FT_UInt;
}

pub const FT_SUBGLYPH_FLAG_ARGS_ARE_WORDS: FT_UInt = 1;
pub const FT_SUBGLYPH_FLAG_ARGS_ARE_XY_VALUES: FT_UInt = 2;
pub const FT_SUBGLYPH_FLAG_ROUND_XY_TO_GRID: FT_UInt = 4;
pub const FT_SUBGLYPH_FLAG_SCALE: FT_UInt = 8;
pub const FT_SUBGLYPH_FLAG_XY_SCALE: FT_UInt = 0x40;
pub const FT_SUBGLYPH_FLAG_2X2: FT_UInt = 0x80;
pub const FT_SUBGLYPH_FLAG_USE_MY_METRICS: FT_UInt = 0x200;

unsafe extern "system" {
    pub fn FT_Get_SubGlyph_Info(
        glyph: FT_GlyphSlot,
        sub_index: FT_UInt,
        p_index: *mut FT_Int,
        p_flags: *mut FT_UInt,
        p_arg1: *mut FT_Int,
        p_arg2: *mut FT_Int,
        p_transform: *mut FT_Matrix,
    ) -> FT_Error;
}

#[repr(C)]
pub struct FT_LayerIterator {
    pub num_layers: FT_UInt,
    pub layer: FT_UInt,
    pub p: *mut FT_Byte,
}

unsafe extern "system" {
    pub fn FT_Get_Color_Glyph_Layer(
        face: FT_Face,
        base_glyph: FT_UInt,
        aglyph_index: *mut FT_UInt,
        acolor_index: *mut FT_UInt,
        iterator: *mut FT_LayerIterator,
    ) -> FT_Bool;
}

pub const FT_FSTYPE_INSTALLABLE_EMBEDDING: FT_UShort = 0x0000;
pub const FT_FSTYPE_RESTRICTED_LICENSE_EMBEDDING: FT_UShort = 0x0002;
pub const FT_FSTYPE_PREVIEW_AND_PRINT_EMBEDDING: FT_UShort = 0x0004;
pub const FT_FSTYPE_EDITABLE_EMBEDDING: FT_UShort = 0x0008;
pub const FT_FSTYPE_NO_SUBSETTING: FT_UShort = 0x0100;
pub const FT_FSTYPE_BITMAP_EMBEDDING_ONLY: FT_UShort = 0x0200;

unsafe extern "system" {
    pub fn FT_Get_FSType_Flags(face: FT_Face) -> FT_UShort;
    pub fn FT_Face_GetCharVariantIndex(
        face: FT_Face,
        charcode: FT_ULong,
        variant_selector: FT_ULong,
    ) -> FT_UInt;
    pub fn FT_Face_GetCharVariantIsDefault(
        face: FT_Face,
        charcode: FT_ULong,
        variant_selector: FT_ULong,
    ) -> FT_Int;
    pub fn FT_Face_GetVariantSelectors(face: FT_Face) -> *mut u32;
    pub fn FT_Face_GetVariantsOfChar(face: FT_Face, charcode: FT_ULong) -> *mut u32;
    pub fn FT_Face_GetCharsOfVariant(face: FT_Face, variant_selector: FT_ULong) -> *mut u32;
    pub fn FT_MulDiv(a: FT_Long, b: FT_Long, c: FT_Long) -> FT_Long;
    pub fn FT_MulFix(a: FT_Long, b: FT_Long) -> FT_Long;
    pub fn FT_DivFix(a: FT_Long, b: FT_Long) -> FT_Long;
    pub fn FT_RoundFix(a: FT_Fixed) -> FT_Fixed;
    pub fn FT_CeilFix(a: FT_Fixed) -> FT_Fixed;
    pub fn FT_FloorFix(a: FT_Fixed) -> FT_Fixed;
    pub fn FT_Vector_Tranform(vector: *mut FT_Vector, matrix: *const FT_Matrix);
}

pub const FREETYPE_MAJOR: FT_Int = 2;
pub const FREETYPE_MINOR: FT_Int = 10;
pub const FREETYPE_PATCH: FT_Int = 1;

unsafe extern "system" {
    pub fn FT_Library_Version(
        library: FT_Library,
        amajor: *mut FT_Int,
        aminor: *mut FT_Int,
        apatch: *mut FT_Int,
    );
    pub fn FT_Face_CheckTrueTypePatents(face: FT_Face) -> FT_Bool;
    #[deprecated(note = "does nothing")]
    pub fn FT_Face_SetUnpatentedHinting(face: FT_Face, value: FT_Bool) -> FT_Bool;
}
