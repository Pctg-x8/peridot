use super::*;

ExternOpaqueStruct!(pub struct _FT_Glyph_Class);
pub type FT_Glyph_Class = *mut _FT_Glyph_Class;

pub type FT_Glyph = *mut FT_GlyphRec;
#[repr(C)]
pub struct FT_GlyphRec {
    pub library: FT_Library,
    pub clazz: *const FT_Glyph_Class,
    pub format: FT_Glyph_Format,
    pub advance: FT_Vector,
}

pub type FT_BitmapGlyph = *mut FT_BitmapGlyphRec;
#[repr(C)]
pub struct FT_BitmapGlyphRec {
    pub root: FT_GlyphRec,
    pub left: FT_Int,
    pub top: FT_Int,
    pub bitmap: FT_Bitmap,
}

pub type FT_OutlineGlyph = *mut FT_OutlineGlyphRec;
#[repr(C)]
pub struct FT_OutlineGlyphRec {
    pub root: FT_GlyphRec,
    pub outline: FT_Outline,
}

unsafe extern "system" {
    pub fn FT_New_Glyph(
        library: FT_Library,
        format: FT_Glyph_Format,
        aglyph: *mut FT_Glyph,
    ) -> FT_Error;
    pub fn FT_Get_Glyph(slot: FT_GlyphSlot, aglyph: *mut FT_Glyph) -> FT_Error;
    pub fn FT_Glyph_Copy(source: FT_Glyph, target: *mut FT_Glyph) -> FT_Error;
    pub fn FT_Glyph_Transform(
        glyph: FT_Glyph,
        matrix: *mut FT_Matrix,
        delta: *mut FT_Vector,
    ) -> FT_Error;
}

pub type FT_Glyph_BBox_Mode = FT_UInt;
pub const FT_GLYPH_BBOX_UNSCALED: FT_Glyph_BBox_Mode = 0;
pub const FT_GLYPH_BBOX_SUBPIXELS: FT_Glyph_BBox_Mode = 0;
pub const FT_GLYPH_BBOX_GRIDFIT: FT_Glyph_BBox_Mode = 1;
pub const FT_GLYPH_BBOX_TRUNCATE: FT_Glyph_BBox_Mode = 2;
pub const FT_GLYPH_BBOX_PIXELS: FT_Glyph_BBox_Mode = 3;

unsafe extern "system" {
    pub fn FT_Glyph_Get_CBox(glyph: FT_Glyph, bbox_mode: FT_UInt, acbox: *mut FT_BBox);
    pub fn FT_Glyph_To_Bitmap(
        the_glyph: *mut FT_Glyph,
        render_mode: FT_Render_Mode,
        origin: *mut FT_Vector,
        destroy: FT_Bool,
    ) -> FT_Error;
    pub fn FT_Done_Glyph(glyph: FT_Glyph);

    pub fn FT_Matrix_Multiply(a: *const FT_Matrix, b: *mut FT_Matrix);
    pub fn FT_Matrix_Invert(matrix: *mut FT_Matrix) -> FT_Error;
}
