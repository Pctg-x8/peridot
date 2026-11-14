use super::*;

unsafe extern "system" {
    pub fn FT_Outline_Decompose(
        outline: *mut FT_Outline,
        func_interface: *const FT_Outline_Funcs,
        user: *mut core::ffi::c_void,
    ) -> FT_Error;
    pub fn FT_Outline_New(
        library: FT_Library,
        num_points: FT_UInt,
        num_contours: FT_Int,
        anoutline: *mut FT_Outline,
    ) -> FT_Error;
    pub fn FT_Outline_Done(library: FT_Library, outline: *mut FT_Outline) -> FT_Error;
    pub fn FT_Outline_Check(outline: *mut FT_Outline) -> FT_Error;
    pub fn FT_Outline_Get_CBox(outline: *const FT_Outline, acbox: *mut FT_BBox);
    pub fn FT_Outline_Translate(outline: *const FT_Outline, x_offset: FT_Pos, y_offset: FT_Pos);
    pub fn FT_Outline_Copy(source: *const FT_Outline, target: *mut FT_Outline) -> FT_Error;
    pub fn FT_Outline_Transform(outline: *const FT_Outline, matrix: *const FT_Matrix);
    pub fn FT_Outline_Embolden(outline: *mut FT_Outline, strength: FT_Pos) -> FT_Error;
    pub fn FT_Outline_EmboldenXY(
        outline: *mut FT_Outline,
        xstrength: FT_Pos,
        ystrength: FT_Pos,
    ) -> FT_Error;
    pub fn FT_Outline_Reverse(outline: *mut FT_Outline);
    pub fn FT_Outline_Get_Bitmap(
        library: FT_Library,
        outline: *mut FT_Outline,
        abitmap: *const FT_Bitmap,
    ) -> FT_Error;
    pub fn FT_Outline_Render(
        library: FT_Library,
        outline: *mut FT_Outline,
        params: *mut FT_Raster_Params,
    ) -> FT_Error;
    pub fn FT_Outline_Get_Orientation(outline: *mut FT_Outline) -> FT_Orientation;
}

pub type FT_Orientation = core::ffi::c_int;
pub const FT_ORIENTATION_TRUETYPE: FT_Orientation = 0;
pub const FT_ORIENTATION_POSTSCRIPT: FT_Orientation = 1;
pub const FT_ORIENTATION_FILL_RIGHT: FT_Orientation = FT_ORIENTATION_TRUETYPE;
pub const FT_ORIENTATION_FILL_LEFT: FT_Orientation = FT_ORIENTATION_POSTSCRIPT;
pub const FT_ORIENTATION_NONE: FT_Orientation = 2;
