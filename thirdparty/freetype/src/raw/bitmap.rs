use super::*;

unsafe extern "system" {
    pub fn FT_Bitmap_Init(abitmap: *mut FT_Bitmap);
    pub fn FT_Bitmap_New(abitmap: *mut FT_Bitmap);
    pub fn FT_Bitmap_Copy(
        library: FT_Library,
        source: *const FT_Bitmap,
        target: *mut FT_Bitmap,
    ) -> FT_Error;
    pub fn FT_Bitmap_Embolden(
        library: FT_Library,
        bitmap: *mut FT_Bitmap,
        x_strength: FT_Pos,
        y_strength: FT_Pos,
    ) -> FT_Error;
    pub fn FT_Bitmap_Convert(
        library: FT_Library,
        source: *const FT_Bitmap,
        target: *mut FT_Bitmap,
        alignment: FT_Int,
    ) -> FT_Error;
    pub fn FT_Bitmap_Blend(
        library: FT_Library,
        source: *const FT_Bitmap,
        source_offset: FT_Vector,
        target: *mut FT_Bitmap,
        atarget_offset: *mut FT_Vector,
        color: FT_Color,
    ) -> FT_Error;
    pub fn FT_GlyphSlot_Own_Bitmap(slot: FT_GlyphSlot) -> FT_Error;
    pub fn FT_Bitmap_Done(library: FT_Library, bitmap: *mut FT_Bitmap) -> FT_Error;
}
