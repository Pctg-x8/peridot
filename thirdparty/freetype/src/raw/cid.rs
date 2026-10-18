use super::*;

unsafe extern "system" {
    pub fn FT_Get_CID_Registry_Ordering_Supplement(
        face: FT_Face,
        registry: *const core::ffi::c_char,
        ordering: *const core::ffi::c_char,
        supplement: *mut FT_Int,
    ) -> FT_Error;
    pub fn FT_Get_CID_Is_Internally_CID_Keyed(face: FT_Face, is_cid: *mut FT_Bool) -> FT_Error;
    pub fn FT_Get_CID_From_Glyph_Index(
        face: FT_Face,
        glyph_index: FT_UInt,
        cid: *mut FT_UInt,
    ) -> FT_Error;
}
