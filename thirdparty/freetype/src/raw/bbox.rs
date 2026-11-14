use super::*;

unsafe extern "system" {
    pub fn FT_Outline_Get_BBox(outline: *mut FT_Outline, abbox: *mut FT_BBox) -> FT_Error;
}
