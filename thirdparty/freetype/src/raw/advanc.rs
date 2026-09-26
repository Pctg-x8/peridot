use super::*;

pub const FT_ADVANCE_FLAG_FAST_ONLY: i32 = 0x2000_0000;

unsafe extern "system" {
    pub fn FT_Get_Advance(
        face: FT_Face,
        gindex: FT_UInt,
        load_flags: i32,
        padvance: *mut FT_Fixed,
    ) -> FT_Error;
    pub fn FT_Get_Advances(
        face: FT_Face,
        start: FT_UInt,
        count: FT_UInt,
        load_flags: i32,
        padvances: *mut FT_Fixed,
    ) -> FT_Error;
}
