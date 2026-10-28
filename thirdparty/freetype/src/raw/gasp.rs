use super::*;

pub const FT_GASP_NO_TABLE: FT_Int = -1;
pub const FT_GASP_DO_GRIDFIT: FT_Int = 0x01;
pub const FT_GASP_DO_GRAY: FT_Int = 0x02;
pub const FT_GASP_SYMMETRIC_GRIDFIT: FT_Int = 0x04;
pub const FT_GASP_SYMMETRIC_SMOOTHING: FT_Int = 0x08;

unsafe extern "system" {
    pub fn FT_Get_Gasp(face: FT_Face, ppem: FT_UInt) -> FT_Int;
}
