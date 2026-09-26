use super::*;

#[repr(C)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FT_Color {
    pub blue: FT_Byte,
    pub green: FT_Byte,
    pub red: FT_Byte,
    pub alpha: FT_Byte,
}

pub const FT_PALETTE_FOR_LIGHT_BACKGROUND: FT_UShort = 0x01;
pub const FT_PALETTE_FOR_DARK_BACKGROUND: FT_UShort = 0x02;
#[repr(C)]
pub struct FT_Palette_Data {
    pub num_palettes: FT_UShort,
    pub palette_name_ids: *const FT_UShort,
    pub palette_flags: *const FT_UShort,

    pub num_palette_entries: FT_UShort,
    pub palette_entry_name_ids: *const FT_UShort,
}

unsafe extern "system" {
    pub fn FT_Palette_Data_Get(face: FT_Face, apalette: *mut FT_Palette_Data) -> FT_Error;
    pub fn FT_Palette_Select(
        face: FT_Face,
        palette_index: FT_UShort,
        apalette: *mut *mut FT_Color,
    ) -> FT_Error;
    pub fn FT_Palette_Set_Foregound_Color(face: FT_Face, foreground_color: FT_Color) -> FT_Error;
}
