use super::*;

pub const FT_HINTING_FREETYPE: u32 = 0;
pub const FT_HINTING_ADOBE: u32 = 1;

pub const TT_INTERPRETER_VERSION_35: u32 = 35;
pub const TT_INTERPRETER_VERSION_38: u32 = 38;
pub const TT_INTERPRETER_VERSION_40: u32 = 40;

pub const FT_AUTOHINTER_SCRIPT_NONE: u32 = 0;
pub const FT_AUTOHINTER_SCRIPT_LATIN: u32 = 1;
pub const FT_AUTOHINTER_SCRIPT_CJK: u32 = 2;
pub const FT_AUTOHINTER_SCRIPT_INDIC: u32 = 3;

#[repr(C)]
pub struct FT_Prop_GlyphToScriptMap {
    pub face: FT_Face,
    pub map: *mut FT_UShort,
}

#[repr(C)]
pub struct FT_Prop_IncreaseXHeight {
    pub face: FT_Face,
    pub limit: FT_UInt,
}
