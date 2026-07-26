use super::*;

ExternOpaqueStruct!(pub struct FT_IncrementalRec);
pub type FT_Incremental = *mut FT_IncrementalRec;

#[repr(C)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FT_Incremental_MetricsRec {
    pub bearing_x: FT_Long,
    pub bearing_y: FT_Long,
    pub advance: FT_Long,
    pub advance_v: FT_Long,
}
pub type FT_Incremental_Metrics = *mut FT_Incremental_MetricsRec;

pub type FT_Incremental_GetGlyphDataFunc = extern "system" fn(
    incremental: FT_Incremental,
    glyph_index: FT_UInt,
    adata: *mut FT_Data,
) -> FT_Error;
pub type FT_Incremental_FreeGlyphDataFunc =
    extern "system" fn(incremental: FT_Incremental, data: *mut FT_Data);
pub type FT_Incremental_GetGlyphMetricsFunc = extern "system" fn(
    incremental: FT_Incremental,
    glyph_index: FT_UInt,
    vertical: FT_Bool,
    ametrics: *mut FT_Incremental_MetricsRec,
) -> FT_Error;
#[repr(C)]
pub struct FT_Incremental_FuncsRec {
    pub get_glyph_data: FT_Incremental_GetGlyphDataFunc,
    pub free_glyph_data: FT_Incremental_FreeGlyphDataFunc,
    pub get_glyph_metrics: FT_Incremental_GetGlyphMetricsFunc,
}

#[repr(C)]
pub struct FT_Incremental_InterfaceRec {
    pub funcs: *const FT_Incremental_FuncsRec,
    pub object: FT_Incremental,
}
pub type FT_Incremental_Interface = *mut FT_Incremental_InterfaceRec;
