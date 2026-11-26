use super::*;

pub const FT_VALIDATE_feat_INDEX: FT_UInt = 0;
pub const FT_VALIDATE_mort_INDEX: FT_UInt = 1;
pub const FT_VALIDATE_morx_INDEX: FT_UInt = 2;
pub const FT_VALIDATE_bsln_INDEX: FT_UInt = 3;
pub const FT_VALIDATE_just_INDEX: FT_UInt = 4;
pub const FT_VALIDATE_kern_INDEX: FT_UInt = 5;
pub const FT_VALIDATE_opbd_INDEX: FT_UInt = 6;
pub const FT_VALIDATE_trak_INDEX: FT_UInt = 7;
pub const FT_VALIDATE_prop_INDEX: FT_UInt = 8;
pub const FT_VALIDATE_lcar_INDEX: FT_UInt = 9;
pub const FT_VALIDATE_GX_LAST_INDEX: FT_UInt = FT_VALIDATE_lcar_INDEX;
pub const FT_VALIDATE_GX_LENGTH: FT_UInt = FT_VALIDATE_GX_LAST_INDEX + 1;

pub const FT_VALIDATE_GX_START: FT_UInt = 0x4000;
pub const FT_VALIDATE_feat: FT_UInt = FT_VALIDATE_GX_START << FT_VALIDATE_feat_INDEX;
pub const FT_VALIDATE_mort: FT_UInt = FT_VALIDATE_GX_START << FT_VALIDATE_mort_INDEX;
pub const FT_VALIDATE_morx: FT_UInt = FT_VALIDATE_GX_START << FT_VALIDATE_morx_INDEX;
pub const FT_VALIDATE_bsln: FT_UInt = FT_VALIDATE_GX_START << FT_VALIDATE_bsln_INDEX;
pub const FT_VALIDATE_just: FT_UInt = FT_VALIDATE_GX_START << FT_VALIDATE_just_INDEX;
pub const FT_VALIDATE_kern: FT_UInt = FT_VALIDATE_GX_START << FT_VALIDATE_kern_INDEX;
pub const FT_VALIDATE_opbd: FT_UInt = FT_VALIDATE_GX_START << FT_VALIDATE_opbd_INDEX;
pub const FT_VALIDATE_trak: FT_UInt = FT_VALIDATE_GX_START << FT_VALIDATE_trak_INDEX;
pub const FT_VALIDATE_prop: FT_UInt = FT_VALIDATE_GX_START << FT_VALIDATE_prop_INDEX;
pub const FT_VALIDATE_lcar: FT_UInt = FT_VALIDATE_GX_START << FT_VALIDATE_lcar_INDEX;
pub const FT_VALIDATE_GX: FT_UInt = FT_VALIDATE_feat
    | FT_VALIDATE_mort
    | FT_VALIDATE_morx
    | FT_VALIDATE_bsln
    | FT_VALIDATE_just
    | FT_VALIDATE_kern
    | FT_VALIDATE_opbd
    | FT_VALIDATE_trak
    | FT_VALIDATE_prop
    | FT_VALIDATE_lcar;

unsafe extern "system" {
    pub fn FT_TrueTypeGX_Validate(
        face: FT_Face,
        validation_flags: FT_UInt,
        tables: *mut FT_Bytes,
        table_length: FT_UInt,
    ) -> FT_Error;
    pub fn FT_TrueTypeGX_Free(face: FT_Face, table: FT_Bytes);
}

pub const FT_VALIDATE_MS: FT_UInt = FT_VALIDATE_GX_START << 0;
pub const FT_VALIDATE_APPLE: FT_UInt = FT_VALIDATE_GX_START << 1;
pub const FT_VALIDATE_CKERN: FT_UInt = FT_VALIDATE_MS | FT_VALIDATE_APPLE;

unsafe extern "system" {
    pub fn FT_ClassicKern_Validate(
        face: FT_Face,
        validation_flags: FT_UInt,
        ckern_table: *mut FT_Bytes,
    ) -> FT_Error;
    pub fn FT_ClassicKern_Free(face: FT_Face, table: FT_Bytes);
}
