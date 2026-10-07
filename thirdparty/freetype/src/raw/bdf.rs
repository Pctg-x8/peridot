use super::*;

pub type BDF_PropertyType = FT_Int;
pub const BDF_PROPERTY_TYPE_NONE: BDF_PropertyType = 0;
pub const BDF_PROPERTY_TYPE_ATOM: BDF_PropertyType = 1;
pub const BDF_PROPERTY_TYPE_INTEGER: BDF_PropertyType = 2;
pub const BDF_PROPERTY_TYPE_CARDINAL: BDF_PropertyType = 3;

pub type BDF_Property = *mut BDF_PropertyRec;
#[repr(C)]
pub union BDF_PropertyRec_U {
    pub atom: *const core::ffi::c_char,
    pub integer: i32,
    pub cardinal: u32,
}
#[repr(C)]
pub struct BDF_PropertyRec {
    pub type_: BDF_PropertyType,
    pub u: BDF_PropertyRec_U,
}

unsafe extern "system" {
    pub fn FT_Get_BDF_Charset_ID(
        face: FT_Face,
        acharset_encoding: *const core::ffi::c_char,
        acharset_registry: *const core::ffi::c_char,
    ) -> FT_Error;
    pub fn FT_Get_BDF_Property(
        face: FT_Face,
        prop_name: *const core::ffi::c_char,
        aproperty: *mut BDF_PropertyRec,
    ) -> FT_Error;
}
