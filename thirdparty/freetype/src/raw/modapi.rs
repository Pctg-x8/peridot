use super::*;

unsafe extern "C" {
    pub unsafe fn FT_Property_Set(
        library: FT_Library,
        module_name: *const FT_String,
        property_name: *const FT_String,
        value: *const core::ffi::c_void,
    ) -> FT_Error;
    pub unsafe fn FT_Property_Get(
        library: FT_Library,
        module_name: *const FT_String,
        property_name: *const FT_String,
        value: *mut core::ffi::c_void,
    ) -> FT_Error;
}
