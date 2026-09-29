use core::ffi::*;

use crate::ffi::{
    OpaqueFFIStruct, hb_bool_t, hb_codepoint_t, hb_destroy_func_t, hb_script_t, hb_user_data_key_t,
};

pub const HB_UNICODE_MAX: u32 = 0x10ffff;

pub type hb_unicode_general_category_t = c_int;

pub type hb_unicode_combining_class_t = c_int;

#[repr(C)]
pub struct hb_unicode_funcs_t(OpaqueFFIStruct);

unsafe extern "C" {
    pub fn hb_unicode_funcs_get_default() -> *mut hb_unicode_funcs_t;
    pub fn hb_unicode_funcs_create(parent: *mut hb_unicode_funcs_t) -> *mut hb_unicode_funcs_t;
    pub fn hb_unicode_funcs_get_empty() -> *mut hb_unicode_funcs_t;
    pub fn hb_unicode_funcs_reference(ufuncs: *mut hb_unicode_funcs_t) -> *mut hb_unicode_funcs_t;
    pub fn hb_unicode_funcs_destroy(ufuncs: *mut hb_unicode_funcs_t);
    pub fn hb_unicode_funcs_set_user_data(
        ufuncs: *mut hb_unicode_funcs_t,
        key: *mut hb_user_data_key_t,
        data: *mut c_void,
        destroy: hb_destroy_func_t,
        replace: hb_bool_t,
    ) -> hb_bool_t;
    pub fn hb_unicode_funcs_get_user_data(
        ufuncs: *const hb_unicode_funcs_t,
        key: *mut hb_user_data_key_t,
    ) -> *mut c_void;
    pub fn hb_unicode_funcs_make_immutable(ufuncs: *mut hb_unicode_funcs_t);
    pub fn hb_unicode_funcs_is_immutable(ufuncs: *mut hb_unicode_funcs_t) -> hb_bool_t;
    pub fn hb_unicode_funcs_get_parent(ufuncs: *mut hb_unicode_funcs_t) -> *mut hb_unicode_funcs_t;
}

pub type hb_unicode_combining_class_func_t = extern "C" fn(
    ufuncs: *mut hb_unicode_funcs_t,
    unicode: hb_codepoint_t,
    user_data: *mut c_void,
) -> hb_unicode_combining_class_t;
pub type hb_unicode_general_category_func_t = extern "C" fn(
    ufuncs: *mut hb_unicode_funcs_t,
    unicode: hb_codepoint_t,
    user_data: *mut c_void,
) -> hb_unicode_general_category_t;
pub type hb_unicode_mirroring_func_t = extern "C" fn(
    ufuncs: *mut hb_unicode_funcs_t,
    unicode: hb_codepoint_t,
    user_data: *mut c_void,
) -> hb_codepoint_t;
pub type hb_unicode_script_func_t = extern "C" fn(
    ufuncs: *mut hb_unicode_funcs_t,
    unicode: hb_codepoint_t,
    user_data: *mut c_void,
) -> hb_script_t;
pub type hb_unicode_composte_func_t = extern "C" fn(
    ufuncs: *mut hb_unicode_funcs_t,
    a: hb_codepoint_t,
    b: hb_codepoint_t,
    ab: *mut hb_codepoint_t,
    user_data: *mut c_void,
) -> hb_bool_t;
pub type hb_unicode_decompose_func_t = extern "C" fn(
    ufuncs: *mut hb_unicode_funcs_t,
    ab: hb_codepoint_t,
    a: *mut hb_codepoint_t,
    b: *mut hb_codepoint_t,
    user_data: *mut c_void,
) -> hb_bool_t;

unsafe extern "C" {
    pub fn hb_unicode_funcs_set_combining_class_func(
        ufuncs: *mut hb_unicode_funcs_t,
        func: hb_unicode_combining_class_func_t,
        user_data: *mut c_void,
        destroy: hb_destroy_func_t,
    ) -> hb_bool_t;
    pub fn hb_unicode_funcs_set_general_category_func(
        ufuncs: *mut hb_unicode_funcs_t,
        func: hb_unicode_general_category_func_t,
        user_data: *mut c_void,
        destroy: hb_destroy_func_t,
    ) -> hb_bool_t;
    pub fn hb_unicode_funcs_set_mirroring_func(
        ufuncs: *mut hb_unicode_funcs_t,
        func: hb_unicode_mirroring_func_t,
        user_data: *mut c_void,
        destroy: hb_destroy_func_t,
    ) -> hb_bool_t;
    pub fn hb_unicode_funcs_set_script_func(
        ufuncs: *mut hb_unicode_funcs_t,
        func: hb_unicode_script_func_t,
        user_data: *mut c_void,
        destroy: hb_destroy_func_t,
    ) -> hb_bool_t;
    pub fn hb_unicode_funcs_set_compose_func(
        ufuncs: *mut hb_unicode_funcs_t,
        func: hb_unicode_composte_func_t,
        user_data: *mut c_void,
        destroy: hb_destroy_func_t,
    ) -> hb_bool_t;
    pub fn hb_unicode_funcs_set_decompose_func(
        ufuncs: *mut hb_unicode_funcs_t,
        func: hb_unicode_decompose_func_t,
        user_data: *mut c_void,
        destroy: hb_destroy_func_t,
    ) -> hb_bool_t;

    pub fn hb_unicode_combining_class(
        ufuncs: *mut hb_unicode_funcs_t,
        unicode: hb_codepoint_t,
    ) -> hb_unicode_combining_class_t;
    pub fn hb_unicode_general_category(
        ufuncs: *mut hb_unicode_funcs_t,
        unicode: hb_codepoint_t,
    ) -> hb_unicode_general_category_t;
    pub fn hb_unicode_mirroring(
        ufuncs: *mut hb_unicode_funcs_t,
        unicode: hb_codepoint_t,
    ) -> hb_codepoint_t;
    pub fn hb_unicode_script(
        ufuncs: *mut hb_unicode_funcs_t,
        unicode: hb_codepoint_t,
    ) -> hb_script_t;
    pub fn hb_unicode_compose(
        ufuncs: *mut hb_unicode_funcs_t,
        a: hb_codepoint_t,
        b: hb_codepoint_t,
        ab: *mut hb_codepoint_t,
    ) -> hb_bool_t;
    pub fn hb_unicode_decompose(
        ufuncs: *mut hb_unicode_funcs_t,
        ab: hb_codepoint_t,
        a: *mut hb_codepoint_t,
        b: *mut hb_codepoint_t,
    ) -> hb_bool_t;
}
