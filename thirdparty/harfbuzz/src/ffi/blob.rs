use core::ffi::*;

use crate::ffi::{OpaqueFFIStruct, hb_bool_t, hb_destroy_func_t, hb_user_data_key_t};

pub type hb_memory_mode_t = c_int;
pub const HB_MEMORY_MODE_DUPLICATE: hb_memory_mode_t = 0;
pub const HB_MEMORY_MODE_READONLY: hb_memory_mode_t = 1;
pub const HB_MEMORY_MODE_WRITABLE: hb_memory_mode_t = 2;
pub const HB_MEMORY_MODE_READONLY_MAY_MAKE_WRITABLE: hb_memory_mode_t = 3;

#[repr(C)]
pub struct hb_blob_t(OpaqueFFIStruct);

unsafe extern "C" {
    pub fn hb_blob_create(
        data: *const c_char,
        length: c_uint,
        mode: hb_memory_mode_t,
        user_data: *mut c_void,
        destroy: hb_destroy_func_t,
    ) -> *mut hb_blob_t;
    pub fn hb_blob_create_or_fail(
        data: *const c_char,
        length: c_uint,
        mode: hb_memory_mode_t,
        user_data: *mut c_void,
        destroy: hb_destroy_func_t,
    ) -> *mut hb_blob_t;
    pub fn hb_blob_create_from_file(file_name: *const c_char) -> *mut hb_blob_t;
    pub fn hb_blob_create_from_file_or_fail(file_name: *const c_char) -> *mut hb_blob_t;
    pub fn hb_blob_create_sub_blob(
        parent: *mut hb_blob_t,
        offset: c_uint,
        length: c_uint,
    ) -> *mut hb_blob_t;
    pub fn hb_blob_copy_writable_or_fail(blob: *mut hb_blob_t) -> *mut hb_blob_t;
    pub fn hb_blob_get_empty() -> *mut hb_blob_t;
    pub fn hb_blob_reference(blob: *mut hb_blob_t) -> *mut hb_blob_t;
    pub fn hb_blob_destroy(blob: *mut hb_blob_t);
    pub fn hb_blob_set_user_data(
        blob: *mut hb_blob_t,
        key: *mut hb_user_data_key_t,
        data: *mut c_void,
        destroy: hb_destroy_func_t,
        replace: hb_bool_t,
    ) -> hb_bool_t;
    pub fn hb_blob_get_user_data(blob: *mut hb_blob_t, key: *mut hb_user_data_key_t)
    -> *mut c_void;
    pub fn hb_blob_make_immutable(blob: *mut hb_blob_t);
    pub fn hb_blob_is_immutable(blob: *mut hb_blob_t) -> hb_bool_t;
    pub fn hb_blob_get_length(blob: *mut hb_blob_t) -> c_uint;
    pub fn hb_blob_get_data(blob: *mut hb_blob_t, length: *mut c_uint) -> *const c_char;
    pub fn hb_blob_get_data_writable(blob: *mut hb_blob_t, length: *mut c_uint) -> *mut c_char;
}
