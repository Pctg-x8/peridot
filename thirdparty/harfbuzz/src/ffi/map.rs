use core::ffi::*;

use crate::ffi::{
    HB_CODEPOINT_INVALID, OpaqueFFIStruct, hb_bool_t, hb_codepoint_t, hb_destroy_func_t, hb_set_t,
    hb_user_data_key_t,
};

pub const HB_MAP_VALUE_INVALID: hb_codepoint_t = HB_CODEPOINT_INVALID;

#[repr(C)]
pub struct hb_map_t(OpaqueFFIStruct);

unsafe extern "C" {
    pub fn hb_map_create() -> *mut hb_map_t;
    pub fn hb_map_get_empty() -> *mut hb_map_t;
    pub fn hb_map_reference(map: *mut hb_map_t) -> *mut hb_map_t;
    pub fn hb_map_destroy(map: *mut hb_map_t);
    pub fn hb_map_set_user_data(
        map: *mut hb_map_t,
        key: *mut hb_user_data_key_t,
        data: *mut c_void,
        destroy: hb_destroy_func_t,
        replace: hb_bool_t,
    ) -> hb_bool_t;
    pub fn hb_map_get_user_data(map: *const hb_map_t, key: *mut hb_user_data_key_t) -> *mut c_void;
    pub fn hb_map_allocation_successful(map: *const hb_map_t) -> hb_bool_t;
    pub fn hb_map_copy(map: *const hb_map_t) -> *mut hb_map_t;
    pub fn hb_map_clear(map: *mut hb_map_t);
    pub fn hb_map_is_empty(map: *const hb_map_t) -> hb_bool_t;
    pub fn hb_map_get_population(map: *const hb_map_t) -> c_uint;
    pub fn hb_map_is_equal(map: *const hb_map_t, other: *const hb_map_t) -> hb_bool_t;
    pub fn hb_map_hash(map: *const hb_map_t) -> c_uint;
    pub fn hb_map_set(map: *mut hb_map_t, key: hb_codepoint_t, value: hb_codepoint_t);
    pub fn hb_map_get(map: *const hb_map_t, key: hb_codepoint_t) -> hb_codepoint_t;
    pub fn hb_map_del(map: *mut hb_map_t, key: hb_codepoint_t);
    pub fn hb_map_has(map: *const hb_map_t, key: hb_codepoint_t) -> hb_bool_t;
    pub fn hb_map_update(map: *mut hb_map_t, other: *const hb_map_t);
    pub fn hb_map_next(
        map: *const hb_map_t,
        idx: *mut c_int,
        key: *mut hb_codepoint_t,
        value: *mut hb_codepoint_t,
    ) -> hb_bool_t;
    pub fn hb_map_keys(map: *const hb_map_t, keys: *mut hb_set_t);
    pub fn hb_map_values(map: *const hb_map_t, values: *mut hb_set_t);
}
