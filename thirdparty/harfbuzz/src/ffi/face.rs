use core::ffi::*;

use crate::ffi::{
    OpaqueFFIStruct, hb_blob_t, hb_bool_t, hb_codepoint_t, hb_destroy_func_t, hb_map_t, hb_set_t,
    hb_tag_t, hb_user_data_key_t,
};

unsafe extern "C" {
    pub fn hb_face_count(blob: *mut hb_blob_t) -> c_uint;
}

#[repr(C)]
pub struct hb_face_t(OpaqueFFIStruct);

unsafe extern "C" {
    pub fn hb_face_create(blob: *mut hb_blob_t, index: c_uint) -> *mut hb_face_t;
    pub fn hb_face_create_or_fail(blob: *mut hb_blob_t, index: c_uint) -> *mut hb_face_t;
    pub fn hb_face_create_or_fail_using(
        blob: *mut hb_blob_t,
        index: c_uint,
        loader_name: *const c_char,
    ) -> *mut hb_face_t;
    pub fn hb_face_create_from_file_or_fail(
        file_name: *const c_char,
        index: c_uint,
    ) -> *mut hb_face_t;
    pub fn hb_face_create_from_file_or_fail_using(
        file_name: *const c_char,
        index: c_uint,
        loader_name: *const c_char,
    ) -> *mut hb_face_t;
    pub fn hb_face_list_loaders() -> *mut *const c_char;
}

pub type hb_reference_table_func_t =
    extern "C" fn(face: *mut hb_face_t, tag: hb_tag_t, user_data: *mut c_void) -> *mut hb_blob_t;

unsafe extern "C" {
    pub fn hb_face_create_for_tables(
        reference_table_func: hb_reference_table_func_t,
        user_data: *mut c_void,
        destroy: hb_destroy_func_t,
    ) -> *mut hb_face_t;
    pub fn hb_face_get_empty(face: *mut hb_face_t) -> *mut hb_face_t;
    pub fn hb_face_reference(face: *mut hb_face_t) -> *mut hb_face_t;
    pub fn hb_face_destroy(face: *mut hb_face_t);
    pub fn hb_face_set_user_data(
        face: *mut hb_face_t,
        key: hb_user_data_key_t,
        data: *mut c_void,
        destroy: hb_destroy_func_t,
        replace: hb_bool_t,
    ) -> hb_bool_t;
    pub fn hb_face_get_user_data(face: *mut hb_face_t, key: hb_user_data_key_t) -> *mut c_void;
    pub fn hb_face_make_immutable(face: *mut hb_face_t);
    pub fn hb_face_is_immutable(face: *mut hb_face_t) -> hb_bool_t;
    pub fn hb_face_reference_table(face: *mut hb_face_t, tag: hb_tag_t) -> *mut hb_blob_t;
    pub fn hb_face_reference_blob(face: *mut hb_face_t) -> *mut hb_blob_t;
    pub fn hb_face_set_index(face: *mut hb_face_t, index: c_uint);
    pub fn hb_face_get_index(face: *const hb_face_t) -> c_uint;
    pub fn hb_face_set_upem(face: *mut hb_face_t, upem: c_uint);
    pub fn hb_face_get_upem(face: *const hb_face_t) -> c_uint;
    pub fn hb_face_set_glyph_count(face: *mut hb_face_t, glyph_count: c_uint);
    pub fn hb_face_get_glyph_count(face: *const hb_face_t) -> c_uint;
}

pub type hb_get_table_tags_func_t = extern "C" fn(
    face: *const hb_face_t,
    start_offset: c_uint,
    table_count: *mut c_uint,
    table_tags: *mut hb_tag_t,
    user_data: *mut c_void,
) -> c_uint;

unsafe extern "C" {
    pub fn hb_face_set_get_table_tags_func(
        face: *mut hb_face_t,
        func: hb_get_table_tags_func_t,
        user_data: *mut c_void,
        destroy: hb_destroy_func_t,
    );
    pub fn hb_face_get_table_tags(
        face: *const hb_face_t,
        start_offset: c_uint,
        table_count: *mut c_uint,
        table_tags: *mut hb_tag_t,
    ) -> c_uint;

    pub fn hb_face_collect_unicodes(face: *mut hb_face_t, out: *mut hb_set_t);
    pub fn hb_face_collect_nominal_glyph_mapping(
        face: *mut hb_face_t,
        mapping: *mut hb_map_t,
        unicodes: *mut hb_set_t,
    );
    pub fn hb_face_collect_variation_selectors(face: *mut hb_face_t, out: *mut hb_set_t);
    pub fn hb_face_collect_variation_unicodes(
        face: *mut hb_face_t,
        variation_selector: hb_codepoint_t,
        out: *mut hb_set_t,
    );

    pub fn hb_face_builder_create() -> *mut hb_face_t;
    pub fn hb_face_builder_add_table(
        face: *mut hb_face_t,
        tag: hb_tag_t,
        blob: *mut hb_blob_t,
    ) -> hb_bool_t;
    pub fn hb_face_builder_sort_tables(face: *mut hb_face_t, tags: *const hb_tag_t);
}
