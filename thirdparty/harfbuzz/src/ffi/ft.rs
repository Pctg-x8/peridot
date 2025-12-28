use core::ffi::*;

use freetype2::FT_Face;

use crate::ffi::{hb_blob_t, hb_bool_t, hb_destroy_func_t, hb_face_t, hb_font_t};

unsafe extern "C" {
    pub fn hb_ft_face_create(ft_face: FT_Face, destroy: hb_destroy_func_t) -> *mut hb_face_t;
    pub fn hb_ft_face_create_cached(ft_face: FT_Face) -> *mut hb_face_t;
    pub fn hb_ft_face_create_referenced(ft_face: FT_Face) -> *mut hb_face_t;
    pub fn hb_ft_face_create_from_file_or_fail(
        file_name: *const c_char,
        index: c_uint,
    ) -> *mut hb_face_t;
    pub fn hb_ft_face_create_from_blob_or_fail(
        blob: *mut hb_blob_t,
        index: c_uint,
    ) -> *mut hb_face_t;

    pub fn hb_ft_font_create(face: FT_Face, destroy: hb_destroy_func_t) -> *mut hb_font_t;
    pub fn hb_ft_font_create_referenced(face: FT_Face) -> *mut hb_font_t;
    pub fn hb_ft_font_get_ft_face(font: *mut hb_font_t) -> FT_Face;
    pub fn hb_ft_font_lock_face(font: *mut hb_font_t) -> FT_Face;
    pub fn hb_ft_font_unlock_face(font: *mut hb_font_t);
    pub fn hb_ft_font_set_load_flags(font: *mut hb_font_t, load_flags: c_int);
    pub fn hb_ft_font_get_load_flags(font: *mut hb_font_t) -> c_int;
    pub fn hb_ft_font_changed(font: *mut hb_font_t);
    pub fn hb_ft_hb_font_changed(font: *mut hb_font_t) -> hb_bool_t;
    pub fn hb_ft_font_set_funcs(font: *mut hb_font_t);
}
