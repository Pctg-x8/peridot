use core::ffi::*;

use crate::ffi::{hb_bool_t, hb_buffer_t, hb_feature_t, hb_font_t};

unsafe extern "C" {
    pub fn hb_shape(
        font: *mut hb_font_t,
        buffer: *mut hb_buffer_t,
        features: *const hb_feature_t,
        num_features: c_uint,
    );
    pub fn hb_shape_full(
        font: *mut hb_font_t,
        buffer: *mut hb_buffer_t,
        features: *const hb_feature_t,
        num_features: c_uint,
        shaper_list: *const *const c_char,
    ) -> hb_bool_t;
    pub fn hb_shape_list_shapers() -> *mut *const c_char;
}
