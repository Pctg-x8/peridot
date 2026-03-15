use core::ffi::*;

use super::*;

#[repr(C)]
pub struct hb_font_extents_t {
    pub ascender: hb_position_t,
    pub descender: hb_position_t,
    pub line_gap: hb_position_t,
    _reserved: [hb_position_t; 9],
}

unsafe extern "C" {
    pub fn hb_font_get_h_extents(
        font: *mut hb_font_t,
        extents: *mut hb_font_extents_t,
    ) -> hb_bool_t;
    pub fn hb_font_get_v_extents(
        font: *mut hb_font_t,
        extents: *mut hb_font_extents_t,
    ) -> hb_bool_t;
    pub fn hb_font_get_nominal_glyph(
        font: *mut hb_font_t,
        unicode: hb_codepoint_t,
        glyph: *mut hb_codepoint_t,
    ) -> hb_bool_t;
    pub fn hb_font_get_variation_glyph(
        font: *mut hb_font_t,
        unicode: hb_codepoint_t,
        variation_selector: hb_codepoint_t,
        glyph: *mut hb_codepoint_t,
    ) -> hb_bool_t;
    pub fn hb_font_get_nominal_glyphs(
        font: *mut hb_font_t,
        count: c_uint,
        first_unicode: *const hb_codepoint_t,
        unicode_stride: c_uint,
        first_glyph: *mut hb_codepoint_t,
        glyph_stride: c_uint,
    ) -> c_uint;
    pub fn hb_font_get_glyph_h_advance(
        font: *mut hb_font_t,
        glyph: hb_codepoint_t,
    ) -> hb_position_t;
    pub fn hb_font_get_glyph_v_advance(
        font: *mut hb_font_t,
        glyph: hb_codepoint_t,
    ) -> hb_position_t;
    pub fn hb_font_get_glyph_h_advances(
        font: *mut hb_font_t,
        count: c_uint,
        first_glyph: *const hb_codepoint_t,
        glyph_stride: c_uint,
        first_advance: *mut hb_position_t,
        advance_stride: c_uint,
    );
    pub fn hb_font_get_glyph_v_advances(
        font: *mut hb_font_t,
        count: c_uint,
        first_glyph: *const hb_codepoint_t,
        glyph_stride: c_uint,
        first_advance: *mut hb_position_t,
        advance_stride: c_uint,
    );
    pub fn hb_font_get_glyph_extents(
        font: *mut hb_font_t,
        glyph: hb_codepoint_t,
        extents: *mut hb_glyph_extents_t,
    ) -> hb_bool_t;

    pub fn hb_font_get_glyph(
        font: *mut hb_font_t,
        unicode: hb_codepoint_t,
        variation_selector: hb_codepoint_t,
        glyph: *mut hb_codepoint_t,
    ) -> hb_bool_t;

    pub fn hb_font_draw_glyph_or_fail(
        font: *mut hb_font_t,
        glyph: hb_codepoint_t,
        dfuncs: *mut hb_draw_funcs_t,
        draw_data: *mut c_void,
    ) -> hb_bool_t;

    pub fn hb_font_create(face: *mut hb_face_t) -> *mut hb_font_t;
    pub fn hb_font_reference(font: *mut hb_font_t) -> *mut hb_font_t;
    pub fn hb_font_destroy(font: *mut hb_font_t);
    pub fn hb_font_set_scale(font: *mut hb_font_t, x_scale: c_int, y_scale: c_int);
    pub fn hb_font_set_variation(font: *mut hb_font_t, tag: hb_tag_t, value: c_float);
    pub fn hb_font_set_var_coords_normalized(
        font: *mut hb_font_t,
        coords: *const c_int,
        coords_length: c_uint,
    );
}
