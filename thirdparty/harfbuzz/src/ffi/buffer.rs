use core::ffi::*;

use crate::ffi::{
    HB_DIRECTION_INVALID, HB_LANGUAGE_INVALID, HB_SCRIPT_INVALID, OpaqueFFIStruct, hb_blob_t,
    hb_bool_t, hb_codepoint_t, hb_destroy_func_t, hb_direction_t, hb_font_t, hb_language_t,
    hb_mask_t, hb_position_t, hb_script_t, hb_unicode_funcs_t, hb_user_data_key_t, hb_var_int_t,
};

#[repr(C)]
#[derive(Clone)]
pub struct hb_glyph_info_t {
    pub codepoint: hb_codepoint_t,
    mask: hb_mask_t,
    pub cluster: u32,
    var1: hb_var_int_t,
    var2: hb_var_int_t,
}
impl core::fmt::Debug for hb_glyph_info_t {
    #[inline]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("hb_glyph_info_t")
            .field("codepoint", &self.codepoint)
            .field("mask", &self.mask)
            .field("cluster", &self.cluster)
            .field("var1", &"<union hb_var_int_t>")
            .field("var2", &"<union hb_var_int_t>")
            .finish()
    }
}

pub type hb_glyph_flags_t = hb_mask_t;
pub const HB_GLYPH_FLAG_UNSAFE_TO_BREAK: hb_glyph_flags_t = 0x00000001;
pub const HB_GLYPH_FLAG_UNSAFE_TO_CONCAT: hb_glyph_flags_t = 0x00000002;
pub const HB_GLYPH_FLAG_SAFE_TO_INSERT_TATWEEL: hb_glyph_flags_t = 0x00000004;
pub const HB_GLYPH_FLAG_DEFINED: hb_glyph_flags_t = 0x00000007;

pub const fn hb_glyph_info_get_glyph_flags(info: &hb_glyph_info_t) -> hb_glyph_flags_t {
    info.mask & HB_GLYPH_FLAG_DEFINED
}

#[repr(C)]
#[derive(Clone)]
pub struct hb_glyph_position_t {
    pub x_advance: hb_position_t,
    pub y_advance: hb_position_t,
    pub x_offset: hb_position_t,
    pub y_offset: hb_position_t,
    var: hb_var_int_t,
}
impl core::fmt::Debug for hb_glyph_position_t {
    #[inline]
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("hb_glyph_position_t")
            .field("x_advance", &self.x_advance)
            .field("y_advance", &self.y_advance)
            .field("x_offset", &self.x_offset)
            .field("y_offset", &self.y_offset)
            .field("var", &"<union hb_var_int_t>")
            .finish()
    }
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct hb_segment_properties_t {
    pub direction: hb_direction_t,
    pub script: hb_script_t,
    pub language: hb_language_t,
    reserved1: *mut c_void,
    reserved2: *mut c_void,
}

pub const HB_SEGMENT_PROPERTIES_DEFAULT: hb_segment_properties_t = hb_segment_properties_t {
    direction: HB_DIRECTION_INVALID,
    script: HB_SCRIPT_INVALID,
    language: HB_LANGUAGE_INVALID,
    reserved1: std::ptr::null_mut(),
    reserved2: std::ptr::null_mut(),
};

unsafe extern "C" {
    pub fn hb_segment_properties_equal(
        a: *const hb_segment_properties_t,
        b: *const hb_segment_properties_t,
    ) -> hb_bool_t;
    pub fn hb_segment_properties_hash(p: *const hb_segment_properties_t) -> c_uint;
    pub fn hb_segment_properties_overlay(
        p: *mut hb_segment_properties_t,
        src: *const hb_segment_properties_t,
    );
}

#[repr(C)]
pub struct hb_buffer_t(OpaqueFFIStruct);

unsafe extern "C" {
    pub fn hb_buffer_create() -> *mut hb_buffer_t;
    pub fn hb_buffer_create_similar(src: *const hb_buffer_t) -> *mut hb_buffer_t;
    pub fn hb_buffer_reset(buffer: *mut hb_buffer_t);
    pub fn hb_buffer_get_empty() -> *mut hb_buffer_t;
    pub fn hb_buffer_reference(buffer: *mut hb_buffer_t) -> *mut hb_buffer_t;
    pub fn hb_buffer_destroy(buffer: *mut hb_buffer_t);
    pub fn hb_buffer_set_user_data(
        buffer: *mut hb_buffer_t,
        key: *mut hb_user_data_key_t,
        data: *mut c_void,
        destroy: hb_destroy_func_t,
        replace: hb_bool_t,
    ) -> hb_bool_t;
    pub fn hb_buffer_get_user_data(
        buffer: *mut hb_buffer_t,
        key: *mut hb_user_data_key_t,
    ) -> *mut c_void;
}

pub type hb_buffer_content_type_t = c_int;
pub const HB_BUFFER_CONTENT_TYPE_INVALID: hb_buffer_content_type_t = 0;
pub const HB_BUFFER_CONTENT_TYPE_UNICODE: hb_buffer_content_type_t = 1;
pub const HB_BUFFER_CONTENT_TYPE_GLYPHS: hb_buffer_content_type_t = 2;

unsafe extern "C" {
    pub fn hb_buffer_set_content_type(
        buffer: *mut hb_buffer_t,
        content_type: hb_buffer_content_type_t,
    );
    pub fn hb_buffer_get_content_type(buffer: *const hb_buffer_t) -> hb_buffer_content_type_t;
    pub fn hb_buffer_set_unicode_funcs(
        buffer: *mut hb_buffer_t,
        unicode_funcs: *mut hb_unicode_funcs_t,
    );
    pub fn hb_buffer_get_unicode_funcs(buffer: *const hb_buffer_t) -> *mut hb_unicode_funcs_t;
    pub fn hb_buffer_set_direction(buffer: *mut hb_buffer_t, direction: hb_direction_t);
    pub fn hb_buffer_get_direction(buffer: *const hb_buffer_t) -> hb_direction_t;
    pub fn hb_buffer_set_script(buffer: *mut hb_buffer_t, script: hb_script_t);
    pub fn hb_buffer_get_script(buffer: *const hb_buffer_t) -> hb_script_t;
    pub fn hb_buffer_set_language(buffer: *mut hb_buffer_t, language: hb_language_t);
    pub fn hb_buffer_get_language(buffer: *const hb_buffer_t) -> hb_language_t;
    pub fn hb_buffer_set_segment_properties(
        buffer: *mut hb_buffer_t,
        props: *const hb_segment_properties_t,
    );
    pub fn hb_buffer_get_segment_properties(
        buffer: *const hb_buffer_t,
        props: *mut hb_segment_properties_t,
    );
    pub fn hb_buffer_guess_segment_properties(buffer: *mut hb_buffer_t);
}

pub type hb_buffer_flags_t = hb_mask_t;
pub const HB_BUFFER_FLAG_DEFAULT: hb_buffer_flags_t = 0;
/// Beginning-of-text
pub const HB_BUFFER_FLAG_BOT: hb_buffer_flags_t = 0x00000001;
/// End-of-text
pub const HB_BUFFER_FLAG_EOT: hb_buffer_flags_t = 0x00000002;
pub const HB_BUFFER_FLAG_PRESERVE_DEFAULT_IGNORABLES: hb_buffer_flags_t = 0x00000004;
pub const HB_BUFFER_FLAG_REMOVE_DEFAULT_IGNORABLES: hb_buffer_flags_t = 0x00000008;
pub const HB_BUFFER_FLAG_DO_NOT_INSERT_DOTTED_CIRCLE: hb_buffer_flags_t = 0x00000010;
pub const HB_BUFFER_FLAG_VERIFY: hb_buffer_flags_t = 0x00000020;
pub const HB_BUFFER_FLAG_PRODUCE_UNSAFE_TO_CONCAT: hb_buffer_flags_t = 0x00000040;
pub const HB_BUFFER_FLAG_PRODUCE_SAFE_TO_INSERT_TATWEEL: hb_buffer_flags_t = 0x00000080;
pub const HB_BUFFER_FLAG_DEFINED: hb_buffer_flags_t = 0x000000ff;

unsafe extern "C" {
    pub fn hb_buffer_set_flags(buffer: *mut hb_buffer_t, flags: hb_buffer_flags_t);
    pub fn hb_buffer_get_flags(buffer: *const hb_buffer_t) -> hb_buffer_flags_t;
}

pub type hb_buffer_cluster_level_t = c_int;
pub const HB_BUFFER_CLUSTER_LEVEL_MONOTONE_GRAPHEMES: hb_buffer_cluster_level_t = 0;
pub const HB_BUFFER_CLUSTER_LEVEL_MONOTONE_CHARACTERS: hb_buffer_cluster_level_t = 1;
pub const HB_BUFFER_CLUSTER_LEVEL_CHARACTERS: hb_buffer_cluster_level_t = 2;
pub const HB_BUFFER_CLUSTER_LEVEL_GRAPHEMES: hb_buffer_cluster_level_t = 3;
pub const HB_BUFFER_CLUSTER_LEVEL_DEFAULT: hb_buffer_cluster_level_t =
    HB_BUFFER_CLUSTER_LEVEL_MONOTONE_GRAPHEMES;

#[inline(always)]
pub const fn HB_BUFFER_CLUSTER_LEVEL_IS_MONOTONE(level: hb_buffer_cluster_level_t) -> bool {
    (1 << level as c_uint)
        & ((1 << HB_BUFFER_CLUSTER_LEVEL_MONOTONE_GRAPHEMES as c_uint)
            | (1 << HB_BUFFER_CLUSTER_LEVEL_MONOTONE_CHARACTERS as c_uint))
        != 0
}

#[inline(always)]
pub const fn HB_BUFFER_CLUSTER_LEVEL_IS_GRAPHEMES(level: hb_buffer_cluster_level_t) -> bool {
    (1 << level as c_uint)
        & ((1 << HB_BUFFER_CLUSTER_LEVEL_MONOTONE_GRAPHEMES as c_uint)
            | (1 << HB_BUFFER_CLUSTER_LEVEL_GRAPHEMES as c_uint))
        != 0
}

#[inline(always)]
pub const fn HB_BUFFER_CLUSTER_LEVEL_IS_CHARACTERS(level: hb_buffer_cluster_level_t) -> bool {
    (1 << level as c_uint)
        & ((1 << HB_BUFFER_CLUSTER_LEVEL_MONOTONE_CHARACTERS as c_uint)
            | (1 << HB_BUFFER_CLUSTER_LEVEL_CHARACTERS as c_uint))
        != 0
}

unsafe extern "C" {
    pub fn hb_buffer_set_cluster_level(buffer: *mut hb_buffer_t, level: hb_buffer_cluster_level_t);
    pub fn hb_buffer_get_cluster_level(buffer: *const hb_buffer_t) -> hb_buffer_cluster_level_t;
}

pub const HB_BUFFER_REPLACEMENT_CODEPOINT_DEFAULT: hb_codepoint_t = 0xFFFD;

unsafe extern "C" {
    pub fn hb_buffer_set_replacement_codepoint(buffer: *mut hb_buffer_t, codepoint: hb_codepoint_t);
    pub fn hb_buffer_get_replacement_codepoint(buffer: *const hb_buffer_t) -> hb_codepoint_t;
    pub fn hb_buffer_set_invisible_glyph(buffer: *mut hb_buffer_t, invisible: hb_codepoint_t);
    pub fn hb_buffer_get_invisible_glyph(buffer: *const hb_buffer_t) -> hb_codepoint_t;
    pub fn hb_buffer_set_not_found_glyph(buffer: *mut hb_buffer_t, not_found: hb_codepoint_t);
    pub fn hb_buffer_get_not_found_glyph(buffer: *const hb_buffer_t) -> hb_codepoint_t;
    pub fn hb_buffer_set_not_found_variation_selector_glyph(
        hb_buffer: *mut hb_buffer_t,
        not_found_variation_selector: hb_codepoint_t,
    );
    pub fn hb_buffer_get_not_found_variation_selector_glyph(
        hb_buffer: *const hb_buffer_t,
    ) -> hb_codepoint_t;
    pub fn hb_buffer_set_random_state(buffer: *mut hb_buffer_t, state: c_uint);
    pub fn hb_buffer_get_random_state(buffer: *const hb_buffer_t) -> c_uint;

    pub fn hb_buffer_clear_contents(buffer: *mut hb_buffer_t);
    pub fn hb_buffer_pre_allocate(buffer: *mut hb_buffer_t, size: c_uint) -> hb_bool_t;
    pub fn hb_buffer_allocation_successful(buffer: *mut hb_buffer_t) -> hb_bool_t;
    pub fn hb_buffer_reserve(buffer: *mut hb_buffer_t);
    pub fn hb_buffer_reserve_range(buffer: *mut hb_buffer_t, start: c_uint, end: c_uint);
    pub fn hb_buffer_reserve_clusters(buffer: *mut hb_buffer_t);

    pub fn hb_buffer_add(buffer: *mut hb_buffer_t, codepoint: hb_codepoint_t, cluster: c_uint);
    pub fn hb_buffer_add_utf8(
        buffer: *mut hb_buffer_t,
        text: *const c_char,
        text_length: c_int,
        item_offset: c_uint,
        item_length: c_int,
    );
    pub fn hb_buffer_add_utf16(
        buffer: *mut hb_buffer_t,
        text: *const u16,
        text_length: c_int,
        item_offset: c_uint,
        item_length: c_int,
    );
    // hb_buffer_add_utf32: omit(will not use in peridot)
    // hb_buffer_add_latin1: omit(will not use in peridot)
    pub fn hb_buffer_add_codepoints(
        buffer: *mut hb_buffer_t,
        text: *const hb_codepoint_t,
        text_length: c_int,
        item_offset: c_uint,
        item_length: c_int,
    );
    pub fn hb_buffer_append(
        buffer: *mut hb_buffer_t,
        source: *const hb_buffer_t,
        start: c_uint,
        end: c_uint,
    );
    pub fn hb_buffer_set_length(buffer: *mut hb_buffer_t, length: c_uint) -> hb_bool_t;
    pub fn hb_buffer_get_length(buffer: *const hb_buffer_t) -> c_uint;

    pub fn hb_buffer_get_glyph_infos(
        buffer: *mut hb_buffer_t,
        length: *mut c_uint,
    ) -> *mut hb_glyph_info_t;
    pub fn hb_buffer_get_glyph_positions(
        buffer: *mut hb_buffer_t,
        length: *mut c_uint,
    ) -> *mut hb_glyph_position_t;
    pub fn hb_buffer_has_positions(buffer: *mut hb_buffer_t) -> hb_bool_t;
    pub fn hb_buffer_normalize_glyphs(buffer: *mut hb_buffer_t);
}

// serializing/deserializing: omit(will not use in peridot)
// diff: omit(will not use in peridot)

pub type hb_buffer_message_func_t = extern "C" fn(
    buffer: *mut hb_buffer_t,
    font: *mut hb_font_t,
    message: *const c_char,
    user_data: *mut c_void,
) -> hb_bool_t;

unsafe extern "C" {
    pub fn hb_buffer_set_message_func(
        buffer: *mut hb_buffer_t,
        func: hb_buffer_message_func_t,
        user_data: *mut c_void,
        destroy: hb_destroy_func_t,
    );
}
