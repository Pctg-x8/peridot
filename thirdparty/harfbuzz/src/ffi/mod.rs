#![allow(non_upper_case_globals, non_camel_case_types, non_snake_case)]

use core::ffi::*;

#[repr(C)]
struct OpaqueFFIStruct(
    [u8; 0],
    core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,
);

pub type hb_bool_t = c_int;
pub type hb_codepoint_t = u32;
pub const HB_CODEPOINT_INVALID: hb_codepoint_t = 0xFFFFFFFF;
pub type hb_position_t = i32;
pub type hb_mask_t = u32;

#[repr(C)]
#[derive(Clone, Copy)]
pub union hb_var_int_t {
    pub r#u32: u32,
    pub r#i32: i32,
    pub r#u16: [u16; 2],
    pub r#i16: [i16; 2],
    pub r#u8: [u8; 4],
    pub r#i8: [i8; 4],
}

#[repr(C)]
#[derive(Clone, Copy)]
pub union hb_var_num_t {
    pub f: c_float,
    pub r#u32: u32,
    pub r#i32: i32,
    pub r#u16: [u16; 2],
    pub r#i16: [i16; 2],
    pub r#u8: [u8; 4],
    pub r#i8: [i8; 4],
}

pub type hb_tag_t = u32;
pub const fn HB_TAG(c1: u8, c2: u8, c3: u8, c4: u8) -> hb_tag_t {
    u32::from_be_bytes([c1, c2, c3, c4])
}
pub const fn HB_TAG_BSTR(bytes: [u8; 4]) -> hb_tag_t {
    u32::from_be_bytes(bytes)
}
pub const fn HB_UNTAG(tag: hb_tag_t) -> [u8; 4] {
    tag.to_be_bytes()
}
pub const HB_TAG_NONE: hb_tag_t = HB_TAG(0, 0, 0, 0);
pub const HB_TAG_MAX: hb_tag_t = HB_TAG(0xff, 0xff, 0xff, 0xff);
pub const HB_TAG_MAX_SIGNED: hb_tag_t = HB_TAG(0x7f, 0xff, 0xff, 0xff);

#[link(name = "harfbuzz")]
unsafe extern "C" {
    pub fn hb_tag_from_string(str: *const c_char, len: c_int) -> hb_tag_t;
    pub fn hb_tag_to_string(tag: hb_tag_t, buf: *mut c_char);
}

pub type hb_direction_t = c_int;
pub const HB_DIRECTION_INVALID: hb_direction_t = 0;
pub const HB_DIRECTION_LTR: hb_direction_t = 4;
pub const HB_DIRECTION_RTL: hb_direction_t = 5;
pub const HB_DIRECTION_TTB: hb_direction_t = 6;
pub const HB_DIRECTION_BTT: hb_direction_t = 7;

unsafe extern "C" {
    pub fn hb_direction_from_string(str: *const c_char, len: c_int) -> hb_direction_t;
    pub fn hb_direction_to_string(direction: hb_direction_t) -> *const c_char;
}

pub const fn HB_DIRECTION_IS_VALID(dir: hb_direction_t) -> bool {
    (dir as u32 & !3) == 4
}

pub const fn HB_DIRECTION_IS_HORIZONTAL(dir: hb_direction_t) -> bool {
    (dir as u32 & !1) == 4
}

pub const fn HB_DIRECTION_IS_VERTICAL(dir: hb_direction_t) -> bool {
    (dir as u32 & !1) == 6
}

pub const fn HB_DIRECTION_IS_FORWARD(dir: hb_direction_t) -> bool {
    (dir as u32 & !2) == 4
}

pub const fn HB_DIRECTION_IS_BACKWARD(dir: hb_direction_t) -> bool {
    (dir as u32 & !2) == 5
}

pub const fn HB_DIRECTION_REVERSE(dir: hb_direction_t) -> hb_direction_t {
    (dir as u32 ^ 1) as hb_direction_t
}

#[repr(C)]
pub struct hb_language_impl_t(OpaqueFFIStruct);
pub type hb_language_t = *const hb_language_impl_t;

unsafe extern "C" {
    pub fn hb_language_from_string(str: *const c_char, len: c_int) -> hb_language_t;
    pub fn hb_language_to_string(language: hb_language_t) -> *const c_char;
}

pub const HB_LANGUAGE_INVALID: hb_language_t = std::ptr::null();

unsafe extern "C" {
    pub fn hb_language_get_default() -> hb_language_t;
    pub fn hb_language_matches(language: hb_language_t, specific: hb_language_t) -> hb_bool_t;

    pub fn hb_script_from_iso15924_tag(tag: hb_tag_t) -> hb_script_t;
    pub fn hb_script_from_string(str: *const c_char, len: c_int) -> hb_script_t;
    pub fn hb_script_to_iso15924_tag(script: hb_script_t) -> hb_tag_t;
    pub fn hb_script_get_horizontal_direction(script: hb_script_t) -> hb_direction_t;
}

#[repr(C)]
pub struct hb_user_data_key_t {
    unused: c_char,
}

pub type hb_destroy_func_t = extern "C" fn(user_data: *mut c_void);

pub const HB_FEATURE_GLOBAL_START: c_uint = 0;
pub const HB_FEATURE_GLOBAL_END: c_uint = 0xffffffff;

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct hb_feature_t {
    pub tag: hb_tag_t,
    pub value: u32,
    pub start: c_uint,
    pub end: c_uint,
}

unsafe extern "C" {
    pub fn hb_feature_from_string(
        str: *const c_char,
        len: c_int,
        feature: *mut hb_feature_t,
    ) -> hb_bool_t;
    pub fn hb_feature_to_string(feature: *mut hb_feature_t, buf: *mut c_char, size: c_uint);
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct hb_variation_t {
    pub tag: hb_tag_t,
    pub value: c_float,
}

unsafe extern "C" {
    pub fn hb_variation_from_string(
        str: *const c_char,
        len: c_int,
        variation: *mut hb_variation_t,
    ) -> hb_bool_t;
    pub fn hb_variation_to_string(variation: *mut hb_variation_t, buf: *mut c_char, size: c_uint);
}

pub type hb_color_t = u32;
pub const fn HB_COLOR(b: u8, g: u8, r: u8, a: u8) -> hb_color_t {
    HB_TAG(b, g, r, a)
}
pub const fn hb_color_get_alpha(c: hb_color_t) -> u8 {
    c as u8
}
pub const fn hb_color_get_red(c: hb_color_t) -> u8 {
    (c >> 8) as u8
}
pub const fn hb_color_get_green(c: hb_color_t) -> u8 {
    (c >> 16) as u8
}
pub const fn hb_color_get_blue(c: hb_color_t) -> u8 {
    (c >> 24) as u8
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct hb_glyph_extents_t {
    pub x_bearing: hb_position_t,
    pub y_bearing: hb_position_t,
    pub width: hb_position_t,
    pub height: hb_position_t,
}

#[repr(C)]
pub struct hb_font_t(OpaqueFFIStruct);

unsafe extern "C" {
    pub fn hb_malloc(size: usize) -> *mut c_void;
    pub fn hb_calloc(nmemb: usize, size: usize) -> *mut c_void;
    pub fn hb_realloc(ptr: *mut c_void, size: usize) -> *mut c_void;
    pub fn hb_free(ptr: *mut c_void);
}

mod blob;
pub use self::blob::*;

mod buffer;
pub use self::buffer::*;

mod face;
pub use self::face::*;

mod ft;
pub use self::ft::*;

mod map;
pub use self::map::*;

mod script_list;
pub use self::script_list::*;

mod set;
pub use self::set::*;

mod shape;
pub use self::shape::*;

mod unicode;
pub use self::unicode::*;
