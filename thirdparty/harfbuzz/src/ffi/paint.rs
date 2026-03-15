use core::ffi::*;

use super::*;

#[repr(C)]
pub struct hb_paint_funcs_t(OpaqueFFIStruct);

pub type hb_paint_push_transform_func_t = extern "C" fn(
    funcs: *mut hb_paint_funcs_t,
    paint_data: *mut c_void,
    xx: c_float,
    yx: c_float,
    xy: c_float,
    yy: c_float,
    dx: c_float,
    dy: c_float,
    user_data: *mut c_void,
);
pub type hb_paint_pop_transform_func_t =
    extern "C" fn(funcs: *mut hb_paint_funcs_t, paint_data: *mut c_void, user_data: *mut c_void);
pub type hb_paint_color_glyph_func_t = extern "C" fn(
    funcs: *mut hb_paint_funcs_t,
    paint_data: *mut c_void,
    glyph: hb_codepoint_t,
    font: *mut hb_font_t,
    user_data: *mut c_void,
) -> hb_bool_t;
pub type hb_paint_push_clip_glyph_func_t = extern "C" fn(
    funcs: *mut hb_paint_funcs_t,
    paint_data: *mut c_void,
    glyph: hb_codepoint_t,
    font: *mut hb_font_t,
    user_data: *mut c_void,
);
pub type hb_paint_push_clip_rectangle_func_t = extern "C" fn(
    funcs: *mut hb_paint_funcs_t,
    paint_data: *mut c_void,
    xmin: c_float,
    ymin: c_float,
    xmax: c_float,
    ymax: c_float,
    user_data: *mut c_void,
);
pub type hb_paint_pop_clip_func_t =
    extern "C" fn(funcs: *mut hb_paint_funcs_t, paint_data: *mut c_void, user_data: *mut c_void);
pub type hb_paint_color_func_t = extern "C" fn(
    funcs: *mut hb_paint_funcs_t,
    paint_data: *mut c_void,
    color: hb_color_t,
    user_data: *mut c_void,
);

pub const HB_PAINT_IMAGE_FORMAT_PNG: hb_tag_t = HB_TAG(b'p', b'n', b'g', b' ');
pub const HB_PAINT_IMAGE_FORMAT_SVG: hb_tag_t = HB_TAG(b's', b'v', b'g', b' ');
pub const HB_PAINT_IMAGE_FORMAT_BGRA: hb_tag_t = HB_TAG(b'B', b'G', b'R', b'A');

pub type hb_paint_image_func_t = extern "C" fn(
    funcs: *mut hb_paint_funcs_t,
    paint_data: *mut c_void,
    image: *mut hb_blob_t,
    width: c_uint,
    height: c_uint,
    format: hb_tag_t,
    slant: c_float,
    extents: *mut hb_glyph_extents_t,
    user_data: *mut c_void,
) -> hb_bool_t;

#[repr(C)]
pub struct hb_color_stop_t {
    pub offset: c_float,
    pub is_foreground: hb_bool_t,
    pub color: hb_color_t,
}

pub type hb_paint_extend_t = c_int;
pub const HB_PAINT_EXTEND_PAD: hb_paint_extend_t = 0;
pub const HB_PAINT_EXTEND_REPEAT: hb_paint_extend_t = 1;
pub const HB_PAINT_EXTEND_REFLECT: hb_paint_extend_t = 2;

#[repr(C)]
pub struct hb_color_line_t {
    pub data: *mut c_void,
    pub get_color_stops: hb_color_line_get_color_stops_func_t,
    pub get_color_stops_user_data: *mut c_void,
    pub get_extend: hb_color_line_get_extend_func_t,
    pub get_extend_user_data: *mut c_void,
    _reserved: [*mut c_void; 8],
}

pub type hb_color_line_get_color_stops_func_t = extern "C" fn(
    color_line: *mut hb_color_line_t,
    color_line_data: *mut c_void,
    start: c_uint,
    count: *mut c_uint,
    color_stops: *mut hb_color_stop_t,
    user_data: *mut c_void,
) -> c_uint;
pub type hb_color_line_get_extend_func_t = extern "C" fn(
    color_line: *mut hb_color_line_t,
    color_line_data: *mut c_void,
    user_data: *mut c_void,
) -> hb_paint_extend_t;

pub type hb_paint_linear_gradient_func_t = extern "C" fn(
    paint_funcs: *mut hb_paint_funcs_t,
    paint_data: *mut c_void,
    color_line: *mut hb_color_line_t,
    x0: c_float,
    y0: c_float,
    x1: c_float,
    y1: c_float,
    x2: c_float,
    y2: c_float,
    user_data: *mut c_void,
);
pub type hb_paint_radial_gradient_func_t = extern "C" fn(
    paint_funcs: *mut hb_paint_funcs_t,
    paint_data: *mut c_void,
    color_line: *mut hb_color_line_t,
    x0: c_float,
    y0: c_float,
    r0: c_float,
    x1: c_float,
    y1: c_float,
    r1: c_float,
    user_data: *mut c_void,
);
pub type hb_paint_sweep_gradient_func_t = extern "C" fn(
    paint_funcs: *mut hb_paint_funcs_t,
    paint_data: *mut c_void,
    color_line: *mut hb_color_line_t,
    x0: c_float,
    y0: c_float,
    start_angle: c_float,
    end_angle: c_float,
    user_data: *mut c_void,
);

pub type hb_paint_composite_mode_t = c_int;
pub const HB_PAINT_COMPOSITE_MODE_CLEAR: hb_paint_composite_mode_t = 0;
pub const HB_PAINT_COMPOSITE_MODE_SRC: hb_paint_composite_mode_t = 1;
pub const HB_PAINT_COMPOSITE_MODE_DEST: hb_paint_composite_mode_t = 2;
pub const HB_PAINT_COMPOSITE_MODE_SRC_OVER: hb_paint_composite_mode_t = 3;
pub const HB_PAINT_COMPOSITE_MODE_DEST_OVER: hb_paint_composite_mode_t = 4;
pub const HB_PAINT_COMPOSITE_MODE_SRC_IN: hb_paint_composite_mode_t = 5;
pub const HB_PAINT_COMPOSITE_MODE_DEST_IN: hb_paint_composite_mode_t = 6;
pub const HB_PAINT_COMPOSITE_MODE_SRC_OUT: hb_paint_composite_mode_t = 7;
pub const HB_PAINT_COMPOSITE_MODE_DEST_OUT: hb_paint_composite_mode_t = 8;
pub const HB_PAINT_COMPOSITE_MODE_SRC_ATOP: hb_paint_composite_mode_t = 9;
pub const HB_PAINT_COMPOSITE_MODE_DEST_ATOP: hb_paint_composite_mode_t = 10;
pub const HB_PAINT_COMPOSITE_MODE_XOR: hb_paint_composite_mode_t = 11;
pub const HB_PAINT_COMPOSITE_MODE_PLUS: hb_paint_composite_mode_t = 12;
pub const HB_PAINT_COMPOSITE_MODE_SCREEN: hb_paint_composite_mode_t = 13;
pub const HB_PAINT_COMPOSITE_MODE_OVERLAY: hb_paint_composite_mode_t = 14;
pub const HB_PAINT_COMPOSITE_MODE_DARKEN: hb_paint_composite_mode_t = 15;
pub const HB_PAINT_COMPOSITE_MODE_LIGHTEN: hb_paint_composite_mode_t = 16;
pub const HB_PAINT_COMPOSITE_MODE_COLOR_DODGE: hb_paint_composite_mode_t = 17;
pub const HB_PAINT_COMPOSITE_MODE_COLOR_BURN: hb_paint_composite_mode_t = 18;
pub const HB_PAINT_COMPOSITE_MODE_HARD_LIGHT: hb_paint_composite_mode_t = 19;
pub const HB_PAINT_COMPOSITE_MODE_SOFT_LIGHT: hb_paint_composite_mode_t = 20;
pub const HB_PAINT_COMPOSITE_MODE_DIFFERENCE: hb_paint_composite_mode_t = 21;
pub const HB_PAINT_COMPOSITE_MODE_EXCLUSION: hb_paint_composite_mode_t = 22;
pub const HB_PAINT_COMPOSITE_MODE_MULTIPLY: hb_paint_composite_mode_t = 23;
pub const HB_PAINT_COMPOSITE_MODE_HSL_HUE: hb_paint_composite_mode_t = 24;
pub const HB_PAINT_COMPOSITE_MODE_HSL_SATURATION: hb_paint_composite_mode_t = 25;
pub const HB_PAINT_COMPOSITE_MODE_HSL_COLOR: hb_paint_composite_mode_t = 26;
pub const HB_PAINT_COMPOSITE_MODE_HSL_LUMINOSITY: hb_paint_composite_mode_t = 27;

pub type hb_paint_push_group_func_t = extern "C" fn(
    paint_funcs: *mut hb_paint_funcs_t,
    paint_data: *mut c_void,
    user_data: *mut c_void,
);
pub type hb_paint_pop_group_func_t = extern "C" fn(
    paint_funcs: *mut hb_paint_funcs_t,
    paint_data: *mut c_void,
    mode: hb_paint_composite_mode_t,
    user_data: *mut c_void,
);
pub type hb_paint_custom_palette_color_func_t = extern "C" fn(
    paint_funcs: *mut hb_paint_funcs_t,
    paint_data: *mut c_void,
    color_index: c_uint,
    color: *mut hb_color_t,
    user_data: *mut c_void,
) -> hb_bool_t;

unsafe extern "C" {
    pub fn hb_paint_funcs_create() -> *mut hb_paint_funcs_t;
    pub fn hb_paint_funcs_get_empty() -> *mut hb_paint_funcs_t;
    pub fn hb_paint_funcs_reference(funcs: *mut hb_paint_funcs_t) -> *mut hb_paint_funcs_t;
    pub fn hb_paint_funcs_destroy(funcs: *mut hb_paint_funcs_t);
    pub fn hb_paint_funcs_set_user_data(
        funcs: *mut hb_paint_funcs_t,
        key: *mut hb_user_data_key_t,
        user_data: *mut c_void,
        destroy: Option<hb_destroy_func_t>,
        replace: hb_bool_t,
    ) -> hb_bool_t;
    pub fn hb_paint_funcs_get_user_data(
        funcs: *mut hb_paint_funcs_t,
        key: *mut hb_user_data_key_t,
    ) -> *mut c_void;
    pub fn hb_paint_funcs_make_immutable(funcs: *mut hb_paint_funcs_t);
    pub fn hb_paint_funcs_is_immutable(funcs: hb_paint_funcs_t) -> hb_bool_t;

    pub fn hb_color_line_get_color_stops(
        color_line: *mut hb_color_line_t,
        start: c_uint,
        count: *mut c_uint,
        color_stops: *mut hb_color_stop_t,
    ) -> c_uint;
    pub fn hb_color_line_get_extend(color_line: *mut hb_color_line_t) -> hb_paint_extend_t;

    pub fn hb_paint_funcs_set_push_transform_func(
        funcs: *mut hb_paint_funcs_t,
        func: hb_paint_push_transform_func_t,
        user_data: *mut c_void,
        destroy: Option<hb_destroy_func_t>,
    );
    pub fn hb_paint_funcs_set_pop_transform_func(
        funcs: *mut hb_paint_funcs_t,
        func: hb_paint_pop_transform_func_t,
        user_data: *mut c_void,
        destroy: Option<hb_destroy_func_t>,
    );
    pub fn hb_paint_funcs_set_color_glyph_func(
        funcs: *mut hb_paint_funcs_t,
        func: hb_paint_color_glyph_func_t,
        user_data: *mut c_void,
        destroy: Option<hb_destroy_func_t>,
    );
    pub fn hb_paint_funcs_set_push_clip_glyph_func(
        funcs: *mut hb_paint_funcs_t,
        func: hb_paint_push_clip_glyph_func_t,
        user_data: *mut c_void,
        destroy: Option<hb_destroy_func_t>,
    );
    pub fn hb_paint_funcs_set_push_clip_rectangle_func(
        funcs: *mut hb_paint_funcs_t,
        func: hb_paint_push_clip_rectangle_func_t,
        user_data: *mut c_void,
        destroy: Option<hb_destroy_func_t>,
    );
    pub fn hb_paint_funcs_set_pop_clip_func(
        funcs: *mut hb_paint_funcs_t,
        func: hb_paint_pop_clip_func_t,
        user_data: *mut c_void,
        destroy: Option<hb_destroy_func_t>,
    );
    pub fn hb_paint_funcs_set_color_func(
        funcs: *mut hb_paint_funcs_t,
        func: hb_paint_color_func_t,
        user_data: *mut c_void,
        destroy: Option<hb_destroy_func_t>,
    );
    pub fn hb_paint_funcs_set_image_func(
        funcs: *mut hb_paint_funcs_t,
        func: hb_paint_image_func_t,
        user_data: *mut c_void,
        destroy: Option<hb_destroy_func_t>,
    );
    pub fn hb_paint_funcs_set_linear_gradient_func(
        funcs: *mut hb_paint_funcs_t,
        func: hb_paint_linear_gradient_func_t,
        user_data: *mut c_void,
        destroy: Option<hb_destroy_func_t>,
    );
    pub fn hb_paint_funcs_set_radial_gradient_func(
        funcs: *mut hb_paint_funcs_t,
        func: hb_paint_radial_gradient_func_t,
        user_data: *mut c_void,
        destroy: Option<hb_destroy_func_t>,
    );
    pub fn hb_paint_funcs_set_sweep_gradient_func(
        funcs: *mut hb_paint_funcs_t,
        func: hb_paint_sweep_gradient_func_t,
        user_data: *mut c_void,
        destroy: Option<hb_destroy_func_t>,
    );
    pub fn hb_paint_funcs_set_push_group_func(
        funcs: *mut hb_paint_funcs_t,
        func: hb_paint_push_group_func_t,
        user_data: *mut c_void,
        destroy: Option<hb_destroy_func_t>,
    );
    pub fn hb_paint_funcs_set_pop_group_func(
        funcs: *mut hb_paint_funcs_t,
        func: hb_paint_pop_group_func_t,
        user_data: *mut c_void,
        destroy: Option<hb_destroy_func_t>,
    );
    pub fn hb_paint_funcs_set_custom_palette_color_func(
        funcs: *mut hb_paint_funcs_t,
        func: hb_paint_custom_palette_color_func_t,
        user_data: *mut c_void,
        destroy: Option<hb_destroy_func_t>,
    );
}
