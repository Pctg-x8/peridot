use core::ffi::*;

use super::*;

#[repr(C)]
pub struct hb_draw_state_t {
    pub path_open: hb_bool_t,
    pub path_start_x: c_float,
    pub path_start_y: c_float,
    pub current_x: c_float,
    pub current_y: c_float,
    _reserved: [hb_var_num_t; 7],
}
pub const HB_DRAW_STATE_DEFAULT: hb_draw_state_t = hb_draw_state_t {
    path_open: 0,
    path_start_x: 0.0,
    path_start_y: 0.0,
    current_x: 0.0,
    current_y: 0.0,
    _reserved: [hb_var_num_t { u32: 0 }; 7],
};

#[repr(C)]
pub struct hb_draw_funcs_t(OpaqueFFIStruct);

pub type hb_draw_move_to_func_t = extern "C" fn(
    dfuncs: *mut hb_draw_funcs_t,
    draw_data: *mut c_void,
    st: *mut hb_draw_state_t,
    to_x: c_float,
    to_y: c_float,
    user_data: *mut c_void,
);
pub type hb_draw_line_to_func_t = extern "C" fn(
    dfuncs: *mut hb_draw_funcs_t,
    draw_data: *mut c_void,
    st: *mut hb_draw_state_t,
    to_x: c_float,
    to_y: c_float,
    user_data: *mut c_void,
);
pub type hb_draw_quadratic_to_func_t = extern "C" fn(
    dfuncs: *mut hb_draw_funcs_t,
    draw_data: *mut c_void,
    st: *mut hb_draw_state_t,
    control_x: c_float,
    control_y: c_float,
    to_x: c_float,
    to_y: c_float,
    user_data: *mut c_void,
);
pub type hb_draw_cubic_to_func_t = extern "C" fn(
    dfuncs: *mut hb_draw_funcs_t,
    draw_data: *mut c_void,
    st: *mut hb_draw_state_t,
    control1_x: c_float,
    control1_y: c_float,
    control2_x: c_float,
    control2_y: c_float,
    to_x: c_float,
    to_y: c_float,
    user_data: *mut c_void,
);
pub type hb_draw_close_path_func_t = extern "C" fn(
    dfuncs: *mut hb_draw_funcs_t,
    draw_data: *mut c_void,
    st: *mut hb_draw_state_t,
    user_data: *mut c_void,
);

unsafe extern "C" {
    pub fn hb_draw_funcs_set_move_to_func(
        dfuncs: *mut hb_draw_funcs_t,
        func: hb_draw_move_to_func_t,
        user_data: *mut c_void,
        destroy: Option<hb_destroy_func_t>,
    );
    pub fn hb_draw_funcs_set_line_to_func(
        dfuncs: *mut hb_draw_funcs_t,
        func: hb_draw_line_to_func_t,
        user_data: *mut c_void,
        destroy: Option<hb_destroy_func_t>,
    );
    pub fn hb_draw_funcs_set_quadratic_to_func(
        dfuncs: *mut hb_draw_funcs_t,
        func: hb_draw_quadratic_to_func_t,
        user_data: *mut c_void,
        destroy: Option<hb_destroy_func_t>,
    );
    pub fn hb_draw_funcs_set_cubic_to_func(
        dfuncs: *mut hb_draw_funcs_t,
        func: hb_draw_cubic_to_func_t,
        user_data: *mut c_void,
        destroy: Option<hb_destroy_func_t>,
    );
    pub fn hb_draw_funcs_set_close_path_func(
        dfuncs: *mut hb_draw_funcs_t,
        func: hb_draw_close_path_func_t,
        user_data: *mut c_void,
        destroy: Option<hb_destroy_func_t>,
    );

    pub fn hb_draw_funcs_create() -> *mut hb_draw_funcs_t;
    pub fn hb_draw_funcs_get_empty() -> *mut hb_draw_funcs_t;
    pub fn hb_draw_funcs_reference(dfuncs: *mut hb_draw_funcs_t) -> *mut hb_draw_funcs_t;
    pub fn hb_draw_funcs_destroy(dfuncs: *mut hb_draw_funcs_t);
    pub fn hb_draw_funcs_set_user_data(
        dfuncs: *mut hb_draw_funcs_t,
        key: *mut hb_user_data_key_t,
        data: *mut c_void,
        destroy: Option<hb_destroy_func_t>,
        replace: hb_bool_t,
    ) -> hb_bool_t;
    pub fn hb_draw_funcs_get_user_data(
        dfuncs: *const hb_draw_funcs_t,
        key: *mut hb_user_data_key_t,
    ) -> *mut c_void;
    pub fn hb_draw_funcs_make_immutable(dfuncs: *mut hb_draw_funcs_t);
    pub fn hb_draw_funcs_is_immutable(dfuncs: hb_draw_funcs_t) -> hb_bool_t;

    pub fn hb_draw_move_to(
        dfuncs: *mut hb_draw_funcs_t,
        draw_data: *mut c_void,
        st: *mut hb_draw_state_t,
        to_x: c_float,
        to_y: c_float,
    );
    pub fn hb_draw_line_to(
        dfuncs: *mut hb_draw_funcs_t,
        draw_data: *mut c_void,
        st: *mut hb_draw_state_t,
        to_x: c_float,
        to_y: c_float,
    );
    pub fn hb_draw_quadratic_to(
        dfuncs: *mut hb_draw_funcs_t,
        draw_data: *mut c_void,
        st: *mut hb_draw_state_t,
        control_x: c_float,
        control_y: c_float,
        to_x: c_float,
        to_y: c_float,
    );
    pub fn hb_draw_cubic_to(
        dfuncs: *mut hb_draw_funcs_t,
        draw_data: *mut c_void,
        st: *mut hb_draw_state_t,
        control_x1: c_float,
        control_y1: c_float,
        control_x2: c_float,
        control_y2: c_float,
        to_x: c_float,
        to_y: c_float,
    );
    pub fn hb_draw_close_path(
        dfuncs: *mut hb_draw_funcs_t,
        draw_data: *mut c_void,
        st: *mut hb_draw_state_t,
    );
}
