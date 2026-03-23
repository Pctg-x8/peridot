use core::ffi::*;

use apple_sdk_port::raw::CFStringRef;
use bitflags::bitflags;

#[repr(C)]
pub struct WindowLink(
    [u8; 0],
    core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,
);

#[repr(C)]
pub struct WindowLinkCallbacks {
    pub destructor: extern "C" fn(this: *mut c_void),
    pub on_window_close: extern "C" fn(caller_context: *mut c_void, window: *mut WindowLink),
    pub on_resize: extern "C" fn(
        caller_context: *mut c_void,
        window: *mut WindowLink,
        width: f64,
        height: f64,
    ),
    pub on_pointer_down:
        extern "C" fn(caller_context: *mut c_void, window: *mut WindowLink, x: f64, y: f64),
    pub on_pointer_move:
        extern "C" fn(caller_context: *mut c_void, window: *mut WindowLink, x: f64, y: f64),
    pub on_pointer_up: extern "C" fn(caller_context: *mut c_void, window: *mut WindowLink),
}

#[repr(u8)]
pub enum CursorShape {
    Arrow = 0,
    Pointer = 1,
    IBeam = 2,
    ResizeHorizontal = 3,
}

bitflags! {
    #[derive(Clone, Copy)]
    pub struct WindowCreationFlags : u32 {
        const MAIN = 0x01;
    }
}

unsafe extern "C" {
    pub fn nsapp_run();

    pub fn ni_create_window(flags: u32) -> *mut WindowLink;
    pub fn ni_release_window(window_link: *mut WindowLink);
    pub fn ni_make_primary_window(window_link: *mut WindowLink);
    pub fn ni_show_window(window_link: *mut WindowLink);
    pub fn ni_get_content_scale(window_link: *mut WindowLink) -> c_float;
    pub fn ni_set_window_callbacks(
        window_link: *mut WindowLink,
        callbacks: *const WindowLinkCallbacks,
        caller_context: *mut c_void,
    );
    pub fn ni_get_window_callback_context(window_link: *mut WindowLink) -> *mut c_void;
    pub fn ni_get_size_logical(
        window_link: *mut WindowLink,
        width: *mut c_double,
        height: *mut c_double,
    );
    pub fn ni_get_metal_layer(window_link: *mut WindowLink) -> *mut c_void;
    pub fn ni_convert_point_to_screen(
        window_link: *mut WindowLink,
        x: *mut c_double,
        y: *mut c_double,
    );
    pub fn ni_set_cursor_shape(shape: u8);

    pub fn ni_show_drag_preview(x: c_double, y: c_double, width: c_double, height: c_double);
    pub fn ni_hide_drag_preview();
    pub fn ni_move_drag_preview(x: c_double, y: c_double);

    pub fn ni_query_filesystem_cachedir_path() -> *const c_char;

    pub fn ni_degreade_thread_priroity_temporarily() -> *mut c_void;
    pub fn ni_restore_thread_priority(context_ptr: *mut c_void);

    pub fn manual_capture_begin(window_link: *mut WindowLink);
    pub fn manual_capture_end();

    pub fn ni_ak_spacing_inline_start() -> CFStringRef;
    pub fn ni_ak_font_id() -> CFStringRef;
}
