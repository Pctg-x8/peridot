use core::ffi::*;
use std::task::Context;

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
    pub on_pointer_down: extern "C" fn(
        caller_context: *mut c_void,
        window: *mut WindowLink,
        x: f64,
        y: f64,
        button: MouseButton,
    ),
    pub on_pointer_move:
        extern "C" fn(caller_context: *mut c_void, window: *mut WindowLink, x: f64, y: f64),
    pub on_pointer_up:
        extern "C" fn(caller_context: *mut c_void, window: *mut WindowLink, button: MouseButton),
    pub on_key_down: extern "C" fn(
        caller_context: *mut c_void,
        window: *mut WindowLink,
        code: u16,
        modifier_flags: u32,
    ),
    pub on_key_down_with_char: extern "C" fn(
        caller_context: *mut c_void,
        window: *mut WindowLink,
        code: u16,
        modifier_flags: u32,
        char: u32,
    ),
    pub on_key_up: extern "C" fn(
        caller_context: *mut c_void,
        window: *mut WindowLink,
        code: u16,
        modifier_flags: u32,
    ),
    pub on_key_focus_state_changed:
        extern "C" fn(caller_context: *mut c_void, window: *mut WindowLink, focused: u8),
}

#[repr(C)]
pub struct ContextMenuSurface(
    [u8; 0],
    core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,
);
#[repr(C)]
pub struct ContextMenuSurfaceCallbacks {
    pub on_pointer_down:
        extern "C" fn(sender: *mut ContextMenuSurface, x: f64, y: f64, button: MouseButton),
    pub on_pointer_move: extern "C" fn(sender: *mut ContextMenuSurface, x: f64, y: f64),
    pub on_pointer_up: extern "C" fn(sender: *mut ContextMenuSurface, button: MouseButton),
}

pub type UnboundCallback = extern "C" fn(caller_context: *mut c_void);
pub type ContextMenuGlobalClickCallback =
    extern "C" fn(caller_context: *mut c_void, on_context_menu_surface: u8);

// NSEventModifierFlags constants
pub const NSEVENT_MODIFIER_FLAG_SHIFT: u32 = 1 << 17;
pub const NSEVENT_MODIFIER_FLAG_CONTROL: u32 = 1 << 18;
pub const NSEVENT_MODIFIER_FLAG_OPTION: u32 = 1 << 19;
pub const NSEVENT_MODIFIER_FLAG_COMMAND: u32 = 1 << 20;

#[repr(u8)]
pub enum CursorShape {
    Arrow = 0,
    Pointer = 1,
    IBeam = 2,
    ResizeHorizontal = 3,
}

#[repr(u8)]
pub enum MouseButton {
    Left = 0,
    Right = 1,
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

    pub fn ni_set_pointer_hovering_timeout();
    pub fn ni_kill_pointer_hovering_timeout();

    pub fn ni_create_context_menu_surface(
        parent: *mut WindowLink,
        x: c_float,
        y: c_float,
        instance_vars: *mut c_void,
        callbacks: *mut ContextMenuSurfaceCallbacks,
    ) -> *mut ContextMenuSurface;
    pub fn ni_release_context_menu_surface(
        surface: *mut ContextMenuSurface,
        ret_instance_vars: *mut *mut c_void,
        ret_callbacks: *mut *mut ContextMenuSurfaceCallbacks,
    );
    pub fn ni_context_menu_get_metal_layer(surface: *mut ContextMenuSurface) -> *mut c_void;
    pub fn ni_context_menu_get_content_scale(surface: *mut ContextMenuSurface) -> c_float;
    pub fn ni_context_menu_resize(
        surface: *mut ContextMenuSurface,
        width: c_float,
        height: c_float,
    );
    pub fn ni_context_menu_instance_vars_ptr(surface: *mut ContextMenuSurface) -> *mut c_void;

    pub fn ni_context_menu_reserve_delayed_action(
        millis: c_int,
        callback: UnboundCallback,
        caller_context: *mut c_void,
    );
    pub fn ni_context_menu_unreserve_delayed_action();

    pub fn ni_context_menu_observe_global_click(
        callback: ContextMenuGlobalClickCallback,
        caller_context: *mut c_void,
    );
    pub fn ni_context_menu_unobserve_global_click();

    pub fn ni_post_unbound_callback_from_thread(f: UnboundCallback, caller_context: *mut c_void);

    pub fn manual_capture_begin(window_link: *mut WindowLink);
    pub fn manual_capture_end();

    pub fn ni_ak_spacing_inline_start() -> CFStringRef;
    pub fn ni_ak_font_id() -> CFStringRef;
}
