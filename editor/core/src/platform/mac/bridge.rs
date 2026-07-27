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
    pub on_pointer_down: extern "C" fn(
        caller_context: *mut c_void,
        window: *mut WindowLink,
        x: f64,
        y: f64,
        button: MouseButton,
    ),
    pub on_pointer_move:
        extern "C" fn(caller_context: *mut c_void, window: *mut WindowLink, x: f64, y: f64),
    pub on_pointer_delta_move:
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
    pub on_key_up_with_char: extern "C" fn(
        caller_context: *mut c_void,
        window: *mut WindowLink,
        code: u16,
        modifier_flags: u32,
        char: u32,
    ),
    pub on_key_focus_state_changed:
        extern "C" fn(caller_context: *mut c_void, window: *mut WindowLink, focused: u8),
    pub on_scroll_wheel: extern "C" fn(
        caller_context: *mut c_void,
        window: *mut WindowLink,
        modifier_flags: u32,
        amount: f64,
    ),
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
    pub on_pointer_leave: extern "C" fn(sender: *mut ContextMenuSurface),
}

pub trait TextInputClientForwarding {
    fn has_marked_text(&self) -> bool;
    fn marked_range(&self, out_location: *mut i64, out_length: *mut i64) -> bool;
    fn selected_range(&self, out_location: *mut i64, out_length: *mut i64);
    fn set_marked_text(
        &self,
        text: &core::ffi::CStr,
        new_selection_location: i64,
        new_selection_length: i64,
        replacement_location: i64,
        replacement_length: i64,
    );
    fn insert_text(
        &self,
        text: &core::ffi::CStr,
        replacement_location: i64,
        replacement_length: i64,
    );
    fn substring(
        &self,
        location: Option<i64>,
        length: i64,
        actual_location: *mut i64,
        actual_length: *mut i64,
        out_chars: *mut *const core::ffi::c_char,
        out_len: *mut u64,
    );
    fn first_rect(
        &self,
        location: i64,
        length: i64,
        actual_location: *mut i64,
        actual_length: *mut i64,
        surface_x: *mut f32,
        surface_y: *mut f32,
        width: *mut f32,
        height: *mut f32,
    );
}
#[repr(C)]
pub struct TextInputClientForwardingFT {
    pub has_marked_text: extern "C" fn(context: *mut c_void) -> u8,
    pub marked_range:
        extern "C" fn(context: *mut c_void, out_location: *mut i64, out_length: *mut i64) -> u8,
    pub selected_range:
        extern "C" fn(context: *mut c_void, out_location: *mut i64, out_length: *mut i64),
    pub set_marked_text: extern "C" fn(
        context: *mut c_void,
        str: *const c_char,
        new_selection_location: i64,
        new_selection_length: i64,
        replacement_location: i64,
        replacement_length: i64,
    ),
    pub insert_text: extern "C" fn(
        context: *mut c_void,
        str: *const c_char,
        replacement_location: i64,
        replacement_length: i64,
    ),
    pub substring: extern "C" fn(
        context: *mut c_void,
        location_is_not_found: u8,
        location: i64,
        length: i64,
        actual_location: *mut i64,
        actual_length: *mut i64,
        out_chars: *mut *const core::ffi::c_char,
        out_len: *mut u64,
    ),
    pub first_rect: extern "C" fn(
        context: *mut c_void,
        location: i64,
        length: i64,
        actual_location: *mut i64,
        actual_length: *mut i64,
        surface_x: *mut f32,
        surface_y: *mut f32,
        width: *mut f32,
        height: *mut f32,
    ),
}
impl TextInputClientForwardingFT {
    pub const fn r#for<T: TextInputClientForwarding>() -> Self {
        extern "C" fn has_marked_text<T: TextInputClientForwarding>(context: *mut c_void) -> u8 {
            if T::has_marked_text(unsafe { &*context.cast::<T>() }) {
                1
            } else {
                0
            }
        }
        extern "C" fn marked_range<T: TextInputClientForwarding>(
            context: *mut c_void,
            out_location: *mut i64,
            out_length: *mut i64,
        ) -> u8 {
            if T::marked_range(unsafe { &*context.cast::<T>() }, out_location, out_length) {
                1
            } else {
                0
            }
        }
        extern "C" fn selected_range<T: TextInputClientForwarding>(
            context: *mut c_void,
            out_location: *mut i64,
            out_length: *mut i64,
        ) {
            T::selected_range(unsafe { &*context.cast::<T>() }, out_location, out_length);
        }
        extern "C" fn set_marked_text<T: TextInputClientForwarding>(
            context: *mut c_void,
            text: *const c_char,
            new_selection_location: i64,
            new_selection_length: i64,
            replacement_location: i64,
            replacement_length: i64,
        ) {
            T::set_marked_text(
                unsafe { &*context.cast::<T>() },
                unsafe { CStr::from_ptr(text) },
                new_selection_location,
                new_selection_length,
                replacement_location,
                replacement_length,
            );
        }
        extern "C" fn insert_text<T: TextInputClientForwarding>(
            context: *mut c_void,
            text: *const c_char,
            replacement_location: i64,
            replacement_length: i64,
        ) {
            T::insert_text(
                unsafe { &*context.cast::<T>() },
                unsafe { CStr::from_ptr(text) },
                replacement_location,
                replacement_length,
            );
        }
        extern "C" fn substring<T: TextInputClientForwarding>(
            context: *mut c_void,
            location_is_not_found: u8,
            location: i64,
            length: i64,
            actual_location: *mut i64,
            actual_length: *mut i64,
            out_chars: *mut *const c_char,
            out_len: *mut u64,
        ) {
            T::substring(
                unsafe { &*context.cast::<T>() },
                if location_is_not_found != 0 {
                    None
                } else {
                    Some(location)
                },
                length,
                actual_location,
                actual_length,
                out_chars,
                out_len,
            );
        }
        extern "C" fn first_rect<T: TextInputClientForwarding>(
            context: *mut c_void,
            location: i64,
            length: i64,
            actual_location: *mut i64,
            actual_length: *mut i64,
            surface_x: *mut f32,
            surface_y: *mut f32,
            width: *mut f32,
            height: *mut f32,
        ) {
            T::first_rect(
                unsafe { &*context.cast::<T>() },
                location,
                length,
                actual_location,
                actual_length,
                surface_x,
                surface_y,
                width,
                height,
            );
        }

        Self {
            has_marked_text: has_marked_text::<T>,
            marked_range: marked_range::<T>,
            selected_range: selected_range::<T>,
            set_marked_text: set_marked_text::<T>,
            insert_text: insert_text::<T>,
            substring: substring::<T>,
            first_rect: first_rect::<T>,
        }
    }
}

pub type UnboundCallback = extern "C" fn(caller_context: *mut c_void);
pub type ContextMenuGlobalClickCallback =
    extern "C" fn(caller_context: *mut c_void, on_context_menu_surface: u8);

// NSEventModifierFlags constants
pub const NSEVENT_MODIFIER_FLAG_SHIFT: u32 = 1 << 17;
pub const NSEVENT_MODIFIER_FLAG_CONTROL: u32 = 1 << 18;
pub const NSEVENT_MODIFIER_FLAG_OPTION: u32 = 1 << 19;
pub const NSEVENT_MODIFIER_FLAG_COMMAND: u32 = 1 << 20;

// https://developer.apple.com/documentation/appkit/function-key-unicode-values
pub const NS_UP_ARROW_FUNCTION_KEY: char = '\u{f700}';
pub const NS_DOWN_ARROW_FUNCTION_KEY: char = '\u{f701}';
pub const NS_LEFT_ARROW_FUNCTION_KEY: char = '\u{f702}';
pub const NS_RIGHT_ARROW_FUNCTION_KEY: char = '\u{f703}';
pub const NS_DELETE_FUNCTION_KEY: char = '\u{f728}';
pub const NS_HOME_FUNCTION_KEY: char = '\u{f729}';
pub const NS_END_FUNCTION_KEY: char = '\u{f72b}';

#[repr(u8)]
pub enum CursorShape {
    Arrow = 0,
    Pointer = 1,
    IBeam = 2,
    ResizeHorizontal = 3,
    ResizeVertical = 4,
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
    pub fn ni_show_window_as_primary(window_link: *mut WindowLink);
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
    pub fn ni_lock_cursor();
    pub fn ni_unlock_cursor();
    pub fn ni_set_cursor_shape(shape: u8);

    pub fn ni_begin_drag(window_link: *mut WindowLink);

    pub fn ni_accepts_key_inputs_to_view(
        window_link: *mut WindowLink,
        forwarding_ft: *const TextInputClientForwardingFT,
        forwarding_ctx: *mut c_void,
    );
    pub fn ni_accepts_key_inputs_to_window(
        window_link: *mut WindowLink,
        ret_forwarding_ft: *mut *const TextInputClientForwardingFT,
        ret_forwarding_ctx: *mut *mut c_void,
    );

    pub fn ni_show_drag_preview(x: c_double, y: c_double, width: c_double, height: c_double);
    pub fn ni_hide_drag_preview();
    pub fn ni_move_drag_preview(x: c_double, y: c_double);

    pub fn ni_query_filesystem_cachedir_path() -> *const c_char;
    pub fn ni_query_filesystem_persist_statedir_path() -> *const c_char;

    pub fn ni_degreade_thread_priroity_temporarily() -> *mut c_void;
    pub fn ni_restore_thread_priority(context_ptr: *mut c_void);

    pub fn ni_set_pointer_hovering_timeout(millis: u32);
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

    pub fn ni_query_range_for_word_at(
        charptr: *const u8,
        charlen: u64,
        at: u64,
        ret_start: *mut u64,
        ret_end: *mut u64,
    );

    pub fn ni_ak_spacing_inline_start() -> CFStringRef;
    pub fn ni_ak_font_id() -> CFStringRef;

    pub fn ni_log_err(charbuf: *const u8);
    pub fn ni_log_warn(charbuf: *const u8);
    pub fn ni_log_info(charbuf: *const u8);
    pub fn ni_log_debug(charbuf: *const u8);
    pub fn ni_log_trace(charbuf: *const u8);
    pub fn ni_log_fault(charbuf: *const u8);
}
