#![allow(non_camel_case_types)]

use core::ffi::*;

pub mod names;

pub use self::names::*;

#[link(name = "xkbcommon")]
unsafe extern "C" {}

#[repr(C)]
struct FFIOpaqueStruct(
    [u8; 0],
    core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,
);

#[repr(C)]
pub struct xkb_context(FFIOpaqueStruct);

#[repr(C)]
pub struct xkb_keymap(FFIOpaqueStruct);

#[repr(C)]
pub struct xkb_state(FFIOpaqueStruct);

pub type xkb_keycode_t = u32;

pub type xkb_keysym_t = u32;

pub type xkb_layout_index_t = u32;
pub type xkb_layout_mask_t = u32;

pub type xkb_level_index_t = u32;

pub type xkb_mod_index_t = u32;
pub type xkb_mod_mask_t = u32;

pub type xkb_led_index_t = u32;
pub type xkb_led_mask_t = u32;

pub const XKB_KEYCODE_INVALID: xkb_keycode_t = 0xffffffff;
pub const XKB_LAYOUT_INVALID: xkb_layout_index_t = 0xffffffff;
pub const XKB_LEVEL_INVALID: xkb_level_index_t = 0xffffffff;
pub const XKB_MOD_INVALID: xkb_mod_index_t = 0xffffffff;
pub const XKB_LED_INVALID: xkb_led_index_t = 0xffffffff;

pub const XKB_KEYCODE_MAX: xkb_keycode_t = 0xffffffff - 1;

pub const XKB_KEYSYM_MAX: xkb_keysym_t = 0x1fffffff;

#[inline(always)]
pub const fn xkb_keycode_is_legal_ext(key: xkb_keycode_t) -> bool {
    key <= XKB_KEYCODE_MAX
}

#[inline(always)]
pub const fn xkb_keycode_is_legal_x11(key: xkb_keycode_t) -> bool {
    key >= 8 && key <= 255
}

#[repr(C)]
pub struct xkb_rmlvo_builder(FFIOpaqueStruct);

pub type xkb_rmlvo_builder_flags = isize;
pub const XKB_RMLVO_BUILDER_NO_FLAGS: xkb_rmlvo_builder_flags = 0;

unsafe extern "C" {
    pub fn xkb_rmlvo_builder_new(
        context: *mut xkb_context,
        rules: *const c_char,
        model: *const c_char,
        flags: xkb_rmlvo_builder_flags,
    ) -> *mut xkb_rmlvo_builder;
    pub fn xkb_rmlvo_builder_append_layout(
        rmlvo: *mut xkb_rmlvo_builder,
        layout: *const c_char,
        variant: *const c_char,
        options: *const *const c_char,
        options_len: usize,
    ) -> bool;
    pub fn xkb_rmlvo_builder_append_option(
        rmlvo: *mut xkb_rmlvo_builder,
        option: *const c_char,
    ) -> bool;
    pub fn xkb_rmlvo_builder_ref(rmlvo: *mut xkb_rmlvo_builder) -> *mut xkb_rmlvo_builder;
    pub fn xkb_rmlvo_builder_unref(rmlvo: *mut xkb_rmlvo_builder);
}

#[repr(C)]
pub struct xkb_rule_names {
    pub rules: *const c_char,
    pub model: *const c_char,
    pub layout: *const c_char,
    pub variant: *const c_char,
    pub options: *const c_char,
}

#[repr(C)]
pub struct xkb_component_names {
    pub keycodes: *mut c_char,
    pub compatibility: *mut c_char,
    pub geometry: *mut c_char,
    pub symbols: *mut c_char,
    pub types: *mut c_char,
}

unsafe extern "C" {
    pub fn xkb_components_names_from_rules(
        context: *mut xkb_context,
        rmlvo_in: *const xkb_rule_names,
        rmlvo_out: *mut xkb_rule_names,
        components_out: *mut xkb_component_names,
    ) -> bool;

    pub fn xkb_keysym_get_name(keysym: xkb_keysym_t, buffer: *mut c_char, size: usize) -> c_int;
}

pub type xkb_keysym_flags = isize;
pub const XKB_KEYSYM_NO_FLAGS: xkb_keysym_flags = 0;
pub const XKB_KEYSYM_CASE_INSENSITIVE: xkb_keysym_flags = 1 << 0;

unsafe extern "C" {
    pub fn xkb_keysym_from_name(name: *const c_char, flags: xkb_keysym_flags) -> xkb_keysym_t;
    pub fn xkb_keysym_to_utf8(keysym: xkb_keysym_t, buffer: *mut c_char, size: usize) -> c_int;
    pub fn xkb_keysym_to_utf32(keysym: xkb_keysym_t) -> u32;
    pub fn xkb_utf32_to_keysym(ucs: u32) -> xkb_keysym_t;
    pub fn xkb_keysym_to_upper(ks: xkb_keysym_t) -> xkb_keysym_t;
    pub fn xkb_keysym_to_lower(ks: xkb_keysym_t) -> xkb_keysym_t;
}

pub type xkb_context_flags = isize;
pub const XKB_CONTEXT_NO_FLAGS: xkb_context_flags = 0;
pub const XKB_CONTEXT_NO_DEFAULT_INCLUDES: xkb_context_flags = 1 << 0;
pub const XKB_CONTEXT_NO_ENVIRONMENT_NAMES: xkb_context_flags = 1 << 1;
pub const XKB_CONTEXT_NO_SECURE_GETENV: xkb_context_flags = 1 << 2;

unsafe extern "C" {
    pub fn xkb_context_new(flags: xkb_context_flags) -> *mut xkb_context;
    pub fn xkb_context_ref(context: *mut xkb_context) -> *mut xkb_context;
    pub fn xkb_context_unref(context: *mut xkb_context);
    pub fn xkb_context_set_user_data(context: *mut xkb_context, user_data: *mut c_void);
    pub fn xkb_context_get_user_data(context: *mut xkb_context) -> *mut c_void;
    pub fn xkb_context_include_path_append(context: *mut xkb_context, path: *const c_char)
    -> c_int;
    pub fn xkb_context_include_path_append_default(context: *mut xkb_context) -> c_int;
    pub fn xkb_context_include_path_reset_defaults(context: *mut xkb_context) -> c_int;
    pub fn xkb_context_include_path_clear(context: *mut xkb_context);
    pub fn xkb_context_num_include_paths(context: *mut xkb_context) -> c_uint;
    pub fn xkb_context_include_path_get(context: *mut xkb_context, index: c_uint) -> *const c_char;
}

pub type xkb_log_level = isize;
pub const XKB_LOG_LEVEL_CRITICAL: xkb_log_level = 10;
pub const XKB_LOG_LEVEL_ERROR: xkb_log_level = 20;
pub const XKB_LOG_LEVEL_WARNING: xkb_log_level = 30;
pub const XKB_LOG_LEVEL_INFO: xkb_log_level = 40;
pub const XKB_LOG_LEVEL_DEBUG: xkb_log_level = 50;

unsafe extern "C" {
    pub fn xkb_context_set_log_level(context: *mut xkb_context, level: xkb_log_level);
    pub fn xkb_context_get_log_level(context: *mut xkb_context) -> xkb_log_level;
    pub fn xkb_context_set_log_verbosity(context: *mut xkb_context, verbosity: c_int);
    pub fn xkb_context_get_log_verbosity(context: *mut xkb_context) -> c_int;
    // xkb_context_set_log_fn
}

pub type xkb_keymap_compile_flags = isize;
pub const XKB_KEYMAP_COMPILE_NO_FLAGS: xkb_keymap_compile_flags = 0;

pub type xkb_keymap_format = isize;
pub const XKB_KEYMAP_FORMAT_TEXT_V1: xkb_keymap_format = 1;
pub const XKB_KEYMAP_FORMAT_TEXT_V2: xkb_keymap_format = 2;

unsafe extern "C" {
    pub fn xkb_keymap_new_from_rmlvo(
        rmlvo: *const xkb_rmlvo_builder,
        format: xkb_keymap_format,
        flags: xkb_keymap_compile_flags,
    ) -> *mut xkb_keymap;
    pub fn xkb_keymap_new_from_names(
        context: *mut xkb_context,
        names: *const xkb_rule_names,
        flags: xkb_keymap_compile_flags,
    ) -> *mut xkb_keymap;
    pub fn xkb_keymap_new_from_names2(
        context: *mut xkb_context,
        names: *const xkb_rule_names,
        format: xkb_keymap_format,
        flags: xkb_keymap_compile_flags,
    ) -> *mut xkb_keymap;
    // xkb_keymap_new_from_file
    pub fn xkb_keymap_new_from_string(
        context: *mut xkb_context,
        string: *const c_char,
        format: xkb_keymap_format,
        flags: xkb_keymap_compile_flags,
    ) -> *mut xkb_keymap;
    pub fn xkb_keymap_new_from_buffer(
        context: *mut xkb_context,
        buffer: *const c_char,
        length: usize,
        format: xkb_keymap_format,
        flags: xkb_keymap_compile_flags,
    ) -> *mut xkb_keymap;
    pub fn xkb_keymap_ref(keymap: *mut xkb_keymap) -> *mut xkb_keymap;
    pub fn xkb_keymap_unref(keymap: *mut xkb_keymap);
}

pub const XKB_KEYMAP_USE_ORIGINAL_FORMAT: xkb_keymap_format = -1;

unsafe extern "C" {
    pub fn xkb_keymap_get_as_string(
        keymap: *mut xkb_keymap,
        format: xkb_keymap_format,
    ) -> *mut c_char;
    pub fn xkb_keymap_min_keycode(keymap: *mut xkb_keymap) -> xkb_keycode_t;
    pub fn xkb_keymap_max_keycode(keymap: *mut xkb_keymap) -> xkb_keycode_t;
}

pub type xkb_keymap_key_iter_t =
    extern "C" fn(keymap: *mut xkb_keymap, key: xkb_keycode_t, data: *mut c_void);

unsafe extern "C" {
    pub fn xkb_keymap_key_for_each(
        keymap: *mut xkb_keymap,
        iter: xkb_keymap_key_iter_t,
        data: *mut c_void,
    );
    pub fn xkb_keymap_key_get_name(keymap: *mut xkb_keymap, key: xkb_keycode_t) -> *const c_char;
    pub fn xkb_keymap_key_by_name(keymap: *mut xkb_keymap, name: *const c_char) -> xkb_keycode_t;
    pub fn xkb_keymap_num_mods(keymap: *mut xkb_keymap) -> xkb_mod_index_t;
    pub fn xkb_keymap_mod_get_name(
        keymap: *mut xkb_keymap,
        index: xkb_mod_index_t,
    ) -> *const c_char;
    pub fn xkb_keymap_mod_get_index(
        keymap: *mut xkb_keymap,
        name: *const c_char,
    ) -> xkb_mod_index_t;
    pub fn xkb_keymap_mod_get_mask(keymap: *mut xkb_keymap, name: *const c_char) -> xkb_mod_mask_t;
    pub fn xkb_keymap_mod_get_mask2(
        keymap: *mut xkb_keymap,
        idx: xkb_mod_index_t,
    ) -> xkb_mod_mask_t;
    pub fn xkb_keymap_num_layouts(keymap: *mut xkb_keymap) -> xkb_layout_index_t;
    pub fn xkb_keymap_layout_get_name(
        keymap: *mut xkb_keymap,
        idx: xkb_layout_index_t,
    ) -> *const c_char;
    pub fn xkb_keymap_layout_get_index(
        keymap: *mut xkb_keymap,
        name: *const c_char,
    ) -> xkb_layout_index_t;
    pub fn xkb_keymap_num_leds(keymap: *mut xkb_keymap) -> xkb_led_index_t;
    pub fn xkb_keymap_led_get_name(
        keymap: *mut xkb_keymap,
        index: xkb_led_index_t,
    ) -> *const c_char;
    pub fn xkb_keymap_led_get_index(
        keymap: *mut xkb_keymap,
        name: *const c_char,
    ) -> xkb_led_index_t;
    pub fn xkb_keymap_num_layouts_for_key(
        keymap: *mut xkb_keymap,
        key: xkb_keycode_t,
    ) -> xkb_layout_index_t;
    pub fn xkb_keymap_num_levels_for_key(
        keymap: *mut xkb_keymap,
        key: xkb_keycode_t,
        layout: xkb_layout_index_t,
    ) -> xkb_level_index_t;
    pub fn xkb_keymap_key_get_mods_for_level(
        keymap: *mut xkb_keymap,
        key: xkb_keycode_t,
        layout: xkb_layout_index_t,
        level: xkb_level_index_t,
        masks_out: *mut xkb_mod_mask_t,
        masks_size: usize,
    ) -> usize;
    pub fn xkb_keymap_key_get_syms_by_level(
        keymap: *mut xkb_keymap,
        key: xkb_keycode_t,
        layout: xkb_layout_index_t,
        level: xkb_level_index_t,
        syms_out: *mut *const xkb_keysym_t,
    ) -> c_int;
    pub fn xkb_keymap_key_repeats(keymap: *mut xkb_keymap, key: xkb_keycode_t) -> c_int;

    pub fn xkb_state_new(keymap: *mut xkb_keymap) -> *mut xkb_state;
    pub fn xkb_state_ref(state: *mut xkb_state) -> *mut xkb_state;
    pub fn xkb_state_unref(state: *mut xkb_state);
    pub fn xkb_state_get_keymap(state: *mut xkb_state) -> *mut xkb_keymap;
}

pub type xkb_key_direction = isize;
pub const XKB_KEY_UP: xkb_key_direction = 0;
pub const XKB_KEY_DOWN: xkb_key_direction = 1;

pub type xkb_state_component = isize;
pub const XKB_STATE_MODS_DEPRESSED: xkb_state_component = 1 << 0;
pub const XKB_STATE_MODS_LATCHED: xkb_state_component = 1 << 1;
pub const XKB_STATE_MODS_LOCKED: xkb_state_component = 1 << 2;
pub const XKB_STATE_MODS_EFFECTIVE: xkb_state_component = 1 << 3;
pub const XKB_STATE_LAYOUT_DEPRESSED: xkb_state_component = 1 << 4;
pub const XKB_STATE_LAYOUT_LATCHED: xkb_state_component = 1 << 5;
pub const XKB_STATE_LAYOUT_LOCKED: xkb_state_component = 1 << 6;
pub const XKB_STATE_LAYOUT_EFFECTIVE: xkb_state_component = 1 << 7;
pub const XKB_STATE_LEDS: xkb_state_component = 1 << 8;

unsafe extern "C" {
    pub fn xkb_state_update_keys(
        state: *mut xkb_state,
        key: xkb_keycode_t,
        direction: xkb_key_direction,
    ) -> xkb_state_component;
    pub fn xkb_state_update_latched_locked(
        state: *mut xkb_state,
        effect_latched_mode: xkb_mod_mask_t,
        latched_mods: xkb_mod_mask_t,
        effect_latched_layout: bool,
        latched_layout: i32,
        effect_locked_mods: xkb_mod_mask_t,
        locked_mods: xkb_mod_mask_t,
        effect_locked_layout: bool,
        locked_layout: i32,
    ) -> xkb_state_component;
    pub fn xkb_state_update_mask(
        state: *mut xkb_state,
        depressed_mode: xkb_mod_mask_t,
        latched_mods: xkb_mod_mask_t,
        locked_mods: xkb_mod_mask_t,
        depressed_layout: xkb_layout_index_t,
        latched_layout: xkb_layout_index_t,
        locked_layout: xkb_layout_index_t,
    ) -> xkb_state_component;
    pub fn xkb_state_key_get_syms(
        state: *mut xkb_state,
        key: xkb_keycode_t,
        syms_out: *mut *const xkb_keysym_t,
    ) -> c_int;
    pub fn xkb_state_key_get_utf8(
        state: *mut xkb_state,
        key: xkb_keycode_t,
        buffer: *mut c_char,
        size: usize,
    ) -> c_int;
    pub fn xkb_state_key_get_utf32(state: *mut xkb_state, key: xkb_keycode_t) -> u32;
    pub fn xkb_state_key_get_one_sym(state: *mut xkb_state, key: xkb_keycode_t) -> xkb_keysym_t;
    pub fn xkb_state_key_get_layout(
        state: *mut xkb_state,
        key: xkb_keycode_t,
    ) -> xkb_layout_index_t;
    pub fn xkb_state_key_get_level(
        state: *mut xkb_state,
        key: xkb_keycode_t,
        layout: xkb_layout_index_t,
    ) -> xkb_level_index_t;
}

pub type xkb_state_match = isize;
pub const XKB_STATE_MATCH_ANY: xkb_state_match = 1 << 0;
pub const XKB_STATE_MATCH_ALL: xkb_state_match = 1 << 1;
pub const XKB_STATE_MATCH_NON_EXCLUSIVE: xkb_state_match = 1 << 16;

unsafe extern "C" {
    pub fn xkb_state_serialize_mods(
        state: *mut xkb_state,
        components: xkb_state_component,
    ) -> xkb_mod_mask_t;
    pub fn xkb_state_serialize_layout(
        state: *mut xkb_state,
        components: xkb_state_component,
    ) -> xkb_layout_index_t;
    pub fn xkb_state_mod_name_is_active(
        state: *mut xkb_state,
        name: *const c_char,
        r#type: xkb_state_component,
    ) -> c_int;
    pub fn xkb_state_mod_names_are_active(
        state: *mut xkb_state,
        r#type: xkb_state_component,
        r#match: xkb_state_match,
        ...
    ) -> c_int;
    pub fn xkb_state_mod_index_is_active(
        state: *mut xkb_state,
        idx: xkb_mod_index_t,
        r#type: xkb_state_component,
    ) -> c_int;
    pub fn xkb_state_mod_indices_are_active(
        state: *mut xkb_state,
        r#type: xkb_state_component,
        r#match: xkb_state_match,
        ...
    ) -> c_int;
}

pub type xkb_consumed_mode = isize;
pub const XKB_CONSUMED_MODE_XKB: xkb_consumed_mode = 0;
pub const XKB_CONSUMED_MODE_GTK: xkb_consumed_mode = 1;

unsafe extern "C" {
    pub fn xkb_state_key_get_consumed_mods2(
        state: *mut xkb_state,
        key: xkb_keycode_t,
        mode: xkb_consumed_mode,
    ) -> xkb_mod_mask_t;
    pub fn xkb_state_key_get_consumed_mods(
        state: *mut xkb_state,
        key: xkb_keycode_t,
    ) -> xkb_mod_mask_t;
    pub fn xkb_state_mod_index_is_consumed2(
        state: *mut xkb_state,
        key: xkb_keycode_t,
        idx: xkb_mod_index_t,
        mode: xkb_consumed_mode,
    ) -> c_int;
    pub fn xkb_state_mod_index_is_consumed(
        state: *mut xkb_state,
        key: xkb_keycode_t,
        idx: xkb_mod_index_t,
    ) -> c_int;
    pub fn xkb_state_mod_mask_remove_consumed(
        state: *mut xkb_state,
        key: xkb_keycode_t,
        mask: xkb_mod_mask_t,
    ) -> xkb_mod_mask_t;
    pub fn xkb_state_layout_name_is_active(
        state: *mut xkb_state,
        name: *const c_char,
        r#type: xkb_state_component,
    ) -> c_int;
    pub fn xkb_state_layout_index_is_active(
        state: *mut xkb_state,
        idx: xkb_layout_index_t,
        r#type: xkb_state_component,
    ) -> c_int;
    pub fn xkb_state_led_name_is_active(state: *mut xkb_state, name: *const c_char) -> c_int;
    pub fn xkb_state_led_index_is_active(state: *mut xkb_state, idx: xkb_led_index_t) -> c_int;
}
