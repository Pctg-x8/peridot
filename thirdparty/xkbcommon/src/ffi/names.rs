use core::ffi::*;

pub const XKB_MOD_NAME_SHIFT: &CStr = c"Shift";
pub const XKB_MOD_NAME_CAPS: &CStr = c"Lock";
pub const XKB_MOD_NAME_CTRL: &CStr = c"Control";
pub const XKB_MOD_NAME_MOD1: &CStr = c"Mod1";
pub const XKB_MOD_NAME_MOD2: &CStr = c"Mod2";
pub const XKB_MOD_NAME_MOD3: &CStr = c"Mod3";
pub const XKB_MOD_NAME_MOD4: &CStr = c"Mod4";
pub const XKB_MOD_NAME_MOD5: &CStr = c"Mod5";

pub const XKB_VMOD_NAME_ALT: &CStr = c"Alt";
pub const XKB_VMOD_NAME_HYPER: &CStr = c"Hyper";
pub const XKB_VMOD_NAME_LEVEL3: &CStr = c"LevelThree";
pub const XKB_VMOD_NAME_LEVEL5: &CStr = c"LevelFive";
pub const XKB_VMOD_NAME_META: &CStr = c"Meta";
pub const XKB_VMOD_NAME_NUM: &CStr = c"NumLock";
pub const XKB_VMOD_NAME_SCROLL: &CStr = c"ScrollLock";
pub const XKB_VMOD_NAME_SUPER: &CStr = c"Super";

pub const XKB_MOD_NAME_ALT: &CStr = c"Mod1";
pub const XKB_MOD_NAME_LOGO: &CStr = c"Mod4";
pub const XKB_MOD_NAME_NUM: &CStr = c"Mod2";

pub const XKB_LED_NAME_NUM: &CStr = c"Num Lock";
pub const XKB_LED_NAME_CAPS: &CStr = c"Caps Lock";
pub const XKB_LED_NAME_SCROLL: &CStr = c"Scroll Lock";
pub const XKB_LED_NAME_COMPOSE: &CStr = c"Compose";
pub const XKB_LED_NAME_KANA: &CStr = c"Kana";
