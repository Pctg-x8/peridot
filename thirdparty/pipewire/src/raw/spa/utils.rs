use core::ffi::*;

pub type spa_direction = c_int;
pub const SPA_DIRECTION_INPUT: spa_direction = 0;
pub const SPA_DIRECTION_OUTPUT: spa_direction = 1;

pub const SPA_ID_INVALID: u32 = 0xffff_ffff;

#[repr(C)]
pub struct spa_rectangle {
    pub width: u32,
    pub height: u32,
}

#[repr(C)]
pub struct spa_point {
    pub x: i32,
    pub y: i32,
}

#[repr(C)]
pub struct spa_region {
    pub position: spa_point,
    pub size: spa_rectangle,
}

#[repr(C)]
pub struct spa_fraction {
    pub num: u32,
    pub denom: u32,
}

#[repr(C)]
pub struct spa_error_location {
    pub line: c_int,
    pub col: c_int,
    pub len: usize,
    pub location: *const c_char,
    pub reason: *const c_char,
}

#[repr(C)]
pub struct spa_list {
    pub next: *mut spa_list,
    pub prev: *mut spa_list,
}
impl spa_list {
    #[inline]
    pub fn is_initialized(&self) -> bool {
        !self.prev.is_null()
    }

    #[inline]
    pub fn remove(&mut self) {
        unsafe {
            (*self.prev).next = self.next;
            (*self.next).prev = self.prev;
        }
    }
}

#[repr(C)]
pub struct spa_dict_item {
    pub key: *const c_char,
    pub value: *const c_char,
}

#[repr(C)]
pub struct spa_dict {
    pub flags: u32,
    pub n_items: u32,
    pub items: *const spa_dict_item,
}

pub const SPA_DICT_FLAG_SORTED: u32 = 1 << 0;

#[repr(C)]
pub struct spa_callbacks {
    pub funcs: *const c_void,
    pub data: *mut c_void,
}

#[repr(C)]
pub struct spa_interface {
    pub r#type: *const c_char,
    pub version: u32,
    pub cb: spa_callbacks,
}

#[repr(C)]
pub struct spa_hook_list {
    pub list: spa_list,
}

#[repr(C)]
pub struct spa_hook {
    pub link: spa_list,
    pub cb: spa_callbacks,
    pub removed: Option<extern "C" fn(hook: *mut spa_hook)>,
    r#priv: *mut c_void,
}
impl spa_hook {
    #[inline]
    pub fn remove(&mut self) {
        if self.link.is_initialized() {
            self.link.remove();
        }

        if let Some(ref removed) = self.removed {
            removed(self);
        }
    }
}
