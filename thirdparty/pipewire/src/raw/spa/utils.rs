use core::ffi::*;

pub type spa_direction = c_int;
pub const SPA_DIRECTION_INPUT: spa_direction = 0;
pub const SPA_DIRECTION_OUTPUT: spa_direction = 1;

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

pub type spa_type = u32;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_START: spa_type = 0x00000;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_None: spa_type = 1;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_Bool: spa_type = 2;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_Id: spa_type = 3;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_Int: spa_type = 4;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_Long: spa_type = 5;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_Float: spa_type = 6;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_Double: spa_type = 7;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_String: spa_type = 8;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_Bytes: spa_type = 9;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_Rectangle: spa_type = 10;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_Fraction: spa_type = 11;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_Bitmap: spa_type = 12;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_Array: spa_type = 13;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_Struct: spa_type = 14;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_Object: spa_type = 15;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_Sequence: spa_type = 16;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_Pointer: spa_type = 17;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_Fd: spa_type = 18;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_Choice: spa_type = 19;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_Pod: spa_type = 20;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_POINTER_START: spa_type = 0x10000;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_POINTER_Buffer: spa_type = SPA_TYPE_POINTER_START + 1;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_POINTER_Meta: spa_type = SPA_TYPE_POINTER_START + 2;
#[allow(non_upper_case_globals)]
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_POINTER_Dict: spa_type = SPA_TYPE_POINTER_START + 3;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_EVENT_START: spa_type = 0x20000;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_EVENT_Device: spa_type = SPA_TYPE_EVENT_START + 1;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_EVENT_Node: spa_type = SPA_TYPE_EVENT_START + 2;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_COMMAND_START: spa_type = 0x30000;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_COMMAND_Device: spa_type = SPA_TYPE_COMMAND_START + 1;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_COMMAND_Node: spa_type = SPA_TYPE_COMMAND_START + 2;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_OBJECT_START: spa_type = 0x40000;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_OBJECT_PropInfo: spa_type = SPA_TYPE_OBJECT_START + 1;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_OBJECT_Props: spa_type = SPA_TYPE_OBJECT_START + 2;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_OBJECT_Format: spa_type = SPA_TYPE_OBJECT_START + 3;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_OBJECT_ParamBuffers: spa_type = SPA_TYPE_OBJECT_START + 4;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_OBJECT_ParamMeta: spa_type = SPA_TYPE_OBJECT_START + 5;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_OBJECT_ParamIO: spa_type = SPA_TYPE_OBJECT_START + 6;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_OBJECT_ParamProfile: spa_type = SPA_TYPE_OBJECT_START + 7;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_OBJECT_ParamPortConfig: spa_type = SPA_TYPE_OBJECT_START + 8;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_OBJECT_ParamRoute: spa_type = SPA_TYPE_OBJECT_START + 9;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_OBJECT_Profiler: spa_type = SPA_TYPE_OBJECT_START + 10;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_OBJECT_ParamLatency: spa_type = SPA_TYPE_OBJECT_START + 11;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_OBJECT_ParamProcessLatency: spa_type = SPA_TYPE_OBJECT_START + 12;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_OBJECT_ParamTag: spa_type = SPA_TYPE_OBJECT_START + 13;
/* vendor extensions */
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_VENDOR_PipeWire: spa_type = 0x02000000;
#[allow(non_upper_case_globals)]
pub const SPA_TYPE_VENDOR_Other: spa_type = 0x7f000000;
