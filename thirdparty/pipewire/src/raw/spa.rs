#![allow(non_camel_case_types)]
use core::ffi::*;

mod utils;
pub use self::utils::*;

mod param;
pub use self::param::*;

mod buffer;
pub use self::buffer::*;

#[repr(C)]
pub struct spa_handle {
    pub version: u32,
    pub get_interface: Option<
        extern "C" fn(
            handle: *mut spa_handle,
            r#type: *const c_char,
            iface: *mut *mut c_void,
        ) -> c_int,
    >,
    pub clear: Option<extern "C" fn(handle: *mut spa_handle) -> c_int>,
}

#[repr(C)]
pub struct spa_interface_info {
    pub r#type: *const c_char,
}

#[repr(C)]
pub struct spa_support {
    pub r#type: *const c_char,
    pub data: *mut c_void,
}

#[repr(C)]
pub struct spa_handle_factory {
    pub version: u32,
    pub name: *const c_char,
    pub info: *const spa_dict,
    pub get_size:
        Option<extern "C" fn(factory: *const spa_handle_factory, params: *const spa_dict) -> usize>,
    pub init: Option<
        extern "C" fn(
            factory: *const spa_handle_factory,
            handle: *mut spa_handle,
            info: *const spa_dict,
            support: *const spa_support,
            n_support: u32,
        ) -> c_int,
    >,
    pub enum_interface_info: Option<
        extern "C" fn(
            factory: *const spa_handle_factory,
            info: *mut *const spa_interface_info,
            index: *mut u32,
        ) -> c_int,
    >,
}

pub type spa_handle_factory_enum_func_t =
    extern "C" fn(factory: *mut *const spa_handle_factory, index: *mut u32) -> c_int;

unsafe extern "C" {
    pub fn spa_handle_factory_enum(
        factory: *mut *const spa_handle_factory,
        index: *mut u32,
    ) -> c_int;
}

#[repr(C)]
pub struct spa_system {
    pub iface: spa_interface,
}

pub const SPA_IO_IN: u32 = 1 << 0;
pub const SPA_IO_OUT: u32 = 1 << 2;
pub const SPA_IO_ERR: u32 = 1 << 3;
pub const SPA_IO_HUP: u32 = 1 << 4;

pub const SPA_FD_CLOEXEC: u32 = 1 << 0;
pub const SPA_FD_NONBLOCK: u32 = 1 << 1;
pub const SPA_FD_EVENT_SEMAPHORE: u32 = 1 << 2;
pub const SPA_FD_TIMER_ABSTIME: u32 = 1 << 3;
pub const SPA_FD_TIMER_CANCEL_ON_SET: u32 = 1 << 4;

#[repr(C)]
pub struct spa_poll_event {
    pub events: u32,
    pub data: *mut c_void,
}

#[repr(C)]
pub struct spa_loop {
    pub iface: spa_interface,
}

#[repr(C)]
pub struct spa_loop_control {
    pub iface: spa_interface,
}

#[repr(C)]
pub struct spa_loop_utils {
    pub iface: spa_interface,
}

pub type spa_source_func_t = extern "C" fn(source: *mut spa_source);

#[repr(C)]
pub struct spa_source {
    pub r#loop: *mut spa_loop,
    pub func: Option<spa_source_func_t>,
    pub data: *mut c_void,
    pub fd: c_int,
    pub mask: u32,
    pub rmask: u32,
    r#priv: *mut c_void,
}

pub type spa_invoke_func_t = extern "C" fn(
    r#loop: *mut spa_loop,
    r#async: bool,
    seq: u32,
    data: *const c_void,
    size: usize,
    user_data: *mut c_void,
) -> c_int;

#[repr(C)]
pub struct spa_loop_methods {
    pub version: u32,
    pub add_source: Option<extern "C" fn(object: *mut c_void, source: *mut spa_source) -> c_int>,
    pub update_source: Option<extern "C" fn(object: *mut c_void, source: *mut spa_source) -> c_int>,
    pub remove_source: Option<extern "C" fn(object: *mut c_void, source: *mut spa_source) -> c_int>,
    pub invoke: Option<
        extern "C" fn(
            object: *mut c_void,
            func: Option<spa_invoke_func_t>,
            seq: u32,
            data: *const c_void,
            size: usize,
            block: bool,
            user_data: *mut c_void,
        ) -> c_int,
    >,
}

#[repr(C)]
pub struct spa_loop_control_hooks {
    pub version: u32,
    pub before: Option<extern "C" fn(data: *mut c_void)>,
    pub after: Option<extern "C" fn(data: *mut c_void)>,
}

#[repr(C)]
pub struct spa_loop_control_methods {
    pub version: u32,
    pub get_fd: Option<extern "C" fn(object: *mut c_void) -> c_int>,
    pub add_hook: Option<
        extern "C" fn(
            object: *mut c_void,
            hook: *mut spa_hook,
            hooks: *const spa_loop_control_hooks,
            data: *mut c_void,
        ),
    >,
    pub enter: Option<extern "C" fn(object: *mut c_void)>,
    pub leave: Option<extern "C" fn(object: *mut c_void)>,
    pub iterate: Option<extern "C" fn(object: *mut c_void, timeout: c_int) -> c_int>,
    pub check: Option<extern "C" fn(object: *mut c_void) -> c_int>,
}

pub type spa_source_io_func_t = extern "C" fn(data: *mut c_void, fd: c_int, mask: u32);
pub type spa_source_idle_func_t = extern "C" fn(data: *mut c_void);
pub type spa_source_event_func_t = extern "C" fn(data: *mut c_void, count: u64);
pub type spa_source_timer_func_t = extern "C" fn(data: *mut c_void, expirations: u64);
pub type spa_source_signal_func_t = extern "C" fn(data: *mut c_void, signal_number: c_int);

#[repr(C)]
pub struct spa_thread(
    [u8; 0],
    core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,
);

#[repr(C)]
pub struct spa_thread_utils {
    pub iface: spa_interface,
}

#[repr(C)]
pub struct spa_thread_utils_methods {
    pub version: u32,
    pub create: Option<
        extern "C" fn(
            object: *mut c_void,
            props: *const spa_dict,
            start: Option<extern "C" fn(*mut c_void) -> *mut c_void>,
            arg: *mut c_void,
        ) -> *mut spa_thread,
    >,
    pub join: Option<
        extern "C" fn(
            object: *mut c_void,
            thread: *mut spa_thread,
            retval: *mut *mut c_void,
        ) -> c_int,
    >,
    pub get_rt_range: Option<
        extern "C" fn(
            object: *mut c_void,
            props: *const spa_dict,
            min: *mut c_int,
            max: *mut c_int,
        ) -> c_int,
    >,
    pub acquire_rt: Option<
        extern "C" fn(object: *mut c_void, thread: *mut spa_thread, priority: c_int) -> c_int,
    >,
    pub drop_rt: Option<extern "C" fn(object: *mut c_void, thread: *mut spa_thread) -> c_int>,
}

#[repr(C)]
pub struct spa_loop_utils_methods {
    pub version: u32,
    pub add_io: Option<
        extern "C" fn(
            object: *mut c_void,
            fd: c_int,
            mask: u32,
            close: bool,
            func: Option<spa_source_io_func_t>,
            data: *mut c_void,
        ) -> *mut spa_source,
    >,
    pub update_io:
        Option<extern "C" fn(object: *mut c_void, source: *mut spa_source, mask: u32) -> c_int>,
    pub add_idle: Option<
        extern "C" fn(
            object: *mut c_void,
            enabled: bool,
            func: Option<spa_source_idle_func_t>,
            data: *mut c_void,
        ) -> *mut spa_source,
    >,
    pub enable_idle:
        Option<extern "C" fn(object: *mut c_void, source: *mut spa_source, enabled: bool) -> c_int>,
    pub add_event: Option<
        extern "C" fn(
            object: *mut c_void,
            func: Option<spa_source_event_func_t>,
            data: *mut c_void,
        ) -> *mut spa_source,
    >,
    pub signal_event: Option<extern "C" fn(object: *mut c_void, source: *mut spa_source) -> c_int>,
    pub add_timer: Option<
        extern "C" fn(
            object: *mut c_void,
            func: Option<spa_source_timer_func_t>,
            data: *mut c_void,
        ) -> *mut spa_source,
    >,
    pub update_timer: Option<
        extern "C" fn(
            object: *mut c_void,
            source: *mut spa_source,
            value: *mut libc::timespec,
            interval: *mut libc::timespec,
            absolute: bool,
        ) -> c_int,
    >,
    pub add_signal: Option<
        extern "C" fn(
            object: *mut c_void,
            signal_number: c_int,
            func: Option<spa_source_signal_func_t>,
            data: *mut c_void,
        ) -> *mut spa_source,
    >,
    pub destroy_source: Option<extern "C" fn(object: *mut c_void, source: *mut spa_source)>,
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

#[repr(C)]
pub struct spa_pod {
    pub size: u32,
    pub r#type: u32,
}
impl spa_pod {
    #[inline(always)]
    pub const fn total_size(&self) -> usize {
        core::mem::size_of::<Self>() + self.size as usize
    }
}

#[repr(C)]
pub struct spa_pod_bool {
    pub pod: spa_pod,
    pub value: i32,
    pub _padding: i32,
}
impl spa_pod_bool {
    #[inline(always)]
    pub const fn as_bool(&self) -> bool {
        self.value != 0
    }
}

#[repr(C)]
pub struct spa_pod_id {
    pub pod: spa_pod,
    pub value: u32,
    pub _padding: i32,
}

#[repr(C)]
pub struct spa_pod_int {
    pub pod: spa_pod,
    pub value: i32,
    pub _padding: i32,
}

#[repr(C)]
pub struct spa_pod_long {
    pub pod: spa_pod,
    pub value: i64,
}

#[repr(C)]
pub struct spa_pod_float {
    pub pod: spa_pod,
    pub value: c_float,
    pub _padding: i32,
}

#[repr(C)]
pub struct spa_pod_double {
    pub pod: spa_pod,
    pub value: c_double,
}

#[repr(C)]
pub struct spa_pod_string {
    pub pod: spa_pod,
    pub value: [u8; 0],
}

#[repr(C)]
pub struct spa_pod_bytes {
    pub pod: spa_pod,
    pub value: [u8; 0],
}

#[repr(C)]
pub struct spa_pod_rectangle {
    pub pod: spa_pod,
    pub value: spa_rectangle,
}

#[repr(C)]
pub struct spa_pod_fraction {
    pub pod: spa_pod,
    pub value: spa_fraction,
}

#[repr(C)]
pub struct spa_pod_bitmap {
    pub pod: spa_pod,
    pub values: [u8; 0],
}

#[repr(C)]
pub struct spa_pod_array_body {
    pub child: spa_pod,
    pub values: [u8; 0],
}

#[repr(C)]
pub struct spa_pod_array {
    pub pod: spa_pod,
    pub body: spa_pod_array_body,
}

pub type spa_choice_type = u32;
#[allow(non_upper_case_globals)]
pub const SPA_CHOICE_None: spa_choice_type = 0;
#[allow(non_upper_case_globals)]
pub const SPA_CHOICE_Range: spa_choice_type = 1;
#[allow(non_upper_case_globals)]
pub const SPA_CHOICE_Step: spa_choice_type = 2;
#[allow(non_upper_case_globals)]
pub const SPA_CHOICE_Enum: spa_choice_type = 3;
#[allow(non_upper_case_globals)]
pub const SPA_CHOICE_Flags: spa_choice_type = 4;

#[repr(C)]
pub struct spa_pod_choice_body {
    pub r#type: u32,
    pub flags: u32,
    pub child: spa_pod,
    pub values: [u8; 0],
}

#[repr(C)]
pub struct spa_pod_choice {
    pub pod: spa_pod,
    pub body: spa_pod_choice_body,
}
impl spa_pod_choice {
    #[inline(always)]
    pub const fn element_pod(&self) -> &spa_pod {
        &self.body.child
    }
}

#[repr(C)]
pub struct spa_pod_struct {
    pub pod: spa_pod,
    pub values: [u8; 0],
}

#[repr(C)]
pub struct spa_pod_object_body {
    pub r#type: u32,
    pub id: u32,
    pub props: [u8; 0],
}

#[repr(C)]
pub struct spa_pod_object {
    pub pod: spa_pod,
    pub body: spa_pod_object_body,
}

#[repr(C)]
pub struct spa_pod_pointer_body {
    pub r#type: u32,
    pub _padding: u32,
    pub value: *const c_void,
}

#[repr(C)]
pub struct spa_pod_pointer {
    pub pod: spa_pod,
    pub body: spa_pod_pointer_body,
}

#[repr(C)]
pub struct spa_pod_fd {
    pub pod: spa_pod,
    pub value: i64,
}

#[repr(C)]
pub struct spa_pod_prop {
    pub key: u32,
    pub flags: u32,
    pub value: spa_pod,
}
impl spa_pod_prop {
    #[inline(always)]
    pub const fn total_size(&self) -> usize {
        core::mem::size_of::<Self>() + self.value.size as usize
    }
}

pub const SPA_POD_PROP_FLAG_READONLY: u32 = 1 << 0;
pub const SPA_POD_PROP_FLAG_HARDWARE: u32 = 1 << 1;
pub const SPA_POD_PROP_FLAG_HINT_DICT: u32 = 1 << 2;
pub const SPA_POD_PROP_FLAG_MANDATORY: u32 = 1 << 3;
pub const SPA_POD_PROP_FLAG_DONT_FIXATE: u32 = 1 << 4;

#[repr(C)]
pub struct spa_pod_control {
    pub offset: u32,
    pub r#type: u32,
    pub value: spa_pod,
    pub value_contents: [u8; 0],
}

#[repr(C)]
pub struct spa_pod_sequence_body {
    pub unit: u32,
    pub pad: u32,
    pub controls: [u8; 0],
}

#[repr(C)]
pub struct spa_pod_sequence {
    pub pod: spa_pod,
    pub body: spa_pod_sequence_body,
}

pub type spa_param_type = c_int;
#[allow(non_upper_case_globals)]
pub const SPA_PARAM_Invalid: spa_param_type = 0;
#[allow(non_upper_case_globals)]
pub const SPA_PARAM_PropInfo: spa_param_type = 1;
#[allow(non_upper_case_globals)]
pub const SPA_PARAM_Props: spa_param_type = 2;
#[allow(non_upper_case_globals)]
pub const SPA_PARAM_EnumFormat: spa_param_type = 3;
#[allow(non_upper_case_globals)]
pub const SPA_PARAM_Format: spa_param_type = 4;
#[allow(non_upper_case_globals)]
pub const SPA_PARAM_Buffers: spa_param_type = 5;
#[allow(non_upper_case_globals)]
pub const SPA_PARAM_Meta: spa_param_type = 6;
#[allow(non_upper_case_globals)]
pub const SPA_PARAM_IO: spa_param_type = 7;
#[allow(non_upper_case_globals)]
pub const SPA_PARAM_EnumProfile: spa_param_type = 8;
#[allow(non_upper_case_globals)]
pub const SPA_PARAM_Profile: spa_param_type = 9;
#[allow(non_upper_case_globals)]
pub const SPA_PARAM_EnumPortConfig: spa_param_type = 10;
#[allow(non_upper_case_globals)]
pub const SPA_PARAM_PortConfig: spa_param_type = 11;
#[allow(non_upper_case_globals)]
pub const SPA_PARAM_EnumRoute: spa_param_type = 12;
#[allow(non_upper_case_globals)]
pub const SPA_PARAM_Route: spa_param_type = 13;
#[allow(non_upper_case_globals)]
pub const SPA_PARAM_Control: spa_param_type = 14;
#[allow(non_upper_case_globals)]
pub const SPA_PARAM_Latency: spa_param_type = 15;
#[allow(non_upper_case_globals)]
pub const SPA_PARAM_ProcessLatency: spa_param_type = 16;
#[allow(non_upper_case_globals)]
pub const SPA_PARAM_Tag: spa_param_type = 17;

#[repr(C)]
pub struct spa_param_info {
    pub id: u32,
    pub flags: u32,
    pub user: u32,
    pub seq: i32,
    pub padding: [u32; 4],
}

pub const SPA_PARAM_INFO_SERIAL: u32 = 1 << 09;
pub const SPA_PARAM_INFO_READ: u32 = 1 << 1;
pub const SPA_PARAM_INFO_WRITE: u32 = 1 << 2;
pub const SPA_PARAM_INFO_READWRITE: u32 = SPA_PARAM_INFO_READ | SPA_PARAM_INFO_WRITE;

pub type spa_param_bitorder = c_int;
#[allow(non_upper_case_globals)]
pub const SPA_PARAM_BITORDER_unknown: spa_param_bitorder = 0;
#[allow(non_upper_case_globals)]
pub const SPA_PARAM_BITORDER_msb: spa_param_bitorder = 1;
#[allow(non_upper_case_globals)]
pub const SPA_PARAM_BITORDER_lsb: spa_param_bitorder = 2;

pub type spa_param_availability = c_int;
#[allow(non_upper_case_globals)]
pub const SPA_PARAM_AVAILABILITY_unknown: spa_param_availability = 0;
#[allow(non_upper_case_globals)]
pub const SPA_PARAM_AVAILABILITY_no: spa_param_availability = 1;
#[allow(non_upper_case_globals)]
pub const SPA_PARAM_AVAILABILITY_yes: spa_param_availability = 2;

#[repr(C)]
pub struct spa_command_body {
    pub body: spa_pod_object_body,
}

#[repr(C)]
pub struct spa_command {
    pub pod: spa_pod,
    pub body: spa_command_body,
}

#[repr(C)]
pub struct spa_event_body {
    pub body: spa_pod_object_body,
}

#[repr(C)]
pub struct spa_event {
    pub pod: spa_pod,
    pub body: spa_event_body,
}
