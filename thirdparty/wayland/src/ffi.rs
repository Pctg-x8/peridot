/// https://doc.rust-lang.org/nomicon/ffi.html#representing-opaque-structs
#[macro_export]
macro_rules! FFIOpaqueStruct {
    ($v: vis struct $t: ident) => {
        #[repr(C)]
        $v struct $t([u8; 0], core::marker::PhantomData<*mut u8>);
    }
}

FFIOpaqueStruct!(pub struct Proxy);
FFIOpaqueStruct!(pub struct Display);
FFIOpaqueStruct!(pub struct Object);

#[repr(C)]
pub struct Message {
    /// Messsage Name
    pub name: *const core::ffi::c_char,
    /// Message signature
    pub signature: *const core::ffi::c_char,
    /// Object argument intefaces
    pub types: *const *const Interface,
}
unsafe impl Sync for Message {}
unsafe impl Send for Message {}

#[repr(C)]
pub struct Interface {
    /// Interface name
    pub name: *const core::ffi::c_char,
    /// Interface version
    pub version: core::ffi::c_int,
    /// Number of methods (requests)
    pub method_count: core::ffi::c_int,
    /// Method (request) signatures
    pub methods: *const Message,
    /// Number of events
    pub event_count: core::ffi::c_int,
    /// Event signatures
    pub events: *const Message,
}
unsafe impl Sync for Interface {}
unsafe impl Send for Interface {}

/// Fixed-point number
#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Fixed(i32);
impl Fixed {
    pub const fn from_f32_lossy(v: f32) -> Self {
        Self((v * 256.0) as _)
    }

    pub const fn to_f32(&self) -> f32 {
        self.0 as f32 / 256.0
    }
}

#[repr(C)]
pub struct Array {
    /// Array size
    pub size: usize,
    /// Allocated space
    pub alloc: usize,
    /// Array data
    pub data: *mut core::ffi::c_void,
}

#[repr(C)]
pub union Argument {
    /// int
    pub i: i32,
    /// uint
    pub u: u32,
    /// fixed
    pub f: Fixed,
    /// string
    pub s: *const core::ffi::c_char,
    /// object
    pub o: *mut Object,
    /// new_id
    pub n: u32,
    /// array
    pub a: *mut Array,
    /// fd
    pub h: i32,
}

pub const MARSHAL_FLAG_DESTROY: u32 = 1 << 0;

#[link(name = "wayland-client")]
unsafe extern "C" {
    pub fn wl_proxy_marshal_array_flags(
        proxy: *mut Proxy,
        opcode: u32,
        interface: *const Interface,
        version: u32,
        flags: u32,
        args: *mut Argument,
    ) -> *mut Proxy;
    pub fn wl_proxy_destroy(p: *mut Proxy);
    pub fn wl_proxy_add_listener(
        proxy: *mut Proxy,
        implementation: *const core::ffi::c_void,
        data: *mut core::ffi::c_void,
    ) -> core::ffi::c_int;
    pub fn wl_proxy_get_version(proxy: *const Proxy) -> u32;
    pub fn wl_proxy_get_display(proxy: *mut Proxy) -> *mut Display;

    pub fn wl_display_connect(name: *const core::ffi::c_char) -> *mut Display;
    pub fn wl_display_disconnect(name: *mut Display);
    pub fn wl_display_get_error(display: *mut Display) -> core::ffi::c_int;
    pub fn wl_display_get_protocol_error(
        display: *mut Display,
        interface: *const *mut Interface,
        id: *mut u32,
    ) -> u32;
    pub fn wl_display_flush(display: *mut Display) -> core::ffi::c_int;
    pub fn wl_display_roundtrip(display: *mut Display) -> core::ffi::c_int;
    pub fn wl_display_get_fd(display: *const Display) -> core::ffi::c_int;
    pub fn wl_display_prepare_read(display: *mut Display) -> core::ffi::c_int;
    pub fn wl_display_cancel_read(display: *mut Display);
    pub fn wl_display_dispatch_pending(display: *mut Display) -> core::ffi::c_int;
    pub fn wl_display_read_events(display: *mut Display) -> core::ffi::c_int;
}
