//! libdbus-1 ffi
#![allow(non_camel_case_types)]

use core::ffi::*;

/// https://doc.rust-lang.org/nomicon/ffi.html#representing-opaque-structs
#[repr(C)]
struct OpaqueStruct(
    [u8; 0],
    core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,
);

#[repr(C)]
pub struct DBusConnection(OpaqueStruct);
#[repr(C)]
pub struct DBusMessage(OpaqueStruct);

#[repr(C)]
pub struct DBusMessageIter {
    // Note: ちゃんとサイズ確保してあげる必要がある(使い方参照)
    _dummy1: *mut c_void,
    _dummy2: *mut c_void,
    _dummy3: u32,
    _dummy4: c_int,
    _dummy5: c_int,
    _dummy6: c_int,
    _dummy7: c_int,
    _dummy8: c_int,
    _dummy9: c_int,
    _dummy10: c_int,
    _dummy11: c_int,
    _pad1: c_int,
    _pad2: *mut c_void,
    _pad3: *mut c_void,
    _marker: core::marker::PhantomData<(core::marker::PhantomPinned, *mut u8)>,
}

#[repr(C)]
pub struct DBusPendingCall(OpaqueStruct);

#[repr(C)]
pub struct DBusError {
    pub name: *const c_char,
    pub message: *const c_char,
    _dummy: c_uint,
    _padding1: *mut c_void,
}

pub type DBusObjectPathUnregisterFunction =
    extern "C" fn(connection: *mut DBusConnection, user_data: *mut c_void);
pub type DBusObjectPathMessageFunction = extern "C" fn(
    connection: *mut DBusConnection,
    message: *mut DBusMessage,
    user_data: *mut c_void,
) -> c_int;
type DBusObjectPathInternalFunction = extern "C" fn(*mut c_void);

#[repr(C)]
pub struct DBusObjectPathVTable {
    pub unregister_function: DBusObjectPathUnregisterFunction,
    pub message_function: DBusObjectPathMessageFunction,
    dbus_internal_pad1: DBusObjectPathInternalFunction,
    dbus_internal_pad2: DBusObjectPathInternalFunction,
    dbus_internal_pad3: DBusObjectPathInternalFunction,
    dbus_internal_pad4: DBusObjectPathInternalFunction,
}

#[repr(C)]
pub struct DBusWatch(OpaqueStruct);

#[repr(C)]
pub enum DBusBusType {
    Session,
}

pub type dbus_bool_t = u32;

pub type DBusAddWatchFunction =
    Option<extern "C" fn(watch: *mut DBusWatch, data: *mut c_void) -> dbus_bool_t>;
pub type DBusRemoveWatchFunction = Option<extern "C" fn(watch: *mut DBusWatch, data: *mut c_void)>;
pub type DBusWatchToggledFunction = Option<extern "C" fn(watch: *mut DBusWatch, data: *mut c_void)>;
pub type DBusFreeFunction = Option<extern "C" fn(memory: *mut c_void)>;

pub const DBUS_MESSAGE_TYPE_INVALID: c_int = 0;
pub const DBUS_MESSAGE_TYPE_METHOD_CALL: c_int = 1;
pub const DBUS_MESSAGE_TYPE_METHOD_RETURN: c_int = 2;
pub const DBUS_MESSAGE_TYPE_ERROR: c_int = 3;
pub const DBUS_MESSAGE_TYPE_SIGNAL: c_int = 4;

pub const DBUS_TYPE_INVALID: c_int = 0;
pub const DBUS_TYPE_BOOLEAN: c_int = b'b' as _;
pub const DBUS_TYPE_STRING: c_int = b's' as _;
pub const DBUS_TYPE_OBJECT_PATH: c_int = b'o' as _;
pub const DBUS_TYPE_ARRAY: c_int = b'a' as _;
pub const DBUS_TYPE_VARIANT: c_int = b'v' as _;
pub const DBUS_TYPE_DICT_ENTRY: c_int = b'e' as _;
pub const DBUS_TYPE_STRUCT: c_int = b'r' as _;
pub const DBUS_TYPE_UINT: c_int = b'u' as _;
pub const DBUS_TYPE_INT: c_int = b'i' as _;

pub type DBusWatchFlags = c_uint;
pub const DBUS_WATCH_READABLE: DBusWatchFlags = 1 << 0;
pub const DBUS_WATCH_WRITABLE: DBusWatchFlags = 1 << 1;
pub const DBUS_WATCH_ERROR: DBusWatchFlags = 1 << 2;
pub const DBUS_WATCH_HANGUP: DBusWatchFlags = 1 << 3;

#[link(name = "dbus-1")]
unsafe extern "C" {
    pub unsafe fn dbus_connection_ref(connection: *mut DBusConnection) -> *mut DBusConnection;
    pub unsafe fn dbus_connection_unref(connection: *mut DBusConnection);
    pub unsafe fn dbus_connection_send(
        connection: *mut DBusConnection,
        message: *mut DBusMessage,
        serial: *mut u32,
    ) -> dbus_bool_t;
    pub unsafe fn dbus_connection_send_with_reply(
        connection: *mut DBusConnection,
        message: *mut DBusMessage,
        pending_return: *mut *mut DBusPendingCall,
        timeout_milliseconds: c_int,
    ) -> u32;
    pub unsafe fn dbus_connection_get_dispatch_status(connection: *mut DBusConnection) -> c_int;
    pub unsafe fn dbus_connection_pop_message(connection: *mut DBusConnection) -> *mut DBusMessage;
    pub unsafe fn dbus_connection_dispatch(connection: *mut DBusConnection) -> c_int;
    pub unsafe fn dbus_connection_read_write(
        connection: *mut DBusConnection,
        timeout_seconds: c_int,
    ) -> dbus_bool_t;
    pub unsafe fn dbus_connection_set_watch_functions(
        connection: *mut DBusConnection,
        add_function: DBusAddWatchFunction,
        remove_function: DBusRemoveWatchFunction,
        toggled_function: DBusWatchToggledFunction,
        data: *mut c_void,
        free_data_function: DBusFreeFunction,
    ) -> dbus_bool_t;
    pub unsafe fn dbus_connection_try_register_object_path(
        connection: *mut DBusConnection,
        path: *const c_char,
        vtable: *const DBusObjectPathVTable,
        user_data: *mut c_void,
        error: *mut DBusError,
    ) -> dbus_bool_t;
    pub unsafe fn dbus_connection_unregister_object_path(
        connection: *mut DBusConnection,
        path: *const c_char,
    ) -> dbus_bool_t;

    pub unsafe fn dbus_bus_get(r#type: DBusBusType, error: *mut DBusError) -> *mut DBusConnection;
    pub unsafe fn dbus_bus_get_unique_name(connection: *mut DBusConnection) -> *const c_char;

    pub unsafe fn dbus_message_new_method_call(
        destination: *const c_char,
        path: *const c_char,
        iface: *const c_char,
        method: *const c_char,
    ) -> *mut DBusMessage;
    pub unsafe fn dbus_message_new_method_return(message: *mut DBusMessage) -> *mut DBusMessage;
    pub unsafe fn dbus_message_ref(message: *mut DBusMessage) -> *mut DBusMessage;
    pub unsafe fn dbus_message_unref(message: *mut DBusMessage);
    pub unsafe fn dbus_message_get_type(message: *mut DBusMessage) -> c_int;
    pub unsafe fn dbus_message_iter_init(
        message: *mut DBusMessage,
        iter: *mut DBusMessageIter,
    ) -> u32;
    pub unsafe fn dbus_message_iter_get_signature(iter: *mut DBusMessageIter) -> *mut c_char;
    pub unsafe fn dbus_message_iter_has_next(iter: *mut DBusMessageIter) -> dbus_bool_t;
    pub unsafe fn dbus_message_iter_next(iter: *mut DBusMessageIter) -> dbus_bool_t;
    pub unsafe fn dbus_message_iter_get_arg_type(iter: *mut DBusMessageIter) -> c_int;
    pub unsafe fn dbus_message_iter_get_basic(iter: *mut DBusMessageIter, value: *mut c_void);
    pub unsafe fn dbus_message_iter_recurse(iter: *mut DBusMessageIter, sub: *mut DBusMessageIter);
    pub unsafe fn dbus_message_iter_init_append(
        message: *mut DBusMessage,
        iter: *mut DBusMessageIter,
    );
    pub unsafe fn dbus_message_iter_append_basic(
        iter: *mut DBusMessageIter,
        r#type: c_int,
        value: *const c_void,
    ) -> dbus_bool_t;
    pub unsafe fn dbus_message_iter_open_container(
        iter: *mut DBusMessageIter,
        r#type: c_int,
        contained_signature: *const c_char,
        sub: *mut DBusMessageIter,
    ) -> dbus_bool_t;
    pub unsafe fn dbus_message_iter_close_container(
        iter: *mut DBusMessageIter,
        sub: *mut DBusMessageIter,
    ) -> dbus_bool_t;
    pub unsafe fn dbus_message_iter_abandon_container(
        iter: *mut DBusMessageIter,
        sub: *mut DBusMessageIter,
    );
    pub unsafe fn dbus_set_error_from_message(
        error: *mut DBusError,
        message: *mut DBusMessage,
    ) -> dbus_bool_t;
    pub unsafe fn dbus_message_get_path(message: *mut DBusMessage) -> *const c_char;
    pub unsafe fn dbus_message_get_interface(message: *mut DBusMessage) -> *const c_char;
    pub unsafe fn dbus_message_get_member(message: *mut DBusMessage) -> *const c_char;
    pub unsafe fn dbus_message_get_signature(message: *mut DBusMessage) -> *const c_char;
    pub unsafe fn dbus_message_get_serial(message: *mut DBusMessage) -> u32;
    pub unsafe fn dbus_message_get_reply_serial(message: *mut DBusMessage) -> u32;

    pub unsafe fn dbus_pending_call_unref(pending: *mut DBusPendingCall);
    pub unsafe fn dbus_pending_call_block(pending: *mut DBusPendingCall);
    pub unsafe fn dbus_pending_call_steal_reply(pending: *mut DBusPendingCall) -> *mut DBusMessage;

    pub unsafe fn dbus_watch_get_unix_fd(watch: *const DBusWatch) -> c_int;
    pub unsafe fn dbus_watch_get_flags(watch: *const DBusWatch) -> DBusWatchFlags;
    pub unsafe fn dbus_watch_get_enabled(watch: *const DBusWatch) -> dbus_bool_t;
    pub unsafe fn dbus_watch_handle(watch: *mut DBusWatch, flags: DBusWatchFlags) -> dbus_bool_t;

    pub unsafe fn dbus_error_init(error: *mut DBusError);
    pub unsafe fn dbus_error_free(error: *mut DBusError);
    pub unsafe fn dbus_error_is_set(error: *const DBusError) -> dbus_bool_t;

    pub unsafe fn dbus_free(memory: *mut c_void);
}
