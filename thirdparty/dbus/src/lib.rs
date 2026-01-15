pub mod ffi;
pub mod introspect_document;

use core::{
    ffi::CStr,
    fmt::Debug,
    marker::PhantomData,
    mem::MaybeUninit,
    ops::{Deref, DerefMut},
    ptr::NonNull,
};
use std::cell::UnsafeCell;
#[cfg(unix)]
use std::os::fd::AsRawFd;

use bitflags::bitflags;

#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MessageType {
    Invalid = ffi::DBUS_MESSAGE_TYPE_INVALID,
    MethodCall = ffi::DBUS_MESSAGE_TYPE_METHOD_CALL,
    MethodReturn = ffi::DBUS_MESSAGE_TYPE_METHOD_RETURN,
    Error = ffi::DBUS_MESSAGE_TYPE_ERROR,
    Signal = ffi::DBUS_MESSAGE_TYPE_SIGNAL,
}

pub use self::ffi::DBusBusType as BusType;

pub use self::ffi::DBUS_TYPE_ARRAY as TYPE_ARRAY;
pub use self::ffi::DBUS_TYPE_DICT_ENTRY as TYPE_DICT_ENTRY;
pub use self::ffi::DBUS_TYPE_INVALID as TYPE_INVALID;
pub use self::ffi::DBUS_TYPE_OBJECT_PATH as TYPE_OBJECT_PATH;
pub use self::ffi::DBUS_TYPE_STRING as TYPE_STRING;
pub use self::ffi::DBUS_TYPE_STRUCT as TYPE_STRUCT;
pub use self::ffi::DBUS_TYPE_UINT as TYPE_UINT;
pub use self::ffi::DBUS_TYPE_VARIANT as TYPE_VARIANT;

bitflags! {
    #[derive(Debug, Clone, Copy)]
    pub struct WatchFlags : ffi::DBusWatchFlags {
        const READABLE = ffi::DBUS_WATCH_READABLE;
        const WRITABLE = ffi::DBUS_WATCH_WRITABLE;
        const ERROR = ffi::DBUS_WATCH_ERROR;
        const HANGUP = ffi::DBUS_WATCH_HANGUP;
    }
}

#[repr(transparent)]
pub struct Error(ffi::DBusError);
unsafe impl Sync for Error {}
unsafe impl Send for Error {}
impl Drop for Error {
    #[inline]
    fn drop(&mut self) {
        unsafe {
            ffi::dbus_error_free(&mut self.0);
        }
    }
}
impl Debug for Error {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "DBusError({:?}: {:?})", self.name(), self.message())
    }
}
impl Error {
    pub fn new() -> Self {
        let mut ptr = MaybeUninit::uninit();
        unsafe {
            ffi::dbus_error_init(ptr.as_mut_ptr());
        }

        Self(unsafe { ptr.assume_init() })
    }

    #[inline]
    pub fn reset(&mut self) {
        unsafe {
            ffi::dbus_error_free(&mut self.0);
        }
    }

    #[inline]
    pub fn is_set(&self) -> bool {
        unsafe { ffi::dbus_error_is_set(&self.0) == 1 }
    }

    pub const fn name(&self) -> &CStr {
        unsafe { CStr::from_ptr(self.0.name) }
    }

    pub const fn message(&self) -> &CStr {
        unsafe { CStr::from_ptr(self.0.message) }
    }
}
impl AsRef<ffi::DBusError> for Error {
    #[inline(always)]
    fn as_ref(&self) -> &ffi::DBusError {
        &self.0
    }
}
impl AsMut<ffi::DBusError> for Error {
    #[inline(always)]
    fn as_mut(&mut self) -> &mut ffi::DBusError {
        &mut self.0
    }
}
impl Deref for Error {
    type Target = ffi::DBusError;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl DerefMut for Error {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[repr(transparent)]
pub struct Connection(NonNull<ffi::DBusConnection>);
unsafe impl Sync for Connection {}
unsafe impl Send for Connection {}
impl Drop for Connection {
    #[inline]
    fn drop(&mut self) {
        unsafe {
            ffi::dbus_connection_unref(self.0.as_ptr());
        }
    }
}
impl Clone for Connection {
    #[inline]
    fn clone(&self) -> Self {
        Self(
            NonNull::new(unsafe { ffi::dbus_connection_ref(self.0.as_ptr()) })
                .expect("dbus_connection_ref failed"),
        )
    }
}
impl Connection {
    pub const unsafe fn from_ptr(p: NonNull<ffi::DBusConnection>) -> Self {
        Self(p)
    }

    pub const fn as_ptr(&self) -> *mut ffi::DBusConnection {
        self.0.as_ptr()
    }

    pub unsafe fn clone_unchecked(&self) -> Self {
        Self(unsafe { NonNull::new_unchecked(ffi::dbus_connection_ref(self.0.as_ptr())) })
    }

    pub fn connect_bus(ty: BusType) -> Result<Self, Error> {
        let mut e = Error::new();
        let r = unsafe { ffi::dbus_bus_get(ty, e.as_mut()) };

        match NonNull::new(r) {
            Some(r) => Ok(unsafe { Self::from_ptr(r) }),
            None => Err(e),
        }
    }

    pub fn unique_name(&self) -> Option<&CStr> {
        let p = unsafe { ffi::dbus_bus_get_unique_name(self.0.as_ptr()) };
        if p.is_null() {
            None
        } else {
            Some(unsafe { CStr::from_ptr(p) })
        }
    }

    pub fn send(&self, message: &mut Message) -> Option<()> {
        let r = unsafe {
            ffi::dbus_connection_send(self.0.as_ptr(), message.0.as_ptr(), core::ptr::null_mut())
        };
        if r != 0 { Some(()) } else { None }
    }

    pub fn send_with_serial(&self, message: &mut Message) -> Option<u32> {
        let mut serial = MaybeUninit::uninit();
        let r = unsafe {
            ffi::dbus_connection_send(self.0.as_ptr(), message.0.as_ptr(), serial.as_mut_ptr())
        };
        if r != 0 {
            Some(unsafe { serial.assume_init() })
        } else {
            None
        }
    }

    pub fn send_with_reply(
        &self,
        message: &mut Message,
        timeout_milliseconds: Option<core::ffi::c_int>,
    ) -> Option<PendingCall> {
        let mut pc = core::mem::MaybeUninit::uninit();
        let r = unsafe {
            ffi::dbus_connection_send_with_reply(
                self.0.as_ptr(),
                message.0.as_ptr(),
                pc.as_mut_ptr(),
                timeout_milliseconds.unwrap_or(-1),
            )
        };

        if r == 0 {
            None
        } else {
            Some(PendingCall(NonNull::new(unsafe { pc.assume_init() })?))
        }
    }

    #[inline]
    pub fn dispatch_status(&self) -> core::ffi::c_int {
        unsafe { ffi::dbus_connection_get_dispatch_status(self.0.as_ptr()) }
    }

    #[inline]
    pub fn dispatch(&mut self) -> core::ffi::c_int {
        unsafe { ffi::dbus_connection_dispatch(self.0.as_ptr()) }
    }

    #[inline]
    pub fn pop_message(&self) -> Option<Message> {
        NonNull::new(unsafe { ffi::dbus_connection_pop_message(self.0.as_ptr()) }).map(Message)
    }

    #[inline]
    pub fn read_write(&mut self, timeout: Option<core::ffi::c_int>) -> bool {
        unsafe { ffi::dbus_connection_read_write(self.0.as_ptr(), timeout.unwrap_or(-1)) != 0 }
    }

    pub unsafe fn set_watch_functions_raw(
        &self,
        add_function: ffi::DBusAddWatchFunction,
        remove_function: ffi::DBusRemoveWatchFunction,
        toggled_function: ffi::DBusWatchToggledFunction,
        data: *mut core::ffi::c_void,
        free_data_function: ffi::DBusFreeFunction,
    ) -> bool {
        unsafe {
            ffi::dbus_connection_set_watch_functions(
                self.0.as_ptr(),
                add_function,
                remove_function,
                toggled_function,
                data,
                free_data_function,
            ) != 0
        }
    }

    pub fn set_watch_functions<WF: WatchFunction>(&self, callback_object: Box<WF>) -> bool {
        extern "C" fn add<WF: WatchFunction>(
            watch: *mut ffi::DBusWatch,
            data: *mut core::ffi::c_void,
        ) -> ffi::dbus_bool_t {
            if WF::add(unsafe { &mut *(data as *mut _) }, unsafe {
                &mut *(watch as *mut _)
            }) {
                1
            } else {
                0
            }
        }
        extern "C" fn remove<WF: WatchFunction>(
            watch: *mut ffi::DBusWatch,
            data: *mut core::ffi::c_void,
        ) {
            WF::remove(unsafe { &mut *(data as *mut _) }, unsafe {
                &mut *(watch as *mut _)
            })
        }
        extern "C" fn toggled<WF: WatchFunction>(
            watch: *mut ffi::DBusWatch,
            data: *mut core::ffi::c_void,
        ) {
            WF::toggled(unsafe { &mut *(data as *mut _) }, unsafe {
                &mut *(watch as *mut _)
            })
        }
        extern "C" fn free_data<WF: WatchFunction>(data: *mut core::ffi::c_void) {
            unsafe {
                drop(Box::from_raw(data as *mut WF));
            }
        }

        unsafe {
            self.set_watch_functions_raw(
                Some(add::<WF>),
                Some(remove::<WF>),
                Some(toggled::<WF>),
                Box::into_raw(callback_object) as _,
                Some(free_data::<WF>),
            )
        }
    }

    pub unsafe fn try_register_object_path(
        &mut self,
        path: &CStr,
        vtable: &ffi::DBusObjectPathVTable,
        user_data: *mut core::ffi::c_void,
        error: &mut Error,
    ) -> bool {
        unsafe {
            ffi::dbus_connection_try_register_object_path(
                self.0.as_ptr(),
                path.as_ptr(),
                vtable as *const _,
                user_data,
                &mut error.0 as *mut _,
            ) != 0
        }
    }

    pub fn unregister_object_path(&mut self, path: &CStr) -> bool {
        unsafe { ffi::dbus_connection_unregister_object_path(self.0.as_ptr(), path.as_ptr()) != 0 }
    }
}

pub trait WatchFunction {
    fn add(&mut self, watch: &mut WatchRef) -> bool;
    fn remove(&mut self, watch: &mut WatchRef);
    fn toggled(&mut self, watch: &mut WatchRef);
}

const fn opt_cstr_ptr(a: Option<&CStr>) -> *const core::ffi::c_char {
    match a {
        Some(x) => x.as_ptr(),
        None => core::ptr::null(),
    }
}

#[repr(transparent)]
pub struct Message(NonNull<ffi::DBusMessage>);
unsafe impl Sync for Message {}
unsafe impl Send for Message {}
impl Drop for Message {
    fn drop(&mut self) {
        unsafe {
            ffi::dbus_message_unref(self.0.as_ptr());
        }
    }
}
impl Clone for Message {
    #[inline]
    fn clone(&self) -> Self {
        Self(unsafe { NonNull::new_unchecked(ffi::dbus_message_ref(self.0.as_ptr())) })
    }
}
impl Message {
    #[inline]
    pub fn new_method_call(
        destination: Option<&CStr>,
        path: &CStr,
        iface: Option<&CStr>,
        method: &CStr,
    ) -> Option<Self> {
        NonNull::new(unsafe {
            ffi::dbus_message_new_method_call(
                opt_cstr_ptr(destination),
                path.as_ptr(),
                opt_cstr_ptr(iface),
                method.as_ptr(),
            )
        })
        .map(Self)
    }

    #[inline]
    pub fn new_method_return(method_call: &Self) -> Option<Self> {
        NonNull::new(unsafe { ffi::dbus_message_new_method_return(method_call.0.as_ptr()) })
            .map(Self)
    }

    #[inline]
    pub fn try_get_error(&self) -> Option<Error> {
        let mut e = Error::new();
        if self.set_error_to(&mut e) {
            Some(e)
        } else {
            None
        }
    }

    #[inline(always)]
    pub fn set_error_to(&self, sink: &mut Error) -> bool {
        unsafe { ffi::dbus_set_error_from_message(&mut sink.0, self.0.as_ptr()) != 0 }
    }

    #[inline(always)]
    pub fn r#type(&self) -> MessageType {
        unsafe { core::mem::transmute(ffi::dbus_message_get_type(self.0.as_ptr())) }
    }

    #[inline]
    pub fn path(&self) -> Option<&CStr> {
        let p = unsafe { ffi::dbus_message_get_path(self.0.as_ptr()) };
        if p.is_null() {
            None
        } else {
            Some(unsafe { CStr::from_ptr(p) })
        }
    }

    #[inline]
    pub fn interface(&self) -> Option<&CStr> {
        let p = unsafe { ffi::dbus_message_get_interface(self.0.as_ptr()) };
        if p.is_null() {
            None
        } else {
            Some(unsafe { CStr::from_ptr(p) })
        }
    }

    #[inline]
    pub fn member(&self) -> Option<&CStr> {
        let p = unsafe { ffi::dbus_message_get_member(self.0.as_ptr()) };
        if p.is_null() {
            None
        } else {
            Some(unsafe { CStr::from_ptr(p) })
        }
    }

    #[inline]
    pub fn signature(&self) -> &CStr {
        unsafe { CStr::from_ptr(ffi::dbus_message_get_signature(self.0.as_ptr())) }
    }

    #[inline]
    pub fn serial(&self) -> u32 {
        unsafe { ffi::dbus_message_get_serial(self.0.as_ptr()) }
    }

    #[inline]
    pub fn reply_serial(&self) -> u32 {
        unsafe { ffi::dbus_message_get_reply_serial(self.0.as_ptr()) }
    }

    #[inline]
    pub fn iter<'m>(&'m self) -> MessageIter<'m> {
        let mut iter = MaybeUninit::uninit();
        unsafe {
            ffi::dbus_message_iter_init(self.0.as_ptr(), iter.as_mut_ptr());
        }

        MessageIter(UnsafeCell::new(unsafe { iter.assume_init() }), PhantomData)
    }

    #[inline]
    pub fn iter_append<'m>(&'m mut self) -> MessageIterAppend<'m> {
        let mut iter = MaybeUninit::uninit();
        unsafe {
            ffi::dbus_message_iter_init_append(self.0.as_ptr(), iter.as_mut_ptr());
        }

        MessageIterAppend(unsafe { iter.assume_init() }, PhantomData)
    }
}

#[repr(transparent)]
pub struct MessageIter<'m>(UnsafeCell<ffi::DBusMessageIter>, PhantomData<&'m Message>);
unsafe impl Sync for MessageIter<'_> {}
unsafe impl Send for MessageIter<'_> {}
impl MessageIter<'_> {
    pub fn signature(&self) -> Option<OwnedStr> {
        NonNull::new(unsafe { ffi::dbus_message_iter_get_signature(self.0.get()) }).map(OwnedStr)
    }

    pub fn recurse(&mut self) -> Self {
        let mut subiter = MaybeUninit::uninit();
        unsafe {
            ffi::dbus_message_iter_recurse(self.0.get_mut(), subiter.as_mut_ptr());
        }

        MessageIter(
            UnsafeCell::new(unsafe { subiter.assume_init() }),
            PhantomData,
        )
    }

    #[inline(always)]
    pub fn has_next(&self) -> bool {
        unsafe { ffi::dbus_message_iter_has_next(self.0.get()) != 0 }
    }

    #[inline(always)]
    pub fn next(&mut self) -> bool {
        unsafe { ffi::dbus_message_iter_next(self.0.get_mut()) != 0 }
    }

    #[inline(always)]
    pub fn arg_type(&self) -> core::ffi::c_int {
        unsafe { ffi::dbus_message_iter_get_arg_type(self.0.get()) }
    }

    #[inline(always)]
    pub unsafe fn get_value_basic(&self, sink: *mut core::ffi::c_void) {
        unsafe { ffi::dbus_message_iter_get_basic(self.0.get(), sink) }
    }

    #[inline]
    pub unsafe fn get_i32_unchecked(&self) -> i32 {
        let mut sink = MaybeUninit::<i32>::uninit();
        unsafe {
            self.get_value_basic(sink.as_mut_ptr() as _);
            sink.assume_init()
        }
    }

    #[inline(always)]
    pub fn try_get_i32(&self) -> Result<i32, core::ffi::c_int> {
        match self.arg_type() {
            ffi::DBUS_TYPE_INT => Ok(unsafe { self.get_i32_unchecked() }),
            v => Err(v),
        }
    }

    #[inline]
    pub unsafe fn get_u32_unchecked(&self) -> u32 {
        let mut sink = MaybeUninit::<u32>::uninit();
        unsafe {
            self.get_value_basic(sink.as_mut_ptr() as _);
            sink.assume_init()
        }
    }

    #[inline(always)]
    pub fn try_get_u32(&self) -> Result<u32, core::ffi::c_int> {
        match self.arg_type() {
            TYPE_UINT => Ok(unsafe { self.get_u32_unchecked() }),
            v => Err(v),
        }
    }

    #[inline]
    pub unsafe fn get_cstr_unchecked(&self) -> &CStr {
        let mut sink = MaybeUninit::<*const core::ffi::c_char>::uninit();
        unsafe {
            self.get_value_basic(sink.as_mut_ptr() as _);
            CStr::from_ptr(sink.assume_init())
        }
    }

    #[inline(always)]
    pub fn try_get_cstr(&self) -> Result<&CStr, core::ffi::c_int> {
        match self.arg_type() {
            TYPE_STRING => Ok(unsafe { self.get_cstr_unchecked() }),
            v => Err(v),
        }
    }

    #[inline(always)]
    pub fn try_get_object_path(&self) -> Result<&CStr, core::ffi::c_int> {
        match self.arg_type() {
            TYPE_OBJECT_PATH => Ok(unsafe { self.get_cstr_unchecked() }),
            v => Err(v),
        }
    }

    #[inline]
    pub fn try_begin_iter_variant_content(&mut self) -> Result<Self, core::ffi::c_int> {
        match self.arg_type() {
            TYPE_VARIANT => Ok(self.recurse()),
            x => Err(x),
        }
    }

    #[inline]
    pub fn try_begin_iter_array_content(&mut self) -> Result<Self, core::ffi::c_int> {
        match self.arg_type() {
            TYPE_ARRAY => Ok(self.recurse()),
            x => Err(x),
        }
    }

    #[inline]
    pub fn try_begin_iter_struct_content(&mut self) -> Result<Self, core::ffi::c_int> {
        match self.arg_type() {
            TYPE_STRUCT => Ok(self.recurse()),
            x => Err(x),
        }
    }

    #[inline]
    pub fn try_begin_iter_dict_entry_content(&mut self) -> Result<Self, core::ffi::c_int> {
        match self.arg_type() {
            TYPE_DICT_ENTRY => Ok(self.recurse()),
            x => Err(x),
        }
    }
}

pub trait MessageIterAppendLike {
    fn ffi_pointer_mut(&mut self) -> *mut ffi::DBusMessageIter;

    #[inline]
    unsafe fn append_basic(
        &mut self,
        r#type: core::ffi::c_int,
        value: *const core::ffi::c_void,
    ) -> Result<(), NotEnoughMemory> {
        if unsafe {
            ffi::dbus_message_iter_append_basic(self.ffi_pointer_mut(), r#type, value) == 0
        } {
            Err(NotEnoughMemory)
        } else {
            Ok(())
        }
    }

    #[inline(always)]
    fn append_i32(&mut self, value: i32) -> Result<(), NotEnoughMemory> {
        unsafe { self.append_basic(ffi::DBUS_TYPE_INT, &value as *const _ as _) }
    }

    #[inline(always)]
    fn append_u32(&mut self, value: u32) -> Result<(), NotEnoughMemory> {
        unsafe { self.append_basic(ffi::DBUS_TYPE_UINT, &value as *const _ as _) }
    }

    #[inline(always)]
    fn append_cstr(&mut self, value: &CStr) -> Result<(), NotEnoughMemory> {
        unsafe { self.append_basic(ffi::DBUS_TYPE_STRING, &value.as_ptr() as *const _ as _) }
    }

    #[inline(always)]
    fn append_bool(&mut self, value: bool) -> Result<(), NotEnoughMemory> {
        let v1: ffi::dbus_bool_t = if value { 1 } else { 0 };
        unsafe { self.append_basic(ffi::DBUS_TYPE_BOOLEAN, &v1 as *const _ as _) }
    }

    #[inline]
    fn open_container<'p1>(
        &'p1 mut self,
        r#type: core::ffi::c_int,
        contained_signature: Option<&CStr>,
    ) -> Result<MessageIterAppendContainer<'p1, Self>, NotEnoughMemory> {
        let mut subiter = MaybeUninit::uninit();
        if unsafe {
            ffi::dbus_message_iter_open_container(
                self.ffi_pointer_mut(),
                r#type,
                contained_signature.map_or_else(core::ptr::null, CStr::as_ptr),
                subiter.as_mut_ptr(),
            ) != 0
        } {
            Ok(MessageIterAppendContainer(
                unsafe { subiter.assume_init() },
                self,
            ))
        } else {
            Err(NotEnoughMemory)
        }
    }

    #[inline(always)]
    fn open_dict_entry_container<'p1>(
        &'p1 mut self,
    ) -> Result<MessageIterAppendContainer<'p1, Self>, NotEnoughMemory> {
        self.open_container(ffi::DBUS_TYPE_DICT_ENTRY, None)
    }

    #[inline(always)]
    fn open_struct_container<'p1>(
        &'p1 mut self,
    ) -> Result<MessageIterAppendContainer<'p1, Self>, NotEnoughMemory> {
        self.open_container(ffi::DBUS_TYPE_STRUCT, None)
    }

    #[inline(always)]
    fn open_array_container<'p1>(
        &'p1 mut self,
        contained_signature: &CStr,
    ) -> Result<MessageIterAppendContainer<'p1, Self>, NotEnoughMemory> {
        self.open_container(ffi::DBUS_TYPE_ARRAY, Some(contained_signature))
    }

    #[inline(always)]
    fn open_variant_container<'p1>(
        &'p1 mut self,
        contained_signature: &CStr,
    ) -> Result<MessageIterAppendContainer<'p1, Self>, NotEnoughMemory> {
        self.open_container(ffi::DBUS_TYPE_VARIANT, Some(contained_signature))
    }

    fn append_variant_bool(&mut self, v: bool) -> Result<(), NotEnoughMemory> {
        let mut c = self.open_variant_container(c"b")?;
        if c.append_bool(v) == Err(NotEnoughMemory) {
            c.abandon();
            return Err(NotEnoughMemory);
        }
        c.close()?;

        Ok(())
    }

    fn append_variant_cstr(&mut self, v: &CStr) -> Result<(), NotEnoughMemory> {
        let mut c = self.open_variant_container(c"s")?;
        if c.append_cstr(v) == Err(NotEnoughMemory) {
            c.abandon();
            return Err(NotEnoughMemory);
        }
        c.close()?;

        Ok(())
    }
}

#[repr(transparent)]
pub struct MessageIterAppend<'m>(ffi::DBusMessageIter, PhantomData<&'m mut Message>);
unsafe impl Sync for MessageIterAppend<'_> {}
unsafe impl Send for MessageIterAppend<'_> {}
impl MessageIterAppendLike for MessageIterAppend<'_> {
    #[inline(always)]
    fn ffi_pointer_mut(&mut self) -> *mut ffi::DBusMessageIter {
        &mut self.0 as _
    }
}

pub struct MessageIterAppendContainer<'p, P: MessageIterAppendLike + ?Sized + 'p>(
    ffi::DBusMessageIter,
    &'p mut P,
);
unsafe impl<P: MessageIterAppendLike + ?Sized + Sync> Sync for MessageIterAppendContainer<'_, P> {}
unsafe impl<P: MessageIterAppendLike + ?Sized + Send> Send for MessageIterAppendContainer<'_, P> {}
impl<'p, P: MessageIterAppendLike + ?Sized + 'p> Drop for MessageIterAppendContainer<'_, P> {
    #[inline]
    fn drop(&mut self) {
        tracing::warn!(
            "Either close or abandon must be called to finalize MessageIterAppendContainer"
        );
    }
}
impl<'p, P: MessageIterAppendLike + ?Sized + 'p> MessageIterAppendContainer<'p, P> {
    #[inline]
    pub fn close(mut self) -> Result<(), NotEnoughMemory> {
        let r = unsafe {
            ffi::dbus_message_iter_close_container(self.1.ffi_pointer_mut(), &mut self.0 as *mut _)
                != 0
        };
        core::mem::forget(self);
        if r { Ok(()) } else { Err(NotEnoughMemory) }
    }

    #[inline]
    pub fn abandon(mut self) {
        unsafe {
            ffi::dbus_message_iter_abandon_container(
                self.1.ffi_pointer_mut(),
                &mut self.0 as *mut _,
            );
        }
        core::mem::forget(self);
    }
}
impl<'p, P: MessageIterAppendLike + ?Sized + 'p> MessageIterAppendLike
    for MessageIterAppendContainer<'p, P>
{
    #[inline(always)]
    fn ffi_pointer_mut(&mut self) -> *mut ffi::DBusMessageIter {
        &mut self.0 as _
    }
}

#[repr(transparent)]
pub struct PendingCall(NonNull<ffi::DBusPendingCall>);
unsafe impl Sync for PendingCall {}
unsafe impl Send for PendingCall {}
impl Drop for PendingCall {
    #[inline(always)]
    fn drop(&mut self) {
        unsafe {
            ffi::dbus_pending_call_unref(self.0.as_ptr());
        }
    }
}
impl PendingCall {
    #[inline]
    pub fn block(&mut self) {
        unsafe {
            ffi::dbus_pending_call_block(self.0.as_ptr());
        }
    }

    #[inline]
    pub fn steal_reply(&mut self) -> Option<Message> {
        NonNull::new(unsafe { ffi::dbus_pending_call_steal_reply(self.0.as_ptr()) }).map(Message)
    }
}

#[repr(transparent)]
pub struct WatchRef(ffi::DBusWatch);
unsafe impl Sync for WatchRef {}
unsafe impl Send for WatchRef {}
#[cfg(unix)]
impl AsRawFd for WatchRef {
    #[inline(always)]
    fn as_raw_fd(&self) -> std::os::unix::prelude::RawFd {
        unsafe { ffi::dbus_watch_get_unix_fd(self as *const _ as _) }
    }
}
impl WatchRef {
    #[inline(always)]
    pub fn enabled(&self) -> bool {
        unsafe { ffi::dbus_watch_get_enabled(self as *const _ as _) != 0 }
    }

    #[inline(always)]
    pub fn flags(&self) -> WatchFlags {
        WatchFlags::from_bits_retain(unsafe { ffi::dbus_watch_get_flags(self as *const _ as _) })
    }

    #[inline(always)]
    pub fn handle(&mut self, flags: WatchFlags) -> bool {
        unsafe { ffi::dbus_watch_handle(self as *mut _ as _, flags.bits()) != 0 }
    }
}

#[repr(transparent)]
pub struct OwnedStr(NonNull<core::ffi::c_char>);
unsafe impl Sync for OwnedStr {}
unsafe impl Send for OwnedStr {}
impl Drop for OwnedStr {
    #[inline(always)]
    fn drop(&mut self) {
        unsafe {
            ffi::dbus_free(self.0.as_ptr() as _);
        }
    }
}
impl core::fmt::Debug for OwnedStr {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        core::fmt::Debug::fmt(self.as_cstr(), f)
    }
}
impl OwnedStr {
    pub const fn as_cstr(&self) -> &CStr {
        unsafe { CStr::from_ptr(self.0.as_ptr()) }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NotEnoughMemory;
