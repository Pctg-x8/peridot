use bitflags::bitflags;

use crate::raw::{
    PW_VERSION_CORE_EVENTS, PW_VERSION_REGISTRY_EVENTS, pw_core_events, pw_core_info,
    pw_registry_events, spa_dict,
};
use core::{
    cell::UnsafeCell,
    ffi::*,
    mem::MaybeUninit,
    ops::{Deref, DerefMut},
    pin::Pin,
    ptr::NonNull,
};

pub mod raw;
pub mod spa;

#[inline(always)]
pub fn init() {
    unsafe { raw::pw_init(core::ptr::null_mut(), core::ptr::null_mut()) }
}

#[inline(always)]
pub fn deinit() {
    unsafe { raw::pw_deinit() }
}

pub trait PipewireDrop {
    unsafe fn pipewire_drop(&mut self);
}

pub unsafe trait PipewireProxy {
    const TYPE_NAME: &CStr;
}

#[repr(transparent)]
pub struct Owned<T: PipewireDrop>(NonNull<T>);
unsafe impl<T: PipewireDrop + Sync> Sync for Owned<T> {}
unsafe impl<T: PipewireDrop + Send> Send for Owned<T> {}
impl<T: PipewireDrop> Drop for Owned<T> {
    #[inline(always)]
    fn drop(&mut self) {
        unsafe { T::pipewire_drop(self.0.as_mut()) }
    }
}
impl<T: PipewireDrop> Deref for Owned<T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref() }
    }
}
impl<T: PipewireDrop> DerefMut for Owned<T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.as_mut() }
    }
}
impl<T: PipewireDrop> Owned<T> {
    fn from_raw(p: *mut T) -> Option<Self> {
        NonNull::new(p).map(Self)
    }

    #[inline(always)]
    pub const fn as_ptr(&self) -> *mut T {
        self.0.as_ptr()
    }

    pub const fn leak(self) -> *mut T {
        let p = self.0.as_ptr();
        core::mem::forget(self);
        p
    }
}

#[repr(transparent)]
pub struct OwnedProxy<T: PipewireProxy>(NonNull<T>);
unsafe impl<T: PipewireProxy + Sync> Sync for OwnedProxy<T> {}
unsafe impl<T: PipewireProxy + Send> Send for OwnedProxy<T> {}
impl<T: PipewireProxy> Drop for OwnedProxy<T> {
    #[inline(always)]
    fn drop(&mut self) {
        unsafe { raw::pw_proxy_unref(self.0.as_ptr().cast()) }
    }
}
impl<T: PipewireProxy> Clone for OwnedProxy<T> {
    #[inline(always)]
    fn clone(&self) -> Self {
        unsafe { raw::pw_proxy_ref(self.0.as_ptr().cast()) };
        Self(self.0)
    }
}
impl<T: PipewireProxy> Deref for OwnedProxy<T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref() }
    }
}
impl<T: PipewireProxy> DerefMut for OwnedProxy<T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.as_mut() }
    }
}
impl<T: PipewireProxy> OwnedProxy<T> {
    #[inline(always)]
    fn from_raw(p: *mut T) -> Option<Self> {
        NonNull::new(p).map(Self)
    }

    #[inline(always)]
    const fn from_nonnull(p: NonNull<T>) -> Self {
        Self(p)
    }

    #[inline(always)]
    pub const fn as_ptr(&self) -> *mut T {
        self.0.as_ptr()
    }

    pub const fn leak(self) -> *mut T {
        let p = self.0.as_ptr();
        core::mem::forget(self);
        p
    }
}

pub trait AsLoop {
    fn as_loop(&self) -> *mut raw::pw_loop;
}
impl<'a, T: 'a + ?Sized> AsLoop for &'a T
where
    T: AsLoop,
{
    #[inline(always)]
    fn as_loop(&self) -> *mut raw::pw_loop {
        T::as_loop(*self)
    }
}
impl<T: PipewireDrop> AsLoop for Owned<T>
where
    T: AsLoop,
{
    #[inline(always)]
    fn as_loop(&self) -> *mut raw::pw_loop {
        T::as_loop(&**self)
    }
}

#[repr(transparent)]
pub struct Context(UnsafeCell<raw::pw_context>);
unsafe impl Sync for Context {}
unsafe impl Send for Context {}
impl PipewireDrop for Context {
    #[inline(always)]
    unsafe fn pipewire_drop(&mut self) {
        unsafe { raw::pw_context_destroy(self.0.get_mut()) }
    }
}
impl Context {
    #[inline]
    pub fn new(
        main_loop: impl AsLoop,
        props: Option<&mut Properties>,
        user_data_size: usize,
    ) -> std::io::Result<Owned<Self>> {
        let r = unsafe {
            raw::pw_context_new(
                main_loop.as_loop(),
                props.map_or_else(core::ptr::null_mut, |x| x as *mut _ as _),
                user_data_size,
            )
        };

        Owned::from_raw(r.cast()).ok_or_else(std::io::Error::last_os_error)
    }

    #[inline]
    pub fn connect(
        &mut self,
        properties: Option<&mut Properties>,
        user_data_size: usize,
    ) -> std::io::Result<Owned<Core>> {
        let r = unsafe {
            raw::pw_context_connect(
                self.0.get_mut(),
                properties.map_or_else(core::ptr::null_mut, |x| x as *mut _ as _),
                user_data_size,
            )
        };

        Owned::from_raw(r.cast()).ok_or_else(std::io::Error::last_os_error)
    }
}

#[repr(transparent)]
pub struct Core(UnsafeCell<raw::pw_core>);
unsafe impl Sync for Core {}
unsafe impl Send for Core {}
impl PipewireDrop for Core {
    #[inline(always)]
    unsafe fn pipewire_drop(&mut self) {
        let r = unsafe { raw::pw_core_disconnect(self.0.get_mut()) };
        if r < 0 {
            eprintln!(
                "pw_core_disconnect error: {:?}",
                std::io::Error::from_raw_os_error(-r)
            );
        }
    }
}
impl Core {
    #[inline(always)]
    pub fn add_listener<L: CoreEventListener + 'static>(
        &mut self,
        mut hook: Pin<&mut MaybeUninit<raw::spa_hook>>,
        listener: &mut L,
    ) -> std::io::Result<()> {
        let r = unsafe {
            raw::pw_core::add_listener(
                self.0.get_mut(),
                hook.as_mut_ptr(),
                core_event_fptbl::<L>(),
                listener as *mut _ as _,
            )
        };
        if r < 0 {
            Err(std::io::Error::from_raw_os_error(-r))
        } else {
            Ok(())
        }
    }

    #[inline(always)]
    pub fn sync(&self) -> std::io::Result<i32> {
        let r = unsafe { raw::pw_core::sync(self.0.get(), 0, 0) };
        if r < 0 {
            Err(std::io::Error::from_raw_os_error(-r))
        } else {
            Ok(r)
        }
    }

    #[inline(always)]
    pub fn get_registry(
        &self,
        version: u32,
        user_data_size: usize,
    ) -> std::io::Result<NonNull<Registry>> {
        let r = unsafe { raw::pw_core::get_registry(self.0.get(), version, user_data_size) };
        NonNull::new(r.cast()).ok_or_else(std::io::Error::last_os_error)
    }
}

#[repr(transparent)]
pub struct Registry(UnsafeCell<raw::pw_registry>);
unsafe impl Sync for Registry {}
unsafe impl Send for Registry {}
impl Registry {
    #[inline(always)]
    pub fn add_listener<L: RegistryEventListener + 'static>(
        &mut self,
        mut hook: Pin<&mut MaybeUninit<raw::spa_hook>>,
        listener: &mut L,
    ) -> std::io::Result<()> {
        let r = unsafe {
            raw::pw_registry::add_listener(
                self.0.get_mut(),
                hook.as_mut_ptr(),
                registry_event_fptbl::<L>(),
                listener as *mut _ as _,
            )
        };
        if r < 0 {
            Err(std::io::Error::from_raw_os_error(-r))
        } else {
            Ok(())
        }
    }

    #[inline(always)]
    pub unsafe fn bind_raw(
        &self,
        id: u32,
        r#type: &CStr,
        version: u32,
        user_data_size: usize,
    ) -> std::io::Result<NonNull<c_void>> {
        let r = unsafe {
            raw::pw_registry::bind(self.0.get(), id, r#type.as_ptr(), version, user_data_size)
        };
        NonNull::new(r).ok_or_else(std::io::Error::last_os_error)
    }

    #[inline(always)]
    pub fn bind<T: PipewireProxy>(
        &self,
        id: u32,
        version: u32,
        user_data_size: usize,
    ) -> std::io::Result<OwnedProxy<T>> {
        Ok(OwnedProxy::from_nonnull(unsafe {
            self.bind_raw(id, T::TYPE_NAME, version, user_data_size)?
                .cast()
        }))
    }
}

#[repr(transparent)]
pub struct Stream(UnsafeCell<raw::pw_stream>);
unsafe impl Sync for Stream {}
unsafe impl Send for Stream {}
impl PipewireDrop for Stream {
    #[inline(always)]
    unsafe fn pipewire_drop(&mut self) {
        unsafe { raw::pw_stream_destroy(self.0.get_mut()) }
    }
}
impl Stream {
    #[inline]
    pub fn new(
        core: &Core,
        name: &CStr,
        props: Option<Owned<Properties>>,
    ) -> std::io::Result<Owned<Self>> {
        let r = unsafe {
            raw::pw_stream_new(
                core.0.get(),
                name.as_ptr(),
                props.map_or_else(core::ptr::null_mut, |x| x.leak()) as _,
            )
        };
        Owned::from_raw(r.cast()).ok_or_else(std::io::Error::last_os_error)
    }

    #[inline]
    pub fn add_listener<L: StreamEventListener + 'static>(
        &mut self,
        mut hook: Pin<&mut MaybeUninit<raw::spa_hook>>,
        listener: &mut L,
    ) {
        unsafe {
            raw::pw_stream_add_listener(
                self.0.get_mut(),
                hook.as_mut_ptr(),
                const { stream_event_fptbl::<L>() },
                listener as *mut _ as _,
            );
        }
    }

    #[inline]
    pub fn connect(
        &mut self,
        direction: Direction,
        flags: StreamFlags,
        params: &mut [*const raw::spa_pod],
    ) -> std::io::Result<()> {
        let r = unsafe {
            raw::pw_stream_connect(
                self.0.get_mut(),
                direction as _,
                raw::PW_ID_ANY,
                flags.bits(),
                params.as_mut_ptr(),
                params.len() as _,
            )
        };
        if r < 0 {
            Err(std::io::Error::from_raw_os_error(-r))
        } else {
            Ok(())
        }
    }

    #[inline]
    pub fn dequeue_buffer_raw<'a>(&'a mut self) -> Option<NonNull<Buffer>> {
        let p = unsafe { raw::pw_stream_dequeue_buffer(self.0.get()) };
        NonNull::new(p.cast::<Buffer>())
    }

    #[inline]
    pub fn dequeue_buffer<'a>(&'a mut self) -> Option<&'a mut Buffer> {
        self.dequeue_buffer_raw().map(|mut x| unsafe { x.as_mut() })
    }

    #[inline]
    pub fn queue_buffer(&mut self, buffer: &mut Buffer) -> std::io::Result<()> {
        let r = unsafe { raw::pw_stream_queue_buffer(self.0.get(), buffer as *mut _ as _) };
        if r < 0 {
            Err(std::io::Error::from_raw_os_error(-r))
        } else {
            Ok(())
        }
    }

    #[inline]
    pub fn return_buffer(&mut self, buffer: &mut Buffer) -> std::io::Result<()> {
        let r = unsafe { raw::pw_stream_return_buffer(self.0.get(), buffer as *mut _ as _) };
        if r < 0 {
            Err(std::io::Error::from_raw_os_error(-r))
        } else {
            Ok(())
        }
    }

    /// Automatic dequeue/enqueue
    #[inline(always)]
    pub fn rent_buffer<'a>(&'a mut self) -> Option<RentBuffer<'a>> {
        Some(RentBuffer {
            buffer: self.dequeue_buffer_raw()?,
            stream: self,
        })
    }
}

pub struct RentBuffer<'a> {
    stream: &'a mut Stream,
    buffer: NonNull<Buffer>,
}
impl<'a> Drop for RentBuffer<'a> {
    #[inline(always)]
    fn drop(&mut self) {
        if let Err(e) = self.stream.queue_buffer(unsafe { self.buffer.as_mut() }) {
            eprintln!("queue_buffer failed: {e:?}");
        }
    }
}
impl<'a> core::ops::Deref for RentBuffer<'a> {
    type Target = Buffer;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        unsafe { self.buffer.as_ref() }
    }
}
impl<'a> core::ops::DerefMut for RentBuffer<'a> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.buffer.as_mut() }
    }
}
impl<'a> RentBuffer<'a> {
    #[inline(always)]
    pub fn r#return(mut self) -> std::io::Result<()> {
        self.stream.return_buffer(unsafe { self.buffer.as_mut() })?;
        core::mem::forget(self);
        Ok(())
    }

    #[inline(always)]
    pub fn done_handled(mut self) -> std::io::Result<()> {
        self.stream.queue_buffer(unsafe { self.buffer.as_mut() })?;
        core::mem::forget(self);
        Ok(())
    }
}

#[repr(transparent)]
pub struct MainLoop(UnsafeCell<raw::pw_main_loop>);
unsafe impl Sync for MainLoop {}
unsafe impl Send for MainLoop {}
impl PipewireDrop for MainLoop {
    #[inline(always)]
    unsafe fn pipewire_drop(&mut self) {
        unsafe { raw::pw_main_loop_destroy(self.0.get_mut()) }
    }
}
impl AsLoop for MainLoop {
    #[inline(always)]
    fn as_loop(&self) -> *mut raw::pw_loop {
        unsafe { raw::pw_main_loop_get_loop(self.0.get()) }
    }
}
impl MainLoop {
    #[inline]
    pub fn new(props: Option<NonNull<spa_dict>>) -> std::io::Result<Owned<Self>> {
        let r = unsafe {
            raw::pw_main_loop_new(props.map_or_else(core::ptr::null, |x| x.as_ptr()) as _)
        };

        Owned::from_raw(r.cast()).ok_or_else(std::io::Error::last_os_error)
    }

    #[inline(always)]
    pub fn r#loop(&self) -> *mut raw::pw_loop {
        unsafe { raw::pw_main_loop_get_loop(self.0.get()) }
    }

    #[inline(always)]
    pub fn run(&self) -> std::io::Result<()> {
        let r = unsafe { raw::pw_main_loop_run(self.0.get()) };
        if r < 0 {
            Err(std::io::Error::from_raw_os_error(-r))
        } else {
            Ok(())
        }
    }

    #[inline(always)]
    pub fn quit(&self) -> std::io::Result<()> {
        let r = unsafe { raw::pw_main_loop_quit(self.0.get()) };
        if r < 0 {
            Err(std::io::Error::from_raw_os_error(-r))
        } else {
            Ok(())
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Direction {
    Input = raw::PW_DIRECTION_INPUT as _,
    Output = raw::PW_DIRECTION_OUTPUT as _,
}

bitflags! {
    #[derive(Debug, Clone, Copy)]
    pub struct StreamFlags : raw::pw_stream_flags {
        const NONE = 0;
        const AUTOCONNECT = raw::PW_STREAM_FLAG_AUTOCONNECT;
        const INACTIVE = raw::PW_STREAM_FLAG_INACTIVE;
        const MAP_BUFFERS = raw::PW_STREAM_FLAG_MAP_BUFFERS;
        const DRIVER = raw::PW_STREAM_FLAG_DRIVER;
        const RT_PROCESS = raw::PW_STREAM_FLAG_RT_PROCESS;
        const NO_CONVERT = raw::PW_STREAM_FLAG_NO_CONVERT;
        const EXCLUSIVE = raw::PW_STREAM_FLAG_EXCLUSIVE;
        const DONT_RECONNECT = raw::PW_STREAM_FLAG_DONT_RECONNECT;
        const ALLOC_BUFFERS = raw::PW_STREAM_FLAG_ALLOC_BUFFERS;
        const TRIGGER = raw::PW_STREAM_FLAG_TRIGGER;
        const ASYNC = raw::PW_STREAM_FLAG_ASYNC;
        const EARLY_PROCESS = raw::PW_STREAM_FLAG_EARLY_PROCESS;
        const RT_TRIGGER_DONE = raw::PW_STREAM_FLAG_RT_TRIGGER_DONE;
    }
}

#[repr(transparent)]
pub struct Port(UnsafeCell<raw::pw_port>);
unsafe impl Sync for Port {}
unsafe impl Send for Port {}
unsafe impl PipewireProxy for Port {
    const TYPE_NAME: &CStr = c"PipeWire:Interface:Port";
}

#[repr(transparent)]
pub struct Node(UnsafeCell<raw::pw_node>);
unsafe impl Sync for Node {}
unsafe impl Send for Node {}
unsafe impl PipewireProxy for Node {
    const TYPE_NAME: &CStr = c"PipeWire:Interface:Node";
}

#[repr(transparent)]
pub struct Device(UnsafeCell<raw::pw_device>);
unsafe impl Sync for Device {}
unsafe impl Send for Device {}
unsafe impl PipewireProxy for Device {
    const TYPE_NAME: &CStr = c"PipeWire:Interface:Device";
}

pub trait CoreEventListener {
    #[allow(unused_variables)]
    fn info(&mut self, info: *const pw_core_info) {}
    #[allow(unused_variables)]
    fn done(&mut self, id: u32, seq: c_int) {}
    #[allow(unused_variables)]
    fn ping(&mut self, id: u32, seq: c_int) {}
    #[allow(unused_variables)]
    fn error(&mut self, id: u32, seq: c_int, res: c_int, mesasge: &CStr) {}
    #[allow(unused_variables)]
    fn remove_id(&mut self, id: u32) {}
    #[allow(unused_variables)]
    fn bound_id(&mut self, id: u32, global_id: u32) {}
    #[allow(unused_variables)]
    fn add_mem(&mut self, id: u32, r#type: u32, fd: c_int, flags: u32) {}
    #[allow(unused_variables)]
    fn remove_mem(&mut self, id: u32) {}
    #[allow(unused_variables)]
    fn bound_props(&mut self, id: u32, global_id: u32, props: *const spa_dict) {}
}
pub const fn core_event_fptbl<L: CoreEventListener + 'static>() -> &'static pw_core_events {
    extern "C" fn info<L: CoreEventListener + 'static>(
        data: *mut c_void,
        info: *const pw_core_info,
    ) {
        unsafe { L::info(&mut *data.cast(), info) }
    }
    extern "C" fn done<L: CoreEventListener + 'static>(data: *mut c_void, id: u32, seq: c_int) {
        unsafe { L::done(&mut *data.cast(), id, seq) }
    }
    extern "C" fn ping<L: CoreEventListener + 'static>(data: *mut c_void, id: u32, seq: c_int) {
        unsafe { L::ping(&mut *data.cast(), id, seq) }
    }
    extern "C" fn error<L: CoreEventListener + 'static>(
        data: *mut c_void,
        id: u32,
        seq: c_int,
        res: c_int,
        message: *const c_char,
    ) {
        unsafe { L::error(&mut *data.cast(), id, seq, res, CStr::from_ptr(message)) }
    }
    extern "C" fn remove_id<L: CoreEventListener + 'static>(data: *mut c_void, id: u32) {
        unsafe { L::remove_id(&mut *data.cast(), id) }
    }
    extern "C" fn bound_id<L: CoreEventListener + 'static>(
        data: *mut c_void,
        id: u32,
        global_id: u32,
    ) {
        unsafe { L::bound_id(&mut *data.cast(), id, global_id) }
    }
    extern "C" fn add_mem<L: CoreEventListener + 'static>(
        data: *mut c_void,
        id: u32,
        r#type: u32,
        fd: c_int,
        flags: u32,
    ) {
        unsafe { L::add_mem(&mut *data.cast(), id, r#type, fd, flags) }
    }
    extern "C" fn remove_mem<L: CoreEventListener + 'static>(data: *mut c_void, id: u32) {
        unsafe { L::remove_mem(&mut *data.cast(), id) }
    }
    extern "C" fn bound_props<L: CoreEventListener + 'static>(
        data: *mut c_void,
        id: u32,
        global_id: u32,
        props: *const spa_dict,
    ) {
        unsafe { L::bound_props(&mut *data.cast(), id, global_id, props) }
    }

    &pw_core_events {
        version: PW_VERSION_CORE_EVENTS,
        info: Some(info::<L>),
        done: Some(done::<L>),
        ping: Some(ping::<L>),
        error: Some(error::<L>),
        remove_id: Some(remove_id::<L>),
        bound_id: Some(bound_id::<L>),
        add_mem: Some(add_mem::<L>),
        remove_mem: Some(remove_mem::<L>),
        bound_props: Some(bound_props::<L>),
    }
}

pub trait RegistryEventListener {
    #[allow(unused_variables)]
    fn global(
        &mut self,
        id: u32,
        permissions: u32,
        r#type: &CStr,
        version: u32,
        props: &spa::Dict,
    ) {
    }
    #[allow(unused_variables)]
    fn global_remove(&mut self, id: u32) {}
}
pub const fn registry_event_fptbl<L: RegistryEventListener + 'static>()
-> &'static pw_registry_events {
    extern "C" fn global<L: RegistryEventListener + 'static>(
        data: *mut c_void,
        id: u32,
        permissions: u32,
        r#type: *const c_char,
        version: u32,
        props: *const spa_dict,
    ) {
        unsafe {
            L::global(
                &mut *data.cast(),
                id,
                permissions,
                CStr::from_ptr(r#type),
                version,
                &*props.cast(),
            )
        }
    }
    extern "C" fn global_remove<L: RegistryEventListener + 'static>(data: *mut c_void, id: u32) {
        unsafe { L::global_remove(&mut *data.cast(), id) }
    }

    &pw_registry_events {
        version: PW_VERSION_REGISTRY_EVENTS,
        global: Some(global::<L>),
        global_remove: Some(global_remove::<L>),
    }
}

pub trait StreamEventListener {
    #[allow(unused_variables)]
    fn destroy(&mut self) {}
    #[allow(unused_variables)]
    fn state_changed(
        &mut self,
        old: Result<StreamState, c_int>,
        state: Result<StreamState, c_int>,
        error: Option<&CStr>,
    ) {
    }
    #[allow(unused_variables)]
    fn control_info(&mut self, id: u32, control: &raw::pw_stream_control) {}
    #[allow(unused_variables)]
    fn io_changed(&mut self, id: u32, area: *mut c_void, size: u32) {}
    #[allow(unused_variables)]
    fn param_changed(&mut self, id: u32, param: *const raw::spa_pod) {}
    #[allow(unused_variables)]
    fn add_buffer(&mut self, buffer: *mut raw::pw_buffer) {}
    #[allow(unused_variables)]
    fn remove_buffer(&mut self, buffer: *mut raw::pw_buffer) {}
    #[allow(unused_variables)]
    fn process(&mut self) {}
    #[allow(unused_variables)]
    fn drained(&mut self) {}
    #[allow(unused_variables)]
    fn command(&mut self, command: *const raw::spa_command) {}
    #[allow(unused_variables)]
    fn trigger_done(&mut self) {}
}
const fn stream_event_fptbl<L: StreamEventListener + 'static>() -> &'static raw::pw_stream_events {
    extern "C" fn destroy<L: StreamEventListener + 'static>(data: *mut c_void) {
        unsafe { L::destroy(&mut *data.cast()) }
    }
    extern "C" fn state_changed<L: StreamEventListener + 'static>(
        data: *mut c_void,
        old: raw::pw_stream_state,
        state: raw::pw_stream_state,
        error: *const c_char,
    ) {
        unsafe {
            L::state_changed(
                &mut *data.cast(),
                old.try_into(),
                state.try_into(),
                if error.is_null() {
                    None
                } else {
                    Some(CStr::from_ptr(error))
                },
            )
        }
    }
    extern "C" fn control_info<L: StreamEventListener + 'static>(
        data: *mut c_void,
        id: u32,
        control: *const raw::pw_stream_control,
    ) {
        unsafe { L::control_info(&mut *data.cast(), id, &*control) }
    }
    extern "C" fn io_changed<L: StreamEventListener + 'static>(
        data: *mut c_void,
        id: u32,
        area: *mut c_void,
        size: u32,
    ) {
        unsafe { L::io_changed(&mut *data.cast(), id, area, size) }
    }
    extern "C" fn param_changed<L: StreamEventListener + 'static>(
        data: *mut c_void,
        id: u32,
        param: *const raw::spa_pod,
    ) {
        unsafe { L::param_changed(&mut *data.cast(), id, param) }
    }
    extern "C" fn add_buffer<L: StreamEventListener + 'static>(
        data: *mut c_void,
        buffer: *mut raw::pw_buffer,
    ) {
        unsafe { L::add_buffer(&mut *data.cast(), buffer) }
    }
    extern "C" fn remove_buffer<L: StreamEventListener + 'static>(
        data: *mut c_void,
        buffer: *mut raw::pw_buffer,
    ) {
        unsafe { L::remove_buffer(&mut *data.cast(), buffer) }
    }
    extern "C" fn process<L: StreamEventListener + 'static>(data: *mut c_void) {
        unsafe { L::process(&mut *data.cast()) }
    }
    extern "C" fn drained<L: StreamEventListener + 'static>(data: *mut c_void) {
        unsafe { L::drained(&mut *data.cast()) }
    }
    extern "C" fn command<L: StreamEventListener + 'static>(
        data: *mut c_void,
        command: *const raw::spa_command,
    ) {
        unsafe { L::command(&mut *data.cast(), command) }
    }
    extern "C" fn trigger_done<L: StreamEventListener + 'static>(data: *mut c_void) {
        unsafe { L::trigger_done(&mut *data.cast()) }
    }

    &const {
        raw::pw_stream_events {
            version: raw::PW_VERSION_STREAM_EVENTS,
            destroy: Some(destroy::<L>),
            state_changed: Some(state_changed::<L>),
            control_info: Some(control_info::<L>),
            io_changed: Some(io_changed::<L>),
            param_changed: Some(param_changed::<L>),
            add_buffer: Some(add_buffer::<L>),
            remove_buffer: Some(remove_buffer::<L>),
            process: Some(process::<L>),
            drained: Some(drained::<L>),
            command: Some(command::<L>),
            trigger_done: Some(trigger_done::<L>),
        }
    }
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StreamState {
    Error = raw::PW_STREAM_STATE_ERROR as _,
    Unconnected = raw::PW_STREAM_STATE_UNCONNECTED as _,
    Connecting = raw::PW_STREAM_STATE_CONNECTING as _,
    Paused = raw::PW_STREAM_STATE_PAUSED as _,
    Streaming = raw::PW_STREAM_STATE_STREAMING as _,
}
impl TryFrom<c_int> for StreamState {
    type Error = c_int;

    #[inline(always)]
    fn try_from(value: c_int) -> Result<Self, c_int> {
        if Self::Error as c_int <= value && value <= Self::Streaming as c_int {
            Ok(unsafe { core::mem::transmute(value) })
        } else {
            Err(value)
        }
    }
}

#[repr(transparent)]
pub struct Buffer(raw::pw_buffer);
impl Buffer {
    #[inline(always)]
    pub const fn frames(&self) -> u64 {
        self.0.size
    }

    #[inline(always)]
    pub const fn requested_frames(&self) -> u64 {
        self.0.requested
    }

    #[inline(always)]
    pub const fn time(&self) -> u64 {
        self.0.time
    }

    #[inline(always)]
    pub const fn datas(&self) -> &[spa::Data] {
        unsafe {
            core::slice::from_raw_parts(
                (*self.0.buffer).datas.cast::<spa::Data>(),
                (*self.0.buffer).n_datas as _,
            )
        }
    }

    #[inline(always)]
    pub const fn datas_mut(&mut self) -> &mut [spa::Data] {
        unsafe {
            core::slice::from_raw_parts_mut(
                (*self.0.buffer).datas.cast::<spa::Data>(),
                (*self.0.buffer).n_datas as _,
            )
        }
    }
}

#[repr(C)]
pub struct Properties(raw::pw_properties);
impl PipewireDrop for Properties {
    #[inline(always)]
    unsafe fn pipewire_drop(&mut self) {
        unsafe { raw::pw_properties_free(&mut self.0) }
    }
}
impl Properties {
    pub fn new(kvp_list: &[spa::DictItem]) -> std::io::Result<Owned<Self>> {
        let spa_dict = raw::spa_dict {
            flags: 0,
            n_items: kvp_list.len() as _,
            items: kvp_list.as_ptr().cast(),
        };
        let p = unsafe { raw::pw_properties_new_dict(&spa_dict) };
        Owned::from_raw(p.cast()).ok_or_else(std::io::Error::last_os_error)
    }
}
