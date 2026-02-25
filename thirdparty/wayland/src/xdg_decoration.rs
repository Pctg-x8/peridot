use crate::{ffi, Proxy, Interface};

static ZXDG_DECORATION_MANAGER_V1_INTERFACE: ffi::Interface = ffi::Interface { name: c"zxdg_decoration_manager_v1".as_ptr(), version: 1, method_count: 2, methods: const { [ffi::Message { name: c"destroy".as_ptr(), signature: c"".as_ptr(), types: const { [] }.as_ptr() },ffi::Message { name: c"get_toplevel_decoration".as_ptr(), signature: c"no".as_ptr(), types: const { [crate::ZxdgToplevelDecorationV1::DEF,crate::XdgToplevel::DEF,] }.as_ptr() },] }.as_ptr(), event_count: 0, events: const { [] }.as_ptr() };

#[repr(transparent)] pub struct ZxdgDecorationManagerV1(pub(crate) Proxy);
unsafe impl Interface for ZxdgDecorationManagerV1 {
    const DEF: *const ffi::Interface = &ZXDG_DECORATION_MANAGER_V1_INTERFACE;

    #[cfg_attr(feature = "tracing", tracing::instrument(name = "<ZxdgDecorationManagerV1 as Interface>::destruct", skip(self)))]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}

impl ZxdgDecorationManagerV1 {
    #[inline(always)] pub fn set_user_data(&mut self, user_data: *mut core::ffi::c_void) { unsafe { self.0.set_user_data(user_data); } }
    #[inline(always)] pub fn user_data(&mut self) -> *mut core::ffi::c_void { unsafe { self.0.user_data() } }

    #[inline] pub fn get_toplevel_decoration(&self,toplevel: &crate::XdgToplevel,) -> crate::Result<crate::Owned<crate::ZxdgToplevelDecorationV1>> {
        Ok(unsafe { crate::Owned::wrap_unchecked(self.0.marshal_array_typed(1, &mut [crate::NEWID_ARG,toplevel.0.as_arg(),])?) })
    }

 }

static ZXDG_TOPLEVEL_DECORATION_V1_INTERFACE: ffi::Interface = ffi::Interface { name: c"zxdg_toplevel_decoration_v1".as_ptr(), version: 1, method_count: 3, methods: const { [ffi::Message { name: c"destroy".as_ptr(), signature: c"".as_ptr(), types: const { [] }.as_ptr() },ffi::Message { name: c"set_mode".as_ptr(), signature: c"u".as_ptr(), types: const { [core::ptr::null(),] }.as_ptr() },ffi::Message { name: c"unset_mode".as_ptr(), signature: c"".as_ptr(), types: const { [] }.as_ptr() },] }.as_ptr(), event_count: 1, events: const { [ffi::Message { name: c"configure".as_ptr(), signature: c"u".as_ptr(), types: const { [core::ptr::null(),] }.as_ptr() },] }.as_ptr() };

#[repr(transparent)] pub struct ZxdgToplevelDecorationV1(pub(crate) Proxy);
unsafe impl Interface for ZxdgToplevelDecorationV1 {
    const DEF: *const ffi::Interface = &ZXDG_TOPLEVEL_DECORATION_V1_INTERFACE;

    #[cfg_attr(feature = "tracing", tracing::instrument(name = "<ZxdgToplevelDecorationV1 as Interface>::destruct", skip(self)))]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}

impl ZxdgToplevelDecorationV1 {
    pub fn set_listener<'l, L: ZxdgToplevelDecorationV1EventListener + 'l>(&'l mut self, listener: &'l mut L) -> crate::SetListenerResult {
        extern "C" fn configure<L: ZxdgToplevelDecorationV1EventListener>(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy,mode: u32,) { L::configure(unsafe { &mut *(data0 as *mut _) }, unsafe { &mut *(sender0 as *mut _) },unsafe { core::mem::transmute(mode) },) }

        #[repr(C)] struct FPTable { configure: extern "C" fn(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy, mode: u32,),
 }
        unsafe { self.0.set_listener(&const { FPTable { configure: configure::<L>,
 } } as &'static FPTable as *const _ as _,listener as *mut _ as _) }
    }

    #[inline(always)] pub fn set_user_data(&mut self, user_data: *mut core::ffi::c_void) { unsafe { self.0.set_user_data(user_data); } }
    #[inline(always)] pub fn user_data(&mut self) -> *mut core::ffi::c_void { unsafe { self.0.user_data() } }

    #[inline] pub fn set_mode(&self,mode: ZxdgToplevelDecorationV1Mode,) -> crate::Result<()> {
        self.0.marshal_array_void(1, &mut [mode.as_arg(),])
    }

    #[inline] pub fn unset_mode(&self,) -> crate::Result<()> {
        self.0.marshal_array_void(2, &mut [])
    }

 }

pub trait ZxdgToplevelDecorationV1EventListener {     fn configure(&mut self, sender: &mut ZxdgToplevelDecorationV1, mode: ZxdgToplevelDecorationV1Mode,);
 }

#[repr(u32)] #[derive(Debug, Clone, Copy, PartialEq, Eq)] pub enum ZxdgToplevelDecorationV1Error {
    UnconfiguredBuffer = 0,
    AlreadyConstructed = 1,
    Orphaned = 2,
    InvalidMode = 3,
}
impl ZxdgToplevelDecorationV1Error { pub const fn as_arg(&self) -> ffi::Argument { ffi::Argument { u: *self as _ } } }

#[repr(u32)] #[derive(Debug, Clone, Copy, PartialEq, Eq)] pub enum ZxdgToplevelDecorationV1Mode {
    ClientSide = 1,
    ServerSide = 2,
}
impl ZxdgToplevelDecorationV1Mode { pub const fn as_arg(&self) -> ffi::Argument { ffi::Argument { u: *self as _ } } }

