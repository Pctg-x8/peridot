use crate::{ffi, Proxy, Interface};

static ZWLR_LAYER_SHELL_V1_INTERFACE: ffi::Interface = ffi::Interface { name: c"zwlr_layer_shell_v1".as_ptr(), version: 4, method_count: 2, methods: const { [ffi::Message { name: c"get_layer_surface".as_ptr(), signature: c"no?ous".as_ptr(), types: const { [crate::ZwlrLayerSurfaceV1::DEF,crate::Surface::DEF,crate::Output::DEF,core::ptr::null(),core::ptr::null(),] }.as_ptr() },ffi::Message { name: c"destroy".as_ptr(), signature: c"3".as_ptr(), types: const { [] }.as_ptr() },] }.as_ptr(), event_count: 0, events: const { [] }.as_ptr() };

#[repr(transparent)] pub struct ZwlrLayerShellV1(pub(crate) Proxy);
unsafe impl Interface for ZwlrLayerShellV1 {
    const DEF: *const ffi::Interface = &ZWLR_LAYER_SHELL_V1_INTERFACE;

    #[cfg_attr(feature = "tracing", tracing::instrument(name = "<ZwlrLayerShellV1 as Interface>::destruct", skip(self)))]
    unsafe fn destruct(&mut self) {
        if self.0.version() < 3 { return; }

        self.0.call_simple_dtor(1);
    }
}

impl ZwlrLayerShellV1 {
    #[inline(always)] pub fn set_user_data(&mut self, user_data: *mut core::ffi::c_void) { unsafe { self.0.set_user_data(user_data); } }
    #[inline(always)] pub fn user_data(&mut self) -> *mut core::ffi::c_void { unsafe { self.0.user_data() } }

    #[inline] pub fn get_layer_surface(&self,surface: &crate::Surface,output: Option<&crate::Output>,layer: ZwlrLayerShellV1Layer,namespace: &core::ffi::CStr,) -> crate::Result<crate::Owned<crate::ZwlrLayerSurfaceV1>> {
        Ok(unsafe { crate::Owned::wrap_unchecked(self.0.marshal_array_typed(0, &mut [crate::NEWID_ARG,surface.0.as_arg(),output.map_or(crate::NULLOBJ_ARG, |x| x.0.as_arg()),layer.as_arg(),ffi::Argument { s: namespace.as_ptr() },])?) })
    }

 }

#[repr(u32)] #[derive(Debug, Clone, Copy, PartialEq, Eq)] pub enum ZwlrLayerShellV1Error {
    Role = 0,
    InvalidLayer = 1,
    AlreadyConstructed = 2,
}
impl ZwlrLayerShellV1Error { pub const fn as_arg(&self) -> ffi::Argument { ffi::Argument { u: *self as _ } } }

#[repr(u32)] #[derive(Debug, Clone, Copy, PartialEq, Eq)] pub enum ZwlrLayerShellV1Layer {
    Background = 0,
    Bottom = 1,
    Top = 2,
    Overlay = 3,
}
impl ZwlrLayerShellV1Layer { pub const fn as_arg(&self) -> ffi::Argument { ffi::Argument { u: *self as _ } } }

static ZWLR_LAYER_SURFACE_V1_INTERFACE: ffi::Interface = ffi::Interface { name: c"zwlr_layer_surface_v1".as_ptr(), version: 4, method_count: 9, methods: const { [ffi::Message { name: c"set_size".as_ptr(), signature: c"uu".as_ptr(), types: const { [core::ptr::null(),core::ptr::null(),] }.as_ptr() },ffi::Message { name: c"set_anchor".as_ptr(), signature: c"u".as_ptr(), types: const { [core::ptr::null(),] }.as_ptr() },ffi::Message { name: c"set_exclusive_zone".as_ptr(), signature: c"i".as_ptr(), types: const { [core::ptr::null(),] }.as_ptr() },ffi::Message { name: c"set_margin".as_ptr(), signature: c"iiii".as_ptr(), types: const { [core::ptr::null(),core::ptr::null(),core::ptr::null(),core::ptr::null(),] }.as_ptr() },ffi::Message { name: c"set_keyboard_interactivity".as_ptr(), signature: c"u".as_ptr(), types: const { [core::ptr::null(),] }.as_ptr() },ffi::Message { name: c"get_popup".as_ptr(), signature: c"o".as_ptr(), types: const { [crate::XdgPopup::DEF,] }.as_ptr() },ffi::Message { name: c"ack_configure".as_ptr(), signature: c"u".as_ptr(), types: const { [core::ptr::null(),] }.as_ptr() },ffi::Message { name: c"destroy".as_ptr(), signature: c"".as_ptr(), types: const { [] }.as_ptr() },ffi::Message { name: c"set_layer".as_ptr(), signature: c"2u".as_ptr(), types: const { [core::ptr::null(),] }.as_ptr() },] }.as_ptr(), event_count: 2, events: const { [ffi::Message { name: c"configure".as_ptr(), signature: c"uuu".as_ptr(), types: const { [core::ptr::null(),core::ptr::null(),core::ptr::null(),] }.as_ptr() },ffi::Message { name: c"closed".as_ptr(), signature: c"".as_ptr(), types: const { [] }.as_ptr() },] }.as_ptr() };

#[repr(transparent)] pub struct ZwlrLayerSurfaceV1(pub(crate) Proxy);
unsafe impl Interface for ZwlrLayerSurfaceV1 {
    const DEF: *const ffi::Interface = &ZWLR_LAYER_SURFACE_V1_INTERFACE;

    #[cfg_attr(feature = "tracing", tracing::instrument(name = "<ZwlrLayerSurfaceV1 as Interface>::destruct", skip(self)))]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(7);
    }
}

impl ZwlrLayerSurfaceV1 {
    pub fn set_listener<'l, L: ZwlrLayerSurfaceV1EventListener + 'l>(&'l mut self, listener: &'l mut L) -> crate::SetListenerResult {
        extern "C" fn configure<L: ZwlrLayerSurfaceV1EventListener>(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy,serial: u32,width: u32,height: u32,) { L::configure(unsafe { &mut *(data0 as *mut _) }, unsafe { &mut *(sender0 as *mut _) },serial,width,height,) }
extern "C" fn closed<L: ZwlrLayerSurfaceV1EventListener>(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy,) { L::closed(unsafe { &mut *(data0 as *mut _) }, unsafe { &mut *(sender0 as *mut _) },) }

        #[repr(C)] struct FPTable { configure: extern "C" fn(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy, serial: u32,width: u32,height: u32,),
closed: extern "C" fn(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy, ),
 }
        unsafe { self.0.set_listener(&const { FPTable { configure: configure::<L>,
closed: closed::<L>,
 } } as &'static FPTable as *const _ as _,listener as *mut _ as _) }
    }

    #[inline(always)] pub fn set_user_data(&mut self, user_data: *mut core::ffi::c_void) { unsafe { self.0.set_user_data(user_data); } }
    #[inline(always)] pub fn user_data(&mut self) -> *mut core::ffi::c_void { unsafe { self.0.user_data() } }

    #[inline] pub fn set_size(&self,width: u32,height: u32,) -> crate::Result<()> {
        self.0.marshal_array_void(0, &mut [ffi::Argument { u: width },ffi::Argument { u: height },])
    }

    #[inline] pub fn set_anchor(&self,anchor: ZwlrLayerSurfaceV1Anchor,) -> crate::Result<()> {
        self.0.marshal_array_void(1, &mut [anchor.as_arg(),])
    }

    #[inline] pub fn set_exclusive_zone(&self,zone: i32,) -> crate::Result<()> {
        self.0.marshal_array_void(2, &mut [ffi::Argument { i: zone },])
    }

    #[inline] pub fn set_margin(&self,top: i32,right: i32,bottom: i32,left: i32,) -> crate::Result<()> {
        self.0.marshal_array_void(3, &mut [ffi::Argument { i: top },ffi::Argument { i: right },ffi::Argument { i: bottom },ffi::Argument { i: left },])
    }

    #[inline] pub fn set_keyboard_interactivity(&self,keyboard_interactivity: ZwlrLayerSurfaceV1KeyboardInteractivity,) -> crate::Result<()> {
        self.0.marshal_array_void(4, &mut [keyboard_interactivity.as_arg(),])
    }

    #[inline] pub fn get_popup(&self,popup: &crate::XdgPopup,) -> crate::Result<()> {
        self.0.marshal_array_void(5, &mut [popup.0.as_arg(),])
    }

    #[inline] pub fn ack_configure(&self,serial: u32,) -> crate::Result<()> {
        self.0.marshal_array_void(6, &mut [ffi::Argument { u: serial },])
    }

    #[inline] pub fn set_layer(&self,layer: ZwlrLayerShellV1Layer,) -> crate::Result<()> {
        self.0.marshal_array_void(8, &mut [layer.as_arg(),])
    }

 }

pub trait ZwlrLayerSurfaceV1EventListener {     fn configure(&mut self, sender: &mut ZwlrLayerSurfaceV1, serial: u32,width: u32,height: u32,);
    fn closed(&mut self, sender: &mut ZwlrLayerSurfaceV1, );
 }

#[repr(u32)] #[derive(Debug, Clone, Copy, PartialEq, Eq)] pub enum ZwlrLayerSurfaceV1KeyboardInteractivity {
    None = 0,
    Exclusive = 1,
    OnDemand = 2,
}
impl ZwlrLayerSurfaceV1KeyboardInteractivity { pub const fn as_arg(&self) -> ffi::Argument { ffi::Argument { u: *self as _ } } }

#[repr(u32)] #[derive(Debug, Clone, Copy, PartialEq, Eq)] pub enum ZwlrLayerSurfaceV1Error {
    InvalidSurfaceState = 0,
    InvalidSize = 1,
    InvalidAnchor = 2,
    InvalidKeyboardInteractivity = 3,
}
impl ZwlrLayerSurfaceV1Error { pub const fn as_arg(&self) -> ffi::Argument { ffi::Argument { u: *self as _ } } }

bitflags::bitflags! { #[derive(Debug, Clone, Copy, PartialEq, Eq)] pub struct ZwlrLayerSurfaceV1Anchor : u32 {
    const Top = 1;
    const Bottom = 2;
    const Left = 4;
    const Right = 8;
} }
impl ZwlrLayerSurfaceV1Anchor { pub const fn as_arg(&self) -> ffi::Argument { ffi::Argument { u: self.bits() } } }

