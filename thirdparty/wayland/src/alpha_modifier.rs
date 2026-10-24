use crate::{ffi, Proxy, ProxyObject, Interface};

static WP_ALPHA_MODIFIER_V1_INTERFACE: ffi::Interface = ffi::Interface { name: c"wp_alpha_modifier_v1".as_ptr(), version: 1, method_count: 2, methods: const { [ffi::Message { name: c"destroy".as_ptr(), signature: c"".as_ptr(), types: const { [] }.as_ptr() },ffi::Message { name: c"get_surface".as_ptr(), signature: c"no".as_ptr(), types: const { [crate::WpAlphaModifierSurfaceV1::DEF,crate::Surface::DEF,] }.as_ptr() },] }.as_ptr(), event_count: 0, events: const { [] }.as_ptr() };

#[repr(transparent)] pub struct WpAlphaModifierV1(pub(crate) Proxy);
unsafe impl Interface for WpAlphaModifierV1 {
    const DEF: *const ffi::Interface = &WP_ALPHA_MODIFIER_V1_INTERFACE;

    #[cfg_attr(feature = "tracing", tracing::instrument(name = "<WpAlphaModifierV1 as Interface>::destruct", skip(self)))]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}
impl ProxyObject for WpAlphaModifierV1 { #[inline(always)] fn as_proxy(&self) -> &Proxy { &self.0 } }
impl WpAlphaModifierV1 {
    #[inline(always)] pub fn set_user_data(&mut self, user_data: *mut core::ffi::c_void) { unsafe { self.0.set_user_data(user_data); } }
    #[inline(always)] pub fn user_data(&mut self) -> *mut core::ffi::c_void { unsafe { self.0.user_data() } }

    #[inline] pub fn get_surface(&self,surface: &crate::Surface,) -> crate::Result<crate::Owned<crate::WpAlphaModifierSurfaceV1>> {
        Ok(unsafe { crate::Owned::wrap_unchecked(self.0.marshal_array_typed(1, &mut [crate::NEWID_ARG,surface.0.as_arg(),])?) })
    }

 }

#[repr(u32)] #[derive(Debug, Clone, Copy, PartialEq, Eq)] pub enum WpAlphaModifierV1Error {
    AlreadyConstructed = 0,
}
impl WpAlphaModifierV1Error { pub const fn as_arg(&self) -> ffi::Argument { ffi::Argument { u: *self as _ } } }

static WP_ALPHA_MODIFIER_SURFACE_V1_INTERFACE: ffi::Interface = ffi::Interface { name: c"wp_alpha_modifier_surface_v1".as_ptr(), version: 1, method_count: 2, methods: const { [ffi::Message { name: c"destroy".as_ptr(), signature: c"".as_ptr(), types: const { [] }.as_ptr() },ffi::Message { name: c"set_multiplier".as_ptr(), signature: c"u".as_ptr(), types: const { [core::ptr::null(),] }.as_ptr() },] }.as_ptr(), event_count: 0, events: const { [] }.as_ptr() };

#[repr(transparent)] pub struct WpAlphaModifierSurfaceV1(pub(crate) Proxy);
unsafe impl Interface for WpAlphaModifierSurfaceV1 {
    const DEF: *const ffi::Interface = &WP_ALPHA_MODIFIER_SURFACE_V1_INTERFACE;

    #[cfg_attr(feature = "tracing", tracing::instrument(name = "<WpAlphaModifierSurfaceV1 as Interface>::destruct", skip(self)))]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}
impl ProxyObject for WpAlphaModifierSurfaceV1 { #[inline(always)] fn as_proxy(&self) -> &Proxy { &self.0 } }
impl WpAlphaModifierSurfaceV1 {
    #[inline(always)] pub fn set_user_data(&mut self, user_data: *mut core::ffi::c_void) { unsafe { self.0.set_user_data(user_data); } }
    #[inline(always)] pub fn user_data(&mut self) -> *mut core::ffi::c_void { unsafe { self.0.user_data() } }

    #[inline] pub fn set_multiplier(&self,factor: u32,) -> crate::Result<()> {
        self.0.marshal_array_void(1, &mut [ffi::Argument { u: factor },])
    }

 }

#[repr(u32)] #[derive(Debug, Clone, Copy, PartialEq, Eq)] pub enum WpAlphaModifierSurfaceV1Error {
    NoSurface = 0,
}
impl WpAlphaModifierSurfaceV1Error { pub const fn as_arg(&self) -> ffi::Argument { ffi::Argument { u: *self as _ } } }

