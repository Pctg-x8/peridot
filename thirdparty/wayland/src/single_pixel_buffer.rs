//! single_pixel_buffer_v1: single pixel buffer factory
//! 
//! This protocol extension allows clients to create single-pixel buffers.
//! 
//! Compositors supporting this protocol extension should also support the
//! viewporter protocol extension. Clients may use viewporter to scale a
//! single-pixel buffer to a desired size.
//! 
//! Warning! The protocol described in this file is currently in the testing
//! phase. Backward compatible changes may be added together with the
//! corresponding interface version bump. Backward incompatible changes can
//! only be done by creating a new major version of the extension.
//!   

use crate::{ffi, Proxy, ProxyObject, Interface};

static WP_SINGLE_PIXEL_BUFFER_MANAGER_V1_INTERFACE: ffi::Interface = ffi::Interface { name: c"wp_single_pixel_buffer_manager_v1".as_ptr(), version: 1, method_count: 2, methods: const { [ffi::Message { name: c"destroy".as_ptr(), signature: c"".as_ptr(), types: const { [] }.as_ptr() },ffi::Message { name: c"create_u32_rgba_buffer".as_ptr(), signature: c"nuuuu".as_ptr(), types: const { [crate::Buffer::DEF,core::ptr::null(),core::ptr::null(),core::ptr::null(),core::ptr::null(),] }.as_ptr() },] }.as_ptr(), event_count: 0, events: const { [] }.as_ptr() };

#[repr(transparent)] pub struct WpSinglePixelBufferManagerV1(pub(crate) Proxy);
unsafe impl Interface for WpSinglePixelBufferManagerV1 {
    const DEF: *const ffi::Interface = &WP_SINGLE_PIXEL_BUFFER_MANAGER_V1_INTERFACE;

    #[cfg_attr(feature = "tracing", tracing::instrument(name = "<WpSinglePixelBufferManagerV1 as Interface>::destruct", skip(self)))]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}
impl ProxyObject for WpSinglePixelBufferManagerV1 { #[inline(always)] fn as_proxy(&self) -> &Proxy { &self.0 } }
impl WpSinglePixelBufferManagerV1 {
    #[inline(always)] pub fn set_user_data(&mut self, user_data: *mut core::ffi::c_void) { unsafe { self.0.set_user_data(user_data); } }
    #[inline(always)] pub fn user_data(&mut self) -> *mut core::ffi::c_void { unsafe { self.0.user_data() } }

    #[inline] pub fn create_u32_rgba_buffer(&self,r: u32,g: u32,b: u32,a: u32,) -> crate::Result<crate::Owned<crate::Buffer>> {
        Ok(unsafe { crate::Owned::wrap_unchecked(self.0.marshal_array_typed(1, &mut [crate::NEWID_ARG,ffi::Argument { u: r },ffi::Argument { u: g },ffi::Argument { u: b },ffi::Argument { u: a },])?) })
    }

 }

