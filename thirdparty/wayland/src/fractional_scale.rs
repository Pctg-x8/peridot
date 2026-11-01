//! fractional_scale_v1: Protocol for requesting fractional surface scales
//! 
//! This protocol allows a compositor to suggest for surfaces to render at
//! fractional scales.
//! 
//! A client can submit scaled content by utilizing wp_viewport. This is done by
//! creating a wp_viewport object for the surface and setting the destination
//! rectangle to the surface size before the scale factor is applied.
//! 
//! The buffer size is calculated by multiplying the surface size by the
//! intended scale.
//! 
//! The wl_surface buffer scale should remain set to 1.
//! 
//! If a surface has a surface-local size of 100 px by 50 px and wishes to
//! submit buffers with a scale of 1.5, then a buffer of 150px by 75 px should
//! be used and the wp_viewport destination rectangle should be 100 px by 50 px.
//! 
//! For toplevel surfaces, the size is rounded halfway away from zero. The
//! rounding algorithm for subsurface position and size is not defined.
//!   

use crate::{ffi, Proxy, ProxyObject, Interface};

static WP_FRACTIONAL_SCALE_MANAGER_V1_INTERFACE: ffi::Interface = ffi::Interface { name: c"wp_fractional_scale_manager_v1".as_ptr(), version: 1, method_count: 2, methods: const { [ffi::Message { name: c"destroy".as_ptr(), signature: c"".as_ptr(), types: const { [] }.as_ptr() },ffi::Message { name: c"get_fractional_scale".as_ptr(), signature: c"no".as_ptr(), types: const { [crate::WpFractionalScaleV1::DEF,crate::Surface::DEF,] }.as_ptr() },] }.as_ptr(), event_count: 0, events: const { [] }.as_ptr() };

#[repr(transparent)] pub struct WpFractionalScaleManagerV1(pub(crate) Proxy);
unsafe impl Interface for WpFractionalScaleManagerV1 {
    const DEF: *const ffi::Interface = &WP_FRACTIONAL_SCALE_MANAGER_V1_INTERFACE;

    #[cfg_attr(feature = "tracing", tracing::instrument(name = "<WpFractionalScaleManagerV1 as Interface>::destruct", skip(self)))]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}
impl ProxyObject for WpFractionalScaleManagerV1 { #[inline(always)] fn as_proxy(&self) -> &Proxy { &self.0 } }
impl WpFractionalScaleManagerV1 {
    #[inline(always)] pub fn set_user_data(&mut self, user_data: *mut core::ffi::c_void) { unsafe { self.0.set_user_data(user_data); } }
    #[inline(always)] pub fn user_data(&mut self) -> *mut core::ffi::c_void { unsafe { self.0.user_data() } }

    #[inline] pub fn get_fractional_scale(&self,surface: &crate::Surface,) -> crate::Result<crate::Owned<crate::WpFractionalScaleV1>> {
        Ok(unsafe { crate::Owned::wrap_unchecked(self.0.marshal_array_typed(1, &mut [crate::NEWID_ARG,surface.0.as_arg(),])?) })
    }

 }

#[repr(u32)] #[derive(Debug, Clone, Copy, PartialEq, Eq)] pub enum WpFractionalScaleManagerV1Error {
    FractionalScaleExists = 0,
}
impl WpFractionalScaleManagerV1Error { pub const fn as_arg(&self) -> ffi::Argument { ffi::Argument { u: *self as _ } } }

static WP_FRACTIONAL_SCALE_V1_INTERFACE: ffi::Interface = ffi::Interface { name: c"wp_fractional_scale_v1".as_ptr(), version: 1, method_count: 1, methods: const { [ffi::Message { name: c"destroy".as_ptr(), signature: c"".as_ptr(), types: const { [] }.as_ptr() },] }.as_ptr(), event_count: 1, events: const { [ffi::Message { name: c"preferred_scale".as_ptr(), signature: c"u".as_ptr(), types: const { [core::ptr::null(),] }.as_ptr() },] }.as_ptr() };

#[repr(transparent)] pub struct WpFractionalScaleV1(pub(crate) Proxy);
unsafe impl Interface for WpFractionalScaleV1 {
    const DEF: *const ffi::Interface = &WP_FRACTIONAL_SCALE_V1_INTERFACE;

    #[cfg_attr(feature = "tracing", tracing::instrument(name = "<WpFractionalScaleV1 as Interface>::destruct", skip(self)))]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}
impl ProxyObject for WpFractionalScaleV1 { #[inline(always)] fn as_proxy(&self) -> &Proxy { &self.0 } }
impl WpFractionalScaleV1 {
    pub fn set_listener<'l, L: WpFractionalScaleV1EventListener + 'l>(&'l mut self, listener: &'l mut L) -> crate::SetListenerResult {
        extern "C" fn preferred_scale<L: WpFractionalScaleV1EventListener>(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy,scale: u32,) { L::preferred_scale(unsafe { &mut *(data0 as *mut _) }, unsafe { &mut *(sender0 as *mut _) },scale,) }

        #[repr(C)] struct FPTable { preferred_scale: extern "C" fn(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy, scale: u32,),
 }
        unsafe { self.0.set_listener(&const { FPTable { preferred_scale: preferred_scale::<L>,
 } } as &'static FPTable as *const _ as _,listener as *mut _ as _) }
    }

    #[inline(always)] pub fn set_user_data(&mut self, user_data: *mut core::ffi::c_void) { unsafe { self.0.set_user_data(user_data); } }
    #[inline(always)] pub fn user_data(&mut self) -> *mut core::ffi::c_void { unsafe { self.0.user_data() } }

 }

pub trait WpFractionalScaleV1EventListener {     fn preferred_scale(&mut self, sender: &mut WpFractionalScaleV1, scale: u32,);
 }

