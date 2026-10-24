//! relative_pointer_unstable_v1: protocol for relative pointer motion events
//! 
//! This protocol specifies a set of interfaces used for making clients able to
//! receive relative pointer events not obstructed by barriers (such as the
//! monitor edge or other pointer barriers).
//! 
//! To start receiving relative pointer events, a client must first bind the
//! global interface "wp_relative_pointer_manager" which, if a compositor
//! supports relative pointer motion events, is exposed by the registry. After
//! having created the relative pointer manager proxy object, the client uses
//! it to create the actual relative pointer object using the
//! "get_relative_pointer" request given a wl_pointer. The relative pointer
//! motion events will then, when applicable, be transmitted via the proxy of
//! the newly created relative pointer object. See the documentation of the
//! relative pointer interface for more details.
//! 
//! Warning! The protocol described in this file is experimental and backward
//! incompatible changes may be made. Backward compatible changes may be added
//! together with the corresponding interface version bump. Backward
//! incompatible changes are done by bumping the version number in the protocol
//! and interface names and resetting the interface version. Once the protocol
//! is to be declared stable, the 'z' prefix and the version number in the
//! protocol and interface names are removed and the interface version number is
//! reset.
//!   

use crate::{ffi, Proxy, ProxyObject, Interface};

static ZWP_RELATIVE_POINTER_MANAGER_V1_INTERFACE: ffi::Interface = ffi::Interface { name: c"zwp_relative_pointer_manager_v1".as_ptr(), version: 1, method_count: 2, methods: const { [ffi::Message { name: c"destroy".as_ptr(), signature: c"".as_ptr(), types: const { [] }.as_ptr() },ffi::Message { name: c"get_relative_pointer".as_ptr(), signature: c"no".as_ptr(), types: const { [crate::ZwpRelativePointerV1::DEF,crate::Pointer::DEF,] }.as_ptr() },] }.as_ptr(), event_count: 0, events: const { [] }.as_ptr() };

#[repr(transparent)] pub struct ZwpRelativePointerManagerV1(pub(crate) Proxy);
unsafe impl Interface for ZwpRelativePointerManagerV1 {
    const DEF: *const ffi::Interface = &ZWP_RELATIVE_POINTER_MANAGER_V1_INTERFACE;

    #[cfg_attr(feature = "tracing", tracing::instrument(name = "<ZwpRelativePointerManagerV1 as Interface>::destruct", skip(self)))]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}
impl ProxyObject for ZwpRelativePointerManagerV1 { #[inline(always)] fn as_proxy(&self) -> &Proxy { &self.0 } }
impl ZwpRelativePointerManagerV1 {
    #[inline(always)] pub fn set_user_data(&mut self, user_data: *mut core::ffi::c_void) { unsafe { self.0.set_user_data(user_data); } }
    #[inline(always)] pub fn user_data(&mut self) -> *mut core::ffi::c_void { unsafe { self.0.user_data() } }

    #[inline] pub fn get_relative_pointer(&self,pointer: &crate::Pointer,) -> crate::Result<crate::Owned<crate::ZwpRelativePointerV1>> {
        Ok(unsafe { crate::Owned::wrap_unchecked(self.0.marshal_array_typed(1, &mut [crate::NEWID_ARG,pointer.0.as_arg(),])?) })
    }

 }

static ZWP_RELATIVE_POINTER_V1_INTERFACE: ffi::Interface = ffi::Interface { name: c"zwp_relative_pointer_v1".as_ptr(), version: 1, method_count: 1, methods: const { [ffi::Message { name: c"destroy".as_ptr(), signature: c"".as_ptr(), types: const { [] }.as_ptr() },] }.as_ptr(), event_count: 1, events: const { [ffi::Message { name: c"relative_motion".as_ptr(), signature: c"uuffff".as_ptr(), types: const { [core::ptr::null(),core::ptr::null(),core::ptr::null(),core::ptr::null(),core::ptr::null(),core::ptr::null(),] }.as_ptr() },] }.as_ptr() };

#[repr(transparent)] pub struct ZwpRelativePointerV1(pub(crate) Proxy);
unsafe impl Interface for ZwpRelativePointerV1 {
    const DEF: *const ffi::Interface = &ZWP_RELATIVE_POINTER_V1_INTERFACE;

    #[cfg_attr(feature = "tracing", tracing::instrument(name = "<ZwpRelativePointerV1 as Interface>::destruct", skip(self)))]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}
impl ProxyObject for ZwpRelativePointerV1 { #[inline(always)] fn as_proxy(&self) -> &Proxy { &self.0 } }
impl ZwpRelativePointerV1 {
    pub fn set_listener<'l, L: ZwpRelativePointerV1EventListener + 'l>(&'l mut self, listener: &'l mut L) -> crate::SetListenerResult {
        extern "C" fn relative_motion<L: ZwpRelativePointerV1EventListener>(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy,utime_hi: u32,utime_lo: u32,dx: ffi::Fixed,dy: ffi::Fixed,dx_unaccel: ffi::Fixed,dy_unaccel: ffi::Fixed,) { L::relative_motion(unsafe { &mut *(data0 as *mut _) }, unsafe { &mut *(sender0 as *mut _) },utime_hi,utime_lo,dx,dy,dx_unaccel,dy_unaccel,) }

        #[repr(C)] struct FPTable { relative_motion: extern "C" fn(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy, utime_hi: u32,utime_lo: u32,dx: ffi::Fixed,dy: ffi::Fixed,dx_unaccel: ffi::Fixed,dy_unaccel: ffi::Fixed,),
 }
        unsafe { self.0.set_listener(&const { FPTable { relative_motion: relative_motion::<L>,
 } } as &'static FPTable as *const _ as _,listener as *mut _ as _) }
    }

    #[inline(always)] pub fn set_user_data(&mut self, user_data: *mut core::ffi::c_void) { unsafe { self.0.set_user_data(user_data); } }
    #[inline(always)] pub fn user_data(&mut self) -> *mut core::ffi::c_void { unsafe { self.0.user_data() } }

 }

pub trait ZwpRelativePointerV1EventListener {     fn relative_motion(&mut self, sender: &mut ZwpRelativePointerV1, utime_hi: u32,utime_lo: u32,dx: crate::Fixed,dy: crate::Fixed,dx_unaccel: crate::Fixed,dy_unaccel: crate::Fixed,);
 }

