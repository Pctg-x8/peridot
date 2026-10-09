//! pointer_constraints_unstable_v1: protocol for constraining pointer motions
//! 
//! This protocol specifies a set of interfaces used for adding constraints to
//! the motion of a pointer. Possible constraints include confining pointer
//! motions to a given region, or locking it to its current position.
//! 
//! In order to constrain the pointer, a client must first bind the global
//! interface "wp_pointer_constraints" which, if a compositor supports pointer
//! constraints, is exposed by the registry. Using the bound global object, the
//! client uses the request that corresponds to the type of constraint it wants
//! to make. See wp_pointer_constraints for more details.
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

static ZWP_POINTER_CONSTRAINTS_V1_INTERFACE: ffi::Interface = ffi::Interface { name: c"zwp_pointer_constraints_v1".as_ptr(), version: 1, method_count: 3, methods: const { [ffi::Message { name: c"destroy".as_ptr(), signature: c"".as_ptr(), types: const { [] }.as_ptr() },ffi::Message { name: c"lock_pointer".as_ptr(), signature: c"noo?ou".as_ptr(), types: const { [crate::ZwpLockedPointerV1::DEF,crate::Surface::DEF,crate::Pointer::DEF,crate::Region::DEF,core::ptr::null(),] }.as_ptr() },ffi::Message { name: c"confine_pointer".as_ptr(), signature: c"noo?ou".as_ptr(), types: const { [crate::ZwpConfinedPointerV1::DEF,crate::Surface::DEF,crate::Pointer::DEF,crate::Region::DEF,core::ptr::null(),] }.as_ptr() },] }.as_ptr(), event_count: 0, events: const { [] }.as_ptr() };

#[repr(transparent)] pub struct ZwpPointerConstraintsV1(pub(crate) Proxy);
unsafe impl Interface for ZwpPointerConstraintsV1 {
    const DEF: *const ffi::Interface = &ZWP_POINTER_CONSTRAINTS_V1_INTERFACE;

    #[cfg_attr(feature = "tracing", tracing::instrument(name = "<ZwpPointerConstraintsV1 as Interface>::destruct", skip(self)))]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}
impl ProxyObject for ZwpPointerConstraintsV1 { #[inline(always)] fn as_proxy(&self) -> &Proxy { &self.0 } }
impl ZwpPointerConstraintsV1 {
    #[inline(always)] pub fn set_user_data(&mut self, user_data: *mut core::ffi::c_void) { unsafe { self.0.set_user_data(user_data); } }
    #[inline(always)] pub fn user_data(&mut self) -> *mut core::ffi::c_void { unsafe { self.0.user_data() } }

    #[inline] pub fn lock_pointer(&self,surface: &crate::Surface,pointer: &crate::Pointer,region: Option<&crate::Region>,lifetime: ZwpPointerConstraintsV1Lifetime,) -> crate::Result<crate::Owned<crate::ZwpLockedPointerV1>> {
        Ok(unsafe { crate::Owned::wrap_unchecked(self.0.marshal_array_typed(1, &mut [crate::NEWID_ARG,surface.0.as_arg(),pointer.0.as_arg(),region.map_or(crate::NULLOBJ_ARG, |x| x.0.as_arg()),lifetime.as_arg(),])?) })
    }

    #[inline] pub fn confine_pointer(&self,surface: &crate::Surface,pointer: &crate::Pointer,region: Option<&crate::Region>,lifetime: ZwpPointerConstraintsV1Lifetime,) -> crate::Result<crate::Owned<crate::ZwpConfinedPointerV1>> {
        Ok(unsafe { crate::Owned::wrap_unchecked(self.0.marshal_array_typed(2, &mut [crate::NEWID_ARG,surface.0.as_arg(),pointer.0.as_arg(),region.map_or(crate::NULLOBJ_ARG, |x| x.0.as_arg()),lifetime.as_arg(),])?) })
    }

 }

#[repr(u32)] #[derive(Debug, Clone, Copy, PartialEq, Eq)] pub enum ZwpPointerConstraintsV1Error {
    AlreadyConstrained = 1,
}
impl ZwpPointerConstraintsV1Error { pub const fn as_arg(&self) -> ffi::Argument { ffi::Argument { u: *self as _ } } }

#[repr(u32)] #[derive(Debug, Clone, Copy, PartialEq, Eq)] pub enum ZwpPointerConstraintsV1Lifetime {
    Oneshot = 1,
    Persistent = 2,
}
impl ZwpPointerConstraintsV1Lifetime { pub const fn as_arg(&self) -> ffi::Argument { ffi::Argument { u: *self as _ } } }

static ZWP_LOCKED_POINTER_V1_INTERFACE: ffi::Interface = ffi::Interface { name: c"zwp_locked_pointer_v1".as_ptr(), version: 1, method_count: 3, methods: const { [ffi::Message { name: c"destroy".as_ptr(), signature: c"".as_ptr(), types: const { [] }.as_ptr() },ffi::Message { name: c"set_cursor_position_hint".as_ptr(), signature: c"ff".as_ptr(), types: const { [core::ptr::null(),core::ptr::null(),] }.as_ptr() },ffi::Message { name: c"set_region".as_ptr(), signature: c"?o".as_ptr(), types: const { [crate::Region::DEF,] }.as_ptr() },] }.as_ptr(), event_count: 2, events: const { [ffi::Message { name: c"locked".as_ptr(), signature: c"".as_ptr(), types: const { [] }.as_ptr() },ffi::Message { name: c"unlocked".as_ptr(), signature: c"".as_ptr(), types: const { [] }.as_ptr() },] }.as_ptr() };

#[repr(transparent)] pub struct ZwpLockedPointerV1(pub(crate) Proxy);
unsafe impl Interface for ZwpLockedPointerV1 {
    const DEF: *const ffi::Interface = &ZWP_LOCKED_POINTER_V1_INTERFACE;

    #[cfg_attr(feature = "tracing", tracing::instrument(name = "<ZwpLockedPointerV1 as Interface>::destruct", skip(self)))]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}
impl ProxyObject for ZwpLockedPointerV1 { #[inline(always)] fn as_proxy(&self) -> &Proxy { &self.0 } }
impl ZwpLockedPointerV1 {
    pub fn set_listener<'l, L: ZwpLockedPointerV1EventListener + 'l>(&'l mut self, listener: &'l mut L) -> crate::SetListenerResult {
        extern "C" fn locked<L: ZwpLockedPointerV1EventListener>(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy,) { L::locked(unsafe { &mut *(data0 as *mut _) }, unsafe { &mut *(sender0 as *mut _) },) }
extern "C" fn unlocked<L: ZwpLockedPointerV1EventListener>(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy,) { L::unlocked(unsafe { &mut *(data0 as *mut _) }, unsafe { &mut *(sender0 as *mut _) },) }

        #[repr(C)] struct FPTable { locked: extern "C" fn(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy, ),
unlocked: extern "C" fn(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy, ),
 }
        unsafe { self.0.set_listener(&const { FPTable { locked: locked::<L>,
unlocked: unlocked::<L>,
 } } as &'static FPTable as *const _ as _,listener as *mut _ as _) }
    }

    #[inline(always)] pub fn set_user_data(&mut self, user_data: *mut core::ffi::c_void) { unsafe { self.0.set_user_data(user_data); } }
    #[inline(always)] pub fn user_data(&mut self) -> *mut core::ffi::c_void { unsafe { self.0.user_data() } }

    #[inline] pub fn set_cursor_position_hint(&self,surface_x: crate::Fixed,surface_y: crate::Fixed,) -> crate::Result<()> {
        self.0.marshal_array_void(1, &mut [ffi::Argument { f: surface_x },ffi::Argument { f: surface_y },])
    }

    #[inline] pub fn set_region(&self,region: Option<&crate::Region>,) -> crate::Result<()> {
        self.0.marshal_array_void(2, &mut [region.map_or(crate::NULLOBJ_ARG, |x| x.0.as_arg()),])
    }

 }

pub trait ZwpLockedPointerV1EventListener {     fn locked(&mut self, sender: &mut ZwpLockedPointerV1, );
    fn unlocked(&mut self, sender: &mut ZwpLockedPointerV1, );
 }

static ZWP_CONFINED_POINTER_V1_INTERFACE: ffi::Interface = ffi::Interface { name: c"zwp_confined_pointer_v1".as_ptr(), version: 1, method_count: 2, methods: const { [ffi::Message { name: c"destroy".as_ptr(), signature: c"".as_ptr(), types: const { [] }.as_ptr() },ffi::Message { name: c"set_region".as_ptr(), signature: c"?o".as_ptr(), types: const { [crate::Region::DEF,] }.as_ptr() },] }.as_ptr(), event_count: 2, events: const { [ffi::Message { name: c"confined".as_ptr(), signature: c"".as_ptr(), types: const { [] }.as_ptr() },ffi::Message { name: c"unconfined".as_ptr(), signature: c"".as_ptr(), types: const { [] }.as_ptr() },] }.as_ptr() };

#[repr(transparent)] pub struct ZwpConfinedPointerV1(pub(crate) Proxy);
unsafe impl Interface for ZwpConfinedPointerV1 {
    const DEF: *const ffi::Interface = &ZWP_CONFINED_POINTER_V1_INTERFACE;

    #[cfg_attr(feature = "tracing", tracing::instrument(name = "<ZwpConfinedPointerV1 as Interface>::destruct", skip(self)))]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}
impl ProxyObject for ZwpConfinedPointerV1 { #[inline(always)] fn as_proxy(&self) -> &Proxy { &self.0 } }
impl ZwpConfinedPointerV1 {
    pub fn set_listener<'l, L: ZwpConfinedPointerV1EventListener + 'l>(&'l mut self, listener: &'l mut L) -> crate::SetListenerResult {
        extern "C" fn confined<L: ZwpConfinedPointerV1EventListener>(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy,) { L::confined(unsafe { &mut *(data0 as *mut _) }, unsafe { &mut *(sender0 as *mut _) },) }
extern "C" fn unconfined<L: ZwpConfinedPointerV1EventListener>(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy,) { L::unconfined(unsafe { &mut *(data0 as *mut _) }, unsafe { &mut *(sender0 as *mut _) },) }

        #[repr(C)] struct FPTable { confined: extern "C" fn(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy, ),
unconfined: extern "C" fn(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy, ),
 }
        unsafe { self.0.set_listener(&const { FPTable { confined: confined::<L>,
unconfined: unconfined::<L>,
 } } as &'static FPTable as *const _ as _,listener as *mut _ as _) }
    }

    #[inline(always)] pub fn set_user_data(&mut self, user_data: *mut core::ffi::c_void) { unsafe { self.0.set_user_data(user_data); } }
    #[inline(always)] pub fn user_data(&mut self) -> *mut core::ffi::c_void { unsafe { self.0.user_data() } }

    #[inline] pub fn set_region(&self,region: Option<&crate::Region>,) -> crate::Result<()> {
        self.0.marshal_array_void(1, &mut [region.map_or(crate::NULLOBJ_ARG, |x| x.0.as_arg()),])
    }

 }

pub trait ZwpConfinedPointerV1EventListener {     fn confined(&mut self, sender: &mut ZwpConfinedPointerV1, );
    fn unconfined(&mut self, sender: &mut ZwpConfinedPointerV1, );
 }

