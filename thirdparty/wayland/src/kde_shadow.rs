use crate::{ffi, Proxy, ProxyObject, Interface};

static ORG_KDE_KWIN_SHADOW_MANAGER_INTERFACE: ffi::Interface = ffi::Interface { name: c"org_kde_kwin_shadow_manager".as_ptr(), version: 2, method_count: 3, methods: const { [ffi::Message { name: c"create".as_ptr(), signature: c"no".as_ptr(), types: const { [crate::OrgKdeKwinShadow::DEF,crate::Surface::DEF,] }.as_ptr() },ffi::Message { name: c"unset".as_ptr(), signature: c"o".as_ptr(), types: const { [crate::Surface::DEF,] }.as_ptr() },ffi::Message { name: c"destroy".as_ptr(), signature: c"2".as_ptr(), types: const { [] }.as_ptr() },] }.as_ptr(), event_count: 0, events: const { [] }.as_ptr() };

#[repr(transparent)] pub struct OrgKdeKwinShadowManager(pub(crate) Proxy);
unsafe impl Interface for OrgKdeKwinShadowManager {
    const DEF: *const ffi::Interface = &ORG_KDE_KWIN_SHADOW_MANAGER_INTERFACE;

    #[cfg_attr(feature = "tracing", tracing::instrument(name = "<OrgKdeKwinShadowManager as Interface>::destruct", skip(self)))]
    unsafe fn destruct(&mut self) {
        if self.0.version() < 2 { return; }

        self.0.call_simple_dtor(2);
    }
}
impl ProxyObject for OrgKdeKwinShadowManager { #[inline(always)] fn as_proxy(&self) -> &Proxy { &self.0 } }
impl OrgKdeKwinShadowManager {
    #[inline(always)] pub fn set_user_data(&mut self, user_data: *mut core::ffi::c_void) { unsafe { self.0.set_user_data(user_data); } }
    #[inline(always)] pub fn user_data(&mut self) -> *mut core::ffi::c_void { unsafe { self.0.user_data() } }

    #[inline] pub fn create(&self,surface: &crate::Surface,) -> crate::Result<crate::Owned<crate::OrgKdeKwinShadow>> {
        Ok(unsafe { crate::Owned::wrap_unchecked(self.0.marshal_array_typed(0, &mut [crate::NEWID_ARG,surface.0.as_arg(),])?) })
    }

    #[inline] pub fn unset(&self,surface: &crate::Surface,) -> crate::Result<()> {
        self.0.marshal_array_void(1, &mut [surface.0.as_arg(),])
    }

 }

static ORG_KDE_KWIN_SHADOW_INTERFACE: ffi::Interface = ffi::Interface { name: c"org_kde_kwin_shadow".as_ptr(), version: 2, method_count: 14, methods: const { [ffi::Message { name: c"commit".as_ptr(), signature: c"".as_ptr(), types: const { [] }.as_ptr() },ffi::Message { name: c"attach_left".as_ptr(), signature: c"o".as_ptr(), types: const { [crate::Buffer::DEF,] }.as_ptr() },ffi::Message { name: c"attach_top_left".as_ptr(), signature: c"o".as_ptr(), types: const { [crate::Buffer::DEF,] }.as_ptr() },ffi::Message { name: c"attach_top".as_ptr(), signature: c"o".as_ptr(), types: const { [crate::Buffer::DEF,] }.as_ptr() },ffi::Message { name: c"attach_top_right".as_ptr(), signature: c"o".as_ptr(), types: const { [crate::Buffer::DEF,] }.as_ptr() },ffi::Message { name: c"attach_right".as_ptr(), signature: c"o".as_ptr(), types: const { [crate::Buffer::DEF,] }.as_ptr() },ffi::Message { name: c"attach_bottom_right".as_ptr(), signature: c"o".as_ptr(), types: const { [crate::Buffer::DEF,] }.as_ptr() },ffi::Message { name: c"attach_bottom".as_ptr(), signature: c"o".as_ptr(), types: const { [crate::Buffer::DEF,] }.as_ptr() },ffi::Message { name: c"attach_bottom_left".as_ptr(), signature: c"o".as_ptr(), types: const { [crate::Buffer::DEF,] }.as_ptr() },ffi::Message { name: c"set_left_offset".as_ptr(), signature: c"f".as_ptr(), types: const { [core::ptr::null(),] }.as_ptr() },ffi::Message { name: c"set_top_offset".as_ptr(), signature: c"f".as_ptr(), types: const { [core::ptr::null(),] }.as_ptr() },ffi::Message { name: c"set_right_offset".as_ptr(), signature: c"f".as_ptr(), types: const { [core::ptr::null(),] }.as_ptr() },ffi::Message { name: c"set_bottom_offset".as_ptr(), signature: c"f".as_ptr(), types: const { [core::ptr::null(),] }.as_ptr() },ffi::Message { name: c"destroy".as_ptr(), signature: c"2".as_ptr(), types: const { [] }.as_ptr() },] }.as_ptr(), event_count: 0, events: const { [] }.as_ptr() };

#[repr(transparent)] pub struct OrgKdeKwinShadow(pub(crate) Proxy);
unsafe impl Interface for OrgKdeKwinShadow {
    const DEF: *const ffi::Interface = &ORG_KDE_KWIN_SHADOW_INTERFACE;

    #[cfg_attr(feature = "tracing", tracing::instrument(name = "<OrgKdeKwinShadow as Interface>::destruct", skip(self)))]
    unsafe fn destruct(&mut self) {
        if self.0.version() < 2 { return; }

        self.0.call_simple_dtor(13);
    }
}
impl ProxyObject for OrgKdeKwinShadow { #[inline(always)] fn as_proxy(&self) -> &Proxy { &self.0 } }
impl OrgKdeKwinShadow {
    #[inline(always)] pub fn set_user_data(&mut self, user_data: *mut core::ffi::c_void) { unsafe { self.0.set_user_data(user_data); } }
    #[inline(always)] pub fn user_data(&mut self) -> *mut core::ffi::c_void { unsafe { self.0.user_data() } }

    #[inline] pub fn commit(&self,) -> crate::Result<()> {
        self.0.marshal_array_void(0, &mut [])
    }

    #[inline] pub fn attach_left(&self,buffer: &crate::Buffer,) -> crate::Result<()> {
        self.0.marshal_array_void(1, &mut [buffer.0.as_arg(),])
    }

    #[inline] pub fn attach_top_left(&self,buffer: &crate::Buffer,) -> crate::Result<()> {
        self.0.marshal_array_void(2, &mut [buffer.0.as_arg(),])
    }

    #[inline] pub fn attach_top(&self,buffer: &crate::Buffer,) -> crate::Result<()> {
        self.0.marshal_array_void(3, &mut [buffer.0.as_arg(),])
    }

    #[inline] pub fn attach_top_right(&self,buffer: &crate::Buffer,) -> crate::Result<()> {
        self.0.marshal_array_void(4, &mut [buffer.0.as_arg(),])
    }

    #[inline] pub fn attach_right(&self,buffer: &crate::Buffer,) -> crate::Result<()> {
        self.0.marshal_array_void(5, &mut [buffer.0.as_arg(),])
    }

    #[inline] pub fn attach_bottom_right(&self,buffer: &crate::Buffer,) -> crate::Result<()> {
        self.0.marshal_array_void(6, &mut [buffer.0.as_arg(),])
    }

    #[inline] pub fn attach_bottom(&self,buffer: &crate::Buffer,) -> crate::Result<()> {
        self.0.marshal_array_void(7, &mut [buffer.0.as_arg(),])
    }

    #[inline] pub fn attach_bottom_left(&self,buffer: &crate::Buffer,) -> crate::Result<()> {
        self.0.marshal_array_void(8, &mut [buffer.0.as_arg(),])
    }

    #[inline] pub fn set_left_offset(&self,offset: crate::Fixed,) -> crate::Result<()> {
        self.0.marshal_array_void(9, &mut [ffi::Argument { f: offset },])
    }

    #[inline] pub fn set_top_offset(&self,offset: crate::Fixed,) -> crate::Result<()> {
        self.0.marshal_array_void(10, &mut [ffi::Argument { f: offset },])
    }

    #[inline] pub fn set_right_offset(&self,offset: crate::Fixed,) -> crate::Result<()> {
        self.0.marshal_array_void(11, &mut [ffi::Argument { f: offset },])
    }

    #[inline] pub fn set_bottom_offset(&self,offset: crate::Fixed,) -> crate::Result<()> {
        self.0.marshal_array_void(12, &mut [ffi::Argument { f: offset },])
    }

 }

