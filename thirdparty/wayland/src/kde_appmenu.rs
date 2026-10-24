use crate::{ffi, Proxy, ProxyObject, Interface};

static ORG_KDE_KWIN_APPMENU_MANAGER_INTERFACE: ffi::Interface = ffi::Interface { name: c"org_kde_kwin_appmenu_manager".as_ptr(), version: 2, method_count: 2, methods: const { [ffi::Message { name: c"create".as_ptr(), signature: c"no".as_ptr(), types: const { [crate::OrgKdeKwinAppmenu::DEF,crate::Surface::DEF,] }.as_ptr() },ffi::Message { name: c"release".as_ptr(), signature: c"2".as_ptr(), types: const { [] }.as_ptr() },] }.as_ptr(), event_count: 0, events: const { [] }.as_ptr() };

#[repr(transparent)] pub struct OrgKdeKwinAppmenuManager(pub(crate) Proxy);
unsafe impl Interface for OrgKdeKwinAppmenuManager {
    const DEF: *const ffi::Interface = &ORG_KDE_KWIN_APPMENU_MANAGER_INTERFACE;

    #[cfg_attr(feature = "tracing", tracing::instrument(name = "<OrgKdeKwinAppmenuManager as Interface>::destruct", skip(self)))]
    unsafe fn destruct(&mut self) {
        if self.0.version() < 2 { return; }

        self.0.call_simple_dtor(1);
    }
}
impl ProxyObject for OrgKdeKwinAppmenuManager { #[inline(always)] fn as_proxy(&self) -> &Proxy { &self.0 } }
impl OrgKdeKwinAppmenuManager {
    #[inline(always)] pub fn set_user_data(&mut self, user_data: *mut core::ffi::c_void) { unsafe { self.0.set_user_data(user_data); } }
    #[inline(always)] pub fn user_data(&mut self) -> *mut core::ffi::c_void { unsafe { self.0.user_data() } }

    #[inline] pub fn create(&self,surface: &crate::Surface,) -> crate::Result<crate::Owned<crate::OrgKdeKwinAppmenu>> {
        Ok(unsafe { crate::Owned::wrap_unchecked(self.0.marshal_array_typed(0, &mut [crate::NEWID_ARG,surface.0.as_arg(),])?) })
    }

 }

static ORG_KDE_KWIN_APPMENU_INTERFACE: ffi::Interface = ffi::Interface { name: c"org_kde_kwin_appmenu".as_ptr(), version: 2, method_count: 2, methods: const { [ffi::Message { name: c"set_address".as_ptr(), signature: c"ss".as_ptr(), types: const { [core::ptr::null(),core::ptr::null(),] }.as_ptr() },ffi::Message { name: c"release".as_ptr(), signature: c"".as_ptr(), types: const { [] }.as_ptr() },] }.as_ptr(), event_count: 0, events: const { [] }.as_ptr() };

#[repr(transparent)] pub struct OrgKdeKwinAppmenu(pub(crate) Proxy);
unsafe impl Interface for OrgKdeKwinAppmenu {
    const DEF: *const ffi::Interface = &ORG_KDE_KWIN_APPMENU_INTERFACE;

    #[cfg_attr(feature = "tracing", tracing::instrument(name = "<OrgKdeKwinAppmenu as Interface>::destruct", skip(self)))]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(1);
    }
}
impl ProxyObject for OrgKdeKwinAppmenu { #[inline(always)] fn as_proxy(&self) -> &Proxy { &self.0 } }
impl OrgKdeKwinAppmenu {
    #[inline(always)] pub fn set_user_data(&mut self, user_data: *mut core::ffi::c_void) { unsafe { self.0.set_user_data(user_data); } }
    #[inline(always)] pub fn user_data(&mut self) -> *mut core::ffi::c_void { unsafe { self.0.user_data() } }

    #[inline] pub fn set_address(&self,service_name: &core::ffi::CStr,object_path: &core::ffi::CStr,) -> crate::Result<()> {
        self.0.marshal_array_void(0, &mut [ffi::Argument { s: service_name.as_ptr() },ffi::Argument { s: object_path.as_ptr() },])
    }

 }

