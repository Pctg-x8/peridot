use crate::{ffi, Proxy, Interface};

static ORG_KDE_KWIN_BLUR_MANAGER_INTERFACE: ffi::Interface = ffi::Interface { name: c"org_kde_kwin_blur_manager".as_ptr(), version: 1, method_count: 2, methods: const { [ffi::Message { name: c"create".as_ptr(), signature: c"no".as_ptr(), types: const { [crate::OrgKdeKwinBlur::DEF,crate::Surface::DEF,] }.as_ptr() },ffi::Message { name: c"unset".as_ptr(), signature: c"o".as_ptr(), types: const { [crate::Surface::DEF,] }.as_ptr() },] }.as_ptr(), event_count: 0, events: const { [] }.as_ptr() };

#[repr(transparent)] pub struct OrgKdeKwinBlurManager(pub(crate) Proxy);
unsafe impl Interface for OrgKdeKwinBlurManager {
    const DEF: *const ffi::Interface = &ORG_KDE_KWIN_BLUR_MANAGER_INTERFACE;
}

impl OrgKdeKwinBlurManager {
    #[inline(always)] pub fn set_user_data(&mut self, user_data: *mut core::ffi::c_void) { unsafe { self.0.set_user_data(user_data); } }
    #[inline(always)] pub fn user_data(&mut self) -> *mut core::ffi::c_void { unsafe { self.0.user_data() } }

    #[inline] pub fn create(&self,surface: &crate::Surface,) -> crate::Result<crate::Owned<crate::OrgKdeKwinBlur>> {
        Ok(unsafe { crate::Owned::wrap_unchecked(self.0.marshal_array_typed(0, &mut [crate::NEWID_ARG,surface.0.as_arg(),])?) })
    }

    #[inline] pub fn unset(&self,surface: &crate::Surface,) -> crate::Result<()> {
        self.0.marshal_array_void(1, &mut [surface.0.as_arg(),])
    }

 }

static ORG_KDE_KWIN_BLUR_INTERFACE: ffi::Interface = ffi::Interface { name: c"org_kde_kwin_blur".as_ptr(), version: 1, method_count: 3, methods: const { [ffi::Message { name: c"commit".as_ptr(), signature: c"".as_ptr(), types: const { [] }.as_ptr() },ffi::Message { name: c"set_region".as_ptr(), signature: c"?o".as_ptr(), types: const { [crate::Region::DEF,] }.as_ptr() },ffi::Message { name: c"release".as_ptr(), signature: c"".as_ptr(), types: const { [] }.as_ptr() },] }.as_ptr(), event_count: 0, events: const { [] }.as_ptr() };

#[repr(transparent)] pub struct OrgKdeKwinBlur(pub(crate) Proxy);
unsafe impl Interface for OrgKdeKwinBlur {
    const DEF: *const ffi::Interface = &ORG_KDE_KWIN_BLUR_INTERFACE;

    #[cfg_attr(feature = "tracing", tracing::instrument(name = "<OrgKdeKwinBlur as Interface>::destruct", skip(self)))]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(2);
    }
}

impl OrgKdeKwinBlur {
    #[inline(always)] pub fn set_user_data(&mut self, user_data: *mut core::ffi::c_void) { unsafe { self.0.set_user_data(user_data); } }
    #[inline(always)] pub fn user_data(&mut self) -> *mut core::ffi::c_void { unsafe { self.0.user_data() } }

    #[inline] pub fn commit(&self,) -> crate::Result<()> {
        self.0.marshal_array_void(0, &mut [])
    }

    #[inline] pub fn set_region(&self,region: Option<&crate::Region>,) -> crate::Result<()> {
        self.0.marshal_array_void(1, &mut [region.map_or(crate::NULLOBJ_ARG, |x| x.0.as_arg()),])
    }

 }

