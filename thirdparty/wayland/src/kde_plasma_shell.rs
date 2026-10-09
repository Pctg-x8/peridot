use crate::{ffi, Proxy, ProxyObject, Interface};

static ORG_KDE_PLASMA_SHELL_INTERFACE: ffi::Interface = ffi::Interface { name: c"org_kde_plasma_shell".as_ptr(), version: 8, method_count: 1, methods: const { [ffi::Message { name: c"get_surface".as_ptr(), signature: c"no".as_ptr(), types: const { [crate::OrgKdePlasmaSurface::DEF,crate::Surface::DEF,] }.as_ptr() },] }.as_ptr(), event_count: 0, events: const { [] }.as_ptr() };

#[repr(transparent)] pub struct OrgKdePlasmaShell(pub(crate) Proxy);
unsafe impl Interface for OrgKdePlasmaShell {
    const DEF: *const ffi::Interface = &ORG_KDE_PLASMA_SHELL_INTERFACE;
}
impl ProxyObject for OrgKdePlasmaShell { #[inline(always)] fn as_proxy(&self) -> &Proxy { &self.0 } }
impl OrgKdePlasmaShell {
    #[inline(always)] pub fn set_user_data(&mut self, user_data: *mut core::ffi::c_void) { unsafe { self.0.set_user_data(user_data); } }
    #[inline(always)] pub fn user_data(&mut self) -> *mut core::ffi::c_void { unsafe { self.0.user_data() } }

    #[inline] pub fn get_surface(&self,surface: &crate::Surface,) -> crate::Result<crate::Owned<crate::OrgKdePlasmaSurface>> {
        Ok(unsafe { crate::Owned::wrap_unchecked(self.0.marshal_array_typed(0, &mut [crate::NEWID_ARG,surface.0.as_arg(),])?) })
    }

 }

static ORG_KDE_PLASMA_SURFACE_INTERFACE: ffi::Interface = ffi::Interface { name: c"org_kde_plasma_surface".as_ptr(), version: 8, method_count: 11, methods: const { [ffi::Message { name: c"destroy".as_ptr(), signature: c"".as_ptr(), types: const { [] }.as_ptr() },ffi::Message { name: c"set_output".as_ptr(), signature: c"o".as_ptr(), types: const { [crate::Output::DEF,] }.as_ptr() },ffi::Message { name: c"set_position".as_ptr(), signature: c"ii".as_ptr(), types: const { [core::ptr::null(),core::ptr::null(),] }.as_ptr() },ffi::Message { name: c"set_role".as_ptr(), signature: c"u".as_ptr(), types: const { [core::ptr::null(),] }.as_ptr() },ffi::Message { name: c"set_panel_behavior".as_ptr(), signature: c"u".as_ptr(), types: const { [core::ptr::null(),] }.as_ptr() },ffi::Message { name: c"set_skip_taskbar".as_ptr(), signature: c"2u".as_ptr(), types: const { [core::ptr::null(),] }.as_ptr() },ffi::Message { name: c"panel_auto_hide_hide".as_ptr(), signature: c"4".as_ptr(), types: const { [] }.as_ptr() },ffi::Message { name: c"panel_auto_hide_show".as_ptr(), signature: c"4".as_ptr(), types: const { [] }.as_ptr() },ffi::Message { name: c"set_panel_takes_focus".as_ptr(), signature: c"4u".as_ptr(), types: const { [core::ptr::null(),] }.as_ptr() },ffi::Message { name: c"set_skip_switcher".as_ptr(), signature: c"5u".as_ptr(), types: const { [core::ptr::null(),] }.as_ptr() },ffi::Message { name: c"open_under_cursor".as_ptr(), signature: c"7".as_ptr(), types: const { [] }.as_ptr() },] }.as_ptr(), event_count: 2, events: const { [ffi::Message { name: c"auto_hidden_panel_hidden".as_ptr(), signature: c"4".as_ptr(), types: const { [] }.as_ptr() },ffi::Message { name: c"auto_hidden_panel_shown".as_ptr(), signature: c"4".as_ptr(), types: const { [] }.as_ptr() },] }.as_ptr() };

#[repr(transparent)] pub struct OrgKdePlasmaSurface(pub(crate) Proxy);
unsafe impl Interface for OrgKdePlasmaSurface {
    const DEF: *const ffi::Interface = &ORG_KDE_PLASMA_SURFACE_INTERFACE;

    #[cfg_attr(feature = "tracing", tracing::instrument(name = "<OrgKdePlasmaSurface as Interface>::destruct", skip(self)))]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}
impl ProxyObject for OrgKdePlasmaSurface { #[inline(always)] fn as_proxy(&self) -> &Proxy { &self.0 } }
impl OrgKdePlasmaSurface {
    pub fn set_listener<'l, L: OrgKdePlasmaSurfaceEventListener + 'l>(&'l mut self, listener: &'l mut L) -> crate::SetListenerResult {
        extern "C" fn auto_hidden_panel_hidden<L: OrgKdePlasmaSurfaceEventListener>(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy,) { L::auto_hidden_panel_hidden(unsafe { &mut *(data0 as *mut _) }, unsafe { &mut *(sender0 as *mut _) },) }
extern "C" fn auto_hidden_panel_shown<L: OrgKdePlasmaSurfaceEventListener>(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy,) { L::auto_hidden_panel_shown(unsafe { &mut *(data0 as *mut _) }, unsafe { &mut *(sender0 as *mut _) },) }

        #[repr(C)] struct FPTable { auto_hidden_panel_hidden: extern "C" fn(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy, ),
auto_hidden_panel_shown: extern "C" fn(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy, ),
 }
        unsafe { self.0.set_listener(&const { FPTable { auto_hidden_panel_hidden: auto_hidden_panel_hidden::<L>,
auto_hidden_panel_shown: auto_hidden_panel_shown::<L>,
 } } as &'static FPTable as *const _ as _,listener as *mut _ as _) }
    }

    #[inline(always)] pub fn set_user_data(&mut self, user_data: *mut core::ffi::c_void) { unsafe { self.0.set_user_data(user_data); } }
    #[inline(always)] pub fn user_data(&mut self) -> *mut core::ffi::c_void { unsafe { self.0.user_data() } }

    #[inline] pub fn set_output(&self,output: &crate::Output,) -> crate::Result<()> {
        self.0.marshal_array_void(1, &mut [output.0.as_arg(),])
    }

    #[inline] pub fn set_position(&self,x: i32,y: i32,) -> crate::Result<()> {
        self.0.marshal_array_void(2, &mut [ffi::Argument { i: x },ffi::Argument { i: y },])
    }

    #[inline] pub fn set_role(&self,role: u32,) -> crate::Result<()> {
        self.0.marshal_array_void(3, &mut [ffi::Argument { u: role },])
    }

    #[inline] pub fn set_panel_behavior(&self,flag: u32,) -> crate::Result<()> {
        self.0.marshal_array_void(4, &mut [ffi::Argument { u: flag },])
    }

    #[inline] pub fn set_skip_taskbar(&self,skip: u32,) -> crate::Result<()> {
        self.0.marshal_array_void(5, &mut [ffi::Argument { u: skip },])
    }

    #[inline] pub fn panel_auto_hide_hide(&self,) -> crate::Result<()> {
        self.0.marshal_array_void(6, &mut [])
    }

    #[inline] pub fn panel_auto_hide_show(&self,) -> crate::Result<()> {
        self.0.marshal_array_void(7, &mut [])
    }

    #[inline] pub fn set_panel_takes_focus(&self,takes_focus: u32,) -> crate::Result<()> {
        self.0.marshal_array_void(8, &mut [ffi::Argument { u: takes_focus },])
    }

    #[inline] pub fn set_skip_switcher(&self,skip: u32,) -> crate::Result<()> {
        self.0.marshal_array_void(9, &mut [ffi::Argument { u: skip },])
    }

    #[inline] pub fn open_under_cursor(&self,) -> crate::Result<()> {
        self.0.marshal_array_void(10, &mut [])
    }

 }

pub trait OrgKdePlasmaSurfaceEventListener {     fn auto_hidden_panel_hidden(&mut self, sender: &mut OrgKdePlasmaSurface, );
    fn auto_hidden_panel_shown(&mut self, sender: &mut OrgKdePlasmaSurface, );
 }

#[repr(u32)] #[derive(Debug, Clone, Copy, PartialEq, Eq)] pub enum OrgKdePlasmaSurfaceRole {
    Normal = 0,
    Desktop = 1,
    Panel = 2,
    Onscreendisplay = 3,
    Notification = 4,
    Tooltip = 5,
    Criticalnotification = 6,
    Appletpopup = 7,
}
impl OrgKdePlasmaSurfaceRole { pub const fn as_arg(&self) -> ffi::Argument { ffi::Argument { u: *self as _ } } }

#[repr(u32)] #[derive(Debug, Clone, Copy, PartialEq, Eq)] pub enum OrgKdePlasmaSurfacePanelBehavior {
    AlwaysVisible = 1,
    AutoHide = 2,
    WindowsCanCover = 3,
    WindowsGoBelow = 4,
}
impl OrgKdePlasmaSurfacePanelBehavior { pub const fn as_arg(&self) -> ffi::Argument { ffi::Argument { u: *self as _ } } }

#[repr(u32)] #[derive(Debug, Clone, Copy, PartialEq, Eq)] pub enum OrgKdePlasmaSurfaceError {
    PanelNotAutoHide = 0,
}
impl OrgKdePlasmaSurfaceError { pub const fn as_arg(&self) -> ffi::Argument { ffi::Argument { u: *self as _ } } }

