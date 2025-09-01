use crate::{Interface, Proxy, ffi, interface, message};

static XDG_WM_BASE_INTERFACE: ffi::Interface = interface(
    c"xdg_wm_base",
    7,
    &[
        message(c"destroy", c"", &[]),
        message(c"create_positioner", c"n", &[crate::XdgPositioner::DEF]),
        message(
            c"get_xdg_surface",
            c"no",
            &[crate::XdgSurface::DEF, crate::Surface::DEF],
        ),
        message(c"pong", c"u", &[core::ptr::null()]),
    ],
    &[message(c"ping", c"u", &[core::ptr::null()])],
);

#[repr(transparent)]
pub struct XdgWmBase(pub(crate) Proxy);
unsafe impl Interface for XdgWmBase {
    const DEF: &'static ffi::Interface = &XDG_WM_BASE_INTERFACE;

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<XdgWmBase as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}

impl XdgWmBase {
    pub fn set_listener<'l, L: XdgWmBaseEventListener + 'l>(
        &'l mut self,
        listener: &'l mut L,
    ) -> crate::SetListenerResult {
        unsafe {
            self.0.set_listener(
                crate::EventFnTable!(for L: XdgWmBaseEventListener {
                    ping(serial: u32 => serial,),

                }) as *const _ as _,
                listener as *mut _ as _,
            )
        }
    }

    #[inline]
    pub fn create_positioner(&self) -> crate::Result<crate::Owned<crate::XdgPositioner>> {
        Ok(unsafe {
            crate::Owned::wrap_unchecked(self.0.marshal_array_typed(1, &mut [crate::NEWID_ARG])?)
        })
    }

    #[inline]
    pub fn get_xdg_surface(
        &self,
        surface: &crate::Surface,
    ) -> crate::Result<crate::Owned<crate::XdgSurface>> {
        Ok(unsafe {
            crate::Owned::wrap_unchecked(
                self.0
                    .marshal_array_typed(2, &mut [crate::NEWID_ARG, surface.0.as_arg()])?,
            )
        })
    }

    #[inline]
    pub fn pong(&self, serial: u32) -> crate::Result<()> {
        self.0
            .marshal_array_void(3, &mut [ffi::Argument { u: serial }])
    }
}

pub trait XdgWmBaseEventListener {
    fn ping(&mut self, sender: &mut XdgWmBase, serial: u32);
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XdgWmBaseError {
    Role = 0,
    DefunctSurfaces = 1,
    NotTheTopmostPopup = 2,
    InvalidPopupParent = 3,
    InvalidSurfaceState = 4,
    InvalidPositioner = 5,
    Unresponsive = 6,
}

static XDG_POSITIONER_INTERFACE: ffi::Interface = interface(
    c"xdg_positioner",
    7,
    &[
        message(c"destroy", c"", &[]),
        message(c"set_size", c"ii", &[core::ptr::null(), core::ptr::null()]),
        message(
            c"set_anchor_rect",
            c"iiii",
            &[
                core::ptr::null(),
                core::ptr::null(),
                core::ptr::null(),
                core::ptr::null(),
            ],
        ),
        message(c"set_anchor", c"u", &[core::ptr::null()]),
        message(c"set_gravity", c"u", &[core::ptr::null()]),
        message(c"set_constraint_adjustment", c"u", &[core::ptr::null()]),
        message(
            c"set_offset",
            c"ii",
            &[core::ptr::null(), core::ptr::null()],
        ),
        message(c"set_reactive", c"3", &[]),
        message(
            c"set_parent_size",
            c"3ii",
            &[core::ptr::null(), core::ptr::null()],
        ),
        message(c"set_parent_configure", c"3u", &[core::ptr::null()]),
    ],
    &[],
);

#[repr(transparent)]
pub struct XdgPositioner(pub(crate) Proxy);
unsafe impl Interface for XdgPositioner {
    const DEF: &'static ffi::Interface = &XDG_POSITIONER_INTERFACE;

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<XdgPositioner as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}

impl XdgPositioner {
    #[inline]
    pub fn set_size(&self, width: i32, height: i32) -> crate::Result<()> {
        self.0.marshal_array_void(
            1,
            &mut [ffi::Argument { i: width }, ffi::Argument { i: height }],
        )
    }

    #[inline]
    pub fn set_anchor_rect(&self, x: i32, y: i32, width: i32, height: i32) -> crate::Result<()> {
        self.0.marshal_array_void(
            2,
            &mut [
                ffi::Argument { i: x },
                ffi::Argument { i: y },
                ffi::Argument { i: width },
                ffi::Argument { i: height },
            ],
        )
    }

    #[inline]
    pub fn set_anchor(&self, anchor: XdgPositionerAnchor) -> crate::Result<()> {
        self.0
            .marshal_array_void(3, &mut [ffi::Argument { u: anchor as _ }])
    }

    #[inline]
    pub fn set_gravity(&self, gravity: XdgPositionerGravity) -> crate::Result<()> {
        self.0
            .marshal_array_void(4, &mut [ffi::Argument { u: gravity as _ }])
    }

    #[inline]
    pub fn set_constraint_adjustment(
        &self,
        constraint_adjustment: XdgPositionerConstraintAdjustment,
    ) -> crate::Result<()> {
        self.0.marshal_array_void(
            5,
            &mut [ffi::Argument {
                u: constraint_adjustment as _,
            }],
        )
    }

    #[inline]
    pub fn set_offset(&self, x: i32, y: i32) -> crate::Result<()> {
        self.0
            .marshal_array_void(6, &mut [ffi::Argument { i: x }, ffi::Argument { i: y }])
    }

    #[inline]
    pub fn set_reactive(&self) -> crate::Result<()> {
        self.0.marshal_array_void(7, &mut [])
    }

    #[inline]
    pub fn set_parent_size(&self, parent_width: i32, parent_height: i32) -> crate::Result<()> {
        self.0.marshal_array_void(
            8,
            &mut [
                ffi::Argument { i: parent_width },
                ffi::Argument { i: parent_height },
            ],
        )
    }

    #[inline]
    pub fn set_parent_configure(&self, serial: u32) -> crate::Result<()> {
        self.0
            .marshal_array_void(9, &mut [ffi::Argument { u: serial }])
    }
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XdgPositionerError {
    InvalidInput = 0,
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XdgPositionerAnchor {
    None = 0,
    Top = 1,
    Bottom = 2,
    Left = 3,
    Right = 4,
    TopLeft = 5,
    BottomLeft = 6,
    TopRight = 7,
    BottomRight = 8,
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XdgPositionerGravity {
    None = 0,
    Top = 1,
    Bottom = 2,
    Left = 3,
    Right = 4,
    TopLeft = 5,
    BottomLeft = 6,
    TopRight = 7,
    BottomRight = 8,
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XdgPositionerConstraintAdjustment {
    None = 0,
    SlideX = 1,
    SlideY = 2,
    FlipX = 4,
    FlipY = 8,
    ResizeX = 16,
    ResizeY = 32,
}

static XDG_SURFACE_INTERFACE: ffi::Interface = interface(
    c"xdg_surface",
    7,
    &[
        message(c"destroy", c"", &[]),
        message(c"get_toplevel", c"n", &[crate::XdgToplevel::DEF]),
        message(
            c"get_popup",
            c"n?oo",
            &[
                crate::XdgPopup::DEF,
                &XDG_SURFACE_INTERFACE,
                crate::XdgPositioner::DEF,
            ],
        ),
        message(
            c"set_window_geometry",
            c"iiii",
            &[
                core::ptr::null(),
                core::ptr::null(),
                core::ptr::null(),
                core::ptr::null(),
            ],
        ),
        message(c"ack_configure", c"u", &[core::ptr::null()]),
    ],
    &[message(c"configure", c"u", &[core::ptr::null()])],
);

#[repr(transparent)]
pub struct XdgSurface(pub(crate) Proxy);
unsafe impl Interface for XdgSurface {
    const DEF: &'static ffi::Interface = &XDG_SURFACE_INTERFACE;

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<XdgSurface as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}

impl XdgSurface {
    pub fn set_listener<'l, L: XdgSurfaceEventListener + 'l>(
        &'l mut self,
        listener: &'l mut L,
    ) -> crate::SetListenerResult {
        unsafe {
            self.0.set_listener(
                crate::EventFnTable!(for L: XdgSurfaceEventListener {
                    configure(serial: u32 => serial,),

                }) as *const _ as _,
                listener as *mut _ as _,
            )
        }
    }

    #[inline]
    pub fn get_toplevel(&self) -> crate::Result<crate::Owned<crate::XdgToplevel>> {
        Ok(unsafe {
            crate::Owned::wrap_unchecked(self.0.marshal_array_typed(1, &mut [crate::NEWID_ARG])?)
        })
    }

    #[inline]
    pub fn get_popup(
        &self,
        parent: Option<&crate::XdgSurface>,
        positioner: &crate::XdgPositioner,
    ) -> crate::Result<crate::Owned<crate::XdgPopup>> {
        Ok(unsafe {
            crate::Owned::wrap_unchecked(self.0.marshal_array_typed(
                2,
                &mut [
                    crate::NEWID_ARG,
                    parent.map_or(crate::NULLOBJ_ARG, |x| x.0.as_arg()),
                    positioner.0.as_arg(),
                ],
            )?)
        })
    }

    #[inline]
    pub fn set_window_geometry(
        &self,
        x: i32,
        y: i32,
        width: i32,
        height: i32,
    ) -> crate::Result<()> {
        self.0.marshal_array_void(
            3,
            &mut [
                ffi::Argument { i: x },
                ffi::Argument { i: y },
                ffi::Argument { i: width },
                ffi::Argument { i: height },
            ],
        )
    }

    #[inline]
    pub fn ack_configure(&self, serial: u32) -> crate::Result<()> {
        self.0
            .marshal_array_void(4, &mut [ffi::Argument { u: serial }])
    }
}

pub trait XdgSurfaceEventListener {
    fn configure(&mut self, sender: &mut XdgSurface, serial: u32);
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XdgSurfaceError {
    NotConstructed = 1,
    AlreadyConstructed = 2,
    UnconfiguredBuffer = 3,
    InvalidSerial = 4,
    InvalidSize = 5,
    DefunctRoleObject = 6,
}

static XDG_TOPLEVEL_INTERFACE: ffi::Interface = interface(
    c"xdg_toplevel",
    7,
    &[
        message(c"destroy", c"", &[]),
        message(c"set_parent", c"?o", &[&XDG_TOPLEVEL_INTERFACE]),
        message(c"set_title", c"s", &[core::ptr::null()]),
        message(c"set_app_id", c"s", &[core::ptr::null()]),
        message(
            c"show_window_menu",
            c"ouii",
            &[
                crate::Seat::DEF,
                core::ptr::null(),
                core::ptr::null(),
                core::ptr::null(),
            ],
        ),
        message(c"move", c"ou", &[crate::Seat::DEF, core::ptr::null()]),
        message(
            c"resize",
            c"ouu",
            &[crate::Seat::DEF, core::ptr::null(), core::ptr::null()],
        ),
        message(
            c"set_max_size",
            c"ii",
            &[core::ptr::null(), core::ptr::null()],
        ),
        message(
            c"set_min_size",
            c"ii",
            &[core::ptr::null(), core::ptr::null()],
        ),
        message(c"set_maximized", c"", &[]),
        message(c"unset_maximized", c"", &[]),
        message(c"set_fullscreen", c"?o", &[crate::Output::DEF]),
        message(c"unset_fullscreen", c"", &[]),
        message(c"set_minimized", c"", &[]),
    ],
    &[
        message(
            c"configure",
            c"iia",
            &[core::ptr::null(), core::ptr::null(), core::ptr::null()],
        ),
        message(c"close", c"", &[]),
        message(
            c"configure_bounds",
            c"4ii",
            &[core::ptr::null(), core::ptr::null()],
        ),
        message(c"wm_capabilities", c"5a", &[core::ptr::null()]),
    ],
);

#[repr(transparent)]
pub struct XdgToplevel(pub(crate) Proxy);
unsafe impl Interface for XdgToplevel {
    const DEF: &'static ffi::Interface = &XDG_TOPLEVEL_INTERFACE;

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<XdgToplevel as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}

impl XdgToplevel {
    pub fn set_listener<'l, L: XdgToplevelEventListener + 'l>(
        &'l mut self,
        listener: &'l mut L,
    ) -> crate::SetListenerResult {
        unsafe {
            self.0.set_listener(
                crate::EventFnTable!(for L: XdgToplevelEventListener {
                    configure(width: i32 => width,height: i32 => height,states: *mut ffi::Array => unsafe { &mut *states },),
                    close(),
                    configure_bounds(width: i32 => width,height: i32 => height,),
                    wm_capabilities(capabilities: *mut ffi::Array => unsafe { &mut *capabilities },),

                }) as *const _ as _,
                listener as *mut _ as _,
            )
        }
    }

    #[inline]
    pub fn set_parent(&self, parent: Option<&crate::XdgToplevel>) -> crate::Result<()> {
        self.0.marshal_array_void(
            1,
            &mut [parent.map_or(crate::NULLOBJ_ARG, |x| x.0.as_arg())],
        )
    }

    #[inline]
    pub fn set_title(&self, title: &core::ffi::CStr) -> crate::Result<()> {
        self.0
            .marshal_array_void(2, &mut [ffi::Argument { s: title.as_ptr() }])
    }

    #[inline]
    pub fn set_app_id(&self, app_id: &core::ffi::CStr) -> crate::Result<()> {
        self.0
            .marshal_array_void(3, &mut [ffi::Argument { s: app_id.as_ptr() }])
    }

    #[inline]
    pub fn show_window_menu(
        &self,
        seat: &crate::Seat,
        serial: u32,
        x: i32,
        y: i32,
    ) -> crate::Result<()> {
        self.0.marshal_array_void(
            4,
            &mut [
                seat.0.as_arg(),
                ffi::Argument { u: serial },
                ffi::Argument { i: x },
                ffi::Argument { i: y },
            ],
        )
    }

    #[inline]
    pub fn r#move(&self, seat: &crate::Seat, serial: u32) -> crate::Result<()> {
        self.0
            .marshal_array_void(5, &mut [seat.0.as_arg(), ffi::Argument { u: serial }])
    }

    #[inline]
    pub fn resize(
        &self,
        seat: &crate::Seat,
        serial: u32,
        edges: XdgToplevelResizeEdge,
    ) -> crate::Result<()> {
        self.0.marshal_array_void(
            6,
            &mut [
                seat.0.as_arg(),
                ffi::Argument { u: serial },
                ffi::Argument { u: edges as _ },
            ],
        )
    }

    #[inline]
    pub fn set_max_size(&self, width: i32, height: i32) -> crate::Result<()> {
        self.0.marshal_array_void(
            7,
            &mut [ffi::Argument { i: width }, ffi::Argument { i: height }],
        )
    }

    #[inline]
    pub fn set_min_size(&self, width: i32, height: i32) -> crate::Result<()> {
        self.0.marshal_array_void(
            8,
            &mut [ffi::Argument { i: width }, ffi::Argument { i: height }],
        )
    }

    #[inline]
    pub fn set_maximized(&self) -> crate::Result<()> {
        self.0.marshal_array_void(9, &mut [])
    }

    #[inline]
    pub fn unset_maximized(&self) -> crate::Result<()> {
        self.0.marshal_array_void(10, &mut [])
    }

    #[inline]
    pub fn set_fullscreen(&self, output: Option<&crate::Output>) -> crate::Result<()> {
        self.0.marshal_array_void(
            11,
            &mut [output.map_or(crate::NULLOBJ_ARG, |x| x.0.as_arg())],
        )
    }

    #[inline]
    pub fn unset_fullscreen(&self) -> crate::Result<()> {
        self.0.marshal_array_void(12, &mut [])
    }

    #[inline]
    pub fn set_minimized(&self) -> crate::Result<()> {
        self.0.marshal_array_void(13, &mut [])
    }
}

pub trait XdgToplevelEventListener {
    fn configure(
        &mut self,
        sender: &mut XdgToplevel,
        width: i32,
        height: i32,
        states: &mut ffi::Array,
    );
    fn close(&mut self, sender: &mut XdgToplevel);
    fn configure_bounds(&mut self, sender: &mut XdgToplevel, width: i32, height: i32);
    fn wm_capabilities(&mut self, sender: &mut XdgToplevel, capabilities: &mut ffi::Array);
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XdgToplevelError {
    InvalidResizeEdge = 0,
    InvalidParent = 1,
    InvalidSize = 2,
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XdgToplevelResizeEdge {
    None = 0,
    Top = 1,
    Bottom = 2,
    Left = 4,
    TopLeft = 5,
    BottomLeft = 6,
    Right = 8,
    TopRight = 9,
    BottomRight = 10,
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XdgToplevelState {
    Maximized = 1,
    Fullscreen = 2,
    Resizing = 3,
    Activated = 4,
    TiledLeft = 5,
    TiledRight = 6,
    TiledTop = 7,
    TiledBottom = 8,
    Suspended = 9,
    ConstrainedLeft = 10,
    ConstrainedRight = 11,
    ConstrainedTop = 12,
    ConstrainedBottom = 13,
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XdgToplevelWmCapabilities {
    WindowMenu = 1,
    Maximize = 2,
    Fullscreen = 3,
    Minimize = 4,
}

static XDG_POPUP_INTERFACE: ffi::Interface = interface(
    c"xdg_popup",
    7,
    &[
        message(c"destroy", c"", &[]),
        message(c"grab", c"ou", &[crate::Seat::DEF, core::ptr::null()]),
        message(
            c"reposition",
            c"3ou",
            &[crate::XdgPositioner::DEF, core::ptr::null()],
        ),
    ],
    &[
        message(
            c"configure",
            c"iiii",
            &[
                core::ptr::null(),
                core::ptr::null(),
                core::ptr::null(),
                core::ptr::null(),
            ],
        ),
        message(c"popup_done", c"", &[]),
        message(c"repositioned", c"3u", &[core::ptr::null()]),
    ],
);

#[repr(transparent)]
pub struct XdgPopup(pub(crate) Proxy);
unsafe impl Interface for XdgPopup {
    const DEF: &'static ffi::Interface = &XDG_POPUP_INTERFACE;

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<XdgPopup as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}

impl XdgPopup {
    pub fn set_listener<'l, L: XdgPopupEventListener + 'l>(
        &'l mut self,
        listener: &'l mut L,
    ) -> crate::SetListenerResult {
        unsafe {
            self.0.set_listener(
                crate::EventFnTable!(for L: XdgPopupEventListener {
                    configure(x: i32 => x,y: i32 => y,width: i32 => width,height: i32 => height,),
                    popup_done(),
                    repositioned(token: u32 => token,),

                }) as *const _ as _,
                listener as *mut _ as _,
            )
        }
    }

    #[inline]
    pub fn grab(&self, seat: &crate::Seat, serial: u32) -> crate::Result<()> {
        self.0
            .marshal_array_void(1, &mut [seat.0.as_arg(), ffi::Argument { u: serial }])
    }

    #[inline]
    pub fn reposition(&self, positioner: &crate::XdgPositioner, token: u32) -> crate::Result<()> {
        self.0
            .marshal_array_void(2, &mut [positioner.0.as_arg(), ffi::Argument { u: token }])
    }
}

pub trait XdgPopupEventListener {
    fn configure(&mut self, sender: &mut XdgPopup, x: i32, y: i32, width: i32, height: i32);
    fn popup_done(&mut self, sender: &mut XdgPopup);
    fn repositioned(&mut self, sender: &mut XdgPopup, token: u32);
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XdgPopupError {
    InvalidGrab = 0,
}
