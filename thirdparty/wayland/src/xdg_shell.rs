use crate::{Interface, Proxy, ffi};

static XDG_WM_BASE_INTERFACE: ffi::Interface = ffi::Interface {
    name: c"xdg_wm_base".as_ptr(),
    version: 7,
    method_count: 4,
    methods: const {
        [
            ffi::Message {
                name: c"destroy".as_ptr(),
                signature: c"".as_ptr(),
                types: const { [] }.as_ptr(),
            },
            ffi::Message {
                name: c"create_positioner".as_ptr(),
                signature: c"n".as_ptr(),
                types: const { [crate::XdgPositioner::DEF as *const _] }.as_ptr(),
            },
            ffi::Message {
                name: c"get_xdg_surface".as_ptr(),
                signature: c"no".as_ptr(),
                types: const {
                    [
                        crate::XdgSurface::DEF as *const _,
                        crate::Surface::DEF as *const _,
                    ]
                }
                .as_ptr(),
            },
            ffi::Message {
                name: c"pong".as_ptr(),
                signature: c"u".as_ptr(),
                types: const { [core::ptr::null()] }.as_ptr(),
            },
        ]
    }
    .as_ptr(),
    event_count: 1,
    events: const {
        [ffi::Message {
            name: c"ping".as_ptr(),
            signature: c"u".as_ptr(),
            types: const { [core::ptr::null()] }.as_ptr(),
        }]
    }
    .as_ptr(),
};

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
        extern "C" fn ping<L: XdgWmBaseEventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            serial: u32,
        ) {
            L::ping(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                serial,
            )
        }

        #[repr(C)]
        struct FPTable {
            ping:
                extern "C" fn(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy, serial: u32),
        }
        unsafe {
            self.0.set_listener(
                &const { FPTable { ping: ping::<L> } } as &'static FPTable as *const _ as _,
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

static XDG_POSITIONER_INTERFACE: ffi::Interface = ffi::Interface {
    name: c"xdg_positioner".as_ptr(),
    version: 7,
    method_count: 10,
    methods: const {
        [
            ffi::Message {
                name: c"destroy".as_ptr(),
                signature: c"".as_ptr(),
                types: const { [] }.as_ptr(),
            },
            ffi::Message {
                name: c"set_size".as_ptr(),
                signature: c"ii".as_ptr(),
                types: const { [core::ptr::null(), core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"set_anchor_rect".as_ptr(),
                signature: c"iiii".as_ptr(),
                types: const {
                    [
                        core::ptr::null(),
                        core::ptr::null(),
                        core::ptr::null(),
                        core::ptr::null(),
                    ]
                }
                .as_ptr(),
            },
            ffi::Message {
                name: c"set_anchor".as_ptr(),
                signature: c"u".as_ptr(),
                types: const { [core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"set_gravity".as_ptr(),
                signature: c"u".as_ptr(),
                types: const { [core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"set_constraint_adjustment".as_ptr(),
                signature: c"u".as_ptr(),
                types: const { [core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"set_offset".as_ptr(),
                signature: c"ii".as_ptr(),
                types: const { [core::ptr::null(), core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"set_reactive".as_ptr(),
                signature: c"3".as_ptr(),
                types: const { [] }.as_ptr(),
            },
            ffi::Message {
                name: c"set_parent_size".as_ptr(),
                signature: c"3ii".as_ptr(),
                types: const { [core::ptr::null(), core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"set_parent_configure".as_ptr(),
                signature: c"3u".as_ptr(),
                types: const { [core::ptr::null()] }.as_ptr(),
            },
        ]
    }
    .as_ptr(),
    event_count: 0,
    events: const { [] }.as_ptr(),
};

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

static XDG_SURFACE_INTERFACE: ffi::Interface = ffi::Interface {
    name: c"xdg_surface".as_ptr(),
    version: 7,
    method_count: 5,
    methods: const {
        [
            ffi::Message {
                name: c"destroy".as_ptr(),
                signature: c"".as_ptr(),
                types: const { [] }.as_ptr(),
            },
            ffi::Message {
                name: c"get_toplevel".as_ptr(),
                signature: c"n".as_ptr(),
                types: const { [crate::XdgToplevel::DEF as *const _] }.as_ptr(),
            },
            ffi::Message {
                name: c"get_popup".as_ptr(),
                signature: c"n?oo".as_ptr(),
                types: const {
                    [
                        crate::XdgPopup::DEF as *const _,
                        &XDG_SURFACE_INTERFACE as *const _,
                        crate::XdgPositioner::DEF as *const _,
                    ]
                }
                .as_ptr(),
            },
            ffi::Message {
                name: c"set_window_geometry".as_ptr(),
                signature: c"iiii".as_ptr(),
                types: const {
                    [
                        core::ptr::null(),
                        core::ptr::null(),
                        core::ptr::null(),
                        core::ptr::null(),
                    ]
                }
                .as_ptr(),
            },
            ffi::Message {
                name: c"ack_configure".as_ptr(),
                signature: c"u".as_ptr(),
                types: const { [core::ptr::null()] }.as_ptr(),
            },
        ]
    }
    .as_ptr(),
    event_count: 1,
    events: const {
        [ffi::Message {
            name: c"configure".as_ptr(),
            signature: c"u".as_ptr(),
            types: const { [core::ptr::null()] }.as_ptr(),
        }]
    }
    .as_ptr(),
};

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
        extern "C" fn configure<L: XdgSurfaceEventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            serial: u32,
        ) {
            L::configure(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                serial,
            )
        }

        #[repr(C)]
        struct FPTable {
            configure:
                extern "C" fn(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy, serial: u32),
        }
        unsafe {
            self.0.set_listener(
                &const {
                    FPTable {
                        configure: configure::<L>,
                    }
                } as &'static FPTable as *const _ as _,
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

static XDG_TOPLEVEL_INTERFACE: ffi::Interface = ffi::Interface {
    name: c"xdg_toplevel".as_ptr(),
    version: 7,
    method_count: 14,
    methods: const {
        [
            ffi::Message {
                name: c"destroy".as_ptr(),
                signature: c"".as_ptr(),
                types: const { [] }.as_ptr(),
            },
            ffi::Message {
                name: c"set_parent".as_ptr(),
                signature: c"?o".as_ptr(),
                types: const { [&XDG_TOPLEVEL_INTERFACE as *const _] }.as_ptr(),
            },
            ffi::Message {
                name: c"set_title".as_ptr(),
                signature: c"s".as_ptr(),
                types: const { [core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"set_app_id".as_ptr(),
                signature: c"s".as_ptr(),
                types: const { [core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"show_window_menu".as_ptr(),
                signature: c"ouii".as_ptr(),
                types: const {
                    [
                        crate::Seat::DEF as *const _,
                        core::ptr::null(),
                        core::ptr::null(),
                        core::ptr::null(),
                    ]
                }
                .as_ptr(),
            },
            ffi::Message {
                name: c"move".as_ptr(),
                signature: c"ou".as_ptr(),
                types: const { [crate::Seat::DEF as *const _, core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"resize".as_ptr(),
                signature: c"ouu".as_ptr(),
                types: const {
                    [
                        crate::Seat::DEF as *const _,
                        core::ptr::null(),
                        core::ptr::null(),
                    ]
                }
                .as_ptr(),
            },
            ffi::Message {
                name: c"set_max_size".as_ptr(),
                signature: c"ii".as_ptr(),
                types: const { [core::ptr::null(), core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"set_min_size".as_ptr(),
                signature: c"ii".as_ptr(),
                types: const { [core::ptr::null(), core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"set_maximized".as_ptr(),
                signature: c"".as_ptr(),
                types: const { [] }.as_ptr(),
            },
            ffi::Message {
                name: c"unset_maximized".as_ptr(),
                signature: c"".as_ptr(),
                types: const { [] }.as_ptr(),
            },
            ffi::Message {
                name: c"set_fullscreen".as_ptr(),
                signature: c"?o".as_ptr(),
                types: const { [crate::Output::DEF as *const _] }.as_ptr(),
            },
            ffi::Message {
                name: c"unset_fullscreen".as_ptr(),
                signature: c"".as_ptr(),
                types: const { [] }.as_ptr(),
            },
            ffi::Message {
                name: c"set_minimized".as_ptr(),
                signature: c"".as_ptr(),
                types: const { [] }.as_ptr(),
            },
        ]
    }
    .as_ptr(),
    event_count: 4,
    events: const {
        [
            ffi::Message {
                name: c"configure".as_ptr(),
                signature: c"iia".as_ptr(),
                types: const { [core::ptr::null(), core::ptr::null(), core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"close".as_ptr(),
                signature: c"".as_ptr(),
                types: const { [] }.as_ptr(),
            },
            ffi::Message {
                name: c"configure_bounds".as_ptr(),
                signature: c"4ii".as_ptr(),
                types: const { [core::ptr::null(), core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"wm_capabilities".as_ptr(),
                signature: c"5a".as_ptr(),
                types: const { [core::ptr::null()] }.as_ptr(),
            },
        ]
    }
    .as_ptr(),
};

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
        extern "C" fn configure<L: XdgToplevelEventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            width: i32,
            height: i32,
            states: *mut ffi::Array,
        ) {
            L::configure(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                width,
                height,
                unsafe { &mut *states },
            )
        }
        extern "C" fn close<L: XdgToplevelEventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
        ) {
            L::close(unsafe { &mut *(data0 as *mut _) }, unsafe {
                &mut *(sender0 as *mut _)
            })
        }
        extern "C" fn configure_bounds<L: XdgToplevelEventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            width: i32,
            height: i32,
        ) {
            L::configure_bounds(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                width,
                height,
            )
        }
        extern "C" fn wm_capabilities<L: XdgToplevelEventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            capabilities: *mut ffi::Array,
        ) {
            L::wm_capabilities(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                unsafe { &mut *capabilities },
            )
        }

        #[repr(C)]
        struct FPTable {
            configure: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                width: i32,
                height: i32,
                states: *mut ffi::Array,
            ),
            close: extern "C" fn(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy),
            configure_bounds: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                width: i32,
                height: i32,
            ),
            wm_capabilities: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                capabilities: *mut ffi::Array,
            ),
        }
        unsafe {
            self.0.set_listener(
                &const {
                    FPTable {
                        configure: configure::<L>,
                        close: close::<L>,
                        configure_bounds: configure_bounds::<L>,
                        wm_capabilities: wm_capabilities::<L>,
                    }
                } as &'static FPTable as *const _ as _,
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

static XDG_POPUP_INTERFACE: ffi::Interface = ffi::Interface {
    name: c"xdg_popup".as_ptr(),
    version: 7,
    method_count: 3,
    methods: const {
        [
            ffi::Message {
                name: c"destroy".as_ptr(),
                signature: c"".as_ptr(),
                types: const { [] }.as_ptr(),
            },
            ffi::Message {
                name: c"grab".as_ptr(),
                signature: c"ou".as_ptr(),
                types: const { [crate::Seat::DEF as *const _, core::ptr::null()] }.as_ptr(),
            },
            ffi::Message {
                name: c"reposition".as_ptr(),
                signature: c"3ou".as_ptr(),
                types: const { [crate::XdgPositioner::DEF as *const _, core::ptr::null()] }
                    .as_ptr(),
            },
        ]
    }
    .as_ptr(),
    event_count: 3,
    events: const {
        [
            ffi::Message {
                name: c"configure".as_ptr(),
                signature: c"iiii".as_ptr(),
                types: const {
                    [
                        core::ptr::null(),
                        core::ptr::null(),
                        core::ptr::null(),
                        core::ptr::null(),
                    ]
                }
                .as_ptr(),
            },
            ffi::Message {
                name: c"popup_done".as_ptr(),
                signature: c"".as_ptr(),
                types: const { [] }.as_ptr(),
            },
            ffi::Message {
                name: c"repositioned".as_ptr(),
                signature: c"3u".as_ptr(),
                types: const { [core::ptr::null()] }.as_ptr(),
            },
        ]
    }
    .as_ptr(),
};

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
        extern "C" fn configure<L: XdgPopupEventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            x: i32,
            y: i32,
            width: i32,
            height: i32,
        ) {
            L::configure(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                x,
                y,
                width,
                height,
            )
        }
        extern "C" fn popup_done<L: XdgPopupEventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
        ) {
            L::popup_done(unsafe { &mut *(data0 as *mut _) }, unsafe {
                &mut *(sender0 as *mut _)
            })
        }
        extern "C" fn repositioned<L: XdgPopupEventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            token: u32,
        ) {
            L::repositioned(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                token,
            )
        }

        #[repr(C)]
        struct FPTable {
            configure: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                x: i32,
                y: i32,
                width: i32,
                height: i32,
            ),
            popup_done: extern "C" fn(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy),
            repositioned:
                extern "C" fn(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy, token: u32),
        }
        unsafe {
            self.0.set_listener(
                &const {
                    FPTable {
                        configure: configure::<L>,
                        popup_done: popup_done::<L>,
                        repositioned: repositioned::<L>,
                    }
                } as &'static FPTable as *const _ as _,
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
