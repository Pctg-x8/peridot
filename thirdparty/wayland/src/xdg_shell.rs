use super::{Interface, NEWID_ARG, Owned, Proxy, ffi, interface, message};
use core::ptr::null;

static XDG_WM_BASE_INTERFACE: ffi::Interface = interface(
    c"xdg_wm_base",
    6,
    &[
        message(c"destroy", c"", &[]),
        message(c"create_positioner", c"n", &[&XDG_POSITIONER_INTERFACE]),
        message(
            c"get_xdg_surface",
            c"no",
            &[&XDG_SURFACE_INTERFACE, unsafe {
                &super::wl_surface_interface
            }],
        ),
        message(c"pong", c"u", &[core::ptr::null()]),
    ],
    &[message(c"ping", c"u", &[core::ptr::null()])],
);

#[repr(transparent)]
pub struct XdgWmBase(Proxy);
unsafe impl Interface for XdgWmBase {
    fn def() -> &'static ffi::Interface {
        &XDG_WM_BASE_INTERFACE
    }

    unsafe fn destruct(&mut self) {
        if let Err(e) = self
            .0
            .marshal_array_flags_void(0, ffi::MARSHAL_FLAG_DESTROY, &mut [])
        {
            let de = unsafe {
                ffi::wl_display_get_error(ffi::wl_proxy_get_display(&mut self.0 as *mut _ as _))
            };

            panic!("Failed to call destroy: {de} {e:?}");
        }
    }
}
impl XdgWmBase {
    #[inline]
    pub fn create_positioner(&self) -> Result<Owned<XdgPositioner>, std::io::Error> {
        let proxy_ptr = self.0.marshal_array_flags(
            1,
            XdgPositioner::def(),
            self.0.version(),
            0,
            &mut [NEWID_ARG],
        )?;

        Ok(unsafe { Owned::from_untyped_unchecked(proxy_ptr) })
    }

    #[inline]
    pub fn get_xdg_surface(
        &self,
        surface: &super::Surface,
    ) -> Result<Owned<XdgSurface>, std::io::Error> {
        let proxy_ptr = self.0.marshal_array_flags(
            2,
            XdgSurface::def(),
            self.0.version(),
            0,
            &mut [
                NEWID_ARG,
                ffi::Argument {
                    o: surface.0.0.get() as _,
                },
            ],
        )?;

        Ok(unsafe { Owned::from_untyped_unchecked(proxy_ptr) })
    }

    #[inline]
    pub fn pong(&mut self, token: u32) -> Result<(), std::io::Error> {
        self.0
            .marshal_array_flags_void(3, 0, &mut [ffi::Argument { u: token }])
    }

    pub fn add_listener<'l, L: XdgWmBaseEventListener + 'l>(
        &'l mut self,
        listener: &'l mut L,
    ) -> Result<(), ()> {
        extern "C" fn ping<L: XdgWmBaseEventListener>(
            data: *mut core::ffi::c_void,
            wm_base: *mut ffi::Proxy,
            serial: u32,
        ) {
            let listener = unsafe { &mut *(data as *mut L) };

            listener.ping(unsafe { core::mem::transmute(&mut *wm_base) }, serial)
        }
        #[repr(C)]
        struct FunctionPointer {
            ping: extern "C" fn(*mut core::ffi::c_void, *mut ffi::Proxy, u32),
        }
        let fp: &'static FunctionPointer = &FunctionPointer { ping: ping::<L> };

        unsafe {
            self.0
                .add_listener(fp as *const _ as _, listener as *mut _ as _)
        }
    }
}

pub trait XdgWmBaseEventListener {
    fn ping(&mut self, wm_base: &mut XdgWmBase, serial: u32);
}

static XDG_POSITIONER_INTERFACE: ffi::Interface = interface(
    c"xdg_positioner",
    6,
    &[
        message(c"destroy", c"", &[]),
        message(c"set_size", c"ii", &[null(), null()]),
        message(
            c"set_anchor_rect",
            c"iiii",
            &[null(), null(), null(), null()],
        ),
        message(c"set_anchor", c"u", &[null()]),
        message(c"set_gravity", c"u", &[null()]),
        message(c"set_constraint_adjustment", c"u", &[null()]),
        message(c"set_offset", c"ii", &[null(), null()]),
        message(c"set_reactive", c"3", &[]),
        message(c"set_parent_size", c"3ii", &[null(), null()]),
        message(c"set_parent_configure", c"3u", &[null()]),
    ],
    &[],
);

#[repr(transparent)]
pub struct XdgPositioner(Proxy);
unsafe impl Interface for XdgPositioner {
    fn def() -> &'static ffi::Interface {
        &XDG_POSITIONER_INTERFACE
    }
}

static XDG_SURFACE_INTERFACE: ffi::Interface = ffi::Interface {
    name: c"xdg_surface".as_ptr(),
    version: 6,
    method_count: 5,
    methods: [
        message(c"destroy", c"", &[]),
        message(c"get_toplevel", c"n", &[&XDG_TOPLEVEL_INTERFACE]),
        message(
            c"get_popup",
            c"n?oo",
            &[
                &XDG_POPUP_INTERFACE,
                &XDG_SURFACE_INTERFACE,
                &XDG_POSITIONER_INTERFACE,
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
    ]
    .as_ptr(),
    event_count: 1,
    events: [message(c"configure", c"u", &[core::ptr::null()])].as_ptr(),
};

#[repr(transparent)]
pub struct XdgSurface(Proxy);
unsafe impl Interface for XdgSurface {
    fn def() -> &'static ffi::Interface {
        &XDG_SURFACE_INTERFACE
    }

    unsafe fn destruct(&mut self) {
        self.0
            .marshal_array_flags_void(0, ffi::MARSHAL_FLAG_DESTROY, &mut [])
            .expect("Failed to destroy xdg_surface");
    }
}
impl XdgSurface {
    pub const fn as_raw(&mut self) -> *mut ffi::Proxy {
        &mut self.0 as *mut _ as _
    }

    pub fn add_listener<'l, L: XdgSurfaceEventListener + 'l>(
        &'l mut self,
        listener: &'l mut L,
    ) -> Result<(), ()> {
        extern "C" fn configure<L: XdgSurfaceEventListener>(
            data: *mut core::ffi::c_void,
            surface: *mut ffi::Proxy,
            serial: u32,
        ) {
            let listener = unsafe { &mut *(data as *mut L) };

            listener.configure(unsafe { core::mem::transmute(&mut *surface) }, serial)
        }
        #[repr(C)]
        struct FunctionPointer {
            configure: extern "C" fn(*mut core::ffi::c_void, *mut ffi::Proxy, u32),
        }
        let fp: &'static FunctionPointer = &FunctionPointer {
            configure: configure::<L>,
        };

        unsafe {
            self.0
                .add_listener(fp as *const _ as _, listener as *mut _ as _)
        }
    }
    #[inline]
    pub fn get_toplevel(&self) -> Result<Owned<XdgToplevel>, std::io::Error> {
        let proxy_ptr = self.0.marshal_array_flags(
            1,
            XdgToplevel::def(),
            self.0.version(),
            0,
            &mut [NEWID_ARG],
        )?;

        Ok(unsafe { Owned::from_untyped_unchecked(proxy_ptr) })
    }

    #[inline]
    pub fn set_window_geometry(
        &self,
        x: i32,
        y: i32,
        width: i32,
        height: i32,
    ) -> Result<(), std::io::Error> {
        self.0.marshal_array_flags_void(
            3,
            0,
            &mut [
                ffi::Argument { i: x },
                ffi::Argument { i: y },
                ffi::Argument { i: width },
                ffi::Argument { i: height },
            ],
        )
    }

    #[inline]
    pub fn ack_configure(&self, serial: u32) -> Result<(), std::io::Error> {
        self.0
            .marshal_array_flags_void(4, 0, &mut [ffi::Argument { u: serial }])
    }
}

pub trait XdgSurfaceEventListener {
    fn configure(&mut self, surface: &mut XdgSurface, serial: u32);
}

pub(super) static XDG_TOPLEVEL_INTERFACE: ffi::Interface = ffi::Interface {
    name: c"xdg_toplevel".as_ptr(),
    version: 6,
    method_count: 14,
    methods: [
        message(c"destroy", c"", &[]),
        message(c"set_parent", c"?o", &[&XDG_TOPLEVEL_INTERFACE]),
        message(c"set_title", c"s", &[core::ptr::null()]),
        message(c"set_app_id", c"s", &[core::ptr::null()]),
        message(
            c"show_window_menu",
            c"ouii",
            &[
                const { unsafe { &super::wl_seat_interface } },
                core::ptr::null(),
                core::ptr::null(),
                core::ptr::null(),
            ],
        ),
        message(
            c"move",
            c"ou",
            &[
                const { unsafe { &super::wl_seat_interface } },
                core::ptr::null(),
            ],
        ),
        message(
            c"resize",
            c"ouu",
            &[
                const { unsafe { &super::wl_seat_interface } },
                core::ptr::null(),
                core::ptr::null(),
            ],
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
        message(
            c"set_fullscreen",
            c"?o",
            &[const { unsafe { &super::wl_output_interface } }],
        ),
        message(c"unset_fullscreen", c"", &[]),
        message(c"set_minimized", c"", &[]),
    ]
    .as_ptr(),
    event_count: 4,
    events: [
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
    ]
    .as_ptr(),
};

#[repr(u32)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum XdgToplevelResizeEdge {
    Top = 1,
    Bottom = 2,
    Left = 4,
    TopLeft = 5,
    BottomLeft = 6,
    Right = 8,
    TopRight = 9,
    BottomRight = 10,
}

#[repr(transparent)]
pub struct XdgToplevel(pub(super) Proxy);
unsafe impl Interface for XdgToplevel {
    fn def() -> &'static ffi::Interface {
        &XDG_TOPLEVEL_INTERFACE
    }

    unsafe fn destruct(&mut self) {
        self.0
            .marshal_array_flags_void(0, ffi::MARSHAL_FLAG_DESTROY, &mut [])
            .expect("Failed to destroy xdg_surface");
    }
}
impl XdgToplevel {
    pub const fn as_raw(&mut self) -> *mut ffi::Proxy {
        &mut self.0 as *mut _ as _
    }

    pub fn add_listener<'l, L: XdgToplevelEventListener + 'l>(
        &'l mut self,
        listener: &'l mut L,
    ) -> Result<(), ()> {
        extern "C" fn configure<L: XdgToplevelEventListener>(
            data: *mut core::ffi::c_void,
            toplevel: *mut ffi::Proxy,
            width: i32,
            height: i32,
            states: *mut ffi::Array,
        ) {
            let listener = unsafe { &mut *(data as *mut L) };

            listener.configure(
                unsafe { core::mem::transmute(&mut *toplevel) },
                width,
                height,
                unsafe {
                    core::slice::from_raw_parts(
                        (*states).data as *mut u32 as *const _,
                        (*states).size >> 2,
                    )
                },
            )
        }
        extern "C" fn close<L: XdgToplevelEventListener>(
            data: *mut core::ffi::c_void,
            toplevel: *mut ffi::Proxy,
        ) {
            let listener = unsafe { &mut *(data as *mut L) };

            listener.close(unsafe { core::mem::transmute(&mut *toplevel) })
        }
        extern "C" fn configure_bounds<L: XdgToplevelEventListener>(
            data: *mut core::ffi::c_void,
            toplevel: *mut ffi::Proxy,
            width: i32,
            height: i32,
        ) {
            let listener = unsafe { &mut *(data as *mut L) };

            listener.configure_bounds(
                unsafe { core::mem::transmute(&mut *toplevel) },
                width,
                height,
            )
        }
        extern "C" fn wm_capabilities<L: XdgToplevelEventListener>(
            data: *mut core::ffi::c_void,
            toplevel: *mut ffi::Proxy,
            capabilities: *mut ffi::Array,
        ) {
            let listener = unsafe { &mut *(data as *mut L) };

            listener.wm_capabilities(unsafe { core::mem::transmute(&mut *toplevel) }, unsafe {
                core::slice::from_raw_parts(
                    (*capabilities).data as *mut u32 as *const _,
                    (*capabilities).size >> 2,
                )
            })
        }
        #[repr(C)]
        struct FunctionPointers {
            configure:
                extern "C" fn(*mut core::ffi::c_void, *mut ffi::Proxy, i32, i32, *mut ffi::Array),
            close: extern "C" fn(*mut core::ffi::c_void, *mut ffi::Proxy),
            configure_bounds: extern "C" fn(*mut core::ffi::c_void, *mut ffi::Proxy, i32, i32),
            wm_capabilities:
                extern "C" fn(*mut core::ffi::c_void, *mut ffi::Proxy, *mut ffi::Array),
        }
        let fp: &'static FunctionPointers = &FunctionPointers {
            configure: configure::<L>,
            close: close::<L>,
            configure_bounds: configure_bounds::<L>,
            wm_capabilities: wm_capabilities::<L>,
        };

        unsafe {
            self.0
                .add_listener(fp as *const _ as _, listener as *mut _ as _)
        }
    }

    #[inline]
    pub fn set_title(&self, title: &core::ffi::CStr) -> Result<(), std::io::Error> {
        self.0
            .marshal_array_flags_void(2, 0, &mut [ffi::Argument { s: title.as_ptr() }])
    }

    #[inline]
    pub fn set_app_id(&self, id: &core::ffi::CStr) -> Result<(), std::io::Error> {
        self.0
            .marshal_array_flags_void(3, 0, &mut [ffi::Argument { s: id.as_ptr() }])
    }

    #[inline]
    pub fn show_window_menu(
        &self,
        seat: &super::Seat,
        serial: u32,
        x: i32,
        y: i32,
    ) -> Result<(), std::io::Error> {
        self.0.marshal_array_flags_void(
            4,
            0,
            &mut [
                seat.0.as_arg(),
                ffi::Argument { u: serial },
                ffi::Argument { i: x },
                ffi::Argument { i: y },
            ],
        )
    }

    #[inline]
    pub fn r#move(&self, seat: &super::Seat, serial: u32) -> Result<(), std::io::Error> {
        self.0
            .marshal_array_flags_void(5, 0, &mut [seat.0.as_arg(), ffi::Argument { u: serial }])
    }

    #[inline]
    pub fn resize(
        &self,
        seat: &super::Seat,
        serial: u32,
        edges: XdgToplevelResizeEdge,
    ) -> Result<(), std::io::Error> {
        self.0.marshal_array_flags_void(
            6,
            0,
            &mut [
                seat.0.as_arg(),
                ffi::Argument { u: serial },
                ffi::Argument { u: edges as _ },
            ],
        )
    }

    #[inline]
    pub fn set_min_size(&self, width: i32, height: i32) -> Result<(), std::io::Error> {
        self.0.marshal_array_flags_void(
            8,
            0,
            &mut [ffi::Argument { i: width }, ffi::Argument { i: height }],
        )
    }

    #[inline]
    pub fn set_maximized(&self) -> Result<(), std::io::Error> {
        self.0.marshal_array_flags_void(9, 0, &mut [])
    }

    #[inline]
    pub fn unset_maximized(&self) -> Result<(), std::io::Error> {
        self.0.marshal_array_flags_void(10, 0, &mut [])
    }

    #[inline]
    pub fn set_minimized(&self) -> Result<(), std::io::Error> {
        self.0.marshal_array_flags_void(13, 0, &mut [])
    }
}

pub trait XdgToplevelEventListener {
    fn configure(&mut self, toplevel: &mut XdgToplevel, width: i32, height: i32, states: &[i32]);
    fn close(&mut self, toplevel: &mut XdgToplevel);
    fn configure_bounds(&mut self, toplevel: &mut XdgToplevel, width: i32, height: i32);
    fn wm_capabilities(&mut self, toplevel: &mut XdgToplevel, capabilities: &[i32]);
}

static XDG_POPUP_INTERFACE: ffi::Interface = ffi::Interface {
    name: c"xdg_popup".as_ptr(),
    version: 6,
    method_count: 2,
    methods: [
        message(c"destroy", c"", &[]),
        message(
            c"grab",
            c"ou",
            &[
                const { unsafe { &super::wl_seat_interface } },
                core::ptr::null(),
            ],
        ),
    ]
    .as_ptr(),
    event_count: 4,
    events: [
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
        message(
            c"reposition",
            c"3ou",
            &[&XDG_POSITIONER_INTERFACE, core::ptr::null()],
        ),
        message(c"repositioned", c"3u", &[core::ptr::null()]),
    ]
    .as_ptr(),
};

#[repr(transparent)]
pub struct XdgPopup(Proxy);
unsafe impl Interface for XdgPopup {
    fn def() -> &'static ffi::Interface {
        &XDG_POPUP_INTERFACE
    }
}
