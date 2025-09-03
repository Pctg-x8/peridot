use crate::{Interface, Proxy, ffi};

static WP_VIEWPORTER_INTERFACE: ffi::Interface = ffi::Interface {
    name: c"wp_viewporter".as_ptr(),
    version: 1,
    method_count: 2,
    methods: const {
        [
            ffi::Message {
                name: c"destroy".as_ptr(),
                signature: c"".as_ptr(),
                types: const { [] }.as_ptr(),
            },
            ffi::Message {
                name: c"get_viewport".as_ptr(),
                signature: c"no".as_ptr(),
                types: const {
                    [
                        crate::WpViewport::DEF as *const _,
                        crate::Surface::DEF as *const _,
                    ]
                }
                .as_ptr(),
            },
        ]
    }
    .as_ptr(),
    event_count: 0,
    events: const { [] }.as_ptr(),
};

#[repr(transparent)]
pub struct WpViewporter(pub(crate) Proxy);
unsafe impl Interface for WpViewporter {
    const DEF: &'static ffi::Interface = &WP_VIEWPORTER_INTERFACE;

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<WpViewporter as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}

impl WpViewporter {
    #[inline]
    pub fn get_viewport(
        &self,
        surface: &crate::Surface,
    ) -> crate::Result<crate::Owned<crate::WpViewport>> {
        Ok(unsafe {
            crate::Owned::wrap_unchecked(
                self.0
                    .marshal_array_typed(1, &mut [crate::NEWID_ARG, surface.0.as_arg()])?,
            )
        })
    }
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WpViewporterError {
    ViewportExists = 0,
}

static WP_VIEWPORT_INTERFACE: ffi::Interface = ffi::Interface {
    name: c"wp_viewport".as_ptr(),
    version: 1,
    method_count: 3,
    methods: const {
        [
            ffi::Message {
                name: c"destroy".as_ptr(),
                signature: c"".as_ptr(),
                types: const { [] }.as_ptr(),
            },
            ffi::Message {
                name: c"set_source".as_ptr(),
                signature: c"ffff".as_ptr(),
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
                name: c"set_destination".as_ptr(),
                signature: c"ii".as_ptr(),
                types: const { [core::ptr::null(), core::ptr::null()] }.as_ptr(),
            },
        ]
    }
    .as_ptr(),
    event_count: 0,
    events: const { [] }.as_ptr(),
};

#[repr(transparent)]
pub struct WpViewport(pub(crate) Proxy);
unsafe impl Interface for WpViewport {
    const DEF: &'static ffi::Interface = &WP_VIEWPORT_INTERFACE;

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<WpViewport as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}

impl WpViewport {
    #[inline]
    pub fn set_source(
        &self,
        x: crate::Fixed,
        y: crate::Fixed,
        width: crate::Fixed,
        height: crate::Fixed,
    ) -> crate::Result<()> {
        self.0.marshal_array_void(
            1,
            &mut [
                ffi::Argument { f: x },
                ffi::Argument { f: y },
                ffi::Argument { f: width },
                ffi::Argument { f: height },
            ],
        )
    }

    #[inline]
    pub fn set_destination(&self, width: i32, height: i32) -> crate::Result<()> {
        self.0.marshal_array_void(
            2,
            &mut [ffi::Argument { i: width }, ffi::Argument { i: height }],
        )
    }
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WpViewportError {
    BadValue = 0,
    BadSize = 1,
    OutOfBuffer = 2,
    NoSurface = 3,
}
