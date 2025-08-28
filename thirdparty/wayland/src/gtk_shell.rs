use crate::EventFnTable;

use super::{Interface, Proxy, ffi, interface, message};

/// gtk_shell is a protocol extension providing additional features for clients implementing it.
#[repr(transparent)]
pub struct GtkShell1(Proxy);
unsafe impl Interface for GtkShell1 {
    fn def() -> &'static ffi::Interface {
        Self::INTERFACE
    }
}
impl GtkShell1 {
    const INTERFACE: &'static ffi::Interface = &interface(
        c"gtk_shell1",
        6,
        &[
            message(
                c"get_gtk_surface",
                c"no",
                &const {
                    [GtkSurface1::INTERFACE, unsafe {
                        &super::wl_surface_interface
                    }]
                },
            ),
            message(c"set_startup_id", c"?s", &[core::ptr::null()]),
            message(c"system_bell", c"o", &[GtkSurface1::INTERFACE]),
            message(c"notify_launch", c"3s", &[core::ptr::null()]),
        ],
        &[message(c"capabilities", c"u", &[core::ptr::null()])],
    );

    #[inline]
    pub fn get_gtk_surface(
        &self,
        surface: &super::Surface,
    ) -> Result<super::Owned<GtkSurface1>, std::io::Error> {
        Ok(unsafe {
            super::Owned::wrap_unchecked(self.0.marshal_array_flags_typed(
                0,
                0,
                &mut [super::NEWID_ARG, surface.0.as_arg()],
            )?)
        })
    }

    pub fn set_listener<'l, L: GtkShell1EventListener + 'l>(
        &'l mut self,
        listener: &'l mut L,
    ) -> Result<(), ()> {
        unsafe {
            self.0.set_listener(
                EventFnTable!(for L: GtkShell1EventListener {
                    capabilities(capabilities: u32 => capabilities)
                }) as *const _ as _,
                listener as *mut _ as _,
            )
        }
    }
}

pub trait GtkShell1EventListener {
    fn capabilities(&mut self, sender: &mut GtkShell1, capabilities: u32);
}

#[repr(transparent)]
pub struct GtkSurface1(Proxy);
unsafe impl Interface for GtkSurface1 {
    fn def() -> &'static ffi::Interface {
        Self::INTERFACE
    }

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<GtkSurface1 as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        if self.0.version() < 4 {
            // no destructor defined prior version 4
            return;
        }

        self.0.call_simple_dtor(5);
    }
}
impl GtkSurface1 {
    const INTERFACE: &'static ffi::Interface = &interface(
        c"gtk_surface1",
        6,
        &[
            message(
                c"set_dbus_properties",
                c"?s?s?s??s?s",
                &[core::ptr::null(); 6],
            ),
            message(c"set_modal", c"", &[]),
            message(c"unset_modal", c"", &[]),
            message(c"present", c"u", &[core::ptr::null()]),
            message(c"request_focus", c"3?s", &[core::ptr::null()]),
            message(c"release", c"4", &[]),
            message(
                c"titlebar_gesture",
                c"5uou",
                &[
                    core::ptr::null(),
                    const { unsafe { &super::wl_seat_interface } },
                    core::ptr::null(),
                ],
            ),
        ],
        &[
            message(c"configure", c"2a", &[core::ptr::null()]),
            message(c"configure_edges", c"2a", &[core::ptr::null()]),
        ],
    );

    #[inline]
    pub fn present(&self, time: u32) -> Result<(), std::io::Error> {
        self.0
            .marshal_array_flags_void(3, 0, &mut [ffi::Argument { u: time }])
    }

    pub fn set_listener<'l, L: GtkSurface1EventListener + 'l>(
        &'l mut self,
        listener: &'l mut L,
    ) -> Result<(), ()> {
        unsafe {
            self.0.set_listener(
                EventFnTable!(for L: GtkSurface1EventListener {
                    configure(states: *mut ffi::Array => unsafe {
                        core::slice::from_raw_parts((*states).data as *const u32, (*states).size >> 2)
                    }),
                    configure_edges(constraints: *mut ffi::Array => unsafe {
                        core::slice::from_raw_parts((*constraints).data as *const u32, (*constraints).size >> 2)
                    })
                }) as *const _ as _,
                listener as *mut _ as _
            )
        }
    }
}

pub trait GtkSurface1EventListener {
    fn configure(&mut self, sender: &mut GtkSurface1, states: &[u32]);
    fn configure_edges(&mut self, sender: &mut GtkSurface1, constraints: &[u32]);
}
