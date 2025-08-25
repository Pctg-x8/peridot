use super::{Interface, NEWID_ARG, Owned, Proxy, ffi, interface, message, wl_surface_interface};

#[repr(transparent)]
pub struct WpViewporter(Proxy);
unsafe impl Interface for WpViewporter {
    fn def() -> &'static ffi::Interface {
        Self::INTERFACE
    }

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<WpViewporter as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        #[cfg_attr(not(feature = "tracing"), allow(unused_variables))]
        if let Err(e) = self
            .0
            .marshal_array_flags_void(0, ffi::MARSHAL_FLAG_DESTROY, &mut [])
        {
            #[cfg(feature = "tracing")]
            tracing::warn!(
                reason = ?e,
                display_error = unsafe { ffi::wl_display_get_error(self.0.display()) },
                "Failed to call destructor"
            );
        }
    }
}
impl WpViewporter {
    const INTERFACE: &'static ffi::Interface = &interface(
        c"wp_viewporter",
        1,
        &[
            message(c"destroy", c"", &[]),
            message(
                c"get_viewport",
                c"no",
                &[
                    WpViewport::INTERFACE,
                    const { unsafe { &wl_surface_interface } },
                ],
            ),
        ],
        &[],
    );

    #[inline]
    pub fn get_viewport(
        &self,
        surface: &super::Surface,
    ) -> Result<Owned<WpViewport>, std::io::Error> {
        Ok(unsafe {
            Owned::wrap_unchecked(self.0.marshal_array_flags_typed::<WpViewport>(
                1,
                self.0.version(),
                0,
                &mut [NEWID_ARG, surface.0.as_arg()],
            )?)
        })
    }
}

#[repr(transparent)]
pub struct WpViewport(Proxy);
unsafe impl Interface for WpViewport {
    fn def() -> &'static ffi::Interface {
        Self::INTERFACE
    }

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<WpViewport as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        #[cfg_attr(not(feature = "tracing"), allow(unused_variables))]
        if let Err(e) = self
            .0
            .marshal_array_flags_void(0, ffi::MARSHAL_FLAG_DESTROY, &mut [])
        {
            #[cfg(feature = "tracing")]
            tracing::warn!(
                reason = ?e,
                display_error = unsafe { ffi::wl_display_get_error(self.0.display()) },
                "Failed to call destructor"
            );
        }
    }
}
impl WpViewport {
    const INTERFACE: &'static ffi::Interface = &interface(
        c"wp_viewport",
        1,
        &[
            message(c"destroy", c"", &[]),
            message(c"set_source", c"ffff", &[]),
            message(c"set_destination", c"ii", &[]),
        ],
        &[],
    );

    #[inline]
    pub fn set_source(
        &self,
        x: f32,
        y: f32,
        width: f32,
        height: f32,
    ) -> Result<(), std::io::Error> {
        self.0.marshal_array_flags_void(
            1,
            0,
            &mut [
                ffi::Argument {
                    f: ffi::Fixed::from_f32_lossy(x),
                },
                ffi::Argument {
                    f: ffi::Fixed::from_f32_lossy(y),
                },
                ffi::Argument {
                    f: ffi::Fixed::from_f32_lossy(width),
                },
                ffi::Argument {
                    f: ffi::Fixed::from_f32_lossy(height),
                },
            ],
        )
    }

    #[inline]
    pub fn set_destination(&self, width: i32, height: i32) -> Result<(), std::io::Error> {
        self.0.marshal_array_flags_void(
            2,
            0,
            &mut [ffi::Argument { i: width }, ffi::Argument { i: height }],
        )
    }
}
