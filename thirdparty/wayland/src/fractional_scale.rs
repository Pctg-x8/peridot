use super::{Interface, ffi, interface, message};

#[repr(transparent)]
pub struct WpFractionalScaleManagerV1(super::Proxy);
unsafe impl super::Interface for WpFractionalScaleManagerV1 {
    fn def() -> &'static ffi::Interface {
        Self::INTERFACE
    }

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(
            name = "<WpFractionalScaleManagerV1 as Interface>::destruct",
            skip(self)
        )
    )]
    unsafe fn destruct(&mut self) {
        if let Err(e) =
            self.0
                .marshal_array_flags_void(0, super::ffi::MARSHAL_FLAG_DESTROY, &mut [])
        {
            let de = unsafe {
                ffi::wl_display_get_error(ffi::wl_proxy_get_display(&mut self.0 as *mut _ as _))
            };

            #[cfg(feature = "tracing")]
            tracing::error!(reason = ?e, display_error = de, "Failed to call destroy");
            #[cfg(not(feature = "tracing"))]
            eprintln!("Failed to call destroy! reason = {e:?}, display_error = {de}");
        }
    }
}
impl WpFractionalScaleManagerV1 {
    const INTERFACE: &'static ffi::Interface = &interface(
        c"wp_fractional_scale_manager_v1",
        1,
        &[
            message(c"destroy", c"", &[]),
            message(
                c"get_fractional_scale",
                c"no",
                &[
                    const { WpFractionalScaleV1::INTERFACE },
                    const { unsafe { &super::wl_surface_interface } },
                ],
            ),
        ],
        &[],
    );

    #[cfg_attr(feature = "tracing", tracing::instrument(
        name = "WpFractionalScaleManagerV1::get_fractional_scale",
        skip(self, surface),
        err(level = tracing::Level::WARN)
    ))]
    pub fn get_fractional_scale(
        &self,
        surface: &super::Surface,
    ) -> Result<super::Owned<WpFractionalScaleV1>, std::io::Error> {
        Ok(unsafe {
            super::Owned::from_untyped_unchecked(self.0.marshal_array_flags(
                1,
                WpFractionalScaleV1::def(),
                self.0.version(),
                0,
                &mut [
                    super::NEWID_ARG,
                    super::ffi::Argument {
                        o: surface.0.0.get() as _,
                    },
                ],
            )?)
        })
    }
}

#[repr(transparent)]
pub struct WpFractionalScaleV1(super::Proxy);
unsafe impl super::Interface for WpFractionalScaleV1 {
    fn def() -> &'static ffi::Interface {
        Self::INTERFACE
    }

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<WpFractionalScaleV1 as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        if let Err(e) =
            self.0
                .marshal_array_flags_void(0, super::ffi::MARSHAL_FLAG_DESTROY, &mut [])
        {
            let de = unsafe {
                ffi::wl_display_get_error(ffi::wl_proxy_get_display(&mut self.0 as *mut _ as _))
            };

            #[cfg(feature = "tracing")]
            tracing::error!(reason = ?e, display_error = de, "Failed to call destroy");
            #[cfg(not(feature = "tracing"))]
            eprintln!("Failed to call destroy! reason = {e:?}, display_error = {de}");
        }
    }
}
impl WpFractionalScaleV1 {
    const INTERFACE: &'static ffi::Interface = &interface(
        c"wp_fractional_scale_v1",
        1,
        &[message(c"destroy", c"", &[])],
        &[message(c"preferred_scale", c"u", &[])],
    );

    pub fn add_listener<'l, L: WpFractionalScaleV1EventListener + 'l>(
        &'l mut self,
        listener: &'l mut L,
    ) -> Result<(), ()> {
        extern "C" fn preferred_scale<L: WpFractionalScaleV1EventListener>(
            data: *mut core::ffi::c_void,
            object: *mut ffi::Proxy,
            scale: u32,
        ) {
            let listener = unsafe { &mut *(data as *mut L) };

            listener.preferred_scale(unsafe { core::mem::transmute(&mut *object) }, scale)
        }
        #[repr(C)]
        struct FunctionPointer {
            preferred_scale: extern "C" fn(*mut core::ffi::c_void, *mut ffi::Proxy, u32),
        }
        let fp: &'static FunctionPointer = &FunctionPointer {
            preferred_scale: preferred_scale::<L>,
        };

        unsafe {
            self.0
                .add_listener(fp as *const _ as _, listener as *mut _ as _)
        }
    }
}

pub trait WpFractionalScaleV1EventListener {
    /// Notification of a new preferred scale for this surface that the compositor suggests that the client should use.
    ///
    /// The sent scale is the numerator of a fraction with a denominator of 120.
    fn preferred_scale(&mut self, object: &mut WpFractionalScaleV1, scale: u32);
}
