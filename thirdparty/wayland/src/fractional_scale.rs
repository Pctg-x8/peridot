use crate::{EventFnTable, Interface, Owned};

use super::{ffi, interface, message};

#[repr(transparent)]
pub struct WpFractionalScaleManagerV1(super::Proxy);
unsafe impl Interface for WpFractionalScaleManagerV1 {
    const DEF: &'static ffi::Interface = &interface(
        c"wp_fractional_scale_manager_v1",
        1,
        &[
            message(c"destroy", c"", &[]),
            message(
                c"get_fractional_scale",
                c"no",
                &[WpFractionalScaleV1::DEF, super::Surface::DEF],
            ),
        ],
        &[],
    );

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(
            name = "<WpFractionalScaleManagerV1 as Interface>::destruct",
            skip(self)
        )
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}
impl WpFractionalScaleManagerV1 {
    #[cfg_attr(feature = "tracing", tracing::instrument(
        name = "WpFractionalScaleManagerV1::get_fractional_scale",
        skip(self, surface),
        err(level = tracing::Level::WARN)
    ))]
    pub fn get_fractional_scale(
        &self,
        surface: &super::Surface,
    ) -> Result<Owned<WpFractionalScaleV1>, std::io::Error> {
        Ok(unsafe {
            Owned::wrap_unchecked(
                self.0
                    .marshal_array_typed(1, &mut [super::NEWID_ARG, surface.0.as_arg()])?,
            )
        })
    }
}

#[repr(transparent)]
pub struct WpFractionalScaleV1(super::Proxy);
unsafe impl Interface for WpFractionalScaleV1 {
    const DEF: &'static ffi::Interface = &interface(
        c"wp_fractional_scale_v1",
        1,
        &[message(c"destroy", c"", &[])],
        &[message(c"preferred_scale", c"u", &[])],
    );

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<WpFractionalScaleV1 as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}
impl WpFractionalScaleV1 {
    pub fn set_listener<'l, L: WpFractionalScaleV1EventListener + 'l>(
        &'l mut self,
        listener: &'l mut L,
    ) -> Result<(), ()> {
        unsafe {
            self.0.set_listener(
                EventFnTable!(for L: WpFractionalScaleV1EventListener {
                    preferred_scale(scale: u32 => scale)
                }) as *const _ as _,
                listener as *mut _ as _,
            )
        }
    }
}

pub trait WpFractionalScaleV1EventListener {
    /// Notification of a new preferred scale for this surface that the compositor suggests that the client should use.
    ///
    /// The sent scale is the numerator of a fraction with a denominator of 120.
    fn preferred_scale(&mut self, object: &mut WpFractionalScaleV1, scale: u32);
}
