use crate::EventFnTable;

use super::{Interface, NEWID_ARG, Owned, Proxy, ffi, interface, message};

#[repr(transparent)]
pub struct ZxdgDecorationManagerV1(Proxy);
unsafe impl Interface for ZxdgDecorationManagerV1 {
    fn def() -> &'static ffi::Interface {
        Self::INTERFACE
    }

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<ZxdgDecorationManagerV1 as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}
impl ZxdgDecorationManagerV1 {
    const INTERFACE: &'static ffi::Interface = &interface(
        c"zxdg_decoration_manager_v1",
        1,
        &[
            message(c"destroy", c"", &[]),
            message(
                c"get_toplevel_decoration",
                c"no",
                &[
                    const { ZxdgToplevelDecorationV1::INTERFACE },
                    const { &super::XDG_TOPLEVEL_INTERFACE },
                ],
            ),
        ],
        &[],
    );

    #[cfg_attr(feature = "tracing", tracing::instrument(
        name = "ZxdgDecorationManagerV1::get_toplevel_decoration",
        skip(self, toplevel),
        err(level = tracing::Level::ERROR)
    ))]
    pub fn get_toplevel_decoration(
        &self,
        toplevel: &super::XdgToplevel,
    ) -> Result<Owned<ZxdgToplevelDecorationV1>, std::io::Error> {
        Ok(unsafe {
            Owned::wrap_unchecked(self.0.marshal_array_flags_typed(
                1,
                0,
                &mut [NEWID_ARG, toplevel.0.as_arg()],
            )?)
        })
    }
}

#[repr(u32)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ZxdgToplevelDecorationMode {
    ClientSide = 1,
    ServerSide = 2,
}

#[repr(transparent)]
pub struct ZxdgToplevelDecorationV1(Proxy);
unsafe impl Interface for ZxdgToplevelDecorationV1 {
    fn def() -> &'static ffi::Interface {
        Self::INTERFACE
    }

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(
            name = "<ZxdgToplevelDecorationV1 as Interface>::destruct",
            skip(self)
        )
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}
impl ZxdgToplevelDecorationV1 {
    const INTERFACE: &'static ffi::Interface = &interface(
        c"zxdg_toplevel_decoration_v1",
        1,
        &[
            message(c"destroy", c"", &[]),
            message(c"set_mode", c"u", &[]),
            message(c"unset_mode", c"", &[]),
        ],
        &[message(c"configure", c"u", &[])],
    );

    pub fn set_listener<'l, L: ZxdgToplevelDecorationV1EventListener + 'l>(
        &'l mut self,
        listener: &'l mut L,
    ) -> Result<(), ()> {
        unsafe {
            self.0.set_listener(
                EventFnTable!(for L: ZxdgToplevelDecorationV1EventListener {
                    configure(mode: u32 => unsafe { core::mem::transmute(mode) })
                }) as *const _ as _,
                listener as *mut _ as _,
            )
        }
    }

    #[inline]
    pub fn set_mode(&self, mode: ZxdgToplevelDecorationMode) -> Result<(), std::io::Error> {
        self.0
            .marshal_array_flags_void(1, 0, &mut [ffi::Argument { u: mode as _ }])
    }
}

pub trait ZxdgToplevelDecorationV1EventListener {
    fn configure(
        &mut self,
        sender: &mut ZxdgToplevelDecorationV1,
        mode: ZxdgToplevelDecorationMode,
    );
}
