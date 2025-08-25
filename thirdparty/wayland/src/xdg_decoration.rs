use super::{Interface, NEWID_ARG, Owned, Proxy, ffi, interface, message};

#[repr(transparent)]
pub struct ZxdgDecorationManagerV1(Proxy);
unsafe impl Interface for ZxdgDecorationManagerV1 {
    fn def() -> &'static ffi::Interface {
        Self::INTERFACE
    }

    unsafe fn destruct(&mut self) {
        self.0
            .marshal_array_flags_void(0, ffi::MARSHAL_FLAG_DESTROY, &mut [])
            .expect("Failed to call destroy");
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
                self.0.version(),
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

    unsafe fn destruct(&mut self) {
        self.0
            .marshal_array_flags_void(0, ffi::MARSHAL_FLAG_DESTROY, &mut [])
            .expect("Failed to call destroy");
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

    pub fn add_listener<'l, L: ZxdgToplevelDecorationV1EventListener + 'l>(
        &'l mut self,
        listener: &'l mut L,
    ) -> Result<(), ()> {
        extern "C" fn configure<L: ZxdgToplevelDecorationV1EventListener>(
            data: *mut core::ffi::c_void,
            sender: *mut ffi::Proxy,
            mode: u32,
        ) {
            L::configure(
                unsafe { &mut *(data as *mut _) },
                unsafe { &mut *(sender as *mut _) },
                unsafe { core::mem::transmute(mode) },
            )
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
