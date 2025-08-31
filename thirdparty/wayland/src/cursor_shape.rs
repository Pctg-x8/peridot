use super::{Interface, NEWID_ARG, Owned, Proxy, ffi, interface, message};

#[repr(transparent)]
pub struct WpCursorShapeManagerV1(Proxy);
unsafe impl Interface for WpCursorShapeManagerV1 {
    const DEF: &'static ffi::Interface = Self::INTERFACE;

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<WpCursorShapeManagerV1 as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}
impl WpCursorShapeManagerV1 {
    const INTERFACE: &'static ffi::Interface = &interface(
        c"wp_cursor_shape_manager_v1",
        1,
        &[
            message(c"destroy", c"", &[]),
            message(
                c"get_pointer",
                c"no",
                &const {
                    [WpCursorShapeDeviceV1::INTERFACE, unsafe {
                        &super::wl_pointer_interface
                    }]
                },
            ),
            // message(
            //     c"get_tablet_tool_v2",
            //     c"no",
            //     &[
            //         &WP_CURSOR_SHAPE_DEVICE_V1_INTERFACE,
            //         &ZWP_TABLET_TOOL_V2_INTERFACE,
            //     ],
            // ),
        ],
        &[],
    );

    pub fn get_pointer(
        &self,
        pointer: &mut super::Pointer,
    ) -> Result<Owned<WpCursorShapeDeviceV1>, std::io::Error> {
        Ok(unsafe {
            Owned::wrap_unchecked(self.0.marshal_array_flags_typed(
                1,
                self.0.version(),
                0,
                &mut [NEWID_ARG, pointer.0.as_arg()],
            )?)
        })
    }
}

#[repr(i32)]
#[derive(Clone, Copy)]
pub enum WpCursorShapeDeviceV1Shape {
    Default = 1,
    ContextMenu = 2,
    Pointer = 4,
    NeResize = 20,
    NwResize = 21,
    SeResize = 23,
    SwResize = 24,
    EwResize = 26,
    NsResize = 27,
}

#[repr(transparent)]
pub struct WpCursorShapeDeviceV1(Proxy);
unsafe impl Interface for WpCursorShapeDeviceV1 {
    const DEF: &'static ffi::Interface = Self::INTERFACE;

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<WpCursorShapeDeviceV1 as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}
impl WpCursorShapeDeviceV1 {
    const INTERFACE: &'static ffi::Interface = &interface(
        c"wp_cursor_shape_device_v1",
        1,
        &[
            message(c"destroy", c"", &[]),
            message(c"set_shape", c"uu", &[]),
        ],
        &[],
    );

    pub const fn as_raw(&self) -> *mut ffi::Proxy {
        self.0.0.get() as *mut _ as _
    }

    #[inline]
    pub fn set_shape(
        &self,
        serial: u32,
        shape: WpCursorShapeDeviceV1Shape,
    ) -> Result<(), std::io::Error> {
        self.0.marshal_array_flags_void(
            1,
            0,
            &mut [ffi::Argument { u: serial }, ffi::Argument { u: shape as _ }],
        )
    }
}
