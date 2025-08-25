use super::{Interface, NEWID_ARG, Owned, Proxy, ffi, interface, message};

#[repr(transparent)]
pub struct WpCursorShapeManagerV1(Proxy);
unsafe impl Interface for WpCursorShapeManagerV1 {
    fn def() -> &'static ffi::Interface {
        Self::INTERFACE
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
        let proxy_ptr = self.0.marshal_array_flags(
            1,
            WpCursorShapeDeviceV1::def(),
            self.0.version(),
            0,
            &mut [
                NEWID_ARG,
                ffi::Argument {
                    o: &mut pointer.0 as *mut _ as _,
                },
            ],
        )?;

        Ok(unsafe { Owned::from_untyped_unchecked(proxy_ptr) })
    }
}

#[repr(i32)]
#[derive(Clone, Copy)]
pub enum WpCursorShapeDeviceV1Shape {
    Default = 1,
    // ContextMenu = 2,
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
    fn def() -> &'static ffi::Interface {
        Self::INTERFACE
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
