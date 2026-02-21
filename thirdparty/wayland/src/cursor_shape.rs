use crate::{Interface, Proxy, ffi};

static WP_CURSOR_SHAPE_MANAGER_V1_INTERFACE: ffi::Interface = ffi::Interface {
    name: c"wp_cursor_shape_manager_v1".as_ptr(),
    version: 2,
    method_count: 3,
    methods: const {
        [
            ffi::Message {
                name: c"destroy".as_ptr(),
                signature: c"".as_ptr(),
                types: const { [] }.as_ptr(),
            },
            ffi::Message {
                name: c"get_pointer".as_ptr(),
                signature: c"no".as_ptr(),
                types: const { [crate::WpCursorShapeDeviceV1::DEF, crate::Pointer::DEF] }.as_ptr(),
            },
            ffi::Message {
                name: c"get_tablet_tool_v2".as_ptr(),
                signature: c"no".as_ptr(),
                types: const {
                    [
                        crate::WpCursorShapeDeviceV1::DEF,
                        crate::ZwpTabletToolV2::DEF,
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
pub struct WpCursorShapeManagerV1(pub(crate) Proxy);
unsafe impl Interface for WpCursorShapeManagerV1 {
    const DEF: *const ffi::Interface = &WP_CURSOR_SHAPE_MANAGER_V1_INTERFACE;

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<WpCursorShapeManagerV1 as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}

impl WpCursorShapeManagerV1 {
    #[inline(always)]
    pub fn set_user_data(&mut self, user_data: *mut core::ffi::c_void) {
        unsafe {
            self.0.set_user_data(user_data);
        }
    }
    #[inline(always)]
    pub fn user_data(&mut self) -> *mut core::ffi::c_void {
        unsafe { self.0.user_data() } }
   

    #[inline]
    pub fn get_pointer(
        &self,
        pointer: &crate::Pointer,
    ) -> crate::Result<crate::Owned<crate::WpCursorShapeDeviceV1>> {
        Ok(unsafe {
            crate::Owned::wrap_unchecked(
                self.0
                    .marshal_array_typed(1, &mut [crate::NEWID_ARG, pointer.0.as_arg()])?,
            )
        })
    }

    #[inline]
    pub fn get_tablet_tool_v2(
        &self,
        tablet_tool: &crate::ZwpTabletToolV2,
    ) -> crate::Result<crate::Owned<crate::WpCursorShapeDeviceV1>> {
        Ok(unsafe {
            crate::Owned::wrap_unchecked(
                self.0
                    .marshal_array_typed(2, &mut [crate::NEWID_ARG, tablet_tool.0.as_arg()])?,
            )
        })
    }
}

static WP_CURSOR_SHAPE_DEVICE_V1_INTERFACE: ffi::Interface = ffi::Interface {
    name: c"wp_cursor_shape_device_v1".as_ptr(),
    version: 2,
    method_count: 2,
    methods: const {
        [
            ffi::Message {
                name: c"destroy".as_ptr(),
                signature: c"".as_ptr(),
                types: const { [] }.as_ptr(),
            },
            ffi::Message {
                name: c"set_shape".as_ptr(),
                signature: c"uu".as_ptr(),
                types: const { [core::ptr::null(), core::ptr::null()] }.as_ptr(),
            },
        ]
    }
    .as_ptr(),
    event_count: 0,
    events: const { [] }.as_ptr(),
};

#[repr(transparent)]
pub struct WpCursorShapeDeviceV1(pub(crate) Proxy);
unsafe impl Interface for WpCursorShapeDeviceV1 {
    const DEF: *const ffi::Interface = &WP_CURSOR_SHAPE_DEVICE_V1_INTERFACE;

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<WpCursorShapeDeviceV1 as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}

impl WpCursorShapeDeviceV1 {
    #[inline(always)]
    pub fn set_user_data(&mut self, user_data: *mut core::ffi::c_void) {
        unsafe {
            self.0.set_user_data(user_data);
        }
    }
    #[inline(always)]
    pub fn user_data(&mut self) -> *mut core::ffi::c_void {
        unsafe { self.0.user_data() } }
   

    #[inline]
    pub fn set_shape(&self, serial: u32, shape: WpCursorShapeDeviceV1Shape) -> crate::Result<()> {
        self.0
            .marshal_array_void(1, &mut [ffi::Argument { u: serial }, shape.as_arg()])
    }
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WpCursorShapeDeviceV1Shape {
    Default = 1,
    ContextMenu = 2,
    Help = 3,
    Pointer = 4,
    Progress = 5,
    Wait = 6,
    Cell = 7,
    Crosshair = 8,
    Text = 9,
    VerticalText = 10,
    Alias = 11,
    Copy = 12,
    Move = 13,
    NoDrop = 14,
    NotAllowed = 15,
    Grab = 16,
    Grabbing = 17,
    EResize = 18,
    NResize = 19,
    NeResize = 20,
    NwResize = 21,
    SResize = 22,
    SeResize = 23,
    SwResize = 24,
    WResize = 25,
    EwResize = 26,
    NsResize = 27,
    NeswResize = 28,
    NwseResize = 29,
    ColResize = 30,
    RowResize = 31,
    AllScroll = 32,
    ZoomIn = 33,
    ZoomOut = 34,
    DndAsk = 35,
    AllResize = 36,
}
impl WpCursorShapeDeviceV1Shape {
    pub const fn as_arg(&self) -> ffi::Argument {
        ffi::Argument { u: *self as _ }
    }
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WpCursorShapeDeviceV1Error {
    InvalidShape = 1,
}
impl WpCursorShapeDeviceV1Error {
    pub const fn as_arg(&self) -> ffi::Argument {
        ffi::Argument { u: *self as _ }
    }
}