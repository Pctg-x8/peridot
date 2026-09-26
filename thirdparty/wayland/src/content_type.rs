use crate::{Interface, Proxy, ffi};

static WP_CONTENT_TYPE_MANAGER_V1_INTERFACE: ffi::Interface = ffi::Interface {
    name: c"wp_content_type_manager_v1".as_ptr(),
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
                name: c"get_surface_content_type".as_ptr(),
                signature: c"no".as_ptr(),
                types: const { [crate::WpContentTypeV1::DEF, crate::Surface::DEF] }.as_ptr(),
            },
        ]
    }
    .as_ptr(),
    event_count: 0,
    events: const { [] }.as_ptr(),
};

#[repr(transparent)]
pub struct WpContentTypeManagerV1(pub(crate) Proxy);
unsafe impl Interface for WpContentTypeManagerV1 {
    const DEF: *const ffi::Interface = &WP_CONTENT_TYPE_MANAGER_V1_INTERFACE;

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<WpContentTypeManagerV1 as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}

impl WpContentTypeManagerV1 {
    #[inline]
    pub fn get_surface_content_type(
        &self,
        surface: &crate::Surface,
    ) -> crate::Result<crate::Owned<crate::WpContentTypeV1>> {
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
pub enum WpContentTypeManagerV1Error {
    AlreadyConstructed = 0,
}

static WP_CONTENT_TYPE_V1_INTERFACE: ffi::Interface = ffi::Interface {
    name: c"wp_content_type_v1".as_ptr(),
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
                name: c"set_content_type".as_ptr(),
                signature: c"u".as_ptr(),
                types: const { [core::ptr::null()] }.as_ptr(),
            },
        ]
    }
    .as_ptr(),
    event_count: 0,
    events: const { [] }.as_ptr(),
};

#[repr(transparent)]
pub struct WpContentTypeV1(pub(crate) Proxy);
unsafe impl Interface for WpContentTypeV1 {
    const DEF: *const ffi::Interface = &WP_CONTENT_TYPE_V1_INTERFACE;

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<WpContentTypeV1 as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}

impl WpContentTypeV1 {
    #[inline]
    pub fn set_content_type(&self, content_type: WpContentTypeV1Type) -> crate::Result<()> {
        self.0.marshal_array_void(
            1,
            &mut [ffi::Argument {
                u: content_type as _,
            }],
        )
    }
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WpContentTypeV1Type {
    None = 0,
    Photo = 1,
    Video = 2,
    Game = 3,
}
