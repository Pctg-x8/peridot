//! xdg_foreign_unstable_v2: Protocol for exporting xdg surface handles
//!
//! This protocol specifies a way for making it possible to reference a surface
//! of a different client. With such a reference, a client can, by using the
//! interfaces provided by this protocol, manipulate the relationship between
//! its own surfaces and the surface of some other client. For example, stack
//! some of its own surface above the other clients surface.
//!
//! In order for a client A to get a reference of a surface of client B, client
//! B must first export its surface using xdg_exporter.export_toplevel. Upon
//! doing this, client B will receive a handle (a unique string) that it may
//! share with client A in some way (for example D-Bus). After client A has
//! received the handle from client B, it may use xdg_importer.import_toplevel
//! to create a reference to the surface client B just exported. See the
//! corresponding requests for details.
//!
//! A possible use case for this is out-of-process dialogs. For example when a
//! sandboxed client without file system access needs the user to select a file
//! on the file system, given sandbox environment support, it can export its
//! surface, passing the exported surface handle to an unsandboxed process that
//! can show a file browser dialog and stack it above the sandboxed client's
//! surface.
//!
//! Warning! The protocol described in this file is experimental and backward
//! incompatible changes may be made. Backward compatible changes may be added
//! together with the corresponding interface version bump. Backward
//! incompatible changes are done by bumping the version number in the protocol
//! and interface names and resetting the interface version. Once the protocol
//! is to be declared stable, the 'z' prefix and the version number in the
//! protocol and interface names are removed and the interface version number is
//! reset.
//!

use crate::{Interface, Proxy, ffi};

static ZXDG_EXPORTER_V2_INTERFACE: ffi::Interface = ffi::Interface {
    name: c"zxdg_exporter_v2".as_ptr(),
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
                name: c"export_toplevel".as_ptr(),
                signature: c"no".as_ptr(),
                types: const { [crate::ZxdgExportedV2::DEF, crate::Surface::DEF] }.as_ptr(),
            },
        ]
    }
    .as_ptr(),
    event_count: 0,
    events: const { [] }.as_ptr(),
};

#[repr(transparent)]
pub struct ZxdgExporterV2(pub(crate) Proxy);
unsafe impl Interface for ZxdgExporterV2 {
    const DEF: *const ffi::Interface = &ZXDG_EXPORTER_V2_INTERFACE;

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<ZxdgExporterV2 as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}

impl ZxdgExporterV2 {
    #[inline]
    pub fn export_toplevel(
        &self,
        surface: &crate::Surface,
    ) -> crate::Result<crate::Owned<crate::ZxdgExportedV2>> {
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
pub enum ZxdgExporterV2Error {
    InvalidSurface = 0,
}

static ZXDG_IMPORTER_V2_INTERFACE: ffi::Interface = ffi::Interface {
    name: c"zxdg_importer_v2".as_ptr(),
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
                name: c"import_toplevel".as_ptr(),
                signature: c"ns".as_ptr(),
                types: const { [crate::ZxdgImportedV2::DEF, core::ptr::null()] }.as_ptr(),
            },
        ]
    }
    .as_ptr(),
    event_count: 0,
    events: const { [] }.as_ptr(),
};

#[repr(transparent)]
pub struct ZxdgImporterV2(pub(crate) Proxy);
unsafe impl Interface for ZxdgImporterV2 {
    const DEF: *const ffi::Interface = &ZXDG_IMPORTER_V2_INTERFACE;

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<ZxdgImporterV2 as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}

impl ZxdgImporterV2 {
    #[inline]
    pub fn import_toplevel(
        &self,
        handle: &core::ffi::CStr,
    ) -> crate::Result<crate::Owned<crate::ZxdgImportedV2>> {
        Ok(unsafe {
            crate::Owned::wrap_unchecked(self.0.marshal_array_typed(
                1,
                &mut [crate::NEWID_ARG, ffi::Argument { s: handle.as_ptr() }],
            )?)
        })
    }
}

static ZXDG_EXPORTED_V2_INTERFACE: ffi::Interface = ffi::Interface {
    name: c"zxdg_exported_v2".as_ptr(),
    version: 1,
    method_count: 1,
    methods: const {
        [ffi::Message {
            name: c"destroy".as_ptr(),
            signature: c"".as_ptr(),
            types: const { [] }.as_ptr(),
        }]
    }
    .as_ptr(),
    event_count: 1,
    events: const {
        [ffi::Message {
            name: c"handle".as_ptr(),
            signature: c"s".as_ptr(),
            types: const { [core::ptr::null()] }.as_ptr(),
        }]
    }
    .as_ptr(),
};

#[repr(transparent)]
pub struct ZxdgExportedV2(pub(crate) Proxy);
unsafe impl Interface for ZxdgExportedV2 {
    const DEF: *const ffi::Interface = &ZXDG_EXPORTED_V2_INTERFACE;

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<ZxdgExportedV2 as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}

impl ZxdgExportedV2 {
    pub fn set_listener<'l, L: ZxdgExportedV2EventListener + 'l>(
        &'l mut self,
        listener: &'l mut L,
    ) -> crate::SetListenerResult {
        extern "C" fn handle<L: ZxdgExportedV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
            handle: *const core::ffi::c_char,
        ) {
            L::handle(
                unsafe { &mut *(data0 as *mut _) },
                unsafe { &mut *(sender0 as *mut _) },
                unsafe { core::ffi::CStr::from_ptr(handle) },
            )
        }

        #[repr(C)]
        struct FPTable {
            handle: extern "C" fn(
                data0: *mut core::ffi::c_void,
                sender0: *mut ffi::Proxy,
                handle: *const core::ffi::c_char,
            ),
        }
        unsafe {
            self.0.set_listener(
                &const {
                    FPTable {
                        handle: handle::<L>,
                    }
                } as &'static FPTable as *const _ as _,
                listener as *mut _ as _,
            )
        }
    }
}

pub trait ZxdgExportedV2EventListener {
    fn handle(&mut self, sender: &mut ZxdgExportedV2, handle: &core::ffi::CStr);
}

static ZXDG_IMPORTED_V2_INTERFACE: ffi::Interface = ffi::Interface {
    name: c"zxdg_imported_v2".as_ptr(),
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
                name: c"set_parent_of".as_ptr(),
                signature: c"o".as_ptr(),
                types: const { [crate::Surface::DEF] }.as_ptr(),
            },
        ]
    }
    .as_ptr(),
    event_count: 1,
    events: const {
        [ffi::Message {
            name: c"destroyed".as_ptr(),
            signature: c"".as_ptr(),
            types: const { [] }.as_ptr(),
        }]
    }
    .as_ptr(),
};

#[repr(transparent)]
pub struct ZxdgImportedV2(pub(crate) Proxy);
unsafe impl Interface for ZxdgImportedV2 {
    const DEF: *const ffi::Interface = &ZXDG_IMPORTED_V2_INTERFACE;

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<ZxdgImportedV2 as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}

impl ZxdgImportedV2 {
    pub fn set_listener<'l, L: ZxdgImportedV2EventListener + 'l>(
        &'l mut self,
        listener: &'l mut L,
    ) -> crate::SetListenerResult {
        extern "C" fn destroyed<L: ZxdgImportedV2EventListener>(
            data0: *mut core::ffi::c_void,
            sender0: *mut ffi::Proxy,
        ) {
            L::destroyed(unsafe { &mut *(data0 as *mut _) }, unsafe {
                &mut *(sender0 as *mut _)
            })
        }

        #[repr(C)]
        struct FPTable {
            destroyed: extern "C" fn(data0: *mut core::ffi::c_void, sender0: *mut ffi::Proxy),
        }
        unsafe {
            self.0.set_listener(
                &const {
                    FPTable {
                        destroyed: destroyed::<L>,
                    }
                } as &'static FPTable as *const _ as _,
                listener as *mut _ as _,
            )
        }
    }

    #[inline]
    pub fn set_parent_of(&self, surface: &crate::Surface) -> crate::Result<()> {
        self.0.marshal_array_void(1, &mut [surface.0.as_arg()])
    }
}

pub trait ZxdgImportedV2EventListener {
    fn destroyed(&mut self, sender: &mut ZxdgImportedV2);
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ZxdgImportedV2Error {
    InvalidSurface = 0,
}
