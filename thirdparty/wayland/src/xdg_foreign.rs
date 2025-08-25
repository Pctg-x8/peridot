//! https://gitlab.freedesktop.org/wayland/wayland-protocols/-/blob/main/unstable/xdg-foreign/xdg-foreign-unstable-v2.xml

use super::{Interface, NEWID_ARG, Owned, Proxy, ffi, interface, message};

static ZXDG_EXPORTER_V2_INTERFACE: ffi::Interface = interface(
    c"zxdg_exporter_v2",
    1,
    &[
        message(c"destroy", c"", &[]),
        message(
            c"export_toplevel",
            c"no",
            &[&ZXDG_EXPORTED_V2_INTERFACE, unsafe {
                &super::wl_surface_interface
            }],
        ),
    ],
    &[],
);

/// A global interface used for exporting surfaces that can later be imported using `xdg_importer`.
#[repr(transparent)]
pub struct ZxdgExporterV2(Proxy);
unsafe impl Interface for ZxdgExporterV2 {
    fn def() -> &'static ffi::Interface {
        &ZXDG_EXPORTER_V2_INTERFACE
    }

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<ZxdgExporterV2 as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}
impl ZxdgExporterV2 {
    /// The export_toplevel request exports the passed surface so that it canlater be imported via `xdg_importer`.
    /// When called, a new `xdg_exported` object will be created and `xdg_exported.handle` will be send immediately.
    /// See the corresponding interface and event for details.
    ///
    /// A surface may be exported multiple times, and each exported handle may be used to create an `xdg_imported` multiple times.
    /// Only `xdg_toplevel` equivalent surfaces may be exported, otherwise an `invalid_surface` protocol error is sent.
    #[inline]
    pub fn export_toplevel(
        &self,
        surface: &super::Surface,
    ) -> Result<Owned<ZxdgExportedV2>, std::io::Error> {
        Ok(unsafe {
            Owned::from_untyped_unchecked(self.0.marshal_array_flags(
                1,
                ZxdgExportedV2::def(),
                self.0.version(),
                0,
                &mut [NEWID_ARG, surface.0.as_arg()],
            )?)
        })
    }
}

static ZXDG_EXPORTED_V2_INTERFACE: ffi::Interface = interface(
    c"zxdg_exported_v2",
    1,
    &[message(c"destroy", c"", &[])],
    &[message(c"handle", c"s", &[])],
);

/// An `xdg_exported` object represents an exported refrence to a surface.
/// The exported surface maybe references as long as the `xdg_exported` object not destroyed.
/// Destroying the `xdg_exported` invalidates any relationship the importer may have established using `xdg_imported`.
#[repr(transparent)]
pub struct ZxdgExportedV2(Proxy);
unsafe impl Interface for ZxdgExportedV2 {
    fn def() -> &'static ffi::Interface {
        &ZXDG_EXPORTED_V2_INTERFACE
    }

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<ZxdgExportedV2 as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}
impl ZxdgExportedV2 {
    pub fn add_listener<'l, L: ZxdgExportedV2EventListener + 'l>(
        &'l mut self,
        listener: &'l mut L,
    ) -> Result<(), ()> {
        extern "C" fn handle<L: ZxdgExportedV2EventListener>(
            data: *mut core::ffi::c_void,
            sender: *mut ffi::Proxy,
            handle: *const core::ffi::c_char,
        ) {
            let listener = unsafe { &mut *(data as *mut L) };

            listener.handle(unsafe { core::mem::transmute(&mut *sender) }, unsafe {
                core::ffi::CStr::from_ptr(handle)
            })
        }
        #[repr(C)]
        struct FunctionPointer {
            handle:
                extern "C" fn(*mut core::ffi::c_void, *mut ffi::Proxy, *const core::ffi::c_char),
        }
        let fp: &'static FunctionPointer = &FunctionPointer {
            handle: handle::<L>,
        };

        unsafe {
            self.0
                .add_listener(fp as *const _ as _, listener as *mut _ as _)
        }
    }
}

pub trait ZxdgExportedV2EventListener {
    /// The handle event contains the unique handle of this exported surface reference.
    /// It may be shared with any client, which then can use it to import the surface by calling `xdg_imported.import_toplevel`.
    /// A handle may be used to import the surface multiple times.
    fn handle(&mut self, sender: &mut ZxdgExportedV2, handle: &core::ffi::CStr);
}
