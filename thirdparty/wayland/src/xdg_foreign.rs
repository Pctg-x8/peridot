//! https://gitlab.freedesktop.org/wayland/wayland-protocols/-/blob/main/unstable/xdg-foreign/xdg-foreign-unstable-v2.xml

use crate::EventFnTable;

use super::{Interface, NEWID_ARG, Owned, Proxy, ffi, interface, message};

/// A global interface used for exporting surfaces that can later be imported using `xdg_importer`.
#[repr(transparent)]
pub struct ZxdgExporterV2(Proxy);
unsafe impl Interface for ZxdgExporterV2 {
    const DEF: &'static ffi::Interface = &interface(
        c"zxdg_exporter_v2",
        1,
        &[
            message(c"destroy", c"", &[]),
            message(
                c"export_toplevel",
                c"no",
                &[ZxdgExportedV2::DEF, super::Surface::DEF],
            ),
        ],
        &[],
    );

    #[cfg_attr(
        feature = "tracing",
        tracing::instrument(name = "<ZxdgExporterV2 as Interface>::destruct", skip(self))
    )]
    unsafe fn destruct(&mut self) {
        self.0.call_simple_dtor(0);
    }
}
impl ZxdgExporterV2 {
    /// The export_toplevel request exports the passed surface so that it can later be imported via `xdg_importer`.
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
            Owned::wrap_unchecked(
                self.0
                    .marshal_array_typed(1, &mut [NEWID_ARG, surface.0.as_arg()])?,
            )
        })
    }
}

/// An `xdg_exported` object represents an exported reference to a surface.
/// The exported surface maybe references as long as the `xdg_exported` object not destroyed.
/// Destroying the `xdg_exported` invalidates any relationship the importer may have established using `xdg_imported`.
#[repr(transparent)]
pub struct ZxdgExportedV2(Proxy);
unsafe impl Interface for ZxdgExportedV2 {
    const DEF: &'static ffi::Interface = &interface(
        c"zxdg_exported_v2",
        1,
        &[message(c"destroy", c"", &[])],
        &[message(c"handle", c"s", &[])],
    );

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
    ) -> Result<(), ()> {
        unsafe {
            self.0.set_listener(
                EventFnTable!(for L: ZxdgExportedV2EventListener {
                    handle(handle: *const core::ffi::c_char => unsafe { core::ffi::CStr::from_ptr(handle) })
                }) as *const _ as _,
                listener as *mut _ as _
            )
        }
    }
}

pub trait ZxdgExportedV2EventListener {
    /// The handle event contains the unique handle of this exported surface reference.
    /// It may be shared with any client, which then can use it to import the surface by calling `xdg_imported.import_toplevel`.
    /// A handle may be used to import the surface multiple times.
    fn handle(&mut self, sender: &mut ZxdgExportedV2, handle: &core::ffi::CStr);
}
