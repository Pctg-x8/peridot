pub const APPMENU_OBJECT_PATH: &core::ffi::CStr = c"/AppMenu";

#[cfg(feature = "wayland")]
pub mod wayland;

#[cfg(feature = "wayland")]
pub type DisplayServerLink = self::wayland::DisplayServerLink;
