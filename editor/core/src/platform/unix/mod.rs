#[cfg(feature = "wayland")]
pub mod wayland;

#[cfg(feature = "wayland")]
pub type DisplayServerLink = self::wayland::DisplayServerLink;
