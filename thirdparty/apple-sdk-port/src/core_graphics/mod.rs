use core::ffi::*;

#[cfg(target_pointer_width = "64")]
pub type CGFloat = c_double;
#[cfg(not(target_pointer_width = "64"))]
pub type CGFloat = c_float;

#[repr(C)]
pub struct __IOSurface(
    [u8; 0],
    core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,
);
pub type IOSurfaceRef = *mut __IOSurface;

mod data_provider;
pub use self::data_provider::*;

mod color_space;
pub use self::color_space::*;

mod color;
pub use self::color::*;

mod font;
pub use self::font::*;

mod geometry;
pub use self::geometry::*;

mod affine_transform;
pub use self::affine_transform::*;

mod path;
pub use self::path::*;
