#[cfg(windows)]
use shared::Color32;
use shared::{PixelsUnit, Point};
use windows::{UI::Color, Win32::Foundation::POINT};

#[inline(always)]
pub const fn point_to_win32(p: &Point<PixelsUnit>) -> POINT {
    POINT { x: p.x, y: p.y }
}

#[inline(always)]
pub const fn point_from_win32(point: POINT) -> Point<PixelsUnit> {
    Point::new_pixels(point.x, point.y)
}

#[inline(always)]
pub const fn windows_native_color(c: &Color32) -> Color {
    Color {
        A: c.a,
        R: c.r,
        G: c.g,
        B: c.b,
    }
}
