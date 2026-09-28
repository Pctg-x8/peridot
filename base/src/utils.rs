//! Peridot Utilities

use bedrock as br;

pub trait PixelGeometryProvider {
    fn pixel_perfect_in_normalized(&self, x: f32, y: f32) -> (f32, f32);
}
impl PixelGeometryProvider for br::vk::VkViewport {
    fn pixel_perfect_in_normalized(&self, x: f32, y: f32) -> (f32, f32) {
        (
            (x * self.width).trunc() * self.width,
            (y * self.height).trunc() * self.height,
        )
    }
}
impl PixelGeometryProvider for br::vk::VkRect2D {
    fn pixel_perfect_in_normalized(&self, x: f32, y: f32) -> (f32, f32) {
        self.extent.pixel_perfect_in_normalized(x, y)
    }
}
impl PixelGeometryProvider for br::vk::VkExtent2D {
    fn pixel_perfect_in_normalized(&self, x: f32, y: f32) -> (f32, f32) {
        (
            (x * self.width as f32).trunc() / self.width as f32,
            (y * self.height as f32).trunc() / self.height as f32,
        )
    }
}

#[inline(always)]
pub fn fmt_hex2(f: &mut (impl core::fmt::Write + ?Sized), v: u8) -> core::fmt::Result {
    #[inline(always)]
    fn h(v: u8) -> char {
        match v {
            0..=9 => (v + b'0') as char,
            10..=15 => (v - 10 + b'a') as char,
            _ => unreachable!(),
        }
    }

    f.write_char(h(v >> 4))?;
    f.write_char(h(v & 0x0f))?;

    Ok(())
}
