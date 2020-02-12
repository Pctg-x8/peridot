//! Peridot Utilities

use bedrock as br;

pub trait PixelGeometryProvider
{
    fn pixel_perfect_in_normalized(&self, x: f32, y: f32) -> (f32, f32);
}
impl PixelGeometryProvider for br::vk::VkViewport
{
    fn pixel_perfect_in_normalized(&self, x: f32, y: f32) -> (f32, f32)
    {
        ((x * self.width).trunc() * self.width, (y * self.height).trunc() * self.height)
    }
}
impl PixelGeometryProvider for br::vk::VkRect2D
{
    fn pixel_perfect_in_normalized(&self, x: f32, y: f32) -> (f32, f32)
    {
        self.extent.pixel_perfect_in_normalized(x, y)
    }
}
impl PixelGeometryProvider for br::vk::VkExtent2D
{
    fn pixel_perfect_in_normalized(&self, x: f32, y: f32) -> (f32, f32)
    {
        ((x * self.width as f32).trunc() / self.width as f32, (y * self.height as f32).trunc() / self.height as f32)
    }
}
impl PixelGeometryProvider for br::Extent2D
{
    fn pixel_perfect_in_normalized(&self, x: f32, y: f32) -> (f32, f32)
    {
        ((x * (self.0 as f32 / 2.0)).trunc() / (self.0 as f32 / 2.0),
            (y * (self.1 as f32 / 2.0)).trunc() / (self.1 as f32 / 2.0))
    }
}
