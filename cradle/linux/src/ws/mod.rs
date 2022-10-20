use bedrock as br;
use std::os::fd::RawFd;

pub mod xcb;
pub mod xlib;

pub trait WindowSystemBackend {
    fn init() -> Self;
    fn flush(&self);
    fn show(&self);
    /// Returns false if application has beed exited
    fn process_all_events(&mut self) -> bool;
    fn mainwnd_geometry(&self) -> &peridot::math::Vector2<usize>;
    fn fd(&self) -> RawFd;
}

pub trait InputSystemBackend {
    fn get_pointer_position(&self) -> Option<(f32, f32)>;
    fn is_focused(&self) -> bool;
    fn query_states_batched(&self) -> (bool, (i16, i16));
}

pub trait VulkanPresentable {
    const REQUIRED_INSTANCE_EXTENSION: &'static str;

    fn presentation_support(
        &self,
        adapter: &impl br::PhysicalDevice,
        render_queue_family: u32,
    ) -> bool;

    fn create_surface<
        PhysicalDevice: br::PhysicalDevice + br::InstanceChild + br::InstanceChildTransferrable,
    >(
        &self,
        adapter: PhysicalDevice,
    ) -> br::Result<br::SurfaceObject<PhysicalDevice::ConcreteInstance>>;
}
