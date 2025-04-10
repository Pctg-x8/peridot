use bedrock as br;

use crate::graphics::VulkanGfx;

pub struct SurfaceInfo {
    pub(crate) fmt: br::SurfaceFormat,
    pub(crate) pres_mode: br::PresentMode,
    pub(crate) available_composite_alpha: br::CompositeAlphaFlags,
}
impl SurfaceInfo {
    pub fn gather_info(
        device: &VulkanGfx,
        obj: &(impl br::VkHandle<Handle = br::vk::VkSurfaceKHR> + ?Sized),
    ) -> br::Result<Self> {
        let mut fmq = br::FormatQueryPred::default();
        fmq.bit(32)
            .components(br::FormatComponents::RGBA)
            .elements(br::ElementType::UNORM);
        let fmt = device
            .surface_formats(obj)?
            .into_iter()
            .find(|sf| fmq.satisfy(sf.format))
            .expect("No suitable format found");
        let pres_modes = device.surface_present_modes(obj)?;
        let &pres_mode = pres_modes
            .iter()
            .find(|&&m| m == br::PresentMode::FIFO || m == br::PresentMode::Mailbox)
            .unwrap_or(&pres_modes[0]);

        let caps = device.surface_capabilities(obj)?;
        let available_composite_alpha =
            if (caps.supportedCompositeAlpha & br::vk::VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR) != 0 {
                br::CompositeAlphaFlags::INHERIT
            } else {
                br::CompositeAlphaFlags::OPAQUE
            };

        Ok(Self {
            fmt,
            pres_mode,
            available_composite_alpha,
        })
    }

    pub const fn format(&self) -> br::vk::VkFormat {
        self.fmt.format
    }
}
