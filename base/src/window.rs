use bedrock as br;

pub struct SurfaceInfo {
    pub(crate) fmt: br::vk::VkSurfaceFormatKHR,
    pub(crate) pres_mode: br::PresentMode,
    pub(crate) available_composite_alpha: br::vk::VkCompositeAlphaFlagsKHR,
}
impl SurfaceInfo {
    pub fn gather_info(pd: &impl br::PhysicalDevice, obj: &impl br::Surface) -> br::Result<Self> {
        let mut fmq = br::FormatQueryPred::default();
        fmq.bit(32)
            .components(br::FormatComponents::RGBA)
            .elements(br::ElementType::UNORM);
        let fmt = pd
            .surface_formats_alloc(&obj)?
            .into_iter()
            .find(|sf| fmq.satisfy(sf.format))
            .expect("No suitable format found");
        let pres_modes = pd.surface_present_modes_alloc(&obj)?;
        let &pres_mode = pres_modes
            .iter()
            .find(|&&m| m == br::PresentMode::FIFO || m == br::PresentMode::Mailbox)
            .unwrap_or(&pres_modes[0]);

        let caps = pd.surface_capabilities(&obj)?;
        let available_composite_alpha =
            if (caps.supportedCompositeAlpha & br::vk::VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR) != 0 {
                br::vk::VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR
            } else {
                br::vk::VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR
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
