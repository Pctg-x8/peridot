use bedrock as br;

pub struct SurfaceInfo {
    pub(crate) obj: br::Surface,
    pub(crate) fmt: br::vk::VkSurfaceFormatKHR,
    pub(crate) pres_mode: br::PresentMode,
    pub(crate) available_composite_alpha: br::CompositeAlpha
}
impl SurfaceInfo {
    pub fn gather_info(pd: &br::PhysicalDevice, obj: br::Surface) -> br::Result<Self> {
        let mut fmq = br::FormatQueryPred::default();
        fmq.bit(32).components(br::FormatComponents::RGBA).elements(br::ElementType::UNORM);
        let fmt = pd.surface_formats(&obj)?.into_iter().find(|sf| fmq.satisfy(sf.format))
            .expect("No suitable format found");
        let pres_modes = pd.surface_present_modes(&obj)?;
        let &pres_mode = pres_modes.iter().find(|&&m| m == br::PresentMode::FIFO || m == br::PresentMode::Mailbox)
            .unwrap_or(&pres_modes[0]);
        
        let caps = pd.surface_capabilities(&obj)?;
        let available_composite_alpha = if (caps.supportedCompositeAlpha & (br::CompositeAlpha::Inherit as u32)) != 0 {
            br::CompositeAlpha::Inherit
        }
        else { br::CompositeAlpha::Opaque };
        
        return Ok(SurfaceInfo { obj, fmt, pres_mode, available_composite_alpha });
    }
    pub fn format(&self) -> br::vk::VkFormat { self.fmt.format }
}
