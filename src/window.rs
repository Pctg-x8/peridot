use super::*;
use bedrock as br;

use std::mem::{replace, forget};

pub struct SurfaceInfo {
    pub(crate) obj: br::Surface,
    pub(crate) fmt: br::vk::VkSurfaceFormatKHR,
    pub(crate) pres_mode: br::PresentMode,
    pub(crate) available_composite_alpha: br::CompositeAlpha
}
impl SurfaceInfo {
    pub fn gather_info(pd: &br::PhysicalDevice, obj: br::Surface) -> br::Result<Self> {
        let mut fmq = br::FormatQueryPred::default();
        fmq.bit(32).components(br::FormatComponents::RGBA).elements(br::ElementType::SRGB);
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

pub enum StateFence { Signaled(br::Fence), Unsignaled(br::Fence) }
impl StateFence {
    pub fn new(d: &br::Device) -> br::Result<Self> { br::Fence::new(d, false).map(StateFence::Unsignaled) }
    /// must be coherent with background API
    pub unsafe fn signal(&mut self) {
        let obj = std::ptr::read(match self { StateFence::Signaled(f) | StateFence::Unsignaled(f) => f as *const _ });
        forget(replace(self, StateFence::Signaled(obj)));
    }
    /// must be coherent with background API
    unsafe fn unsignal(&mut self) {
        let obj = std::ptr::read(match self { StateFence::Signaled(f) | StateFence::Unsignaled(f) => f as *const _ });
        forget(replace(self, StateFence::Unsignaled(obj)));
    }

    pub fn wait(&mut self) -> br::Result<()> {
        if let StateFence::Signaled(ref f) = *self { f.wait()?; f.reset()?; }
        unsafe { self.unsignal(); } return Ok(());
    }

    pub fn object(&self) -> &br::Fence {
        match *self { StateFence::Signaled(ref f) | StateFence::Unsignaled(ref f) => f }
    }
}
