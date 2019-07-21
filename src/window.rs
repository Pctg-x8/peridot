#![allow(clippy::needless_return)]

use super::*;
use bedrock as br;

use std::mem::{uninitialized, replace, forget};

pub trait PlatformRenderTarget {
    fn surface_extension_name(&self) -> &'static str;
    fn create_surface(&self, vi: &br::Instance, pd: &br::PhysicalDevice, renderer_queue_family: u32)
            -> br::Result<SurfaceInfo>;
    fn current_geometry_extent(&self) -> (usize, usize);
}

pub struct SurfaceInfo {
    obj: br::Surface, fmt: br::vk::VkSurfaceFormatKHR, pres_mode: br::PresentMode,
    available_composite_alpha: br::CompositeAlpha
}
impl SurfaceInfo
{
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
        else {
            br::CompositeAlpha::Opaque
        };
        
        return Ok(SurfaceInfo { obj, fmt, pres_mode, available_composite_alpha });
    }
    pub fn format(&self) -> br::vk::VkFormat { self.fmt.format }
}

pub(super) struct WindowRenderTargets
{
    chain: br::Swapchain, bb: Vec<br::ImageView>
}
impl WindowRenderTargets
{
    pub(super) fn new<PRT: PlatformRenderTarget>(g: &Graphics, s: &SurfaceInfo, prt: &PRT) -> br::Result<Self>
    {
        let si = g.adapter.surface_capabilities(&s.obj)?;
        let ew =
            if si.currentExtent.width == 0xffff_ffff { prt.current_geometry_extent().0 as _ }
            else { si.currentExtent.width };
        let eh =
            if si.currentExtent.height == 0xffff_ffff { prt.current_geometry_extent().1 as _ }
            else { si.currentExtent.height };
        let ext = br::Extent2D(ew, eh);
        let buffer_count = 2.max(si.minImageCount).min(si.maxImageCount);
        let chain = br::SwapchainBuilder::new(&s.obj, buffer_count, &s.fmt, &ext, br::ImageUsage::COLOR_ATTACHMENT)
            .present_mode(s.pres_mode)
            .composite_alpha(s.available_composite_alpha).pre_transform(br::SurfaceTransform::Identity)
            .create(&g.device)?;
        
        let isr_c0 = br::ImageSubresourceRange::color(0, 0);
        let images = chain.get_images()?;
        let mut bb = Vec::with_capacity(images.len());
        for x in images
        {
            bb.push(x.create_view(None, None, &Default::default(), &isr_c0)?);
        }

        return Ok(WindowRenderTargets { bb, chain });
    }

    pub(super) fn emit_initialize_backbuffers_commands(&self, recorder: &mut br::CmdRecord) {
        let image_barriers: Vec<_> = self.bb.iter()
            .map(|v| br::ImageSubref::color(v, 0, 0))
            .map(|s| br::ImageMemoryBarrier::new(&s, br::ImageLayout::Undefined, br::ImageLayout::PresentSrc))
            .collect();
        recorder.pipeline_barrier(br::PipelineStageFlags::TOP_OF_PIPE, br::PipelineStageFlags::BOTTOM_OF_PIPE, false,
            &[], &[], &image_barriers);
    }

    pub fn backbuffers(&self) -> &[br::ImageView] { &self.bb }
    pub fn acquire_next_backbuffer_index(&self, timeout: Option<u64>, completion_handler: br::CompletionHandler)
            -> br::Result<u32> {
        self.chain.acquire_next(timeout, completion_handler)
    }
    pub fn present_on(&self, q: &br::Queue, index: u32, occurence_after: &[&br::Semaphore]) -> br::Result<()> {
        self.chain.queue_present(q, index, occurence_after)
    }
}

pub enum StateFence { Signaled(br::Fence), Unsignaled(br::Fence) }
impl StateFence {
    pub fn new(d: &br::Device) -> br::Result<Self> { br::Fence::new(d, false).map(StateFence::Unsignaled) }
    /// must be coherent with background API
    pub unsafe fn signal(&mut self) {
        let unsafe_ = replace(self, uninitialized());
        forget(replace(self, StateFence::Signaled(unsafe_.take_object())));
    }
    /// must be coherent with background API
    unsafe fn unsignal(&mut self) {
        let unsafe_ = replace(self, uninitialized());
        forget(replace(self, StateFence::Unsignaled(unsafe_.take_object())));
    }

    pub fn wait(&mut self) -> br::Result<()> {
        if let StateFence::Signaled(ref f) = *self { f.wait()?; f.reset()?; }
        unsafe { self.unsignal(); } return Ok(());
    }
    pub fn is_signaled(&self) -> bool { match *self { StateFence::Signaled(_) => true, _ => false } }

    pub fn object(&self) -> &br::Fence {
        match *self { StateFence::Signaled(ref f) | StateFence::Unsignaled(ref f) => f }
    }
    fn take_object(self) -> br::Fence { match self { StateFence::Signaled(f) | StateFence::Unsignaled(f) => f } }
}
impl Deref for StateFence
{
    type Target = br::Fence;
    fn deref(&self) -> &br::Fence { self.object() }
}
