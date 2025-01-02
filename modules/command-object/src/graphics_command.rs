use bedrock::{self as br, VkHandle};
use br::vk::VkCommandBuffer;

use crate::{
    vk_pipeline_stage_mask_requirements_for_image_layout, BufferUsageTransitionBarrier,
    IndexedMesh, Mesh,
};

pub trait GraphicsCommand<Device: br::Device + ?Sized> {
    fn execute<'r>(
        &self,
        cb: br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device>,
    ) -> br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device>;

    fn execute_and_finish(
        &self,
        cb: br::CmdRecord<'_, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device>,
    ) -> br::Result<()> {
        self.execute(cb).end()
    }
}
impl<T: GraphicsCommand<Device> + ?Sized, Device: br::Device + ?Sized> GraphicsCommand<Device>
    for Box<T>
{
    fn execute<'r>(
        &self,
        cb: br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device>,
    ) -> br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device> {
        T::execute(&*self, cb)
    }
}
impl<T: GraphicsCommand<Device> + ?Sized, Device: br::Device + ?Sized> GraphicsCommand<Device>
    for std::rc::Rc<T>
{
    fn execute<'r>(
        &self,
        cb: br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device>,
    ) -> br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device> {
        T::execute(&*self, cb)
    }
}
impl<T: GraphicsCommand<Device> + ?Sized, Device: br::Device + ?Sized> GraphicsCommand<Device>
    for std::sync::Arc<T>
{
    fn execute<'r>(
        &self,
        cb: br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device>,
    ) -> br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device> {
        T::execute(&*self, cb)
    }
}
impl<T: GraphicsCommand<Device> + ?Sized, Device: br::Device + ?Sized> GraphicsCommand<Device>
    for &'_ T
{
    fn execute<'r>(
        &self,
        cb: br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device>,
    ) -> br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device> {
        T::execute(*self, cb)
    }
}

pub trait GraphicsCommandSubmission: GraphicsCommand<peridot::DeviceObject> {
    fn submit(&self, engine: &mut peridot::Engine<impl peridot::NativeLinker>) -> br::Result<()> {
        engine.submit_commands(|mut r| {
            let _ = self.execute(r.as_dyn_ref());
            r
        })
    }
}
impl<T: GraphicsCommand<peridot::DeviceObject>> GraphicsCommandSubmission for T {}
pub trait GraphicsCommandCombiner: Sized {
    #[inline]
    fn then<C>(self, next: C) -> (Self, C) {
        (self, next)
    }

    #[inline]
    fn after_of<B>(self, before: B) -> (B, Self) {
        (before, self)
    }

    #[inline]
    fn between<B, A>(self, before: B, after: A) -> (B, Self, A) {
        (before, self, after)
    }

    #[inline]
    fn boxed<Device: br::Device + ?Sized>(self) -> Box<dyn GraphicsCommand<Device>>
    where
        Self: GraphicsCommand<Device> + 'static,
    {
        Box::new(self) as _
    }
}
impl<T: Sized> GraphicsCommandCombiner for T {}

/// consecutive exec
impl<A: GraphicsCommand<Device>, B: GraphicsCommand<Device>, Device: br::Device + ?Sized>
    GraphicsCommand<Device> for (A, B)
{
    fn execute<'r>(
        &self,
        cb: br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device>,
    ) -> br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device> {
        let cb = self.0.execute(cb);
        self.1.execute(cb)
    }
}
/// consecutive exec
impl<
        A: GraphicsCommand<Device>,
        B: GraphicsCommand<Device>,
        C: GraphicsCommand<Device>,
        Device: br::Device,
    > GraphicsCommand<Device> for (A, B, C)
{
    fn execute<'r>(
        &self,
        cb: br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device>,
    ) -> br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device> {
        let cb = self.0.execute(cb);
        let cb = self.1.execute(cb);
        self.2.execute(cb)
    }
}
/// consecutive exec
impl<
        A: GraphicsCommand<Device>,
        B: GraphicsCommand<Device>,
        C: GraphicsCommand<Device>,
        D: GraphicsCommand<Device>,
        Device: br::Device,
    > GraphicsCommand<Device> for (A, B, C, D)
{
    fn execute<'r>(
        &self,
        cb: br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device>,
    ) -> br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device> {
        let cb = self.0.execute(cb);
        let cb = self.1.execute(cb);
        let cb = self.2.execute(cb);
        self.3.execute(cb)
    }
}
/// consecutive exec
impl<
        A: GraphicsCommand<Device>,
        B: GraphicsCommand<Device>,
        C: GraphicsCommand<Device>,
        D: GraphicsCommand<Device>,
        E: GraphicsCommand<Device>,
        Device: br::Device,
    > GraphicsCommand<Device> for (A, B, C, D, E)
{
    fn execute<'r>(
        &self,
        cb: br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device>,
    ) -> br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device> {
        let cb = self.0.execute(cb);
        let cb = self.1.execute(cb);
        let cb = self.2.execute(cb);
        let cb = self.3.execute(cb);
        self.4.execute(cb)
    }
}
/// consecutive exec
impl<T: GraphicsCommand<Device>, Device: br::Device + ?Sized> GraphicsCommand<Device> for Vec<T> {
    fn execute<'r>(
        &self,
        cb: br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device>,
    ) -> br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device> {
        (&self[..]).execute(cb)
    }
}
impl<T, Device: br::Device + ?Sized> GraphicsCommand<Device> for [T]
where
    for<'x> &'x T: GraphicsCommand<Device>,
{
    fn execute<'r>(
        &self,
        cb: br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device>,
    ) -> br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device> {
        self.iter().fold(cb, |cb, r| r.execute(cb))
    }
}

impl<P: br::Pipeline, L: br::PipelineLayout, Device: br::Device + ?Sized> GraphicsCommand<Device>
    for peridot::LayoutedPipeline<P, L>
{
    fn execute<'r>(
        &self,
        cb: br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device>,
    ) -> br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device> {
        self.bind(cb)
    }
}

pub trait PipelineBarrierEntry {
    fn add_into(self, barrier: &mut PipelineBarrier);

    #[allow(unused_variables)]
    fn reserve_hints(barrier: &mut PipelineBarrier, count: usize) {}
}
impl PipelineBarrierEntry for br::ImageMemoryBarrier {
    fn add_into(self, barrier: &mut PipelineBarrier) {
        let r: br::vk::VkImageMemoryBarrier = self.into();
        barrier.src_stage_mask |= vk_pipeline_stage_mask_requirements_for_image_layout(unsafe {
            std::mem::transmute(r.oldLayout)
        });
        barrier.dst_stage_mask |= vk_pipeline_stage_mask_requirements_for_image_layout(unsafe {
            std::mem::transmute(r.newLayout)
        });
        barrier.image_barriers.push(r.into());
    }

    fn reserve_hints(barrier: &mut PipelineBarrier, count: usize) {
        barrier.image_barriers.reserve(count);
    }
}

pub struct PipelineBarrier {
    pub src_stage_mask: br::PipelineStageFlags,
    pub dst_stage_mask: br::PipelineStageFlags,
    pub by_region: bool,
    pub buffer_barriers: Vec<br::BufferMemoryBarrier>,
    pub image_barriers: Vec<br::ImageMemoryBarrier>,
}
impl PipelineBarrier {
    pub const fn new() -> Self {
        Self {
            src_stage_mask: br::PipelineStageFlags(0),
            dst_stage_mask: br::PipelineStageFlags(0),
            by_region: false,
            buffer_barriers: Vec::new(),
            image_barriers: Vec::new(),
        }
    }

    pub fn by_region(self) -> Self {
        Self {
            by_region: true,
            ..self
        }
    }

    pub fn with_barrier(mut self, b: impl PipelineBarrierEntry) -> Self {
        b.add_into(&mut self);
        self
    }

    pub fn with_barriers<I>(mut self, bs: I) -> Self
    where
        I: IntoIterator,
        <I as IntoIterator>::Item: PipelineBarrierEntry,
    {
        let iter = bs.into_iter();
        let (expected_min_size, _) = iter.size_hint();
        <<I as IntoIterator>::Item as PipelineBarrierEntry>::reserve_hints(
            &mut self,
            expected_min_size,
        );

        iter.fold(self, |t, b| t.with_barrier(b))
    }
}
impl<Device: br::Device + ?Sized> GraphicsCommand<Device> for PipelineBarrier {
    fn execute<'r>(
        &self,
        cb: br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device>,
    ) -> br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device> {
        // Note: src_stage_mask=0はVulkanの仕様上だめらしい
        let src_stage_mask = if self.src_stage_mask.0 == 0 {
            br::PipelineStageFlags::TOP_OF_PIPE
        } else {
            self.src_stage_mask
        };

        cb.pipeline_barrier(
            src_stage_mask,
            self.dst_stage_mask,
            self.by_region,
            &[],
            &self.buffer_barriers,
            &self.image_barriers,
        )
    }
}
impl From<br::ImageMemoryBarrier> for PipelineBarrier {
    fn from(value: br::ImageMemoryBarrier) -> Self {
        Self::new().with_barrier(value)
    }
}
impl<B: br::Buffer> From<BufferUsageTransitionBarrier<B>> for PipelineBarrier {
    fn from(value: BufferUsageTransitionBarrier<B>) -> Self {
        Self::new().with_barrier(value)
    }
}

pub struct CopyBuffer<
    S: br::VkHandle<Handle = br::vk::VkBuffer>,
    D: br::VkHandle<Handle = br::vk::VkBuffer>,
>(S, D, Vec<br::BufferCopy>);
impl<S: br::VkHandle<Handle = br::vk::VkBuffer>, D: br::VkHandle<Handle = br::vk::VkBuffer>>
    CopyBuffer<S, D>
{
    pub const fn new(source: S, dest: D) -> Self {
        Self(source, dest, Vec::new())
    }

    pub fn with_ranges(mut self, ranges: impl IntoIterator<Item = br::BufferCopy>) -> Self {
        self.2.extend(ranges);

        self
    }

    pub fn with_range(mut self, src_offset: u64, dest_offset: u64, size: usize) -> Self {
        self.2.push(
            br::vk::VkBufferCopy {
                srcOffset: src_offset,
                dstOffset: dest_offset,
                size: size as _,
            }
            .into(),
        );
        self
    }

    pub fn with_range_for_type<T>(self, src_offset: u64, dest_offset: u64) -> Self {
        self.with_range(src_offset, dest_offset, std::mem::size_of::<T>())
    }

    pub fn with_mirroring(self, offset: u64, size: usize) -> Self {
        self.with_range(offset, offset, size)
    }

    pub fn with_mirroring_for_type<T>(self, offset: u64) -> Self {
        self.with_mirroring(offset, std::mem::size_of::<T>())
    }
}
impl<
        S: br::VkHandle<Handle = br::vk::VkBuffer>,
        D: br::VkHandle<Handle = br::vk::VkBuffer>,
        Device: br::Device + ?Sized,
    > GraphicsCommand<Device> for CopyBuffer<S, D>
{
    fn execute<'r>(
        &self,
        cb: br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device>,
    ) -> br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device> {
        cb.copy_buffer(&self.0, &self.1, &self.2)
    }
}

pub struct CopyBufferToImage<S: br::Buffer, D: br::Image> {
    source: S,
    dest: D,
    dest_image_layout: br::ImageLayout,
    regions: Vec<br::vk::VkBufferImageCopy>,
}
impl<S: br::Buffer, D: br::Image> CopyBufferToImage<S, D> {
    pub const fn new(source: S, dest: D) -> Self {
        Self {
            source,
            dest,
            dest_image_layout: br::ImageLayout::TransferDestOpt,
            regions: Vec::new(),
        }
    }

    /// default is TransferDestOpt
    pub fn with_more_optimal_image_layout(mut self, layout: br::ImageLayout) -> Self {
        self.dest_image_layout = layout;
        self
    }

    pub fn with_range(
        mut self,
        buffer_data_desc: BufferImageDataDesc,
        image_range: ImageResourceRange,
    ) -> Self {
        self.regions.push(br::vk::VkBufferImageCopy {
            bufferOffset: buffer_data_desc.offset,
            bufferRowLength: buffer_data_desc.row_texels,
            bufferImageHeight: buffer_data_desc.image_height,
            imageSubresource: image_range.layers,
            imageOffset: image_range.offset,
            imageExtent: image_range.extent,
        });
        self
    }
}
impl<
        S: br::Buffer + br::DeviceChild<ConcreteDevice = Device>,
        D: br::Image + br::DeviceChild<ConcreteDevice = Device>,
        Device: br::Device + ?Sized,
    > GraphicsCommand<Device> for CopyBufferToImage<S, D>
{
    fn execute<'r>(
        &self,
        cb: br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device>,
    ) -> br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device> {
        cb.copy_buffer_to_image(
            &self.source,
            &self.dest,
            self.dest_image_layout,
            &self.regions,
        )
    }
}

/// Default is for inline commands execution(for bundle execution needs explicit switching)
pub struct BeginRenderPass<R: br::RenderPass, F: br::Framebuffer> {
    render_pass: R,
    framebuffer: F,
    rect: br::vk::VkRect2D,
    clear_values: Vec<br::ClearValue>,
    inline_commands: bool,
}
impl<R: br::RenderPass, F: br::Framebuffer> BeginRenderPass<R, F> {
    pub const fn new(render_pass: R, framebuffer: F, rect: br::vk::VkRect2D) -> Self {
        Self {
            render_pass,
            framebuffer,
            rect,
            clear_values: Vec::new(),
            inline_commands: true,
        }
    }

    pub fn with_clear_values(self, clear_values: Vec<br::ClearValue>) -> Self {
        Self {
            clear_values,
            ..self
        }
    }

    pub fn non_inline_commands(self) -> Self {
        Self {
            inline_commands: false,
            ..self
        }
    }
}
impl<
        R: br::RenderPass + br::DeviceChild<ConcreteDevice = Device>,
        F: br::Framebuffer + br::DeviceChild<ConcreteDevice = Device>,
        Device: br::Device + ?Sized,
    > GraphicsCommand<Device> for BeginRenderPass<R, F>
{
    fn execute<'r>(
        &self,
        cb: br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device>,
    ) -> br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device> {
        cb.begin_render_pass(
            &self.render_pass,
            &self.framebuffer,
            self.rect.clone(),
            &self.clear_values,
            self.inline_commands,
        )
    }
}

pub struct NextSubpass(bool);
impl NextSubpass {
    pub const WITH_INLINE_COMMANDS: Self = Self(true);
    pub const WITH_COMMAND_BUFFER_EXECUTIONS: Self = Self(false);
}
impl<Device: br::Device + ?Sized> GraphicsCommand<Device> for NextSubpass {
    fn execute<'r>(
        &self,
        cb: br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device>,
    ) -> br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device> {
        cb.next_subpass(self.0)
    }
}

pub struct EndRenderPass;
impl<Device: br::Device + ?Sized> GraphicsCommand<Device> for EndRenderPass {
    fn execute<'r>(
        &self,
        cb: br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device>,
    ) -> br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device> {
        cb.end_render_pass()
    }
}

#[repr(transparent)]
pub struct BindGraphicsPipeline<Pipeline: br::Pipeline>(pub Pipeline);
impl<Pipeline, Device> GraphicsCommand<Device> for BindGraphicsPipeline<Pipeline>
where
    Pipeline: br::Pipeline + br::DeviceChild<ConcreteDevice = Device>,
    Device: br::Device,
{
    #[inline]
    fn execute<'r>(
        &self,
        cb: bedrock::CmdRecord<'r, dyn bedrock::VkHandleMut<Handle = VkCommandBuffer>, Device>,
    ) -> bedrock::CmdRecord<'r, dyn bedrock::VkHandleMut<Handle = VkCommandBuffer>, Device> {
        cb.bind_graphics_pipeline(&self.0)
    }
}

#[repr(transparent)]
pub struct DescriptorSets(pub Vec<br::DescriptorSet>);
impl DescriptorSets {
    pub fn bind_graphics<PipelineLayout: br::PipelineLayout>(
        &self,
        layout: PipelineLayout,
    ) -> BindGraphicsDescriptorSets<PipelineLayout, &[br::DescriptorSet], &'static [u32]> {
        BindGraphicsDescriptorSets::new(layout, &self.0[..])
    }

    pub fn into_bind_graphics<'p, PipelineLayout: br::PipelineLayout + 'p>(
        self,
        layout: PipelineLayout,
    ) -> BindGraphicsDescriptorSets<PipelineLayout, Vec<br::DescriptorSet>, &'static [u32]> {
        BindGraphicsDescriptorSets::new(layout, self.0)
    }
}

pub struct BindGraphicsDescriptorSets<
    PipelineLayout: br::PipelineLayout,
    Sets = &'static [br::DescriptorSet],
    DynamicOffsets = &'static [u32],
> where
    Sets: AsRef<[br::DescriptorSet]>,
    DynamicOffsets: AsRef<[u32]>,
{
    layout: PipelineLayout,
    from: u32,
    sets: Sets,
    dynamic_offsets: DynamicOffsets,
}
impl<PipelineLayout, Sets> BindGraphicsDescriptorSets<PipelineLayout, Sets, &'static [u32]>
where
    PipelineLayout: br::PipelineLayout,
    Sets: AsRef<[br::DescriptorSet]>,
{
    pub const fn new(layout: PipelineLayout, sets: Sets) -> Self {
        Self {
            layout,
            from: 0,
            sets,
            dynamic_offsets: &[],
        }
    }

    pub const fn with_first(layout: PipelineLayout, first: u32, sets: Sets) -> Self {
        Self {
            layout,
            from: first,
            sets,
            dynamic_offsets: &[],
        }
    }
}
impl<PipelineLayout, Sets, DynamicOffsets>
    BindGraphicsDescriptorSets<PipelineLayout, Sets, DynamicOffsets>
where
    PipelineLayout: br::PipelineLayout,
    Sets: AsRef<[br::DescriptorSet]>,
    DynamicOffsets: AsRef<[u32]>,
{
    pub fn from(self, layout: PipelineLayout, first: u32) -> Self {
        Self {
            layout,
            from: first,
            ..self
        }
    }
}
impl<PipelineLayout, Sets, DynamicOffsets, Device: br::Device + ?Sized> GraphicsCommand<Device>
    for BindGraphicsDescriptorSets<PipelineLayout, Sets, DynamicOffsets>
where
    PipelineLayout: br::PipelineLayout + br::DeviceChild<ConcreteDevice = Device>,
    Sets: AsRef<[br::DescriptorSet]>,
    DynamicOffsets: AsRef<[u32]>,
{
    fn execute<'r>(
        &self,
        cb: br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device>,
    ) -> br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device> {
        cb.bind_graphics_descriptor_sets(
            &self.layout,
            self.from,
            self.sets.as_ref(),
            self.dynamic_offsets.as_ref(),
        )
    }
}

pub struct PushConstant<PipelineLayout, T>
where
    PipelineLayout: br::PipelineLayout,
{
    pub layout: PipelineLayout,
    pub shader_stage: br::vk::VkShaderStageFlags,
    pub offset: u32,
    pub value: T,
}
impl<PipelineLayout, T> PushConstant<PipelineLayout, T>
where
    PipelineLayout: br::PipelineLayout,
{
    pub const fn for_fragment(layout: PipelineLayout, offset: u32, value: T) -> Self {
        Self {
            layout,
            shader_stage: br::vk::VK_SHADER_STAGE_FRAGMENT_BIT,
            offset,
            value,
        }
    }

    pub const fn for_vertex(layout: PipelineLayout, offset: u32, value: T) -> Self {
        Self {
            layout,
            shader_stage: br::vk::VK_SHADER_STAGE_VERTEX_BIT,
            offset,
            value,
        }
    }
}
impl<PipelineLayout, T, Device: br::Device + ?Sized> GraphicsCommand<Device>
    for PushConstant<PipelineLayout, T>
where
    PipelineLayout: br::PipelineLayout + br::DeviceChild<ConcreteDevice = Device>,
{
    #[inline]
    fn execute<'r>(
        &self,
        cb: br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device>,
    ) -> br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device> {
        cb.push_constant(&self.layout, self.shader_stage, self.offset, &self.value)
    }
}

pub struct ViewportWithScissorRect(pub br::vk::VkViewport, pub br::vk::VkRect2D);
impl<Device: br::Device + ?Sized> GraphicsCommand<Device> for ViewportWithScissorRect {
    fn execute<'r>(
        &self,
        cb: br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device>,
    ) -> br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device> {
        cb.set_viewport(0, &[self.0.clone()])
            .set_scissor(0, &[self.1.clone()])
    }
}
impl<const N: usize, Device: br::Device + ?Sized> GraphicsCommand<Device>
    for [ViewportWithScissorRect; N]
{
    fn execute<'r>(
        &self,
        cb: br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device>,
    ) -> br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device> {
        let (viewports, scissors): (Vec<_>, Vec<_>) =
            self.iter().map(|a| (a.0.clone(), a.1.clone())).unzip();

        cb.set_viewport(0, &viewports).set_scissor(0, &scissors)
    }
}

pub struct ViewportScissorRects {
    viewports: Vec<br::vk::VkViewport>,
    scissors: Vec<br::vk::VkRect2D>,
}
impl ViewportScissorRects {
    pub const fn new() -> Self {
        Self {
            viewports: Vec::new(),
            scissors: Vec::new(),
        }
    }

    pub fn add(mut self, viewport: br::vk::VkViewport, scissor: br::vk::VkRect2D) -> Self {
        self.viewports.push(viewport);
        self.scissors.push(scissor);

        self
    }
}
impl From<ViewportWithScissorRect> for ViewportScissorRects {
    fn from(value: ViewportWithScissorRect) -> Self {
        Self {
            viewports: vec![value.0],
            scissors: vec![value.1],
        }
    }
}
impl From<Vec<ViewportWithScissorRect>> for ViewportScissorRects {
    fn from(value: Vec<ViewportWithScissorRect>) -> Self {
        let (viewports, scissors) = value.into_iter().map(|v| (v.0, v.1)).unzip();

        Self {
            viewports,
            scissors,
        }
    }
}
impl<const N: usize> From<[ViewportWithScissorRect; N]> for ViewportScissorRects {
    fn from(value: [ViewportWithScissorRect; N]) -> Self {
        let (viewports, scissors) = value.into_iter().map(|v| (v.0, v.1)).unzip();

        Self {
            viewports,
            scissors,
        }
    }
}
impl<Device: br::Device + ?Sized> GraphicsCommand<Device> for ViewportScissorRects {
    fn execute<'r>(
        &self,
        cb: br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device>,
    ) -> br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device> {
        cb.set_viewport(0, &self.viewports)
            .set_scissor(0, &self.scissors)
    }
}

pub struct PreConfigureDraw<M: Mesh>(pub M);
impl<M: Mesh, Device: br::Device + ?Sized> GraphicsCommand<Device> for PreConfigureDraw<M> {
    fn execute<'r>(
        &self,
        cb: br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device>,
    ) -> br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device> {
        let (buffers, offsets): (Vec<_>, Vec<_>) = self
            .0
            .vertex_buffers()
            .iter()
            .map(|rb| {
                (
                    rb.0.as_transparent_ref(),
                    rb.1.start as br::vk::VkDeviceSize,
                )
            })
            .unzip();

        cb.bind_vertex_buffers(0, &buffers, &offsets)
    }
}

pub struct PreConfigureDrawIndexed<M: IndexedMesh>(pub M);
impl<M: IndexedMesh, Device: br::Device + ?Sized> GraphicsCommand<Device>
    for PreConfigureDrawIndexed<M>
{
    fn execute<'r>(
        &self,
        cb: br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device>,
    ) -> br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device> {
        let (buffers, offsets): (Vec<_>, Vec<_>) = self
            .0
            .vertex_buffers()
            .iter()
            .map(|rb| {
                (
                    rb.0.as_transparent_ref(),
                    rb.1.start as br::vk::VkDeviceSize,
                )
            })
            .unzip();

        cb.bind_vertex_buffers(0, &buffers, &offsets)
            .bind_index_buffer(
                &self.0.index_buffer().inner_ref(),
                self.0.index_buffer().offset() as _,
                self.0.index_type(),
            )
    }
}

pub struct SimpleDraw(pub u32, pub u32, pub u32, pub u32);
impl<Device: br::Device + ?Sized> GraphicsCommand<Device> for SimpleDraw {
    fn execute<'r>(
        &self,
        cb: br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device>,
    ) -> br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device> {
        cb.draw(self.0, self.1, self.2, self.3)
    }
}

pub struct SimpleDrawIndexed {
    pub index_count: u32,
    pub instance_count: u32,
    pub first_index: u32,
    pub vertex_offset: i32,
    pub first_instance: u32,
}
impl SimpleDrawIndexed {
    pub const fn new(index_count: u32, instance_count: u32) -> Self {
        Self {
            index_count,
            instance_count,
            first_index: 0,
            vertex_offset: 0,
            first_instance: 0,
        }
    }

    pub const fn with_vertex_offset(self, vertex_offset: i32) -> Self {
        Self {
            vertex_offset,
            ..self
        }
    }

    pub const fn from_index(self, first_index: u32) -> Self {
        Self {
            first_index,
            ..self
        }
    }
}
impl<Device: br::Device + ?Sized> GraphicsCommand<Device> for SimpleDrawIndexed {
    fn execute<'r>(
        &self,
        cb: br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device>,
    ) -> br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device> {
        cb.draw_indexed(
            self.index_count,
            self.instance_count,
            self.first_index,
            self.vertex_offset,
            self.first_instance,
        )
    }
}

pub struct DrawMesh<M: Mesh> {
    pub mesh: M,
    pub instance_count: u32,
    pub vertex_start: u32,
    pub instance_start: u32,
}
impl<M: Mesh, Device: br::Device + ?Sized> GraphicsCommand<Device> for DrawMesh<M>
where
    for<'r> &'r M: Mesh,
{
    fn execute<'r>(
        &self,
        cb: br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device>,
    ) -> br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device> {
        let vertex_count = self.mesh.vertex_count();

        (
            PreConfigureDraw(&self.mesh),
            SimpleDraw(
                vertex_count,
                self.instance_count,
                self.vertex_start,
                self.instance_start,
            ),
        )
            .execute(cb)
    }
}

pub struct DrawIndexedMesh<M: IndexedMesh> {
    pub mesh: M,
    pub instance_count: u32,
    pub vertex_start: u32,
    pub index_offset: i32,
    pub instance_start: u32,
}
impl<M: IndexedMesh, Device: br::Device + ?Sized> GraphicsCommand<Device> for DrawIndexedMesh<M>
where
    for<'r> &'r M: IndexedMesh,
{
    fn execute<'r>(
        &self,
        cb: br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device>,
    ) -> br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device> {
        (
            PreConfigureDrawIndexed(&self.mesh),
            SimpleDrawIndexed {
                index_count: self.mesh.vertex_count(),
                instance_count: self.instance_count,
                first_index: self.vertex_start,
                vertex_offset: self.index_offset,
                first_instance: self.instance_start,
            },
        )
            .execute(cb)
    }
}

#[repr(transparent)]
pub struct CommandBuffers(pub Vec<VkCommandBuffer>);

#[repr(transparent)]
pub struct CommandBuffersRef<'s>(pub &'s [VkCommandBuffer]);

impl<Device: br::Device + ?Sized> GraphicsCommand<Device> for CommandBuffersRef<'_> {
    fn execute<'r>(
        &self,
        cb: br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device>,
    ) -> br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device> {
        unsafe { cb.execute_commands(self.0) }
    }
}
impl<const N: usize, Device: br::Device + ?Sized> GraphicsCommand<Device>
    for [br::vk::VkCommandBuffer; N]
{
    fn execute<'r>(
        &self,
        cb: br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device>,
    ) -> br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device> {
        unsafe { cb.execute_commands(self) }
    }
}
impl<Device: br::Device + ?Sized> GraphicsCommand<Device> for CommandBuffers {
    fn execute<'r>(
        &self,
        cb: br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device>,
    ) -> br::CmdRecord<'r, dyn br::VkHandleMut<Handle = VkCommandBuffer>, Device> {
        unsafe { cb.execute_commands(&self.0[..]) }
    }
}

pub struct BufferImageDataDesc {
    offset: u64,
    row_texels: u32,
    image_height: u32,
}
impl BufferImageDataDesc {
    pub const fn new(offset: u64, row_texels: u32) -> Self {
        Self {
            offset,
            row_texels,
            image_height: 0,
        }
    }

    pub const fn with_explicit_image_height(self, image_height: u32) -> Self {
        Self {
            image_height,
            ..self
        }
    }
}

pub struct ImageResourceRange {
    layers: br::vk::VkImageSubresourceLayers,
    offset: br::vk::VkOffset3D,
    extent: br::vk::VkExtent3D,
}
impl ImageResourceRange {
    pub const fn for_single_color_from_rect2d(rect: br::vk::VkRect2D) -> Self {
        Self {
            layers: br::vk::VkImageSubresourceLayers {
                aspectMask: br::vk::VK_IMAGE_ASPECT_COLOR_BIT,
                mipLevel: 0,
                baseArrayLayer: 0,
                layerCount: 1,
            },
            offset: rect.offset.with_z(0),
            extent: rect.extent.with_depth(1),
        }
    }

    /// default is 0
    pub const fn with_mip_level(mut self, level: u32) -> Self {
        self.layers.mipLevel = level;

        self
    }
}

// Peridot base Model integration
// pub struct RenderBaseModel<'r, 'e, 'b, R, NL, Device, B>
// where
//     R: peridot::DefaultRenderCommands<'e, Device>,
//     B: br::Buffer<ConcreteDevice = Device>,
//     Device: br::Device,
//     NL: peridot::NativeLinker,
// {
//     pub provider: &'r R,
//     pub engine: &'e peridot::Engine<NL>,
//     pub buffer: &'b B,
//     pub extras: R::Extras,
// }
// impl<'e, R, NL, Device, B> GraphicsCommand for RenderBaseModel<'_, 'e, '_, R, NL, Device, B>
// where
//     R: peridot::DefaultRenderCommands<'e, Device>,
//     B: br::Buffer<ConcreteDevice = Device>,
//     Device: br::Device,
//     NL: peridot::NativeLinker,
// {
//     fn execute(&self, cb: &mut br::CmdRecord<'_, dyn br::VkHandleMut<Handle = VkCommandBuffer>>) {
//         self.provider
//             .default_render_commands(self.engine, cb, self.buffer, self.extras)
//     }
// }
