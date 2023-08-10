use bedrock as br;

use crate::{vk_pipeline_stage_mask_requirements_for_image_layout, IndexedMesh, Mesh};

pub trait GraphicsCommand {
    fn execute(self, cb: &mut br::CmdRecord<'_, impl br::CommandBuffer + br::VkHandleMut>);

    fn execute_and_finish(
        self,
        mut cb: br::CmdRecord<'_, impl br::CommandBuffer + br::VkHandleMut>,
    ) -> br::Result<()>
    where
        Self: Sized,
    {
        self.execute(&mut cb);
        cb.end()
    }

    #[deprecated = "please use execute_and_finish instead, due to reduce maintenance cost"]
    unsafe fn execute_into_ext_sync(
        self,
        cb: &mut (impl br::CommandBuffer + br::VkHandleMut),
    ) -> br::Result<()>
    where
        Self: Sized,
    {
        self.execute_and_finish(cb.begin()?)
    }
    #[deprecated = "please use execute_and_finish instead, due to reduce maintenance cost"]
    fn execute_into(
        self,
        mut sync_cb: br::SynchronizedCommandBuffer<
            '_,
            '_,
            impl br::CommandPool + br::VkHandleMut,
            impl br::CommandBuffer + br::VkHandleMut,
        >,
    ) -> br::Result<()>
    where
        Self: Sized,
    {
        self.execute_and_finish(sync_cb.begin()?)
    }

    fn submit(self, engine: &mut peridot::Engine<impl peridot::NativeLinker>) -> br::Result<()>
    where
        Self: Sized,
    {
        engine.submit_commands(|mut r| {
            self.execute(&mut r);
            r
        })
    }

    #[inline]
    fn then<C>(self, next: C) -> (Self, C)
    where
        Self: Sized,
    {
        (self, next)
    }

    #[inline]
    fn after_of<B>(self, before: B) -> (B, Self)
    where
        Self: Sized,
    {
        (before, self)
    }

    #[inline]
    fn between<B, A>(self, before: B, after: A) -> (B, Self, A)
    where
        Self: Sized,
    {
        (before, self, after)
    }
}
/// consecutive exec
impl<A: GraphicsCommand, B: GraphicsCommand> GraphicsCommand for (A, B) {
    fn execute(self, cb: &mut br::CmdRecord<'_, impl br::CommandBuffer + br::VkHandleMut>) {
        self.0.execute(cb);
        self.1.execute(cb);
    }
}
/// consecutive exec
impl<A: GraphicsCommand, B: GraphicsCommand, C: GraphicsCommand> GraphicsCommand for (A, B, C) {
    fn execute(self, cb: &mut br::CmdRecord<'_, impl br::CommandBuffer + br::VkHandleMut>) {
        self.0.execute(cb);
        self.1.execute(cb);
        self.2.execute(cb);
    }
}

impl<P: br::Pipeline, L: br::PipelineLayout> GraphicsCommand for peridot::LayoutedPipeline<P, L> {
    fn execute(self, cb: &mut br::CmdRecord<'_, impl br::CommandBuffer + br::VkHandleMut>) {
        self.bind(cb);
    }
}
impl<P: br::Pipeline, L: br::PipelineLayout> GraphicsCommand
    for &'_ peridot::LayoutedPipeline<P, L>
{
    fn execute(self, cb: &mut br::CmdRecord<'_, impl br::CommandBuffer + br::VkHandleMut>) {
        self.bind(cb);
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
impl GraphicsCommand for PipelineBarrier {
    fn execute(self, cb: &mut br::CmdRecord<'_, impl br::CommandBuffer + br::VkHandleMut>) {
        // Note: src_stage_mask=0はVulkanの仕様上だめらしい
        let src_stage_mask = if self.src_stage_mask.0 == 0 {
            br::PipelineStageFlags::TOP_OF_PIPE
        } else {
            self.src_stage_mask
        };

        let _ = cb.pipeline_barrier(
            src_stage_mask,
            self.dst_stage_mask,
            self.by_region,
            &[],
            &self.buffer_barriers,
            &self.image_barriers,
        );
    }
}

pub struct CopyBuffer<S: br::Buffer, D: br::Buffer>(S, D, Vec<br::vk::VkBufferCopy>);
impl<S: br::Buffer, D: br::Buffer> CopyBuffer<S, D> {
    pub const fn new(source: S, dest: D) -> Self {
        Self(source, dest, Vec::new())
    }

    pub fn with_range(mut self, src_offset: u64, dest_offset: u64, size: usize) -> Self {
        self.2.push(br::vk::VkBufferCopy {
            srcOffset: src_offset,
            dstOffset: dest_offset,
            size: size as _,
        });
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
impl<S: br::Buffer, D: br::Buffer> GraphicsCommand for CopyBuffer<S, D> {
    fn execute(self, cb: &mut br::CmdRecord<'_, impl br::CommandBuffer + br::VkHandleMut>) {
        let _ = cb.copy_buffer(&self.0, &self.1, &self.2);
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
            bufferRowLength: buffer_data_desc.row_bytes,
            bufferImageHeight: buffer_data_desc.image_height,
            imageSubresource: image_range.layers,
            imageOffset: image_range.offset,
            imageExtent: image_range.extent,
        });
        self
    }
}
impl<S: br::Buffer, D: br::Image> GraphicsCommand for CopyBufferToImage<S, D> {
    fn execute(self, cb: &mut br::CmdRecord<'_, impl br::CommandBuffer + br::VkHandleMut>) {
        let _ = cb.copy_buffer_to_image(
            &self.source,
            &self.dest,
            self.dest_image_layout,
            &self.regions,
        );
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
impl<'f, R, D, I> BeginRenderPass<R, &'f br::FramebufferObject<D, I>>
where
    R: br::RenderPass<ConcreteDevice = D>,
    D: br::Device,
    I: br::ImageView<ConcreteDevice = D>,
{
    pub fn for_entire_framebuffer(
        render_pass: R,
        framebuffer: &'f br::FramebufferObject<D, I>,
        clear_values: Vec<br::ClearValue>,
    ) -> Self {
        Self {
            rect: framebuffer
                .size()
                .clone()
                .into_rect(br::vk::VkOffset2D { x: 0, y: 0 }),
            render_pass,
            framebuffer,
            clear_values,
            inline_commands: true,
        }
    }
}
impl<R: br::RenderPass, F: br::Framebuffer> GraphicsCommand for BeginRenderPass<R, F> {
    fn execute(self, cb: &mut br::CmdRecord<'_, impl br::CommandBuffer + br::VkHandleMut>) {
        let _ = cb.begin_render_pass(
            &self.render_pass,
            &self.framebuffer,
            self.rect,
            &self.clear_values,
            self.inline_commands,
        );
    }
}

pub struct NextSubpass(bool);
impl NextSubpass {
    pub const WITH_INLINE_COMMANDS: Self = Self(true);
    pub const WITH_COMMAND_BUFFER_EXECUTIONS: Self = Self(false);
}
impl GraphicsCommand for NextSubpass {
    fn execute(self, cb: &mut br::CmdRecord<'_, impl br::CommandBuffer + br::VkHandleMut>) {
        let _ = cb.next_subpass(self.0);
    }
}

pub struct EndRenderPass;
impl GraphicsCommand for EndRenderPass {
    fn execute(self, cb: &mut br::CmdRecord<'_, impl br::CommandBuffer + br::VkHandleMut>) {
        let _ = cb.end_render_pass();
    }
}

#[repr(transparent)]
pub struct DescriptorSets(pub Vec<br::vk::VkDescriptorSet>);
impl DescriptorSets {
    pub fn bind_graphics(&self) -> BindGraphicsDescriptorSets {
        BindGraphicsDescriptorSets {
            from: 0,
            sets: &self.0[..],
            dynamic_offsets: &[],
        }
    }
}

pub struct BindGraphicsDescriptorSets<'s> {
    from: u32,
    sets: &'s [br::vk::VkDescriptorSet],
    dynamic_offsets: &'s [u32],
}
impl<'s> BindGraphicsDescriptorSets<'s> {
    pub const fn new(sets: &'s [br::vk::VkDescriptorSet]) -> Self {
        Self {
            from: 0,
            sets,
            dynamic_offsets: &[],
        }
    }

    pub const fn with_first(first: u32, sets: &'s [br::vk::VkDescriptorSet]) -> Self {
        Self {
            from: first,
            sets,
            dynamic_offsets: &[],
        }
    }

    pub const fn from(self, first: u32) -> Self {
        Self {
            from: first,
            ..self
        }
    }
}
impl GraphicsCommand for BindGraphicsDescriptorSets<'_> {
    fn execute(self, cb: &mut br::CmdRecord<'_, impl br::CommandBuffer + br::VkHandleMut>) {
        let _ = cb.bind_graphics_descriptor_sets(self.from, self.sets, self.dynamic_offsets);
    }
}

pub struct PushConstant<T> {
    shader_stage: br::ShaderStage,
    offset: u32,
    value: T,
}
impl<T> PushConstant<T> {
    pub const fn for_fragment(offset: u32, value: T) -> Self {
        Self {
            shader_stage: br::ShaderStage::FRAGMENT,
            offset,
            value,
        }
    }
}
impl<T> GraphicsCommand for PushConstant<T> {
    fn execute(self, cb: &mut br::CmdRecord<'_, impl br::CommandBuffer + br::VkHandleMut>) {
        if (self.shader_stage.0 & br::vk::VK_SHADER_STAGE_COMPUTE_BIT) != 0 {
            // assumes compute pipeline
            let _ = cb.push_compute_constant(self.shader_stage, self.offset, &self.value);
        } else {
            let _ = cb.push_graphics_constant(self.shader_stage, self.offset, &self.value);
        }
    }
}

pub struct ViewportWithScissorRect(pub br::vk::VkViewport, pub br::vk::VkRect2D);
impl GraphicsCommand for ViewportWithScissorRect {
    fn execute(self, cb: &mut br::CmdRecord<'_, impl br::CommandBuffer + br::VkHandleMut>) {
        let _ = cb.set_viewport(0, &[self.0]).set_scissor(0, &[self.1]);
    }
}
impl<const N: usize> GraphicsCommand for [ViewportWithScissorRect; N] {
    fn execute(self, cb: &mut br::CmdRecord<'_, impl br::CommandBuffer + br::VkHandleMut>) {
        let (viewports, scissors): (Vec<_>, Vec<_>) = self.into_iter().map(|a| (a.0, a.1)).unzip();

        let _ = cb.set_viewport(0, &viewports).set_scissor(0, &scissors);
    }
}

pub struct PreConfigureDraw<'m, B: br::Buffer>(pub &'m Mesh<B>);
impl<B: br::Buffer> GraphicsCommand for PreConfigureDraw<'_, B> {
    fn execute(self, cb: &mut br::CmdRecord<'_, impl br::CommandBuffer + br::VkHandleMut>) {
        let vertex_buffers = self
            .0
            .vertex_buffers
            .iter()
            .map(|rb| (&rb.0, rb.1.start as usize))
            .collect::<Vec<_>>();

        let _ = cb.bind_vertex_buffers(0, &vertex_buffers);
    }
}

pub struct SimpleDraw(pub u32, pub u32, pub u32, pub u32);
impl GraphicsCommand for SimpleDraw {
    fn execute(self, cb: &mut br::CmdRecord<'_, impl br::CommandBuffer + br::VkHandleMut>) {
        let _ = cb.draw(self.0, self.1, self.2, self.3);
    }
}

pub struct DrawMesh<'m, B: br::Buffer> {
    pub mesh: &'m Mesh<B>,
    pub instance_count: u32,
    pub vertex_start: u32,
    pub instance_start: u32,
}
impl<B: br::Buffer> GraphicsCommand for DrawMesh<'_, B> {
    fn execute(self, cb: &mut br::CmdRecord<'_, impl br::CommandBuffer + br::VkHandleMut>) {
        PreConfigureDraw(self.mesh)
            .then(SimpleDraw(
                self.mesh.vertex_count,
                self.instance_count,
                self.vertex_start,
                self.instance_start,
            ))
            .execute(cb);
    }
}

pub struct DrawIndexedMesh<'m, B: br::Buffer, IB: br::Buffer> {
    pub mesh: &'m IndexedMesh<B, IB>,
    pub instance_count: u32,
    pub vertex_start: u32,
    pub index_offset: i32,
    pub instance_start: u32,
}
impl<B: br::Buffer, IB: br::Buffer> GraphicsCommand for DrawIndexedMesh<'_, B, IB> {
    fn execute(self, cb: &mut br::CmdRecord<'_, impl br::CommandBuffer + br::VkHandleMut>) {
        let vertex_buffers = self
            .mesh
            .vertex_buffers
            .iter()
            .map(|rb| (&rb.0, rb.1.start as usize))
            .collect::<Vec<_>>();

        let _ = cb
            .bind_vertex_buffers(0, &vertex_buffers)
            .bind_index_buffer(
                &self.mesh.index_buffer.inner_ref(),
                self.mesh.index_buffer.offset() as _,
                self.mesh.index_type,
            )
            .draw_indexed(
                self.mesh.vertex_count,
                self.instance_count,
                self.vertex_start,
                self.index_offset,
                self.instance_start,
            );
    }
}

impl<const N: usize> GraphicsCommand for [br::vk::VkCommandBuffer; N] {
    fn execute(self, cb: &mut br::CmdRecord<'_, impl br::CommandBuffer + br::VkHandleMut>) {
        let _ = unsafe { cb.execute_commands(&self) };
    }
}
impl GraphicsCommand for Vec<br::vk::VkCommandBuffer> {
    fn execute(self, cb: &mut br::CmdRecord<'_, impl br::CommandBuffer + br::VkHandleMut>) {
        let _ = unsafe { cb.execute_commands(&self[..]) };
    }
}

pub struct BufferImageDataDesc {
    offset: u64,
    row_bytes: u32,
    image_height: u32,
}
impl BufferImageDataDesc {
    pub const fn new(offset: u64, row_bytes: u32) -> Self {
        Self {
            offset,
            row_bytes,
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
