use bedrock as br;

use crate::{vk_pipeline_stage_mask_requirements_for_image_layout, Mesh};

pub trait GraphicsCommand {
    fn execute(self, cb: &mut br::CmdRecord<'_, impl br::CommandBuffer + br::VkHandleMut>);

    unsafe fn execute_into_ext_sync(
        self,
        cb: &mut (impl br::CommandBuffer + br::VkHandleMut),
    ) -> br::Result<()>
    where
        Self: Sized,
    {
        let mut r = cb.begin()?;
        self.execute(&mut r);
        r.end()
    }
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
        let mut r = sync_cb.begin()?;
        self.execute(&mut r);
        r.end()
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
        let _ = cb.pipeline_barrier(
            self.src_stage_mask,
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

pub struct BeginRenderPass<R: br::RenderPass, F: br::Framebuffer> {
    render_pass: R,
    framebuffer: F,
    rect: br::vk::VkRect2D,
    clear_values: Vec<br::ClearValue>,
    inline_commands: bool,
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

    pub fn non_inline_commands(self) -> Self {
        Self {
            inline_commands: false,
            ..self
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

pub struct BindGraphicsDescriptorSets {
    from: u32,
    sets: Vec<br::vk::VkDescriptorSet>,
    dynamic_offsets: Vec<u32>,
}
impl BindGraphicsDescriptorSets {
    pub const fn new(sets: Vec<br::vk::VkDescriptorSet>) -> Self {
        Self {
            from: 0,
            sets,
            dynamic_offsets: Vec::new(),
        }
    }

    pub const fn with_first(first: u32, sets: Vec<br::vk::VkDescriptorSet>) -> Self {
        Self {
            from: first,
            sets,
            dynamic_offsets: Vec::new(),
        }
    }
}
impl GraphicsCommand for BindGraphicsDescriptorSets {
    fn execute(self, cb: &mut br::CmdRecord<'_, impl br::CommandBuffer + br::VkHandleMut>) {
        let _ = cb.bind_graphics_descriptor_sets(self.from, &self.sets, &self.dynamic_offsets);
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
        let vertex_buffers = self
            .mesh
            .vertex_buffers
            .iter()
            .map(|rb| (&rb.0, rb.1.start as usize))
            .collect::<Vec<_>>();

        let _ = cb.bind_vertex_buffers(0, &vertex_buffers).draw(
            self.mesh.vertex_count,
            self.instance_count,
            self.vertex_start,
            self.instance_start,
        );
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
