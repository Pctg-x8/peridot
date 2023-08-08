use std::ops::Range;

use bedrock as br;

#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BufferUsage(pub u64);
impl BufferUsage {
    pub const UNUSED: Self = Self(0);

    pub const HOST_RO: Self = Self(1 << 0);
    pub const HOST_RW: Self = Self(1 << 1);
    pub const TRANSFER_SRC: Self = Self(1 << 2);
    pub const TRANSFER_DST: Self = Self(1 << 3);
    pub const VERTEX_BUFFER: Self = Self(1 << 4);
    pub const INDEX_BUFFER: Self = Self(1 << 5);
    pub const INDIRECT_BUFFER: Self = Self(1 << 6);

    // shader resources usage
    pub const VERTEX_UNIFORM: Self = Self(1 << 8);
    pub const VERTEX_STORAGE_RO: Self = Self(1 << 9);
    pub const VERTEX_STORAGE_RW: Self = Self(1 << 10);
    pub const FRAGMENT_UNIFORM: Self = Self(1 << 11);
    pub const FRAGMENT_STORAGE_RO: Self = Self(1 << 12);
    pub const FRAGMENT_STORAGE_RW: Self = Self(1 << 13);

    pub const fn has_bits(&self, bits: Self) -> bool {
        (self.0 & bits.0) != 0
    }

    pub fn vk_pipeline_stage_mask_requirements(&self) -> br::PipelineStageFlags {
        let mut f = br::PipelineStageFlags(0);

        f = if self.has_bits(Self::HOST_RO | Self::HOST_RW) {
            f.host()
        } else {
            f
        };
        f = if self.has_bits(Self::TRANSFER_SRC | Self::TRANSFER_DST) {
            f.transfer()
        } else {
            f
        };
        f = if self.has_bits(Self::VERTEX_BUFFER) {
            f.vertex_input()
        } else {
            f
        };
        f = if self.has_bits(Self::INDEX_BUFFER) {
            f.vertex_input()
        } else {
            f
        };
        f = if self.has_bits(Self::INDIRECT_BUFFER) {
            f.draw_indirect()
        } else {
            f
        };
        f = if self
            .has_bits(Self::VERTEX_UNIFORM | Self::VERTEX_STORAGE_RO | Self::VERTEX_STORAGE_RW)
        {
            f.vertex_shader()
        } else {
            f
        };
        f = if self.has_bits(
            Self::FRAGMENT_UNIFORM | Self::FRAGMENT_STORAGE_RO | Self::FRAGMENT_STORAGE_RW,
        ) {
            f.fragment_shader()
        } else {
            f
        };

        f
    }

    pub fn vk_access_flags_requirements(&self) -> br::vk::VkAccessFlags {
        let mut f = 0;

        f |= if self.has_bits(Self::HOST_RO) {
            br::AccessFlags::HOST.read
        } else {
            0
        };
        f |= if self.has_bits(Self::HOST_RW) {
            br::AccessFlags::HOST.write | br::AccessFlags::HOST.read
        } else {
            0
        };
        f |= if self.has_bits(Self::TRANSFER_SRC) {
            br::AccessFlags::TRANSFER.read
        } else {
            0
        };
        f |= if self.has_bits(Self::TRANSFER_DST) {
            br::AccessFlags::TRANSFER.write
        } else {
            0
        };
        f |= if self.has_bits(Self::VERTEX_BUFFER) {
            br::AccessFlags::VERTEX_ATTRIBUTE_READ
        } else {
            0
        };
        f |= if self.has_bits(Self::INDEX_BUFFER) {
            br::AccessFlags::INDEX_READ
        } else {
            0
        };
        f |= if self.has_bits(Self::INDIRECT_BUFFER) {
            br::AccessFlags::INDIRECT_COMMAND_READ
        } else {
            0
        };
        f |= if self.has_bits(Self::VERTEX_UNIFORM | Self::FRAGMENT_UNIFORM) {
            br::AccessFlags::UNIFORM_READ
        } else {
            0
        };
        f |= if self.has_bits(Self::VERTEX_STORAGE_RO | Self::FRAGMENT_STORAGE_RO) {
            br::AccessFlags::SHADER.read
        } else {
            0
        };
        f |= if self.has_bits(Self::VERTEX_STORAGE_RW | Self::FRAGMENT_STORAGE_RW) {
            br::AccessFlags::SHADER.read | br::AccessFlags::SHADER.write
        } else {
            0
        };

        f
    }
}
impl std::ops::BitOr for BufferUsage {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}
impl std::ops::BitOrAssign for BufferUsage {
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0;
    }
}

fn vk_pipeline_stage_mask_requirements_for_image_layout(
    l: br::ImageLayout,
) -> br::PipelineStageFlags {
    match l {
        br::ImageLayout::Undefined | br::ImageLayout::Preinitialized => br::PipelineStageFlags(0),
        br::ImageLayout::General => br::PipelineStageFlags::ALL_COMMANDS,
        br::ImageLayout::ColorAttachmentOpt => br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
        br::ImageLayout::DepthStencilAttachmentOpt => {
            br::PipelineStageFlags::EARLY_FRAGMENT_TESTS.late_fragment_tests()
        }
        br::ImageLayout::DepthStencilReadOnlyOpt => br::PipelineStageFlags::EARLY_FRAGMENT_TESTS,
        br::ImageLayout::ShaderReadOnlyOpt => {
            br::PipelineStageFlags::VERTEX_SHADER.fragment_shader()
        }
        br::ImageLayout::TransferSrcOpt | br::ImageLayout::TransferDestOpt => {
            br::PipelineStageFlags::TRANSFER
        }
        br::ImageLayout::PresentSrc => br::PipelineStageFlags::BOTTOM_OF_PIPE,
    }
}

pub struct BufferUsageTransitionBarrier<'r, B: br::Buffer> {
    pub buffer: &'r RangedBuffer<B>,
    pub from_usage: BufferUsage,
    pub to_usage: BufferUsage,
}
impl<B: br::Buffer> BufferUsageTransitionBarrier<'_, B> {
    pub fn make_vk_barrier(&self) -> br::BufferMemoryBarrier {
        br::BufferMemoryBarrier::new(
            &self.buffer.0,
            self.buffer.1.clone(),
            self.from_usage.vk_access_flags_requirements(),
            self.to_usage.vk_access_flags_requirements(),
        )
    }
}
impl<B: br::Buffer> PipelineBarrierEntry for BufferUsageTransitionBarrier<'_, B> {
    fn add_into(self, barrier: &mut PipelineBarrier) {
        barrier.buffer_barriers.push(self.make_vk_barrier());
        barrier.src_stage_mask |= self.from_usage.vk_pipeline_stage_mask_requirements();
        barrier.dst_stage_mask |= self.to_usage.vk_pipeline_stage_mask_requirements();
    }

    fn reserve_hints(barrier: &mut PipelineBarrier, count: usize) {
        barrier.buffer_barriers.reserve(count);
    }
}

pub struct RangedBuffer<B: br::Buffer>(pub B, pub Range<u64>);
impl<B: br::Buffer> RangedBuffer<B> {
    pub const fn from_offset_length(buffer: B, offset: u64, length: usize) -> Self {
        Self(buffer, offset..offset + length as u64)
    }

    pub const fn for_type<T>(buffer: B, offset: u64) -> Self {
        Self::from_offset_length(buffer, offset, std::mem::size_of::<T>())
    }

    pub const fn offset(&self) -> u64 {
        self.1.start
    }

    pub const fn byte_length(&self) -> u64 {
        self.1.end - self.1.start
    }

    pub fn make_ref<'s>(&'s self) -> RangedBuffer<&'s B> {
        RangedBuffer(&self.0, self.1.clone())
    }

    pub fn descriptor_uniform_buffer_write_info(&self) -> (br::vk::VkBuffer, Range<usize>) {
        (
            self.0.native_ptr(),
            self.1.start as usize..self.1.end as usize,
        )
    }

    pub fn barrier(
        &self,
        from_access_mask: br::vk::VkAccessFlags,
        to_access_mask: br::vk::VkAccessFlags,
    ) -> br::BufferMemoryBarrier {
        br::BufferMemoryBarrier::new(&self.0, self.1.clone(), from_access_mask, to_access_mask)
    }

    pub fn usage_barrier(
        &self,
        from_usage: BufferUsage,
        to_usage: BufferUsage,
    ) -> BufferUsageTransitionBarrier<B> {
        BufferUsageTransitionBarrier {
            buffer: &self,
            from_usage,
            to_usage,
        }
    }

    pub fn usage_barrier3(
        &self,
        first_usage: BufferUsage,
        intermedial_usage: BufferUsage,
        last_usage: BufferUsage,
    ) -> [BufferUsageTransitionBarrier<B>; 2] {
        [
            self.usage_barrier(first_usage, intermedial_usage),
            self.usage_barrier(intermedial_usage, last_usage),
        ]
    }

    pub fn usage_barrier3_switching(
        &self,
        first_usage: BufferUsage,
        intermedial_usage: BufferUsage,
    ) -> [BufferUsageTransitionBarrier<B>; 2] {
        self.usage_barrier3(first_usage, intermedial_usage, first_usage)
    }

    pub fn inner_ref(&self) -> &B {
        &self.0
    }
}
impl<B: br::Buffer + Clone> RangedBuffer<&'_ B> {
    pub fn clone_inner_ref(&self) -> RangedBuffer<B> {
        RangedBuffer(self.0.clone(), self.1.clone())
    }
}
impl<B: br::Buffer + br::MemoryBound + br::VkHandleMut, M: br::DeviceMemory + br::VkHandleMut>
    RangedBuffer<peridot::Buffer<B, M>>
{
    pub fn guard_map<R>(
        &mut self,
        action: impl FnOnce(&br::MappedMemoryRange<M>) -> R,
    ) -> br::Result<R> {
        self.0.guard_map(self.1.clone(), action)
    }
}

pub struct RangedImage<R: br::Image>(R, br::ImageSubresourceRange);
impl<R: br::Image> RangedImage<R> {
    pub fn single_color_plane(resource: R) -> Self {
        Self(resource, br::ImageSubresourceRange::color(0..1, 0..1))
    }

    pub fn barrier(
        &self,
        from_layout: br::ImageLayout,
        to_layout: br::ImageLayout,
    ) -> br::ImageMemoryBarrier {
        br::ImageMemoryBarrier::new(&self.0, self.1.clone(), from_layout, to_layout)
    }

    pub fn barrier3(
        &self,
        first_layout: br::ImageLayout,
        intermedial_layout: br::ImageLayout,
        last_layout: br::ImageLayout,
    ) -> [br::ImageMemoryBarrier; 2] {
        [
            self.barrier(first_layout, intermedial_layout),
            self.barrier(intermedial_layout, last_layout),
        ]
    }
}

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

impl<const N: usize, B: br::Buffer> GraphicsCommand for [BufferUsageTransitionBarrier<'_, B>; N] {
    fn execute(self, cb: &mut br::CmdRecord<'_, impl br::CommandBuffer + br::VkHandleMut>) {
        PipelineBarrier::new().with_barriers(self).execute(cb)
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

    pub fn add_buffer_barrier(mut self, b: br::BufferMemoryBarrier) -> Self {
        self.buffer_barriers.push(b);
        self
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
    const WITH_INLINE_COMMANDS: Self = Self(true);
    const WITH_COMMAND_BUFFER_EXECUTIONS: Self = Self(false);
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

pub struct Mesh<B: br::Buffer> {
    pub vertex_buffers: Vec<RangedBuffer<B>>,
    pub vertex_count: u32,
}
impl<B: br::Buffer> Mesh<B> {
    pub const fn draw<'m>(&'m self, instance_count: u32) -> DrawMesh<'m, B> {
        DrawMesh {
            mesh: self,
            instance_count,
            vertex_start: 0,
            instance_start: 0,
        }
    }
}

pub struct DrawMesh<'m, B: br::Buffer> {
    mesh: &'m Mesh<B>,
    instance_count: u32,
    vertex_start: u32,
    instance_start: u32,
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
            offset: br::vk::VkOffset3D {
                x: rect.offset.x,
                y: rect.offset.y,
                z: 0,
            },
            extent: br::vk::VkExtent3D {
                width: rect.extent.width,
                height: rect.extent.height,
                depth: 1,
            },
        }
    }

    /// default is 0
    pub const fn with_mip_level(mut self, level: u32) -> Self {
        self.layers.mipLevel = level;

        self
    }
}
