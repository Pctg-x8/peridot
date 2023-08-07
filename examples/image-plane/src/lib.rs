use bedrock as br;
use bedrock::traits::*;
use br::{resources::Image, SubmissionBatch};
use br::{DescriptorPool, Device, ImageChild};
use log::*;
use peridot::math::{
    Camera, Matrix4, Matrix4F32, One, ProjectionMethod, Quaternion, Vector2, Vector3, Vector3F32,
};
use peridot::mthelper::{DynamicMutabilityProvider, SharedRef};
use peridot::{
    audio::StreamingPlayableWav, BufferContent, BufferPrealloc, CBSubmissionType, CommandBundle,
    DescriptorSetUpdateBatch, LayoutedPipeline, SubpassDependencyTemplates,
};
use peridot_vertex_processing_pack::PvpShaderModules;
use std::convert::TryInto;
use std::marker::PhantomData;
use std::mem::{align_of, size_of};
use std::ops::Range;
use std::sync::{Arc, RwLock};
use std::time::Duration;

#[cfg(feature = "debug")]
use br::VkObject;

fn range_from_length<N>(start: N, length: N) -> Range<N>
where
    N: std::ops::Add<N, Output = N> + Copy,
{
    start..start + length
}

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

pub struct RangedBuffer<B: br::Buffer>(B, Range<u64>);
impl<B: br::Buffer> RangedBuffer<B> {
    pub const fn from_offset_length(buffer: B, offset: u64, length: usize) -> Self {
        Self(buffer, offset..offset + length as u64)
    }

    pub const fn for_type<T>(buffer: B, offset: u64) -> Self {
        Self::from_offset_length(buffer, offset, std::mem::size_of::<T>())
    }

    pub fn make_ref<'s>(&'s self) -> RangedBuffer<&'s B> {
        RangedBuffer(&self.0, self.1.clone())
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

pub struct DrawMesh<'b, B: br::Buffer> {
    vertex_buffers: Vec<RangedBuffer<&'b B>>,
    vertex_count: u32,
    instance_count: u32,
    vertex_start: u32,
    instance_start: u32,
}
impl<'b, B: br::Buffer> DrawMesh<'b, B> {
    pub const fn new(vertex_buffers: Vec<RangedBuffer<&'b B>>, vertex_count: u32) -> Self {
        Self {
            vertex_buffers,
            vertex_count,
            instance_count: 1,
            vertex_start: 0,
            instance_start: 0,
        }
    }

    pub fn with_instance_count(self, count: u32) -> Self {
        Self {
            instance_count: count,
            ..self
        }
    }
}
impl<B: br::Buffer> GraphicsCommand for DrawMesh<'_, B> {
    fn execute(self, cb: &mut br::CmdRecord<'_, impl br::CommandBuffer + br::VkHandleMut>) {
        let vertex_buffers = self
            .vertex_buffers
            .iter()
            .map(|rb| (rb.0, rb.1.start as usize))
            .collect::<Vec<_>>();

        let _ = cb.bind_vertex_buffers(0, &vertex_buffers).draw(
            self.vertex_count,
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

pub struct Game<PL: peridot::NativeLinker> {
    ph: PhantomData<*const PL>,
    rot: f32,
    render_cb: peridot::CommandBundle<peridot::DeviceObject>,
    update_cb: peridot::CommandBundle<peridot::DeviceObject>,
    renderpass: br::RenderPassObject<peridot::DeviceObject>,
    framebuffers: Vec<
        br::FramebufferObject<
            peridot::DeviceObject,
            SharedRef<<PL::Presenter as peridot::PlatformPresenter>::Backbuffer>,
        >,
    >,
    gp_main: LayoutedPipeline<
        br::PipelineObject<peridot::DeviceObject>,
        br::PipelineLayoutObject<peridot::DeviceObject>,
    >,
    descriptor: (
        br::DescriptorSetLayoutObject<peridot::DeviceObject>,
        br::DescriptorPoolObject<peridot::DeviceObject>,
        Vec<br::DescriptorSet>,
    ),
    _sampler: br::SamplerObject<peridot::DeviceObject>,
    vertices_offset: u64,
    buffer: peridot::Buffer<
        br::BufferObject<peridot::DeviceObject>,
        br::DeviceMemoryObject<peridot::DeviceObject>,
    >,
    mutable_buffer: peridot::Buffer<
        br::BufferObject<peridot::DeviceObject>,
        br::DeviceMemoryObject<peridot::DeviceObject>,
    >,
    _image_view: br::ImageViewObject<
        peridot::Image<
            br::ImageObject<peridot::DeviceObject>,
            br::DeviceMemoryObject<peridot::DeviceObject>,
        >,
    >,
    mut_uniform_offset: u64,
}
impl<PL: peridot::NativeLinker> peridot::FeatureRequests for Game<PL> {}
impl<PL: peridot::NativeLinker> peridot::EngineEvents<PL> for Game<PL> {
    fn init(e: &mut peridot::Engine<PL>) -> Self {
        let screen_size = e
            .backbuffer(0)
            .expect("no backbuffers")
            .image()
            .size()
            .clone();
        let screen_aspect = screen_size.width as f32 / screen_size.height as f32;

        let image_data: peridot_image::PNG = e.load("images.example").expect("No image found");
        debug!("Image: {}x{}", image_data.0.size.x(), image_data.0.size.y());
        debug!("ImageFormat: {:?}", image_data.0.format);
        debug!("ImageStride: {} bytes", image_data.0.stride);

        let bgm = Arc::new(RwLock::new(
            e.streaming::<StreamingPlayableWav>("bgm")
                .expect("Loading BGM"),
        ));
        e.audio_mixer()
            .write()
            .expect("Adding AudioProcess")
            .add_process(bgm.clone());
        e.audio_mixer()
            .write()
            .expect("Setting MasterVolume")
            .set_master_volume(0.5);

        let plane_mesh = peridot::Primitive::uv_plane_centric_xy(1.0, 0.0);
        let mut cam = Camera {
            projection: Some(ProjectionMethod::Perspective {
                fov: 75.0f32.to_radians(),
            }),
            position: Vector3(-4.0, -1.0, -3.0),
            rotation: Quaternion::ONE,
            // position: Vector3(0.0, 0.0, -3.0), rotation: Quaternion::ONE,
            depth_range: 1.0..10.0,
        };
        cam.look_at(Vector3(0.0, 0.0, 0.0));

        let mut bp = BufferPrealloc::new(e.graphics());
        let vertices_offset = bp.add(BufferContent::vertices::<peridot::VertexUV>(
            plane_mesh.vertices.len(),
        ));

        let mut bp_stg = bp.clone();
        let copy_buffer_data_length = bp_stg.total_size();
        let staging_image_offset = bp_stg.add(BufferContent::Raw(
            image_data.0.u8_pixels().len() as _,
            align_of::<u32>() as _,
        ));

        let mut bp_mut = BufferPrealloc::new(e.graphics());
        let mut_uniform_offset = bp_mut.add(BufferContent::uniform::<Uniform>());
        let mutable_data_offset = bp.merge(&bp_mut);

        let mut mb = peridot::MemoryBadget::new(e.graphics());
        mb.add(peridot::MemoryBadgetEntry::Buffer(
            bp.build_transferred().expect("Failed to create buffer"),
        ));
        mb.add(peridot::MemoryBadgetEntry::Image(
            br::ImageDesc::new(
                &image_data.0.size,
                image_data.0.format as _,
                br::ImageUsage::SAMPLED.transfer_dest(),
                br::ImageLayout::Preinitialized,
            )
            .create(e.graphics().device().clone())
            .expect("Failed to create main image object"),
        ));
        let Ok::<[_; 2], _>([
            peridot::MemoryBoundResource::Buffer(buffer),
            peridot::MemoryBoundResource::Image(image)
        ]) = mb.alloc().expect("Failed to allocate memory").try_into() else {
            unreachable!("invalid return combination");
        };
        let mut mb =
            peridot::MemoryBadget::<_, br::ImageObject<peridot::DeviceObject>>::new(e.graphics());
        mb.add(peridot::MemoryBadgetEntry::Buffer(
            bp_mut
                .build_upload()
                .expect("Failed to create mutable data buffer"),
        ));
        let Ok::<[_; 1], _>([
            peridot::MemoryBoundResource::Buffer(mut mut_buffer)
        ]) = mb.alloc_upload().expect("Failed to allocate mutable data memory").try_into() else {
            unreachable!("invalid return combination");
        };

        let (mut buffer_staging, stg_requires_flushing) = {
            let mut mb = peridot::MemoryBadget::<_, br::ImageObject<peridot::DeviceObject>>::new(
                e.graphics(),
            );
            mb.add(peridot::MemoryBadgetEntry::Buffer(
                bp_stg
                    .build_upload()
                    .expect("Failed to create staging buffer"),
            ));
            let Ok::<[_; 1], _>([
                peridot::MemoryBoundResource::Buffer(b)
            ]) = mb.alloc_upload().expect("Failed to allocate staging memory").try_into() else {
                unreachable!("invalid return combination")
            };

            // TODO: requires flushing?
            (b, false)
        };

        buffer_staging
            .guard_map(0..bp_stg.total_size(), |r| unsafe {
                r.clone_from_slice_at(vertices_offset as _, &plane_mesh.vertices);
                r.clone_from_slice_at(staging_image_offset as _, image_data.0.u8_pixels());
            })
            .expect("Failed to setup staging data");
        mut_buffer
            .guard_map(0..bp_mut.total_size(), |r| unsafe {
                *r.get_mut(mut_uniform_offset as _) = Uniform {
                    camera: cam.view_projection_matrix(screen_aspect),
                    object: Matrix4::ONE,
                };
            })
            .expect("Failed to setup mutable data");
        let vertex_buffer = RangedBuffer::from_offset_length(
            &buffer,
            vertices_offset,
            std::mem::size_of::<peridot::VertexUV>() * plane_mesh.vertices.len(),
        );

        let pre_configure_task = e
            .submit_commands_async(|mut r| {
                let all_buffer = RangedBuffer::from_offset_length(&buffer, 0, bp.total_size() as _);
                let mut_buffer = RangedBuffer::for_type::<Uniform>(&mut_buffer, mut_uniform_offset);
                let staging_init =
                    RangedBuffer::from_offset_length(&buffer_staging, 0, bp_stg.total_size() as _);
                let uniform_buffer =
                    RangedBuffer::for_type::<Uniform>(&buffer, mutable_data_offset);
                let texture = RangedImage::single_color_plane(&image);

                let in_barriers = PipelineBarrier::new()
                    .with_barrier(
                        all_buffer.usage_barrier(BufferUsage::UNUSED, BufferUsage::TRANSFER_DST),
                    )
                    .with_barrier(
                        mut_buffer.usage_barrier(BufferUsage::HOST_RW, BufferUsage::TRANSFER_SRC),
                    )
                    .with_barrier(
                        staging_init.usage_barrier(BufferUsage::HOST_RW, BufferUsage::TRANSFER_SRC),
                    )
                    .with_barrier(texture.barrier(
                        br::ImageLayout::Preinitialized,
                        br::ImageLayout::TransferDestOpt,
                    ))
                    .by_region();
                let out_barriers = PipelineBarrier::new()
                    .with_barrier(
                        vertex_buffer
                            .usage_barrier(BufferUsage::TRANSFER_DST, BufferUsage::VERTEX_BUFFER),
                    )
                    .with_barrier(
                        uniform_buffer
                            .usage_barrier(BufferUsage::TRANSFER_DST, BufferUsage::VERTEX_UNIFORM),
                    )
                    .with_barrier(
                        mut_buffer.usage_barrier(BufferUsage::TRANSFER_SRC, BufferUsage::HOST_RW),
                    )
                    .with_barrier(texture.barrier(
                        br::ImageLayout::TransferDestOpt,
                        br::ImageLayout::ShaderReadOnlyOpt,
                    ))
                    .by_region();
                let staging_copy = CopyBuffer::new(&buffer_staging, &buffer)
                    .with_mirroring(0, copy_buffer_data_length as _);
                let mutable_copy = CopyBuffer::new(&mut_buffer.0, &buffer)
                    .with_range_for_type::<Uniform>(mut_uniform_offset, mutable_data_offset);
                let tex_copy = CopyBufferToImage::new(&buffer_staging, &image).with_range(
                    BufferImageDataDesc::new(
                        staging_image_offset,
                        (image_data.0.stride / (image_data.0.format.bpp() >> 3)) as _,
                    ),
                    ImageResourceRange::for_single_color_from_rect2d(br::vk::VkRect2D {
                        offset: br::vk::VkOffset2D { x: 0, y: 0 },
                        extent: image_data.0.size.into(),
                    }),
                );
                let copies = (staging_copy, mutable_copy, tex_copy);

                copies.between(in_barriers, out_barriers).execute(&mut r);
                r
            })
            .expect("Failed to submit pre-configure commands");

        let mut update_cb = CommandBundle::new(&e.graphics(), CBSubmissionType::Graphics, 1)
            .expect("Alloc UpdateCB");
        {
            let uniform_buffer = RangedBuffer::for_type::<Uniform>(&buffer, mutable_data_offset);
            let staging_uniform_buffer =
                RangedBuffer::for_type::<Uniform>(&mut_buffer, mut_uniform_offset);

            let in_barriers = [
                uniform_buffer
                    .usage_barrier(BufferUsage::VERTEX_UNIFORM, BufferUsage::TRANSFER_DST),
                staging_uniform_buffer
                    .usage_barrier(BufferUsage::HOST_RW, BufferUsage::TRANSFER_SRC),
            ];
            let out_barriers = [
                uniform_buffer
                    .usage_barrier(BufferUsage::TRANSFER_DST, BufferUsage::VERTEX_UNIFORM),
                staging_uniform_buffer
                    .usage_barrier(BufferUsage::TRANSFER_SRC, BufferUsage::HOST_RW),
            ];
            let copy_uniform = CopyBuffer::new(&mut_buffer, &buffer)
                .with_range_for_type::<Uniform>(mut_uniform_offset, mutable_data_offset);

            copy_uniform
                .between(in_barriers, out_barriers)
                .execute_into(unsafe { update_cb.synchronized_nth(0) })
                .expect("Failed to record update commands");
        }

        let outer_layout = e.requesting_backbuffer_layout().0;
        let attdesc =
            br::AttachmentDescription::new(e.backbuffer_format(), outer_layout, outer_layout)
                .load_op(br::LoadOp::Clear)
                .store_op(br::StoreOp::Store);
        let renderpass = br::RenderPassBuilder::new()
            .add_attachment(attdesc)
            .add_subpass(br::SubpassDescription::new().add_color_output(
                0,
                br::ImageLayout::ColorAttachmentOpt,
                None,
            ))
            .add_dependency(SubpassDependencyTemplates::to_color_attachment_in(
                None, 0, true,
            ))
            .create(e.graphics().device().clone())
            .expect("Create RenderPass");
        let framebuffers = e
            .iter_backbuffers()
            .map(|b| {
                e.graphics().device().clone().new_framebuffer(
                    &renderpass,
                    vec![b.clone()],
                    b.image().size().as_ref(),
                    1,
                )
            })
            .collect::<Result<Vec<_>, _>>()
            .expect("Bind Framebuffer");

        let smp = br::SamplerBuilder::default()
            .create(e.graphics().device().clone())
            .expect("Creating Sampler");
        let descriptor_layout = e
            .graphics()
            .device()
            .clone()
            .new_descriptor_set_layout(&[
                br::DescriptorSetLayoutBinding::UniformBuffer(1, br::ShaderStage::VERTEX),
                br::DescriptorSetLayoutBinding::CombinedImageSampler(
                    1,
                    br::ShaderStage::FRAGMENT,
                    &[smp.native_ptr()],
                ),
            ])
            .expect("Create DescriptorSetLayout");
        let mut descriptor_pool = e
            .graphics()
            .device()
            .clone()
            .new_descriptor_pool(
                1,
                &[
                    br::DescriptorPoolSize(br::DescriptorType::UniformBuffer, 1),
                    br::DescriptorPoolSize(br::DescriptorType::CombinedImageSampler, 1),
                ],
                false,
            )
            .expect("Create DescriptorPool");

        let shader = e
            .load("builtin.shaders.unlit_image")
            .expect("Loading shader");
        let shader =
            PvpShaderModules::new(e.graphics().device(), shader).expect("Create ShaderModules");
        let vp = [br::vk::VkViewport {
            width: screen_size.width as _,
            height: screen_size.height as _,
            x: 0.0,
            y: 0.0,
            minDepth: 0.0,
            maxDepth: 1.0,
        }];
        let sc = [br::vk::VkRect2D {
            offset: br::vk::VkOffset2D { x: 0, y: 0 },
            extent: br::vk::VkExtent2D {
                width: screen_size.width,
                height: screen_size.height,
            },
        }];
        let pl = e
            .graphics()
            .device()
            .clone()
            .new_pipeline_layout(&[&descriptor_layout], &[])
            .expect("Create PipelineLayout");
        let vps = shader.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP);
        let gp = br::GraphicsPipelineBuilder::<
            _,
            br::PipelineObject<peridot::DeviceObject>,
            _,
            _,
            _,
            _,
            _,
            _,
        >::new(&pl, (&renderpass, 0), vps)
        .viewport_scissors(
            br::DynamicArrayState::Static(&vp),
            br::DynamicArrayState::Static(&sc),
        )
        .multisample_state(br::MultisampleState::new().into())
        .add_attachment_blend(br::AttachmentColorBlendState::noblend())
        .create(
            e.graphics().device().clone(),
            None::<&br::PipelineCacheObject<peridot::DeviceObject>>,
        )
        .expect("Create GraphicsPipeline");
        #[cfg(feature = "debug")]
        gp.set_name(Some(
            &std::ffi::CString::new("Main Pipeline").expect("invalid sequence?"),
        ))
        .expect("Failed to set pipeline name");
        let gp = LayoutedPipeline::combine(gp, pl);

        async_std::task::block_on(pre_configure_task).expect("Failed to pre-configure resources");

        let image_view = image
            .create_view(
                None,
                None,
                &br::ComponentMapping::default(),
                &br::ImageSubresourceRange::color(0..1, 0..1),
            )
            .expect("Failed to create main image view");
        let descriptor_main = descriptor_pool
            .alloc(&[&descriptor_layout])
            .expect("Create main Descriptor");
        let mut dsub = DescriptorSetUpdateBatch::new();
        dsub.write(
            descriptor_main[0],
            0,
            br::DescriptorUpdateInfo::UniformBuffer(vec![(
                buffer.native_ptr(),
                range_from_length(mutable_data_offset as _, std::mem::size_of::<Uniform>()),
            )]),
        );
        dsub.write(
            descriptor_main[0],
            1,
            br::DescriptorUpdateInfo::CombinedImageSampler(vec![(
                None,
                image_view.native_ptr(),
                br::ImageLayout::ShaderReadOnlyOpt,
            )]),
        );
        dsub.submit(e.graphics().device());

        let mut render_cb = CommandBundle::new(
            e.graphics(),
            CBSubmissionType::Graphics,
            e.backbuffer_count(),
        )
        .expect("Alloc RenderCB");
        #[allow(unused_variables)]
        for (n, (cb, fb)) in render_cb.iter_mut().zip(&framebuffers).enumerate() {
            #[cfg(feature = "debug")]
            br::DebugUtilsObjectNameInfo::new(
                cb,
                Some(
                    &std::ffi::CString::new(format!("Primary Render Commands #{}", n))
                        .expect("invalid sequence?"),
                ),
            )
            .apply(e.graphics().device())
            .expect("Failed to set render cb name");

            let begin_main_rp = BeginRenderPass::for_entire_framebuffer(
                &renderpass,
                fb,
                vec![br::ClearValue::color([0.0; 4])],
            );
            let render_image_plane = DrawMesh::new(vec![vertex_buffer.make_ref()], 4)
                .after_of(BindGraphicsDescriptorSets::new(vec![*descriptor_main[0]]));

            unsafe {
                (&gp, render_image_plane)
                    .between(begin_main_rp, EndRenderPass)
                    .execute_into_ext_sync(cb)
                    .expect("Failed to record render commands");
            }
        }

        bgm.write().expect("Starting BGM").play();

        Game {
            render_cb,
            renderpass,
            framebuffers,
            descriptor: (descriptor_layout, descriptor_pool, descriptor_main),
            gp_main: gp,
            rot: 0.0,
            vertices_offset,
            _sampler: smp,
            buffer,
            mutable_buffer: mut_buffer,
            _image_view: image_view,
            update_cb,
            mut_uniform_offset,
            ph: PhantomData,
        }
    }

    fn update(&mut self, e: &mut peridot::Engine<PL>, on_backbuffer_of: u32, delta_time: Duration) {
        let dtsec = delta_time.as_secs() as f32 + delta_time.subsec_micros() as f32 / 1000_0000.0;
        self.rot += dtsec * 15.0;
        let (mut_uniform_offset, rot) = (self.mut_uniform_offset, self.rot);
        self.mutable_buffer
            .guard_map(
                0..mut_uniform_offset + size_of::<Uniform>() as u64,
                |m| unsafe {
                    m.get_mut::<Uniform>(mut_uniform_offset as _).object =
                        Quaternion::new(rot, Vector3F32::UP).into();
                },
            )
            .expect("Update DynamicStgBuffer");

        e.do_render(
            on_backbuffer_of,
            Some(br::EmptySubmissionBatch.with_command_buffers(&self.update_cb)),
            br::EmptySubmissionBatch.with_command_buffers(
                &self.render_cb[on_backbuffer_of as usize..on_backbuffer_of as usize + 1],
            ),
        )
        .expect("Falied to present");
    }

    fn discard_backbuffer_resources(&mut self) {
        self.framebuffers.clear();
        self.render_cb.reset().expect("Resetting RenderCB");
    }
    fn on_resize(&mut self, e: &mut peridot::Engine<PL>, _new_size: Vector2<usize>) {
        self.framebuffers = e
            .iter_backbuffers()
            .map(|b| {
                e.graphics().device().clone().new_framebuffer(
                    &self.renderpass,
                    vec![b.clone()],
                    b.image().size().as_ref(),
                    1,
                )
            })
            .collect::<Result<Vec<_>, _>>()
            .expect("Bind Framebuffers");
        self.populate_render_commands();
    }
}
impl<PL: peridot::NativeLinker> Game<PL> {
    fn populate_render_commands(&mut self) {
        let vertex_buffer = RangedBuffer::from_offset_length(
            &self.buffer,
            self.vertices_offset,
            std::mem::size_of::<peridot::VertexUV>(),
        );

        for (cb, fb) in self.render_cb.iter_mut().zip(&self.framebuffers) {
            let begin_main_rp = BeginRenderPass::for_entire_framebuffer(
                &self.renderpass,
                fb,
                vec![br::ClearValue::color([0.0; 4])],
            );
            let render_image_plane = DrawMesh::new(vec![vertex_buffer.make_ref()], 4)
                .after_of(BindGraphicsDescriptorSets::new(vec![*self.descriptor.2[0]]));

            unsafe {
                (&self.gp_main, render_image_plane)
                    .between(begin_main_rp, EndRenderPass)
                    .execute_into_ext_sync(cb)
                    .expect("Failed to record render commands");
            }
        }
    }
}

#[repr(C)]
struct Uniform {
    camera: Matrix4F32,
    object: Matrix4F32,
}
