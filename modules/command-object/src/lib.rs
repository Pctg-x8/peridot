use bedrock as br;

mod ranged_resources;
pub use self::ranged_resources::*;
mod graphics_command;
pub use self::graphics_command::*;
mod blending;
pub use self::blending::*;

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

        if self.has_bits(Self::HOST_RO | Self::HOST_RW) {
            f = f.host();
        }
        if self.has_bits(Self::TRANSFER_SRC | Self::TRANSFER_DST) {
            f = f.transfer();
        }
        if self.has_bits(Self::VERTEX_BUFFER | Self::INDEX_BUFFER) {
            f = f.vertex_input();
        }
        if self.has_bits(Self::INDIRECT_BUFFER) {
            f = f.draw_indirect();
        }
        if self.has_bits(Self::VERTEX_UNIFORM | Self::VERTEX_STORAGE_RO | Self::VERTEX_STORAGE_RW) {
            f = f.vertex_shader();
        }
        if self.has_bits(
            Self::FRAGMENT_UNIFORM | Self::FRAGMENT_STORAGE_RO | Self::FRAGMENT_STORAGE_RW,
        ) {
            f = f.fragment_shader();
        }

        f
    }

    pub fn vk_access_flags_requirements(&self) -> br::vk::VkAccessFlags {
        let mut f = 0;

        if self.has_bits(Self::HOST_RO) {
            f |= br::AccessFlags::HOST.read;
        }
        if self.has_bits(Self::HOST_RW) {
            f |= br::AccessFlags::HOST.write | br::AccessFlags::HOST.read;
        }
        if self.has_bits(Self::TRANSFER_SRC) {
            f |= br::AccessFlags::TRANSFER.read;
        }
        if self.has_bits(Self::TRANSFER_DST) {
            f |= br::AccessFlags::TRANSFER.write;
        }
        if self.has_bits(Self::VERTEX_BUFFER) {
            f |= br::AccessFlags::VERTEX_ATTRIBUTE_READ;
        }
        if self.has_bits(Self::INDEX_BUFFER) {
            f |= br::AccessFlags::INDEX_READ;
        }
        if self.has_bits(Self::INDIRECT_BUFFER) {
            f |= br::AccessFlags::INDIRECT_COMMAND_READ;
        }
        if self.has_bits(Self::VERTEX_UNIFORM | Self::FRAGMENT_UNIFORM) {
            f |= br::AccessFlags::UNIFORM_READ;
        }
        if self.has_bits(Self::VERTEX_STORAGE_RO | Self::FRAGMENT_STORAGE_RO) {
            f |= br::AccessFlags::SHADER.read;
        }
        if self.has_bits(Self::VERTEX_STORAGE_RW | Self::FRAGMENT_STORAGE_RW) {
            f |= br::AccessFlags::SHADER.read | br::AccessFlags::SHADER.write;
        }

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

pub(crate) fn vk_pipeline_stage_mask_requirements_for_image_layout(
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

#[derive(Clone)]
pub struct BufferUsageTransitionBarrier<B: br::Buffer> {
    pub buffer: RangedBuffer<B>,
    pub from_usage: BufferUsage,
    pub to_usage: BufferUsage,
}
impl<B: br::Buffer> BufferUsageTransitionBarrier<B> {
    pub fn make_vk_barrier(&self) -> br::BufferMemoryBarrier {
        br::BufferMemoryBarrier::new(
            &self.buffer.0,
            self.buffer.1.clone(),
            self.from_usage.vk_access_flags_requirements(),
            self.to_usage.vk_access_flags_requirements(),
        )
    }
}
impl<B: br::Buffer> PipelineBarrierEntry for BufferUsageTransitionBarrier<B> {
    fn add_into(self, barrier: &mut PipelineBarrier) {
        barrier.buffer_barriers.push(self.make_vk_barrier());
        barrier.src_stage_mask |= self.from_usage.vk_pipeline_stage_mask_requirements();
        barrier.dst_stage_mask |= self.to_usage.vk_pipeline_stage_mask_requirements();
    }

    fn reserve_hints(barrier: &mut PipelineBarrier, count: usize) {
        barrier.buffer_barriers.reserve(count);
    }
}
impl<const N: usize, B: br::Buffer + Clone> GraphicsCommand
    for [BufferUsageTransitionBarrier<B>; N]
{
    fn execute(
        &self,
        cb: &mut br::CmdRecord<'_, dyn br::VkHandleMut<Handle = br::vk::VkCommandBuffer>>,
    ) {
        PipelineBarrier::new()
            .with_barriers(self.iter().cloned())
            .execute(cb)
    }
}

pub trait Mesh {
    type VertexBuffer: br::Buffer;

    fn vertex_buffers(&self) -> &[RangedBuffer<Self::VertexBuffer>];
    fn vertex_count(&self) -> u32;
}
impl<M: Mesh> Mesh for &'_ M {
    type VertexBuffer = M::VertexBuffer;

    fn vertex_buffers(&self) -> &[RangedBuffer<Self::VertexBuffer>] {
        M::vertex_buffers(&**self)
    }

    fn vertex_count(&self) -> u32 {
        M::vertex_count(&**self)
    }
}
pub trait IndexedMesh: Mesh {
    type IndexBuffer: br::Buffer;

    fn index_buffer(&self) -> &RangedBuffer<Self::IndexBuffer>;
    fn index_type(&self) -> br::IndexType;
}
impl<M: IndexedMesh> IndexedMesh for &'_ M {
    type IndexBuffer = M::IndexBuffer;

    fn index_buffer(&self) -> &RangedBuffer<Self::IndexBuffer> {
        M::index_buffer(&**self)
    }

    fn index_type(&self) -> br::IndexType {
        M::index_type(&**self)
    }
}

pub struct StandardMesh<B: br::Buffer> {
    pub vertex_buffers: Vec<RangedBuffer<B>>,
    pub vertex_count: u32,
}
impl<B: br::Buffer> StandardMesh<B> {
    pub const fn draw(self, instance_count: u32) -> DrawMesh<Self> {
        DrawMesh {
            mesh: self,
            instance_count,
            vertex_start: 0,
            instance_start: 0,
        }
    }

    pub const fn pre_configure_for_draw(self) -> PreConfigureDraw<Self> {
        PreConfigureDraw(self)
    }

    pub const fn ref_draw(&self, instance_count: u32) -> DrawMesh<&Self> {
        DrawMesh {
            mesh: self,
            instance_count,
            vertex_start: 0,
            instance_start: 0,
        }
    }

    pub const fn ref_pre_configure_for_draw(&self) -> PreConfigureDraw<&Self> {
        PreConfigureDraw(self)
    }
}
impl<B: br::Buffer> Mesh for StandardMesh<B> {
    type VertexBuffer = B;

    fn vertex_buffers(&self) -> &[RangedBuffer<B>] {
        &self.vertex_buffers
    }

    fn vertex_count(&self) -> u32 {
        self.vertex_count
    }
}

pub struct StandardIndexedMesh<B: br::Buffer, IB: br::Buffer> {
    pub vertex_buffers: Vec<RangedBuffer<B>>,
    pub index_buffer: RangedBuffer<IB>,
    pub index_type: br::IndexType,
    pub vertex_count: u32,
}
impl<B: br::Buffer, IB: br::Buffer> StandardIndexedMesh<B, IB> {
    pub const fn draw(self, instance_count: u32) -> DrawIndexedMesh<Self> {
        DrawIndexedMesh {
            mesh: self,
            instance_count,
            vertex_start: 0,
            index_offset: 0,
            instance_start: 0,
        }
    }

    pub const fn pre_configure_for_draw(self) -> PreConfigureDrawIndexed<Self> {
        PreConfigureDrawIndexed(self)
    }

    pub const fn ref_draw(&self, instance_count: u32) -> DrawIndexedMesh<&Self> {
        DrawIndexedMesh {
            mesh: self,
            instance_count,
            vertex_start: 0,
            index_offset: 0,
            instance_start: 0,
        }
    }

    pub const fn ref_pre_configure_for_draw(&self) -> PreConfigureDrawIndexed<&Self> {
        PreConfigureDrawIndexed(self)
    }
}
impl<B: br::Buffer, IB: br::Buffer> Mesh for StandardIndexedMesh<B, IB> {
    type VertexBuffer = B;

    fn vertex_buffers(&self) -> &[RangedBuffer<B>] {
        &self.vertex_buffers
    }

    fn vertex_count(&self) -> u32 {
        self.vertex_count
    }
}
impl<B: br::Buffer, IB: br::Buffer> IndexedMesh for StandardIndexedMesh<B, IB> {
    type IndexBuffer = IB;

    fn index_buffer(&self) -> &RangedBuffer<IB> {
        &self.index_buffer
    }

    fn index_type(&self) -> br::IndexType {
        self.index_type
    }
}
