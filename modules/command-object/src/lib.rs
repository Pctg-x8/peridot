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
impl<const N: usize, B: br::Buffer> GraphicsCommand for [BufferUsageTransitionBarrier<'_, B>; N] {
    fn execute(
        self,
        cb: &mut br::CmdRecord<'_, impl br::CommandBuffer + br::VkHandleMut + ?Sized>,
    ) {
        PipelineBarrier::new().with_barriers(self).execute(cb)
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

    pub const fn pre_configure_for_draw<'m>(&'m self) -> PreConfigureDraw<'m, B> {
        PreConfigureDraw(self)
    }
}

pub struct IndexedMesh<B: br::Buffer, IB: br::Buffer> {
    pub vertex_buffers: Vec<RangedBuffer<B>>,
    pub index_buffer: RangedBuffer<IB>,
    pub index_type: br::IndexType,
    pub vertex_count: u32,
}
impl<B: br::Buffer, IB: br::Buffer> IndexedMesh<B, IB> {
    pub const fn draw<'m>(&'m self, instance_count: u32) -> DrawIndexedMesh<'m, B, IB> {
        DrawIndexedMesh {
            mesh: self,
            instance_count,
            vertex_start: 0,
            index_offset: 0,
            instance_start: 0,
        }
    }

    pub const fn pre_configure_for_draw<'m>(&'m self) -> PreConfigureDrawIndexed<'m, B, IB> {
        PreConfigureDrawIndexed(self)
    }
}
