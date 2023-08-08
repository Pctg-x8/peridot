use bedrock as br;

// Pointing specific descriptor in descriptor set
#[derive(Clone, Copy)]
pub struct DescriptorPointer {
    set: br::vk::VkDescriptorSet,
    binding: u32,
    array_index: u32,
}
impl DescriptorPointer {
    pub fn new(set: impl Into<br::vk::VkDescriptorSet>) -> Self {
        Self {
            set: set.into(),
            binding: 0,
            array_index: 0,
        }
    }

    /// default is 0
    pub const fn bound_at(self, binding: u32) -> Self {
        Self { binding, ..self }
    }

    /// default is 0
    pub const fn at(self, array_index: u32) -> Self {
        Self {
            array_index,
            ..self
        }
    }

    /// write via batching
    pub fn write(
        self,
        batch: &mut peridot::DescriptorSetUpdateBatch,
        content: br::DescriptorUpdateInfo,
    ) {
        batch.write_index(self.set, self.binding, self.array_index, content);
    }
}
