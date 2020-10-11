//! Layout Cache

use bedrock as br;
use std::collections::HashMap;
use std::rc::Rc;

/// An object cache for DescriptorSetLayout
pub struct DescriptorSetLayoutCache<'d> {
    object_map: HashMap<&'d [br::DescriptorSetLayoutBinding<'d>], br::DescriptorSetLayout>
}
impl<'d> DescriptorSetLayoutCache<'d> {
    /// Create an empty object cache
    pub fn new() -> Self {
        DescriptorSetLayoutCache {
            object_map: HashMap::new()
        }
    }

    /// Get an object. And create a new object if not exists.
    pub fn query(
        &mut self,
        device: &br::Device,
        bindings: &'d [br::DescriptorSetLayoutBinding<'d>]
    ) -> &br::DescriptorSetLayout {
        self.object_map.entry(bindings).or_insert_with(||
            br::DescriptorSetLayout::new(device, bindings).expect("Failed to create DescriptorSetLayout")
        )
    }
}
