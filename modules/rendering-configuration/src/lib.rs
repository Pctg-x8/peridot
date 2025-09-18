use std::collections::HashMap;

pub mod codegen;
pub mod syntax;
pub mod tokenizer;

pub enum ShadingPassVk {
    SimpleDeriveBuiltinPass {
        name: String,
    },
    Custom {
        vertex_semantic_to_location: HashMap<String, usize>,
        code: Vec<u32>,
    },
}

#[derive(Debug)]
pub enum PropertyMappingVk {
    Direct(PropertyDestinationVk),
    Splitted(Vec<PropertyDestinationVk>),
}

#[derive(Debug)]
pub enum PropertyDestinationVk {
    SpecConstant(usize),
    PushConstantBlockOffset(usize),
    DescriptorSet(usize),
    RealtimeBufferOffset(usize),
}

/// converted asset data
pub struct CompiledRenderingConfigurationVk {
    pub property_mappings: HashMap<String, PropertyMappingVk>,
    pub passes: HashMap<String, ShadingPassVk>,
}
