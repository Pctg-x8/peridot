use std::{
    collections::HashMap,
    io::{BufRead, Seek, SeekFrom, Write},
};

pub use peridot_semantic_shader::VertexInputSemantic;

mod file;

/// converted asset data
pub struct CompiledRenderingConfigurationVk {
    pub property_mappings: HashMap<String, (PropertyType, PropertyMappingVk)>,
    pub descriptor_set_bindings: Vec<DescriptorTypeVk>,
    pub push_constant_buffer_size_bytes: usize,
    pub passes: HashMap<String, ShadingPassVk>,
}

#[cfg(feature = "with-loader-impl")]
impl peridot::LogicalAssetData for CompiledRenderingConfigurationVk {
    const EXT: &'static str = "prcc";
}
#[cfg(feature = "with-loader-impl")]
impl peridot::FromAsset for CompiledRenderingConfigurationVk {
    type Error = std::io::Error;

    fn from_asset<Asset: std::io::Read + Seek + 'static>(
        asset: Asset,
    ) -> Result<Self, Self::Error> {
        read(&mut std::io::BufReader::new(asset))
    }
}

pub enum ShadingPassVk {
    SimpleDeriveBuiltinPass {
        name: String,
    },
    Custom {
        option_overrides: RenderingOptionOverrides,
        vertex_semantic_to_location: HashMap<VertexInputSemantic, u32>,
        vertex_entry_point_name: Option<String>,
        fragment_entry_point_name: Option<String>,
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
    PushConstantBlock(usize),
    DescriptorSet(usize),
    RealtimeBuffer(usize),
}

#[derive(Debug)]
pub enum DescriptorTypeVk {
    UniformBuffer { size_bytes: usize },
    CombinedImageSampler,
}

#[derive(Debug, Clone)]
pub struct RenderingOptionOverrides {
    pub mode: Option<PolygonRasterizationMode>,
    pub culling: Option<FaceCulling>,
    pub front_face: Option<FrontFace>,
}
impl Default for RenderingOptionOverrides {
    #[inline(always)]
    fn default() -> Self {
        Self {
            mode: None,
            culling: None,
            front_face: None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum PolygonRasterizationMode {
    Point,
    Line,
    Fill,
}
impl Default for PolygonRasterizationMode {
    #[inline(always)]
    fn default() -> Self {
        Self::Fill
    }
}

#[derive(Debug, Clone, Copy)]
pub enum FaceCulling {
    None,
    Front,
    Back,
    Both,
}
impl Default for FaceCulling {
    #[inline(always)]
    fn default() -> Self {
        Self::Back
    }
}

#[derive(Debug, Clone, Copy)]
pub enum FrontFace {
    Clockwise,
    CounterClockwise,
}
impl Default for FrontFace {
    #[inline(always)]
    fn default() -> Self {
        Self::CounterClockwise
    }
}

#[derive(Debug, Clone)]
pub enum PropertyType {
    Texture2D,
    RGB,
    UInt,
    Int,
    Float,
    Float2,
    Float4,
}

pub fn write(
    sink: &mut impl Write,
    compiled: CompiledRenderingConfigurationVk,
) -> std::io::Result<usize> {
    let mut writes = 0;

    let mut header = file::Header {
        shading_pass_directory_offset: 0,
    };

    writes += file::PropertyDirectory {
        entries: compiled
            .property_mappings
            .into_iter()
            .map(|(n, (t, m))| (n, t, m))
            .collect(),
        descriptor_set_bindings: compiled.descriptor_set_bindings,
        push_constant_buffer_size_bytes: compiled.push_constant_buffer_size_bytes,
    }
    .write(sink)?;

    let mut shading_pass_directory = file::ShadingPassDirectory {
        entries: Vec::with_capacity(compiled.passes.len()),
    };
    for (n, p) in compiled.passes {
        match p {
            ShadingPassVk::SimpleDeriveBuiltinPass { name } => {
                shading_pass_directory.entries.push((
                    n,
                    file::ShadingPassDirectoryEntry::SimpleDeriveBuiltin(name),
                ));
            }
            ShadingPassVk::Custom {
                option_overrides,
                vertex_semantic_to_location,
                vertex_entry_point_name,
                fragment_entry_point_name,
                code,
            } => {
                shading_pass_directory
                    .entries
                    .push((n, file::ShadingPassDirectoryEntry::Located(writes as _)));
                writes += file::ShadingPassVk {
                    option_overrides,
                    vertex_semantic_to_location: vertex_semantic_to_location.into_iter().collect(),
                    vertex_entry_point_name,
                    fragment_entry_point_name,
                    code,
                }
                .write(sink)?;
            }
        }
    }
    header.shading_pass_directory_offset = writes as _;
    writes += shading_pass_directory.write(sink)?;

    writes += header.write(sink)?;
    Ok(writes)
}

pub fn read(
    source: &mut (impl BufRead + Seek),
) -> std::io::Result<CompiledRenderingConfigurationVk> {
    source.seek(file::Header::READ_SEEK_POS)?;
    let (header, _swap_bytes) = file::Header::read(source)?;
    source.seek(SeekFrom::Start(0))?;
    let property_directory = file::PropertyDirectory::read(source)?;
    source.seek(SeekFrom::Start(header.shading_pass_directory_offset))?;
    let shading_pass_directory = file::ShadingPassDirectory::read(source)?;

    let mut result = CompiledRenderingConfigurationVk {
        property_mappings: HashMap::with_capacity(property_directory.entries.len()),
        descriptor_set_bindings: property_directory.descriptor_set_bindings,
        push_constant_buffer_size_bytes: property_directory.push_constant_buffer_size_bytes,
        passes: HashMap::with_capacity(shading_pass_directory.entries.len()),
    };
    for (n, t, m) in property_directory.entries {
        match result.property_mappings.entry(n) {
            std::collections::hash_map::Entry::Vacant(x) => {
                x.insert((t, m));
            }
            std::collections::hash_map::Entry::Occupied(x) => {
                panic!("conflicting property: {}", x.key());
            }
        }
    }
    for (n, p) in shading_pass_directory.entries {
        match result.passes.entry(n) {
            std::collections::hash_map::Entry::Vacant(x) => match p {
                file::ShadingPassDirectoryEntry::SimpleDeriveBuiltin(name) => {
                    x.insert(ShadingPassVk::SimpleDeriveBuiltinPass { name });
                }
                file::ShadingPassDirectoryEntry::Located(loc) => {
                    source.seek(SeekFrom::Start(loc))?;
                    let pass_data = file::ShadingPassVk::read(source)?;

                    let mut vertex_semantic_to_location =
                        HashMap::with_capacity(pass_data.vertex_semantic_to_location.len());
                    for (n, l) in pass_data.vertex_semantic_to_location {
                        match vertex_semantic_to_location.entry(n) {
                            std::collections::hash_map::Entry::Vacant(x) => {
                                x.insert(l);
                            }
                            std::collections::hash_map::Entry::Occupied(x) => {
                                panic!("conflicting vertex semantic: {:?}", x.key());
                            }
                        }
                    }

                    x.insert(ShadingPassVk::Custom {
                        option_overrides: pass_data.option_overrides,
                        vertex_semantic_to_location,
                        vertex_entry_point_name: pass_data.vertex_entry_point_name,
                        fragment_entry_point_name: pass_data.fragment_entry_point_name,
                        code: pass_data.code,
                    });
                }
            },
            std::collections::hash_map::Entry::Occupied(x) => {
                panic!("conflicting pass: {}", x.key());
            }
        }
    }

    Ok(result)
}
