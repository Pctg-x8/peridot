use std::{
    collections::HashMap,
    io::{BufRead, Seek, SeekFrom, Write},
};
#[cfg(feature = "with-asset-processing")]
use std::{
    ffi::OsStr,
    path::{Path, PathBuf},
};

pub use peridot_semantic_shader::VertexInputSemantic;

#[cfg(feature = "compilation")]
pub mod compilation;
mod file;

/// converted asset data
pub struct CompiledRenderingConfigurationVk {
    pub property_mappings: HashMap<String, (PropertyType, PropertyMappingVk)>,
    pub passes: HashMap<String, ShadingPassVk>,
}

#[cfg(feature = "with-loader-impl")]
impl peridot::LogicalAssetData for CompiledRenderingConfigurationVk {
    const EXT: &'static str = "pa1-rendering-configuration";
}
#[cfg(feature = "with-loader-impl")]
impl peridot::FromAssetBlob for CompiledRenderingConfigurationVk {
    type Error = std::io::Error;

    #[inline(always)]
    fn from_asset_blob<'a, Blob: peridot::AssetBlob + 'a>(blob: Blob) -> Result<Self, Self::Error> {
        read(&mut std::io::BufReader::new(
            peridot::native_io::RandomBlobReadSeekAdapter::new(blob),
        ))
    }
}
#[cfg(feature = "with-loader-impl")]
impl peridot::FromAssetBlobAsync for CompiledRenderingConfigurationVk {
    type Error = std::io::Error;

    #[inline(always)]
    fn from_asset_blob_async<'a, Blob: peridot::AssetBlobAsync + 'a>(
        blob: Blob,
    ) -> impl core::future::Future<Output = Result<Self, Self::Error>> {
        async move { read_async(&blob).await }
    }
}

#[cfg(feature = "with-asset-processing")]
#[derive(thiserror::Error, Debug)]
pub enum AssetProcessError {
    #[error("Failed to read source file: {0}")]
    ReadingFailed(std::io::Error),
    #[error("Error generating asset")]
    GeneratingAssetFailure,
    #[error("Failed to open destination file for writing: {0}")]
    DestWriteOpenFailed(std::io::Error),
    #[error("Error writing asset: {0}")]
    WritingAssetFailure(std::io::Error),
}

#[cfg(feature = "with-asset-processing")]
pub struct AssetProcessor;
#[cfg(feature = "with-asset-processing")]
impl peridot_asset_processing::AssetProcessor for AssetProcessor {
    fn can_process(&self, source_path: &Path) -> bool {
        source_path.extension().is_some_and(|x| x == "prc")
    }

    fn dest_path(&self, source_file_name: &OsStr, out_dir_path: &Path) -> PathBuf {
        out_dir_path
            .join(source_file_name)
            .with_extension("pa1-rendering-configuration")
    }

    fn process(
        &self,
        source_path: &Path,
        _metadata: &HashMap<peridot_asset_processing::metadata::Key, String>,
        out_path: &Path,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let content =
            std::fs::read_to_string(source_path).map_err(AssetProcessError::ReadingFailed)?;
        let asset =
            compilation::compile(&content).ok_or(AssetProcessError::GeneratingAssetFailure)?;
        write(
            &mut std::fs::File::options()
                .write(true)
                .truncate(true)
                .create(true)
                .open(out_path)
                .map_err(AssetProcessError::DestWriteOpenFailed)?,
            asset,
        )
        .map_err(AssetProcessError::WritingAssetFailure)?;

        Ok(())
    }
}

pub enum ShadingPassVk {
    SimpleDeriveBuiltinPass {
        name: String,
    },
    Custom {
        option_overrides: RenderingOptionOverrides,
        variants: HashMap<VariantKey, Code>,
    },
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct VariantKey {
    pub instancing: bool,
}
impl Default for VariantKey {
    #[inline(always)]
    fn default() -> Self {
        Self { instancing: false }
    }
}

pub struct Code {
    pub push_constant_buffer_size_bytes: usize,
    pub descriptor_set_bindings: Vec<DescriptorTypeVk>,
    pub vertex_semantic_to_location: HashMap<VertexInputSemantic, u32>,
    pub vertex_entry_point_name: Option<String>,
    pub fragment_entry_point_name: Option<String>,
    pub words: Vec<u32>,
}

#[derive(Debug)]
pub enum PropertyMappingVk {
    Direct(PropertyDestinationVk),
    Splitted(Vec<PropertyDestinationVk>),
    Texture2D {
        object: PropertyDestinationVk,
        uvst: VectorPropertyMappingVk,
    },
}

#[derive(Debug)]
pub enum VectorPropertyMappingVk {
    Direct(PropertyDestinationVk),
    Splitted(Vec<PropertyDestinationVk>),
}

#[derive(Debug)]
pub enum PropertyDestinationVk {
    SpecConstant(usize),
    PushConstantBlock(usize),
    DescriptorSet(usize),
    DescriptorSetUniformBuffer(usize),
    RealtimeBuffer(usize),
    InstanceBuffer(usize),
}

#[derive(Debug)]
pub enum DescriptorTypeVk {
    UniformBuffer { size_bytes: usize },
    StorageBuffer { size_bytes: usize },
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

#[derive(Debug, Clone, Copy)]
pub enum InstancingSupport {
    None,
    Allowed,
    Only,
}
impl Default for InstancingSupport {
    #[inline(always)]
    fn default() -> Self {
        Self::None
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

#[cfg(feature = "with-loader-impl")]
#[repr(C)]
pub struct UniformCameraParameters {
    pub view_projection_matrix: peridot::math::Matrix4F32,
}

#[cfg(feature = "with-loader-impl")]
#[repr(C)]
pub struct UniformObjectParameters {
    pub transform_matrix: peridot::math::Matrix4F32,
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
                variants,
            } => {
                shading_pass_directory
                    .entries
                    .push((n, file::ShadingPassDirectoryEntry::Located(writes as _)));
                writes += file::ShadingPassVk {
                    option_overrides,
                    variants: variants
                        .into_iter()
                        .map(|(k, v)| {
                            (
                                k,
                                file::Code {
                                    push_constant_buffer_size_bytes: v
                                        .push_constant_buffer_size_bytes,
                                    descriptor_set_bindings: v.descriptor_set_bindings,
                                    vertex_semantic_to_location: v
                                        .vertex_semantic_to_location
                                        .into_iter()
                                        .collect(),
                                    vertex_entry_point_name: v.vertex_entry_point_name,
                                    fragment_entry_point_name: v.fragment_entry_point_name,
                                    words: v.words,
                                },
                            )
                        })
                        .collect(),
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

pub async fn read_async(
    source: &(
         impl peridot_native_io::RandomReadBlobAsync + peridot_native_io::BlobMetadataAsync + ?Sized
     ),
) -> std::io::Result<CompiledRenderingConfigurationVk> {
    let header_pos = source.byte_length_async().await? - file::Header::BYTE_LENGTH as u64;
    let (header, _swap_bytes) = file::Header::read_async(
        &mut peridot_native_io::BufferedRandomBlobReader::new(&source),
        header_pos,
    )
    .await?;
    let property_directory =
        file::PropertyDirectory::read_async(core::pin::pin!(futures_util::io::BufReader::new(
            peridot_native_io::RandomBlobAsyncReadSeekAdapter::new(&source)
        )))
        .await?;
    let shading_pass_directory =
        file::ShadingPassDirectory::read_async(core::pin::pin!(futures_util::io::BufReader::new(
            peridot_native_io::RandomBlobAsyncReadSeekAdapter::with_pos(
                &source,
                header.shading_pass_directory_offset
            )
        )))
        .await?;

    let mut result = CompiledRenderingConfigurationVk {
        property_mappings: HashMap::with_capacity(property_directory.entries.len()),
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
                    let pass_data = file::ShadingPassVk::read_async(core::pin::pin!(
                        futures_util::io::BufReader::new(
                            peridot_native_io::RandomBlobAsyncReadSeekAdapter::with_pos(
                                &source, loc
                            )
                        )
                    ))
                    .await?;

                    let mut variants = HashMap::with_capacity(pass_data.variants.len());
                    for (k, v) in pass_data.variants {
                        let mut vertex_semantic_to_location =
                            HashMap::with_capacity(v.vertex_semantic_to_location.len());
                        for (n, l) in v.vertex_semantic_to_location {
                            match vertex_semantic_to_location.entry(n) {
                                std::collections::hash_map::Entry::Vacant(x) => {
                                    x.insert(l);
                                }
                                std::collections::hash_map::Entry::Occupied(x) => {
                                    panic!("conflicting vertex semantic: {:?}", x.key());
                                }
                            }
                        }

                        match variants.entry(k) {
                            std::collections::hash_map::Entry::Vacant(x) => {
                                x.insert(Code {
                                    push_constant_buffer_size_bytes: v
                                        .push_constant_buffer_size_bytes,
                                    descriptor_set_bindings: v.descriptor_set_bindings,
                                    vertex_semantic_to_location,
                                    vertex_entry_point_name: v.vertex_entry_point_name,
                                    fragment_entry_point_name: v.fragment_entry_point_name,
                                    words: v.words,
                                });
                            }
                            std::collections::hash_map::Entry::Occupied(x) => {
                                panic!("conflicting variant: {:?}", x.key());
                            }
                        }
                    }

                    x.insert(ShadingPassVk::Custom {
                        option_overrides: pass_data.option_overrides,
                        variants,
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

                    let mut variants = HashMap::with_capacity(pass_data.variants.len());
                    for (k, v) in pass_data.variants {
                        let mut vertex_semantic_to_location =
                            HashMap::with_capacity(v.vertex_semantic_to_location.len());
                        for (n, l) in v.vertex_semantic_to_location {
                            match vertex_semantic_to_location.entry(n) {
                                std::collections::hash_map::Entry::Vacant(x) => {
                                    x.insert(l);
                                }
                                std::collections::hash_map::Entry::Occupied(x) => {
                                    panic!("conflicting vertex semantic: {:?}", x.key());
                                }
                            }
                        }

                        match variants.entry(k) {
                            std::collections::hash_map::Entry::Vacant(x) => {
                                x.insert(Code {
                                    push_constant_buffer_size_bytes: v
                                        .push_constant_buffer_size_bytes,
                                    descriptor_set_bindings: v.descriptor_set_bindings,
                                    vertex_semantic_to_location,
                                    vertex_entry_point_name: v.vertex_entry_point_name,
                                    fragment_entry_point_name: v.fragment_entry_point_name,
                                    words: v.words,
                                });
                            }
                            std::collections::hash_map::Entry::Occupied(x) => {
                                panic!("conflicting variant: {:?}", x.key());
                            }
                        }
                    }

                    x.insert(ShadingPassVk::Custom {
                        option_overrides: pass_data.option_overrides,
                        variants,
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
