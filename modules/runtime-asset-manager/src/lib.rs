use std::{collections::HashMap, num::NonZeroUsize};

use bedrock::{self as br, ImageChild, TypedVulkanStructure};
use ktx::Texture;

pub struct RuntimeAssetManager {
    loaded_assets: HashMap<peridot::AssetID, AssetResourceHandle>,
    asset_resources: Vec<AssetResource>,
}
impl RuntimeAssetManager {
    pub fn new() -> Self {
        Self {
            loaded_assets: HashMap::new(),
            asset_resources: Vec::new(),
        }
    }

    pub fn load<T: AssetResourceType + peridot::LogicalAssetData>(
        &mut self,
        identifier: &str,
        engine: &mut peridot::Engine<impl peridot::NativeLinker>,
        mm: &mut peridot_memory_manager::MemoryManager,
    ) -> Result<Option<TypedAssetResourceHandle<T::RuntimeResourceType>>, T::Error> {
        let Some(asset_id) = engine.resolve_runtime_asset_id::<T>(identifier) else {
            return Ok(None);
        };

        self.load_by_id::<T>(&asset_id, engine, mm)
    }

    fn load_by_id<T: AssetResourceType>(
        &mut self,
        id: &peridot::AssetID,
        engine: &mut peridot::Engine<impl peridot::NativeLinker>,
        mm: &mut peridot_memory_manager::MemoryManager,
    ) -> Result<Option<TypedAssetResourceHandle<T::RuntimeResourceType>>, T::Error> {
        if let Some(&loaded) = self.loaded_assets.get(id) {
            tracing::info!(?id, "Asset has been loaded already");
            return Ok(Some(TypedAssetResourceHandle(
                loaded,
                core::marker::PhantomData,
            )));
        }

        tracing::info!(?id, "Loading Asset");
        let asset = T::load_core(id, self, engine, mm)?;
        self.asset_resources.push(asset);
        let handle = AssetResourceHandle::from_array_index(self.asset_resources.len() - 1);
        self.loaded_assets.insert(id.clone(), handle);

        Ok(Some(TypedAssetResourceHandle(
            handle,
            core::marker::PhantomData,
        )))
    }

    #[inline(always)]
    pub fn get(&self, handle: &AssetResourceHandle) -> Option<&AssetResource> {
        self.asset_resources.get(handle.to_array_index())
    }

    #[inline(always)]
    pub fn get_of<T: RuntimeResource>(
        &self,
        handle: &AssetResourceHandle,
    ) -> Result<&T, GetResourceError> {
        T::extract_ref(self.get(handle).ok_or(GetResourceError::InvalidHandle)?)
            .ok_or(GetResourceError::NotAType)
    }

    #[inline(always)]
    pub fn get_typed<T: RuntimeResource>(
        &self,
        handle: &TypedAssetResourceHandle<T>,
    ) -> Result<&T, GetResourceError> {
        self.get_of::<T>(&handle.0)
    }
}

#[derive(thiserror::Error, Debug)]
pub enum GetResourceError {
    #[error("invalid handle")]
    InvalidHandle,
    #[error("not a type")]
    NotAType,
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AssetResourceHandle(NonZeroUsize);
impl AssetResourceHandle {
    const fn to_array_index(self) -> usize {
        self.0.get() - 1
    }

    const fn from_array_index(x: usize) -> Self {
        Self(unsafe { NonZeroUsize::new_unchecked(x + 1) })
    }
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypedAssetResourceHandle<T: RuntimeResource>(
    AssetResourceHandle,
    core::marker::PhantomData<T>,
);
impl<T: RuntimeResource> TypedAssetResourceHandle<T> {
    #[inline(always)]
    pub const fn into_untyped(self) -> AssetResourceHandle {
        self.0
    }
}

pub enum AssetResource {
    Texture2D(RuntimeTexture2D),
    Mesh(RuntimeMesh),
    Sprite(RuntimeSprite),
    SpriteAtlas(RuntimeSpriteAtlas),
}

pub trait RuntimeResource {
    fn extract_ref(packed: &AssetResource) -> Option<&Self>;
}

pub struct RuntimeSprite {
    pub data: peridot_sprite_atlas::SpriteAsset,
    pub dep_atlas: TypedAssetResourceHandle<RuntimeTexture2D>,
}
impl RuntimeResource for RuntimeSprite {
    #[inline(always)]
    fn extract_ref(packed: &AssetResource) -> Option<&Self> {
        match packed {
            AssetResource::Sprite(x) => Some(x),
            _ => None,
        }
    }
}
impl RuntimeSprite {
    #[inline(always)]
    pub fn uvst(&self, atlas: &RuntimeTexture2D) -> peridot_math::Vector4F32 {
        self.data.uvst(atlas.width, atlas.height)
    }
}

pub struct RuntimeSpriteAtlas {
    pub texture: TypedAssetResourceHandle<RuntimeTexture2D>,
    pub sprites: HashMap<String, TypedAssetResourceHandle<RuntimeSprite>>,
}
impl RuntimeResource for RuntimeSpriteAtlas {
    #[inline(always)]
    fn extract_ref(packed: &AssetResource) -> Option<&Self> {
        match packed {
            AssetResource::SpriteAtlas(x) => Some(x),
            _ => None,
        }
    }
}

pub struct RuntimeTexture2D {
    pub gfx_resource: br::ImageViewObject<peridot_memory_manager::Image>,
    pub width: u32,
    pub height: u32,
}
impl RuntimeResource for RuntimeTexture2D {
    #[inline(always)]
    fn extract_ref(packed: &AssetResource) -> Option<&Self> {
        match packed {
            AssetResource::Texture2D(x) => Some(x),
            _ => None,
        }
    }
}

pub trait AssetResourceType {
    type Error;
    type RuntimeResourceType: RuntimeResource;

    fn load_core(
        id: &peridot::AssetID,
        dep_loader: &mut RuntimeAssetManager,
        engine: &mut peridot::Engine<impl peridot::NativeLinker>,
        mm: &mut peridot_memory_manager::MemoryManager,
    ) -> Result<AssetResource, Self::Error>;
}
impl AssetResourceType for peridot_image::StdTexture2DAsset {
    type Error = <Self as peridot::FromAssetBlob>::Error;
    type RuntimeResourceType = RuntimeTexture2D;

    fn load_core(
        id: &peridot::AssetID,
        _dep_loader: &mut RuntimeAssetManager,
        engine: &mut peridot::Engine<impl peridot::NativeLinker>,
        mm: &mut peridot_memory_manager::MemoryManager,
    ) -> Result<AssetResource, Self::Error> {
        let mut content = <Self as peridot::FromAssetBlob>::from_asset_blob(
            engine.open_raw_asset_by_id(id.clone())?,
        )?;

        // TODO: proper handle transcoding
        assert!(content.0.needs_transcoding());
        content
            .0
            .transcode_basis(ktx::ffi::KTX_TTF_BC7_RGBA, ktx::TranscodeFlags::empty())
            .expect("transcode_basis");
        let width = content.0.base_width();
        let height = content.0.base_height();
        let offs = content.0.image_offset(0, 0, 0).expect("image_offset");

        // TODO: batch gfx resource allocation and uploading
        let gfx_resource = mm
            .allocate_device_local_image(
                engine.graphics(),
                br::ImageCreateInfo::new(
                    br::Extent2D { width, height },
                    br::vk::VK_FORMAT_BC7_SRGB_BLOCK,
                )
                .with_usage(br::ImageUsageFlags::SAMPLED | br::ImageUsageFlags::TRANSFER_DEST),
            )
            .expect("gfx_resource.allocate");
        let gfx_resource = br::ImageViewBuilder::new(
            gfx_resource,
            br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
        )
        .create()
        .expect("gfx_resource.view.create");
        let mut staging_resource = mm
            .allocate_upload_linear_image_buffer(
                engine.graphics(),
                width,
                height,
                peridot::PixelFormat::BC7_SRGB,
                br::BufferUsage::TRANSFER_SRC,
            )
            .expect("staging_resource.allocate");
        staging_resource
            .copy_content_from_slice(unsafe {
                core::slice::from_raw_parts(content.0.data().add(offs), content.0.data_size())
            })
            .expect("write image upload buffer");

        engine
            .submit_commands(|c| {
                c.pipeline_barrier(
                    br::PipelineStageFlags::TOP_OF_PIPE,
                    br::PipelineStageFlags::TRANSFER,
                    0,
                    &[],
                    &[],
                    &[br::ImageMemoryBarrier::new(
                        gfx_resource.image(),
                        br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1).0,
                        br::ImageLayout::TransferDestOpt.from_undefined(),
                    )],
                )
                .copy_buffer_to_image(
                    &staging_resource.inner,
                    gfx_resource.image(),
                    br::ImageLayout::TransferDestOpt,
                    &[br::vk::VkBufferImageCopy {
                        bufferOffset: 0,
                        bufferRowLength: staging_resource.row_texels,
                        bufferImageHeight: height,
                        imageSubresource: br::ImageSubresourceLayers::new(
                            br::AspectMask::COLOR,
                            0,
                            0..1,
                        )
                        .0,
                        imageOffset: br::Offset3D { x: 0, y: 0, z: 0 },
                        imageExtent: br::Extent3D {
                            width,
                            height,
                            depth: 1,
                        },
                    }],
                )
                .pipeline_barrier(
                    br::PipelineStageFlags::TRANSFER,
                    br::PipelineStageFlags::FRAGMENT_SHADER,
                    0,
                    &[],
                    &[],
                    &[br::ImageMemoryBarrier::new(
                        gfx_resource.image(),
                        br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1).0,
                        br::ImageLayout::ShaderReadOnlyOpt.from_undefined(),
                    )
                    .access_mask_transition(0, br::AccessFlags::SHADER.read)],
                )
            })
            .expect("submit data copy");

        Ok(AssetResource::Texture2D(RuntimeTexture2D {
            gfx_resource,
            width,
            height,
        }))
    }
}
impl AssetResourceType for peridot_sprite_atlas::SpriteAsset {
    type Error = <Self as peridot::FromAssetBlob>::Error;
    type RuntimeResourceType = RuntimeSprite;

    fn load_core(
        id: &peridot::AssetID,
        dep_loader: &mut RuntimeAssetManager,
        engine: &mut peridot::Engine<impl peridot::NativeLinker>,
        mm: &mut peridot_memory_manager::MemoryManager,
    ) -> Result<AssetResource, Self::Error> {
        let content = <Self as peridot::FromAssetBlob>::from_asset_blob(
            engine.open_raw_asset_by_id(id.clone())?,
        )?;

        let dep_atlas = dep_loader
            .load_by_id::<peridot_image::StdTexture2DAsset>(&content.atlas_texture_id, engine, mm)
            .expect("failed to load dependency")
            .expect("no dep");

        Ok(AssetResource::Sprite(RuntimeSprite {
            data: content,
            dep_atlas,
        }))
    }
}
impl AssetResourceType for peridot_sprite_atlas::SpriteAtlasAsset {
    type Error = <Self as peridot::FromAssetBlob>::Error;
    type RuntimeResourceType = RuntimeSpriteAtlas;

    fn load_core(
        id: &peridot::AssetID,
        dep_loader: &mut RuntimeAssetManager,
        engine: &mut peridot::Engine<impl peridot::NativeLinker>,
        mm: &mut peridot_memory_manager::MemoryManager,
    ) -> Result<AssetResource, Self::Error> {
        let content = <Self as peridot::FromAssetBlob>::from_asset_blob(
            engine.open_raw_asset_by_id(id.clone())?,
        )?;

        let texture = dep_loader
            .load_by_id::<peridot_image::StdTexture2DAsset>(&content.texture_asset_id, engine, mm)
            .expect("Failed to load dependency")
            .expect("no dependent asset?");
        let sprites = content
            .sprite_ids
            .into_iter()
            .map(|(name, id)| {
                (
                    name,
                    dep_loader
                        .load_by_id::<peridot_sprite_atlas::SpriteAsset>(&id, engine, mm)
                        .expect("Failed to load dependency")
                        .expect("no dependent asset?"),
                )
            })
            .collect::<HashMap<_, _>>();

        Ok(AssetResource::SpriteAtlas(RuntimeSpriteAtlas {
            texture,
            sprites,
        }))
    }
}

pub struct RuntimeMeshVertexStream {
    pub buffer_offset: u64,
    pub buffer_stride: u32,
    pub attributes: Vec<(peridot_mesh::Attribute, peridot_mesh::AttributeData)>,
}
pub struct RuntimeMeshIndexStream {
    pub buffer_offset: u64,
    pub r#type: peridot_mesh::IndexType,
    pub count: u32,
}
pub struct RuntimeMesh {
    pub gfx_resource: peridot_memory_manager::Buffer,
    pub topology: peridot_mesh::PrimitiveTopology,
    pub vertex_count: u32,
    pub vertex_streams: Vec<RuntimeMeshVertexStream>,
    pub index: Option<RuntimeMeshIndexStream>,
}
impl RuntimeResource for RuntimeMesh {
    #[inline(always)]
    fn extract_ref(packed: &AssetResource) -> Option<&Self> {
        match packed {
            AssetResource::Mesh(x) => Some(x),
            _ => None,
        }
    }
}

impl AssetResourceType for peridot_mesh::AssetCore {
    type Error = std::io::Error;
    type RuntimeResourceType = RuntimeMesh;

    fn load_core(
        id: &peridot::AssetID,
        _dep_loader: &mut RuntimeAssetManager,
        engine: &mut peridot::Engine<impl peridot::NativeLinker>,
        mm: &mut peridot_memory_manager::MemoryManager,
    ) -> Result<AssetResource, Self::Error> {
        let content = peridot_mesh::Asset::open(engine.open_raw_asset_by_id(id.clone())?)?;

        let mut buffer_top = 0u64;
        let mut vk_buffer_usage = br::BufferUsage::TRANSFER_DEST;
        let mut vertex_streams = Vec::with_capacity(content.vertex_streams.len());
        let mut vertex_byte_length_per_stream = Vec::with_capacity(content.vertex_streams.len());
        for (stream, attrs) in content.vertex_streams.iter() {
            let buffer_offset = peridot_math::round_up_pow2n_u64(
                buffer_top,
                stream.buffer.device_alignment_requirement as _,
            );
            let mut buffer_stride = 0;
            for (_a, ad) in attrs.iter() {
                buffer_stride = buffer_stride.max(ad.offset as u32 + ad.element_type.size() as u32);
            }

            vertex_streams.push(RuntimeMeshVertexStream {
                buffer_offset,
                buffer_stride,
                attributes: attrs.clone(),
            });
            vertex_byte_length_per_stream.push(stream.buffer.byte_length);

            buffer_top = buffer_offset + stream.buffer.byte_length as u64;
            vk_buffer_usage |= br::BufferUsage::VERTEX_BUFFER;
        }
        let (index_offset_in_buffer, index_byte_length);
        let index_stream = match content.index_stream {
            peridot_mesh::IndexStream::None => {
                index_offset_in_buffer = 0;
                index_byte_length = 0;
                None
            }
            peridot_mesh::IndexStream::Stream {
                index_type,
                ref buffer,
            } => {
                let buffer_offset = peridot_math::round_up_pow2n_u64(
                    buffer_top,
                    buffer.device_alignment_requirement as _,
                );

                buffer_top = buffer_offset + buffer.byte_length as u64;
                vk_buffer_usage |= br::BufferUsage::INDEX_BUFFER;

                index_offset_in_buffer = buffer_offset;
                index_byte_length = buffer.byte_length;
                Some(RuntimeMeshIndexStream {
                    buffer_offset,
                    r#type: index_type,
                    count: content.index_count(),
                })
            }
        };

        // TODO: batch resource creation/uploading
        let buffer = mm
            .allocate_device_local_buffer(
                engine.graphics(),
                br::BufferCreateInfo::new(buffer_top, vk_buffer_usage),
            )
            .expect("buffer.create");
        let mut upload_buffer = mm
            .allocate_upload_buffer(
                engine.graphics(),
                br::BufferCreateInfo::new(buffer_top, br::BufferUsage::TRANSFER_SRC),
            )
            .expect("upload_buffer.create");
        upload_buffer
            .guard_map(peridot_memory_manager::BufferMapMode::Write, |p| {
                for (n, v) in vertex_streams.iter().enumerate() {
                    content
                        .read_vertex_buffer_into(n, unsafe {
                            core::slice::from_raw_parts_mut(
                                p.ptr().byte_add(v.buffer_offset as _).cast().as_ptr(),
                                content.vertex_streams[n].0.buffer.byte_length as _,
                            )
                        })
                        .expect("vertex_buffer.read");
                }

                if index_stream.is_some() {
                    content
                        .read_index_buffer_into(unsafe {
                            core::slice::from_raw_parts_mut(
                                p.ptr()
                                    .byte_add(index_offset_in_buffer as _)
                                    .cast()
                                    .as_ptr(),
                                index_byte_length as _,
                            )
                        })
                        .expect("index_buffer.read");
                }
            })
            .expect("upload_buffer.write");

        let runtime_mesh = RuntimeMesh {
            gfx_resource: buffer,
            topology: content.header.primitive_topology,
            vertex_count: vertex_byte_length_per_stream[0] / vertex_streams[0].buffer_stride,
            vertex_streams,
            index: index_stream,
        };
        drop(content);

        engine
            .submit_commands(|rec| {
                rec.copy_buffer(
                    &upload_buffer,
                    &runtime_mesh.gfx_resource,
                    &[br::BufferCopy::mirror(0, buffer_top)],
                )
                .pipeline_barrier(
                    br::PipelineStageFlags::TRANSFER,
                    br::PipelineStageFlags::VERTEX_INPUT,
                    0,
                    &[br::vk::VkMemoryBarrier {
                        sType: br::vk::VkMemoryBarrier::TYPE,
                        pNext: core::ptr::null(),
                        srcAccessMask: br::AccessFlags::TRANSFER.write,
                        dstAccessMask: br::AccessFlags::VERTEX_ATTRIBUTE_READ,
                    }],
                    &[],
                    &[],
                )
            })
            .expect("upload.cmd");

        Ok(AssetResource::Mesh(runtime_mesh))
    }
}
