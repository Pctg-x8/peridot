use std::{collections::HashMap, num::NonZeroUsize};

use bedrock::{self as br, ImageChild};
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

    pub fn load<T: AssetResourceType>(
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
    Sprite(RuntimeSprite),
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

pub trait AssetResourceType: peridot::FromAssetBlob {
    type RuntimeResourceType: RuntimeResource;

    fn load_core(
        id: &peridot::AssetID,
        dep_loader: &mut RuntimeAssetManager,
        engine: &mut peridot::Engine<impl peridot::NativeLinker>,
        mm: &mut peridot_memory_manager::MemoryManager,
    ) -> Result<AssetResource, Self::Error>;
}
impl AssetResourceType for peridot_sprite_atlas::SpriteAsset {
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
            .load_by_id::<peridot_sprite_atlas::SpriteAtlasAsset>(&content.atlas_id, engine, mm)
            .expect("failed to load dependency")
            .expect("no dep");

        Ok(AssetResource::Sprite(RuntimeSprite {
            data: content,
            dep_atlas,
        }))
    }
}
impl AssetResourceType for peridot_sprite_atlas::SpriteAtlasAsset {
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
        assert!(content.content.needs_transcoding());
        content
            .content
            .transcode_basis(ktx::ffi::KTX_TTF_BC7_RGBA, ktx::TranscodeFlags::empty())
            .expect("transcode_basis");
        let atlas_width = content.width;
        let atlas_height = content.height;
        let offs = content.content.image_offset(0, 0, 0).expect("image_offset");

        // TODO: batch gfx resource allocation and uploading
        let gfx_resource = mm
            .allocate_device_local_image(
                engine.graphics(),
                br::ImageCreateInfo::new(
                    br::Extent2D {
                        width: atlas_width,
                        height: atlas_height,
                    },
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
                atlas_width,
                atlas_height,
                peridot::PixelFormat::BC7_SRGB,
                br::BufferUsage::TRANSFER_SRC,
            )
            .expect("staging_resource.allocate");
        staging_resource
            .copy_content_from_slice(unsafe {
                core::slice::from_raw_parts(
                    content.content.data().add(offs),
                    content.content.data_size(),
                )
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
                        bufferImageHeight: atlas_height,
                        imageSubresource: br::ImageSubresourceLayers::new(
                            br::AspectMask::COLOR,
                            0,
                            0..1,
                        )
                        .0,
                        imageOffset: br::Offset3D { x: 0, y: 0, z: 0 },
                        imageExtent: br::Extent3D {
                            width: atlas_width,
                            height: atlas_height,
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
            width: content.width,
            height: content.height,
        }))
    }
}
