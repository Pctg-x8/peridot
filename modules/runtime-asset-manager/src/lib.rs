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
    ) -> Result<Option<AssetResourceHandle>, T::Error> {
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
    ) -> Result<Option<AssetResourceHandle>, T::Error> {
        if let Some(&loaded) = self.loaded_assets.get(id) {
            tracing::info!(?id, "Asset has been loaded already");
            return Ok(Some(loaded));
        }

        tracing::info!(?id, "Loading Asset");
        let asset = T::load_core(id, self, engine, mm)?;
        self.asset_resources.push(asset);
        let handle = AssetResourceHandle::from_array_index(self.asset_resources.len() - 1);
        self.loaded_assets.insert(id.clone(), handle);

        Ok(Some(handle))
    }

    #[inline(always)]
    pub fn get(&self, handle: &AssetResourceHandle) -> Option<&AssetResource> {
        self.asset_resources.get(handle.to_array_index())
    }
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

pub enum AssetResource {
    Sprite(RuntimeSprite),
    SpriteAtlas(RuntimeSpriteAtlas),
}

pub struct RuntimeSprite {
    pub data: peridot_sprite_atlas::SpriteAsset,
    pub dep_atlas: AssetResourceHandle,
}

pub struct RuntimeSpriteAtlas {
    pub data: peridot_sprite_atlas::SpriteAtlasAsset,
    pub gfx_resource: br::ImageViewObject<peridot_memory_manager::Image>,
}

pub trait AssetResourceType: peridot::FromAssetBlob {
    fn load_core(
        id: &peridot::AssetID,
        dep_loader: &mut RuntimeAssetManager,
        engine: &mut peridot::Engine<impl peridot::NativeLinker>,
        mm: &mut peridot_memory_manager::MemoryManager,
    ) -> Result<AssetResource, Self::Error>;
}
impl AssetResourceType for peridot_sprite_atlas::SpriteAsset {
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

        Ok(AssetResource::SpriteAtlas(RuntimeSpriteAtlas {
            data: content,
            gfx_resource,
        }))
    }
}
