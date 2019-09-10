//! Texture Helpers

use bedrock as br;
use crate::resource::*;
use crate::batches::TransferBatch;
use peridot_math::Vector2;
use std::ops::{Deref, Range};

pub struct Texture2D(br::ImageView, Image);
impl Texture2D
{
    pub fn init(g: &br::Device, size: &Vector2<u32>, format: PixelFormat, prealloc: &mut BufferPrealloc)
        -> br::Result<(br::Image, u64)>
    {
        let idesc = br::ImageDesc::new(size, format as _, br::ImageUsage::SAMPLED.transfer_dest(),
            br::ImageLayout::Preinitialized);
        let bytes_per_pixel = (format.bpp() >> 3) as u64;
        let pixels_stg =
            prealloc.add(BufferContent::Raw((size.x() * size.y()) as u64 * bytes_per_pixel, bytes_per_pixel));
        return idesc.create(g).map(|o| (o, pixels_stg));
    }
    pub fn new(img: Image) -> br::Result<Self>
    {
        let (fmt, cmap) = match img.format()
        {
            PixelFormat::RGB24 =>
            (
                Some(PixelFormat::RGBA32 as _),
                br::ComponentMapping(br::ComponentSwizzle::Identity, br::ComponentSwizzle::Identity,
                    br::ComponentSwizzle::Identity, br::ComponentSwizzle::One)
            ),
            PixelFormat::BGR24 =>
            (
                Some(PixelFormat::BGRA32 as _),
                br::ComponentMapping(br::ComponentSwizzle::Identity, br::ComponentSwizzle::Identity,
                    br::ComponentSwizzle::Identity, br::ComponentSwizzle::One)
            ),
            _ => (None, br::ComponentMapping::default())
        };

        img.create_view(fmt, None, &cmap, &br::ImageSubresourceRange::color(0, 0)).map(|v| Texture2D(v, img))
    }

    pub fn image(&self) -> &Image { &self.1 }
}
impl Deref for Texture2D
{
    type Target = br::ImageView;
    fn deref(&self) -> &br::ImageView { &self.0 }
}

/// Stg1. Group what textures are being initialized
pub struct TextureInitializationGroup<'g>(&'g br::Device, Vec<PixelData>);
/// Stg2. Describes where textures are being staged
pub struct TexturePreallocatedGroup(Vec<(PixelData, u64)>, Vec<br::Image>);
/// Stg3. Describes where textures are being staged, allocated and bound their memory
pub struct TextureInstantiatedGroup(Vec<(PixelData, u64)>, Vec<Texture2D>);

/// Low Dynamic Range(8bit colors) image asset
pub trait LDRImageAsset
{
    fn into_pixel_data_info(self) -> PixelData;
}

impl<'g> TextureInitializationGroup<'g>
{
    pub fn new(device: &'g br::Device) -> Self { TextureInitializationGroup(device, Vec::new()) }
    pub fn add<A: LDRImageAsset>(&mut self, asset: A) -> usize
    {
        let index = self.1.len();
        self.1.push(asset.into_pixel_data_info());
        return index;
    }
    pub fn prealloc(self, prealloc: &mut BufferPrealloc) -> br::Result<TexturePreallocatedGroup>
    {
        let (mut images, mut stage_info) = (Vec::with_capacity(self.1.len()), Vec::with_capacity(self.1.len()));
        for pd in self.1 {
            let (o, offs) = Texture2D::init(self.0, &pd.size, pd.format, prealloc)?;
            images.push(o); stage_info.push((pd, offs));
        }
        return Ok(TexturePreallocatedGroup(stage_info, images));
    }
}
impl TexturePreallocatedGroup
{
    pub fn alloc_and_instantiate(self, mut badget: MemoryBadget, device: &br::Device, pd: &br::PhysicalDevice)
        -> br::Result<(TextureInstantiatedGroup, Vec<MemoryBoundResource>)>
    {
        let img_count = self.1.len();
        for isrc in self.1 { badget.add(isrc); }
        let mtindex = pd.memory_properties().find_device_local_index(badget.memory_type_index_mask())
            .expect("no matching memory type index for allocation?");
        let mut resources = badget.alloc(device, mtindex)?;
        let textures = resources.drain(resources.len() - img_count ..)
            .map(|r| Texture2D::new(r.unwrap_image())).collect::<Result<Vec<_>, _>>()?;

        return Ok((TextureInstantiatedGroup(self.0, textures), resources));
    }
}
impl TextureInstantiatedGroup
{
    /// Copy texture pixels into a staging buffer.
    pub fn stage_data(&self, mr: &br::MappedMemoryRange)
    {
        trace!("Staging Texture Data...");
        for &(ref pd, offs) in &self.0
        {
            let s = unsafe
            {
                mr.slice_mut(offs as _, (pd.size.x() * pd.size.y()) as usize * (pd.format.bpp() >> 3) as usize)
            };
            s.copy_from_slice(pd.u8_pixels());
        }
    }
    /// Push transferring operations into a batcher.
    pub fn copy_from_stage_batches(&self, tb: &mut TransferBatch, stgbuf: &Buffer)
    {
        for (t, &(_, offs)) in self.1.iter().zip(self.0.iter())
        {
            tb.init_image_from(t.image(), (stgbuf, offs));
            tb.add_image_graphics_ready(br::PipelineStageFlags::FRAGMENT_SHADER, t.image(),
                br::ImageLayout::ShaderReadOnlyOpt);
        }
    }

    /// Returns a list of Texture2D.
    pub fn into_textures(self) -> Vec<Texture2D> { return self.1; }
}
impl Deref for TextureInstantiatedGroup
{
    type Target = [Texture2D];
    fn deref(&self) -> &[Texture2D] { &self.1 }
}
