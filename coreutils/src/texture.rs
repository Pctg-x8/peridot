//! Texture Helpers

use bedrock as br;
use crate::resource::*;
use crate::batches::TransferBatch;
use peridot_math::Vector2;
use std::ops::Deref;

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
    pub fn alloc_and_instantiate(self, mut badget: MemoryBadget)
        -> br::Result<(TextureInstantiatedGroup, Vec<MemoryBoundResource>)>
    {
        let img_count = self.1.len();
        for isrc in self.1 { badget.add(isrc); }
        let mut resources = badget.alloc(self.0)?;
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

/// Describing the type that can be used as initializer of `FixedBuffer`s
pub trait FixedBufferInitializer
{
    /// Setup memory data in staging buffer
    fn stage_data(&mut self, m: &br::MappedMemoryRange);
    fn buffer_graphics_ready(&self, tfb: &mut TransferBatch, buf: &Buffer, range: Range<u64>);
}
/// The Fix-sized buffers and textures manager
pub struct FixedMemory
{
    /// Device accessible buffer object
    pub buffer: (Buffer, u64),
    /// Host buffer staging per-frame mutable data
    pub mut_buffer: (Buffer, u64),
    /// The placement offset of mut_buffer data in buffer
    pub mut_buffer_placement: u64,
    /// Textures
    pub textures: Vec<Texture2D>
}
impl FixedMemory
{
    /// Initialize a FixedMemory using preallocation structures
    pub fn new<'g, I: FixedBufferInitializer + ?Sized>(
        g: &'g Graphics,
        mut prealloc: BufferPrealloc<'g>,
        prealloc_mut: BufferPrealloc<'g>,
        textures: TextureInitializationGroup<'g>,
        initializer: &mut I, tfb: &mut TransferBatch) -> br::Result<Self>
    {
        let mut_buffer = prealloc_mut.build_upload()?;
        let mut p_bufferdata_prealloc = prealloc.clone();
        let imm_buffer_size = p_bufferdata_prealloc.total_size();
        let mut_buffer_placement = p_bufferdata_prealloc.merge(&prealloc_mut);
        let buffer = p_bufferdata_prealloc.build_transferred()?;

        let tex_preallocs = textures.prealloc(&mut prealloc)?;
        let stg_buffer_fullsize = prealloc.total_size();
        let stg_buffer = prealloc.build_upload()?;

        let (mut mb, mut mb_mut) = (MemoryBadget::new(g), MemoryBadget::new(g));
        mb.add(buffer);
        mb_mut.add(mut_buffer);
        let (textures, mut bufs) = tex_preallocs.alloc_and_instantiate(mb)?;
        let buffer = bufs.pop().expect("objectless").unwrap_buffer();
        let mut_buffer = mb_mut.alloc_upload()?.pop().expect("objectless").unwrap_buffer();
        let mut mb_stg = MemoryBadget::new(g);
        mb_stg.add(stg_buffer);
        let stg_buffer = mb_stg.alloc_upload()?.pop().expect("objectless").unwrap_buffer();

        stg_buffer.guard_map(stg_buffer_fullsize, |m| { textures.stage_data(m); initializer.stage_data(m); })?;

        textures.copy_from_stage_batches(tfb, &stg_buffer);
        tfb.add_mirroring_buffer(&stg_buffer, &buffer, 0, imm_buffer_size);
        initializer.buffer_graphics_ready(tfb, &buffer, 0 .. imm_buffer_size);

        Ok(FixedMemory
        {
            buffer: (buffer, imm_buffer_size), mut_buffer: (mut_buffer, prealloc_mut.total_size()),
            mut_buffer_placement,
            textures: textures.into_textures()
        })
    }

    pub fn range_in_mut_buffer<T>(&self, r: Range<T>) -> Range<T> where
        T: std::ops::Add<Output = T> + std::convert::TryFrom<u64> + Copy
    {
        match T::try_from(self.mut_buffer_placement)
        {
            Ok(p) => r.start + p .. r.end + p,
            Err(_) => panic!("Overflowing Placement offset")
        }
    }
}
