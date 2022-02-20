use super::*;
use bedrock as br;
use std::ops::{Deref, Range};

mod memory;
pub use self::memory::*;
mod buffer;
pub use self::buffer::*;
mod image;
pub use self::image::*;

#[macro_export]
macro_rules! align2 {
    ($v: expr, $a: expr) => {
        ($v + ($a - 1)) & !($a - 1)
    };
}

#[derive(Clone, Copy, Debug)]
#[repr(i32)]
pub enum PixelFormat {
    RGBA32 = br::vk::VK_FORMAT_R8G8B8A8_UNORM,
    BGRA32 = br::vk::VK_FORMAT_B8G8R8A8_UNORM,
    RGB24 = br::vk::VK_FORMAT_R8G8B8_UNORM,
    BGR24 = br::vk::VK_FORMAT_B8G8R8_UNORM,
    RGBA64F = br::vk::VK_FORMAT_R16G16B16A16_SFLOAT,
}
impl PixelFormat {
    /// Bits per pixel for each format enums
    pub fn bpp(self) -> usize {
        match self {
            PixelFormat::RGBA32 | PixelFormat::BGRA32 => 32,
            PixelFormat::RGB24 | PixelFormat::BGR24 => 24,
            PixelFormat::RGBA64F => 64,
        }
    }
}

pub struct Texture2D(br::ImageView, Image);
impl Texture2D {
    pub fn init(
        g: &br::Device,
        size: &math::Vector2<u32>,
        format: PixelFormat,
        prealloc: &mut BufferPrealloc,
    ) -> br::Result<(br::Image, u64)> {
        let idesc = br::ImageDesc::new(
            size,
            format as _,
            br::ImageUsage::SAMPLED.transfer_dest(),
            br::ImageLayout::Preinitialized,
        );
        let bytes_per_pixel = (format.bpp() >> 3) as u64;
        let pixels_stg = prealloc.add(BufferContent::Raw(
            (size.x() * size.y()) as u64 * bytes_per_pixel,
            bytes_per_pixel,
        ));
        return idesc.create(g).map(|o| (o, pixels_stg));
    }
    pub fn new(img: Image) -> br::Result<Self> {
        let (fmt, cmap) = match img.format() {
            PixelFormat::RGB24 => (
                Some(PixelFormat::RGBA32 as _),
                br::ComponentMapping(
                    br::ComponentSwizzle::Identity,
                    br::ComponentSwizzle::Identity,
                    br::ComponentSwizzle::Identity,
                    br::ComponentSwizzle::One,
                ),
            ),
            PixelFormat::BGR24 => (
                Some(PixelFormat::BGRA32 as _),
                br::ComponentMapping(
                    br::ComponentSwizzle::Identity,
                    br::ComponentSwizzle::Identity,
                    br::ComponentSwizzle::Identity,
                    br::ComponentSwizzle::One,
                ),
            ),
            _ => (None, br::ComponentMapping::default()),
        };

        img.create_view(fmt, None, &cmap, &br::ImageSubresourceRange::color(0, 0))
            .map(|v| Texture2D(v, img))
    }

    pub fn image(&self) -> &Image {
        &self.1
    }
}
impl Deref for Texture2D {
    type Target = br::ImageView;
    fn deref(&self) -> &br::ImageView {
        &self.0
    }
}

pub struct DecodedPixelData {
    pub pixels: Vec<u8>,
    pub size: math::Vector2<u32>,
    pub format: PixelFormat,
    pub stride: usize,
}
impl DecodedPixelData {
    pub fn u8_pixels(&self) -> &[u8] {
        &self.pixels
    }
}
/// Low Dynamic Range(8bit colors) image asset
pub trait LDRImageAsset {
    fn into_pixel_data_info(self) -> DecodedPixelData;
}

/// Stg1. Group what textures are being initialized
pub struct TextureInitializationGroup<'g>(&'g br::Device, Vec<DecodedPixelData>);
/// Stg2. Describes where textures are being staged
pub struct TexturePreallocatedGroup(Vec<(DecodedPixelData, u64)>, Vec<br::Image>);
/// Stg3. Describes where textures are being staged, allocated and bound their memory
pub struct TextureInstantiatedGroup(Vec<(DecodedPixelData, u64)>, Vec<Texture2D>);

impl<'g> TextureInitializationGroup<'g> {
    pub fn new(device: &'g br::Device) -> Self {
        TextureInitializationGroup(device, Vec::new())
    }
    pub fn add<A: LDRImageAsset>(&mut self, asset: A) -> usize {
        let index = self.1.len();
        self.1.push(asset.into_pixel_data_info());
        return index;
    }
    pub fn prealloc(self, prealloc: &mut BufferPrealloc) -> br::Result<TexturePreallocatedGroup> {
        let (mut images, mut stage_info) = (
            Vec::with_capacity(self.1.len()),
            Vec::with_capacity(self.1.len()),
        );
        for pd in self.1 {
            let (o, offs) = Texture2D::init(self.0, &pd.size, pd.format, prealloc)?;
            images.push(o);
            stage_info.push((pd, offs));
        }
        return Ok(TexturePreallocatedGroup(stage_info, images));
    }
}
impl TexturePreallocatedGroup {
    pub fn alloc_and_instantiate(
        self,
        mut badget: MemoryBadget,
    ) -> br::Result<(TextureInstantiatedGroup, Vec<MemoryBoundResource>)> {
        let img_count = self.1.len();
        for isrc in self.1 {
            badget.add(isrc);
        }
        let mut resources = badget.alloc()?;
        let textures = resources
            .drain(resources.len() - img_count..)
            .map(|r| Texture2D::new(r.unwrap_image()))
            .collect::<Result<Vec<_>, _>>()?;

        return Ok((TextureInstantiatedGroup(self.0, textures), resources));
    }
}
impl TextureInstantiatedGroup {
    /// Copy texture pixels into a staging buffer.
    pub fn stage_data(&self, mr: &br::MappedMemoryRange) {
        trace!("Staging Texture Data...");
        for &(ref pd, offs) in &self.0 {
            let s = unsafe {
                mr.slice_mut(
                    offs as _,
                    (pd.size.x() * pd.size.y()) as usize * (pd.format.bpp() >> 3) as usize,
                )
            };
            s.copy_from_slice(pd.u8_pixels());
        }
    }
    /// Push transferring operations into a batcher.
    pub fn copy_from_stage_batches(&self, tb: &mut TransferBatch, stgbuf: &Buffer) {
        for (t, &(_, offs)) in self.1.iter().zip(self.0.iter()) {
            tb.init_image_from(t.image(), stgbuf.with_dev_offset(offs));
            tb.add_image_graphics_ready(
                br::PipelineStageFlags::FRAGMENT_SHADER,
                t.image(),
                br::ImageLayout::ShaderReadOnlyOpt,
            );
        }
    }

    /// Returns a list of Texture2D.
    pub fn into_textures(self) -> Vec<Texture2D> {
        return self.1;
    }
}
impl Deref for TextureInstantiatedGroup {
    type Target = [Texture2D];
    fn deref(&self) -> &[Texture2D] {
        &self.1
    }
}

/// RenderTexture2D without Readback to CPU
pub struct DeviceWorkingTexture2D {
    size: math::Vector2<u32>,
    format: PixelFormat,
    res: Image,
    view: br::ImageView,
}
impl DeviceWorkingTexture2D {
    /// Size of this texture
    pub fn size(&self) -> &math::Vector2<u32> {
        &self.size
    }
    /// Width of this texture
    pub fn width(&self) -> u32 {
        self.size.0
    }
    /// Height of this texture
    pub fn height(&self) -> u32 {
        self.size.1
    }
    /// Format of this texture
    pub fn format(&self) -> PixelFormat {
        self.format
    }

    /// Gets underlying resource object
    pub fn underlying(&self) -> &Image {
        &self.res
    }
}
impl Deref for DeviceWorkingTexture2D {
    type Target = br::ImageView;
    fn deref(&self) -> &br::ImageView {
        &self.view
    }
}
/// RenderTexture3D without Readback to CPU
pub struct DeviceWorkingTexture3D {
    size: math::Vector3<u32>,
    format: PixelFormat,
    res: Image,
    view: br::ImageView,
}
impl DeviceWorkingTexture3D {
    /// Size of this texture
    pub fn size(&self) -> &math::Vector3<u32> {
        &self.size
    }
    /// Width of this texture
    pub fn width(&self) -> u32 {
        self.size.0
    }
    /// Height of this texture
    pub fn height(&self) -> u32 {
        self.size.1
    }
    /// Depth of this texture
    pub fn depth(&self) -> u32 {
        self.size.2
    }
    /// Format of this texture
    pub fn format(&self) -> PixelFormat {
        self.format
    }

    /// Gets underlying resource object
    pub fn underlying(&self) -> &Image {
        &self.res
    }
}
impl Deref for DeviceWorkingTexture3D {
    type Target = br::ImageView;
    fn deref(&self) -> &br::ImageView {
        &self.view
    }
}

/// RenderCubeTexture without Readback to CPU
pub struct DeviceWorkingCubeTexture {
    size: math::Vector2<u32>,
    format: PixelFormat,
    res: Image,
    view: br::ImageView,
}

#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct DeviceWorkingTexture2DRef(usize);
#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct DeviceWorkingTexture3DRef(usize);
#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct DeviceWorkingCubeTextureRef(usize);
/// DeviceWorkingTexture Management Arena
pub struct DeviceWorkingTextureAllocator<'d> {
    planes: Vec<br::ImageDesc<'d>>,
    volumes: Vec<br::ImageDesc<'d>>,
    cube: Vec<br::ImageDesc<'d>>,
}
impl DeviceWorkingTextureAllocator<'_> {
    /// Initializes the allocator
    pub fn new() -> Self {
        DeviceWorkingTextureAllocator {
            planes: Vec::new(),
            volumes: Vec::new(),
            cube: Vec::new(),
        }
    }

    /// Add new DeviceWorkingTexture2D allocation
    pub fn new2d(
        &mut self,
        size: math::Vector2<u32>,
        format: PixelFormat,
        usage: br::ImageUsage,
    ) -> DeviceWorkingTexture2DRef {
        self.planes.push(br::ImageDesc::new(
            &size,
            format as _,
            usage,
            br::ImageLayout::Preinitialized,
        ));
        DeviceWorkingTexture2DRef(self.planes.len() - 1)
    }

    /// Add new DeviceWorkingTexture3D allocation
    pub fn new3d(
        &mut self,
        size: math::Vector3<u32>,
        format: PixelFormat,
        usage: br::ImageUsage,
    ) -> DeviceWorkingTexture3DRef {
        self.volumes.push(br::ImageDesc::new(
            &size,
            format as _,
            usage,
            br::ImageLayout::Preinitialized,
        ));
        DeviceWorkingTexture3DRef(self.volumes.len() - 1)
    }

    /// Add new DeviceWorkingCubeTexture allocation
    pub fn new_cube(
        &mut self,
        size: math::Vector2<u32>,
        format: PixelFormat,
        usage: br::ImageUsage,
    ) -> DeviceWorkingCubeTextureRef {
        let mut id = br::ImageDesc::new(&size, format as _, usage, br::ImageLayout::Preinitialized);
        id.flags(br::ImageFlags::CUBE_COMPATIBLE).array_layers(6);
        self.cube.push(id);

        DeviceWorkingCubeTextureRef(self.cube.len() - 1)
    }

    /// Allocates all of added textures
    pub fn alloc(self, g: &Graphics) -> br::Result<DeviceWorkingTextureStore> {
        let images2 = self.planes.iter().map(|d| d.create(g));
        let images_cube = self.cube.iter().map(|d| d.create(g));
        let images3 = self.volumes.iter().map(|d| d.create(g));
        let images: Vec<_> = images2
            .chain(images_cube)
            .chain(images3)
            .collect::<Result<_, _>>()?;
        let mut mb = MemoryBadget::new(g);
        for img in images {
            mb.add(img);
        }
        let mut bound_images = mb.alloc()?;

        let mut cs_v3s = bound_images.split_off(self.planes.len());
        let v3s = cs_v3s.split_off(self.cube.len());
        Ok(DeviceWorkingTextureStore {
            planes: self
                .planes
                .into_iter()
                .zip(bound_images.into_iter())
                .map(|(d, res)| {
                    let res = res.unwrap_image();
                    let view = res.create_view(
                        None,
                        None,
                        &br::ComponentMapping::default(),
                        &br::ImageSubresourceRange::color(0..1, 0..1),
                    )?;

                    Ok(DeviceWorkingTexture2D {
                        size: math::Vector2(d.as_ref().extent.width, d.as_ref().extent.height),
                        format: unsafe { std::mem::transmute(d.as_ref().format) },
                        view,
                        res,
                    })
                })
                .collect::<Result<_, _>>()?,
            cubes: self
                .cube
                .into_iter()
                .zip(cs_v3s.into_iter())
                .map(|(d, res)| {
                    let res = res.unwrap_image();
                    let view = res.create_view(
                        None,
                        Some(br::vk::VK_IMAGE_VIEW_TYPE_CUBE),
                        &br::ComponentMapping::default(),
                        &br::ImageSubresourceRange::color(0..1, 0..6),
                    )?;

                    Ok(DeviceWorkingCubeTexture {
                        size: math::Vector2(d.as_ref().extent.width, d.as_ref().extent.height),
                        format: unsafe { std::mem::transmute(d.as_ref().format) },
                        view,
                        res,
                    })
                })
                .collect::<Result<_, _>>()?,
            volumes: self
                .volumes
                .into_iter()
                .zip(v3s.into_iter())
                .map(|(d, res)| {
                    let res = res.unwrap_image();
                    let view = res.create_view(
                        None,
                        None,
                        &br::ComponentMapping::default(),
                        &br::ImageSubresourceRange::color(0..1, 0..1),
                    )?;

                    Ok(DeviceWorkingTexture3D {
                        size: math::Vector3(
                            d.as_ref().extent.width,
                            d.as_ref().extent.height,
                            d.as_ref().extent.depth,
                        ),
                        format: unsafe { std::mem::transmute(d.as_ref().format) },
                        view,
                        res,
                    })
                })
                .collect::<Result<_, _>>()?,
        })
    }
}
/// Allocated DeviceWorkingTexture Arena
pub struct DeviceWorkingTextureStore {
    planes: Vec<DeviceWorkingTexture2D>,
    cubes: Vec<DeviceWorkingCubeTexture>,
    volumes: Vec<DeviceWorkingTexture3D>,
}
/// DeviceWorkingTexture Reference
pub trait DeviceWorkingTextureRef {
    /// Type of the Texture that this reference referring to
    type TextureT;
    /// Gets texture object from the store
    fn get(self, store: &DeviceWorkingTextureStore) -> &Self::TextureT;
}
impl DeviceWorkingTextureStore {
    /// Gets texture object by References
    pub fn get<R: DeviceWorkingTextureRef>(
        &self,
        r: R,
    ) -> &<R as DeviceWorkingTextureRef>::TextureT {
        r.get(self)
    }
}
impl DeviceWorkingTextureRef for DeviceWorkingTexture2DRef {
    type TextureT = DeviceWorkingTexture2D;

    fn get(self, store: &DeviceWorkingTextureStore) -> &DeviceWorkingTexture2D {
        &store.planes[self.0]
    }
}
impl DeviceWorkingTextureRef for DeviceWorkingCubeTextureRef {
    type TextureT = DeviceWorkingCubeTexture;

    fn get(self, store: &DeviceWorkingTextureStore) -> &DeviceWorkingCubeTexture {
        &store.cubes[self.0]
    }
}
impl DeviceWorkingTextureRef for DeviceWorkingTexture3DRef {
    type TextureT = DeviceWorkingTexture3D;

    fn get(self, store: &DeviceWorkingTextureStore) -> &DeviceWorkingTexture3D {
        &store.volumes[self.0]
    }
}

/// Describing the type that can be used as initializer of `FixedBuffer`s
pub trait FixedBufferInitializer {
    /// Setup memory data in staging buffer
    fn stage_data(&mut self, m: &br::MappedMemoryRange);
    fn buffer_graphics_ready(&self, tfb: &mut TransferBatch, buf: &Buffer, range: Range<u64>);
}
/// The Fix-sized buffers and textures manager
pub struct FixedMemory {
    /// Device accessible buffer object
    pub buffer: (Buffer, u64),
    /// Host buffer staging per-frame mutable data
    pub mut_buffer: (Buffer, u64),
    /// The placement offset of mut_buffer data in buffer
    pub mut_buffer_placement: u64,
    /// Textures
    pub textures: Vec<Texture2D>,
}
impl FixedMemory {
    /// Initialize a FixedMemory using preallocation structures
    pub fn new<'g, I: FixedBufferInitializer + ?Sized>(
        g: &'g Graphics,
        mut prealloc: BufferPrealloc<'g>,
        prealloc_mut: BufferPrealloc<'g>,
        textures: TextureInitializationGroup<'g>,
        initializer: &mut I,
        tfb: &mut TransferBatch,
    ) -> br::Result<Self> {
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
        let mut_buffer = mb_mut
            .alloc_upload()?
            .pop()
            .expect("objectless")
            .unwrap_buffer();
        let mut mb_stg = MemoryBadget::new(g);
        mb_stg.add(stg_buffer);
        let mut stg_buffer = mb_stg
            .alloc_upload()?
            .pop()
            .expect("objectless")
            .unwrap_buffer();

        stg_buffer.guard_map(0..stg_buffer_fullsize, |m| {
            textures.stage_data(m);
            initializer.stage_data(m);
        })?;

        textures.copy_from_stage_batches(tfb, &stg_buffer);
        tfb.add_mirroring_buffer(&stg_buffer, &buffer, 0, imm_buffer_size);
        initializer.buffer_graphics_ready(tfb, &buffer, 0..imm_buffer_size);

        Ok(FixedMemory {
            buffer: (buffer, imm_buffer_size),
            mut_buffer: (mut_buffer, prealloc_mut.total_size()),
            mut_buffer_placement,
            textures: textures.into_textures(),
        })
    }

    pub fn range_in_mut_buffer<T>(&self, r: Range<T>) -> Range<T>
    where
        T: std::ops::Add<Output = T> + std::convert::TryFrom<u64> + Copy,
    {
        match T::try_from(self.mut_buffer_placement) {
            Ok(p) => r.start + p..r.end + p,
            Err(_) => panic!("Overflowing Placement offset"),
        }
    }
}
