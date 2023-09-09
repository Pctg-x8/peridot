use super::*;
use bedrock as br;
use br::{ImageChild, ImageSubresourceSlice};
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
impl From<br::vk::VkFormat> for PixelFormat {
    fn from(v: br::vk::VkFormat) -> Self {
        // PixelFormat has exactly same bits as VkFormat
        unsafe { std::mem::transmute(v) }
    }
}

pub struct Texture2D<Image: br::Image>(br::ImageViewObject<Image>);
impl<Device: br::Device> Texture2D<br::ImageObject<Device>> {
    pub fn init(
        g: Device,
        size: math::Vector2<u32>,
        format: PixelFormat,
        prealloc: &mut BufferPrealloc,
    ) -> br::Result<(br::ImageObject<Device>, u64)> {
        let idesc = br::ImageDesc::new(
            size.clone(),
            format as _,
            br::ImageUsage::SAMPLED.transfer_dest(),
            br::ImageLayout::Preinitialized,
        );
        let bytes_per_pixel = (format.bpp() >> 3) as u64;
        let pixels_stg = prealloc.add(BufferContent::Raw(
            (size.x() * size.y()) as u64 * bytes_per_pixel,
            bytes_per_pixel,
        ));

        idesc.create(g).map(|o| (o, pixels_stg))
    }
}
impl<Image: br::Image> Texture2D<Image> {
    pub fn new(img: Image) -> br::Result<Self> {
        let pf = PixelFormat::from(img.format());

        let view_builder = img
            .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
            .view_builder();
        let view_builder = match pf {
            PixelFormat::RGB24 => view_builder
                .with_format_mutation(PixelFormat::RGBA32 as _)
                .with_mapping(br::ComponentMapping(
                    br::ComponentSwizzle::Identity,
                    br::ComponentSwizzle::Identity,
                    br::ComponentSwizzle::Identity,
                    br::ComponentSwizzle::One,
                )),
            PixelFormat::BGR24 => view_builder
                .with_format_mutation(PixelFormat::BGRA32 as _)
                .with_mapping(br::ComponentMapping(
                    br::ComponentSwizzle::Identity,
                    br::ComponentSwizzle::Identity,
                    br::ComponentSwizzle::Identity,
                    br::ComponentSwizzle::One,
                )),
            _ => view_builder,
        };

        view_builder.create().map(Texture2D)
    }

    pub fn image(&self) -> &Image {
        &self.0
    }
}
impl<Image: br::Image> Deref for Texture2D<Image> {
    type Target = br::ImageViewObject<Image>;

    fn deref(&self) -> &br::ImageViewObject<Image> {
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
pub struct TextureInitializationGroup<Device: br::Device>(Device, Vec<DecodedPixelData>);
/// Stg2. Describes where textures are being staged
pub struct TexturePreallocatedGroup<Image: br::Image>(Vec<(DecodedPixelData, u64)>, Vec<Image>);
/// Stg3. Describes where textures are being staged, allocated and bound their memory
pub struct TextureInstantiatedGroup<Device: br::Device>(
    Vec<(DecodedPixelData, u64)>,
    Vec<Texture2D<SharedRef<Image<br::ImageObject<Device>, br::DeviceMemoryObject<Device>>>>>,
);

impl<Device: br::Device> TextureInitializationGroup<Device> {
    pub const fn new(device: Device) -> Self {
        TextureInitializationGroup(device, Vec::new())
    }

    pub fn add<A: LDRImageAsset>(&mut self, asset: A) -> usize {
        let index = self.1.len();
        self.1.push(asset.into_pixel_data_info());
        return index;
    }

    pub fn prealloc(
        self,
        prealloc: &mut BufferPrealloc,
    ) -> br::Result<TexturePreallocatedGroup<br::ImageObject<Device>>>
    where
        Device: Clone,
    {
        let (mut images, mut stage_info) = (
            Vec::with_capacity(self.1.len()),
            Vec::with_capacity(self.1.len()),
        );
        for pd in self.1 {
            let (o, offs) = Texture2D::init(self.0.clone(), pd.size.clone(), pd.format, prealloc)?;
            images.push(o);
            stage_info.push((pd, offs));
        }

        Ok(TexturePreallocatedGroup(stage_info, images))
    }
}
impl TexturePreallocatedGroup<br::ImageObject<DeviceObject>> {
    pub fn alloc_and_instantiate<
        Buffer: br::Buffer<ConcreteDevice = DeviceObject> + br::MemoryBound + br::VkHandleMut,
    >(
        self,
        mut badget: MemoryBadget<Buffer, br::ImageObject<DeviceObject>>,
    ) -> br::Result<(
        TextureInstantiatedGroup<DeviceObject>,
        Vec<
            MemoryBoundResource<
                Buffer,
                br::ImageObject<DeviceObject>,
                br::DeviceMemoryObject<DeviceObject>,
            >,
        >,
    )> {
        let img_count = self.1.len();
        for isrc in self.1 {
            badget.add(MemoryBadgetEntry::Image(isrc));
        }
        let mut resources = badget.alloc()?;
        let textures = resources
            .drain(resources.len() - img_count..)
            .map(|r| Texture2D::new(SharedRef::new(r.unwrap_image())))
            .collect::<Result<Vec<_>, _>>()?;

        Ok((TextureInstantiatedGroup(self.0, textures), resources))
    }
}
impl<Device: br::Device + 'static> TextureInstantiatedGroup<Device> {
    /// Copy texture pixels into a staging buffer.
    pub fn stage_data(
        &self,
        mr: &br::MappedMemoryRange<impl br::DeviceMemory + br::VkHandleMut + ?Sized>,
    ) {
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
    pub fn copy_from_stage_batches(
        &self,
        tb: &mut TransferBatch<Device>,
        stgbuf: &SharedRef<
            Buffer<
                impl br::Buffer<ConcreteDevice = Device> + 'static,
                impl br::DeviceMemory + 'static,
            >,
        >,
    ) {
        for (t, &(_, offs)) in self.1.iter().zip(self.0.iter()) {
            tb.init_image_from(
                t.image().clone(),
                DeviceBufferView {
                    buffer: stgbuf.clone(),
                    offset: offs,
                },
            );
            tb.add_image_graphics_ready(
                br::PipelineStageFlags::FRAGMENT_SHADER,
                t.image().clone(),
                br::ImageLayout::ShaderReadOnlyOpt,
            );
        }
    }

    /// Returns a list of Texture2D.
    pub fn into_textures(
        self,
    ) -> Vec<Texture2D<SharedRef<Image<br::ImageObject<Device>, br::DeviceMemoryObject<Device>>>>>
    {
        self.1
    }
}
impl<Device: br::Device> Deref for TextureInstantiatedGroup<Device> {
    type Target =
        [Texture2D<SharedRef<Image<br::ImageObject<Device>, br::DeviceMemoryObject<Device>>>>];

    fn deref(&self) -> &Self::Target {
        &self.1
    }
}

/// RenderTexture2D without Readback to CPU
pub struct DeviceWorkingTexture2D<Image: br::Image> {
    size: math::Vector2<u32>,
    format: PixelFormat,
    view: br::ImageViewObject<Image>,
}
impl<Image: br::Image> DeviceWorkingTexture2D<Image> {
    /// Size of this texture
    pub const fn size(&self) -> &math::Vector2<u32> {
        &self.size
    }

    /// Width of this texture
    pub const fn width(&self) -> u32 {
        self.size.0
    }

    /// Height of this texture
    pub const fn height(&self) -> u32 {
        self.size.1
    }

    /// Format of this texture
    pub const fn format(&self) -> PixelFormat {
        self.format
    }

    /// Gets underlying resource object
    pub fn underlying(&self) -> &Image {
        &*self.view
    }

    /// Gets underlying view object
    pub fn underlying_view(&self) -> &br::ImageViewObject<Image> {
        &self.view
    }
}
impl<Image: br::Image> Deref for DeviceWorkingTexture2D<Image> {
    type Target = br::ImageViewObject<Image>;

    fn deref(&self) -> &Self::Target {
        &self.view
    }
}
/// RenderTexture3D without Readback to CPU
pub struct DeviceWorkingTexture3D<Image: br::Image> {
    size: math::Vector3<u32>,
    format: PixelFormat,
    view: br::ImageViewObject<Image>,
}
impl<Image: br::Image> DeviceWorkingTexture3D<Image> {
    /// Size of this texture
    pub const fn size(&self) -> &math::Vector3<u32> {
        &self.size
    }

    /// Width of this texture
    pub const fn width(&self) -> u32 {
        self.size.0
    }

    /// Height of this texture
    pub const fn height(&self) -> u32 {
        self.size.1
    }

    /// Depth of this texture
    pub const fn depth(&self) -> u32 {
        self.size.2
    }

    /// Format of this texture
    pub const fn format(&self) -> PixelFormat {
        self.format
    }

    /// Gets underlying resource object
    pub fn underlying(&self) -> &Image {
        &*self.view
    }

    /// Gets underlying view object
    pub fn underlying_view(&self) -> &br::ImageViewObject<Image> {
        &self.view
    }
}
impl<Image: br::Image> Deref for DeviceWorkingTexture3D<Image> {
    type Target = br::ImageViewObject<Image>;

    fn deref(&self) -> &Self::Target {
        &self.view
    }
}

/// RenderCubeTexture without Readback to CPU
pub struct DeviceWorkingCubeTexture<Image: br::Image> {
    size: math::Vector2<u32>,
    format: PixelFormat,
    view: br::ImageViewObject<Image>,
}
impl<Image: br::Image> DeviceWorkingCubeTexture<Image> {
    /// Size of a plane in this texture
    pub const fn size(&self) -> &math::Vector2<u32> {
        &self.size
    }

    /// Width of a plane in this texture
    pub const fn width(&self) -> u32 {
        self.size.0
    }

    /// Height of a plane in this texture
    pub const fn height(&self) -> u32 {
        self.size.1
    }

    /// Format of a plane in this texture
    pub const fn format(&self) -> PixelFormat {
        self.format
    }

    /// Gets underlying resource object
    pub fn underlying(&self) -> &Image {
        &*self.view
    }

    /// Gets underlying view object
    pub fn underlying_view(&self) -> &br::ImageViewObject<Image> {
        &self.view
    }
}
impl<Image: br::Image> Deref for DeviceWorkingCubeTexture<Image> {
    type Target = br::ImageViewObject<Image>;

    fn deref(&self) -> &Self::Target {
        &self.view
    }
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
            size,
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
            size,
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
        let id = br::ImageDesc::new(size, format as _, usage, br::ImageLayout::Preinitialized)
            .flags(br::ImageFlags::CUBE_COMPATIBLE)
            .array_layers(6);
        self.cube.push(id);

        DeviceWorkingCubeTextureRef(self.cube.len() - 1)
    }

    /// Add new mipmapped DeviceWorkingTexture allocation
    pub fn new_cube_mipmapped(
        &mut self,
        size: math::Vector2<u32>,
        format: PixelFormat,
        usage: br::ImageUsage,
        mipmaps: u32,
    ) -> DeviceWorkingCubeTextureRef {
        let id = br::ImageDesc::new(size, format as _, usage, br::ImageLayout::Preinitialized)
            .flags(br::ImageFlags::CUBE_COMPATIBLE)
            .array_layers(6)
            .mip_levels(mipmaps);
        self.cube.push(id);

        DeviceWorkingCubeTextureRef(self.cube.len() - 1)
    }

    /// Allocates all of added textures
    pub fn alloc(
        self,
        g: &Graphics,
    ) -> br::Result<
        DeviceWorkingTextureStore<
            Image<br::ImageObject<DeviceObject>, br::DeviceMemoryObject<DeviceObject>>,
        >,
    > {
        let plane_count = self.planes.len();
        let cube_count = self.cube.len();

        let images2 = self.planes.into_iter().map(|d| d.create(g.device.clone()));
        let images_cube = self.cube.into_iter().map(|d| d.create(g.device.clone()));
        let images3 = self.volumes.into_iter().map(|d| d.create(g.device.clone()));
        let images: Vec<_> = images2
            .chain(images_cube)
            .chain(images3)
            .collect::<Result<_, _>>()?;
        let mut mb = MemoryBadget::<br::BufferObject<DeviceObject>, _>::new(g);
        for img in images {
            mb.add(MemoryBadgetEntry::Image(img));
        }
        let mut bound_images = mb.alloc()?;

        let mut cs_v3s = bound_images.split_off(plane_count);
        let v3s = cs_v3s.split_off(cube_count);
        Ok(DeviceWorkingTextureStore {
            planes: bound_images
                .into_iter()
                .map(|res| {
                    use br::Image;

                    let res = res.unwrap_image();
                    let view = res
                        .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                        .view_builder()
                        .create()?;

                    Ok(DeviceWorkingTexture2D {
                        size: view.image().size().wh().into(),
                        format: unsafe { core::mem::transmute(view.image().format()) },
                        view,
                    })
                })
                .collect::<Result<_, _>>()?,
            cubes: cs_v3s
                .into_iter()
                .map(|res| {
                    use br::Image;

                    let res = res.unwrap_image();
                    let view = res
                        .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                        .view_builder()
                        .with_dimension(br::vk::VK_IMAGE_VIEW_TYPE_CUBE)
                        .create()?;

                    Ok(DeviceWorkingCubeTexture {
                        size: view.image().size().wh().into(),
                        format: unsafe { core::mem::transmute(view.image().format()) },
                        view,
                    })
                })
                .collect::<Result<_, _>>()?,
            volumes: v3s
                .into_iter()
                .map(|res| {
                    use br::Image;

                    let res = res.unwrap_image();
                    let view = res
                        .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                        .view_builder()
                        .create()?;

                    Ok(DeviceWorkingTexture3D {
                        size: view.image().size().clone().into(),
                        format: unsafe { std::mem::transmute(view.image().format()) },
                        view,
                    })
                })
                .collect::<Result<_, _>>()?,
        })
    }
}
/// Allocated DeviceWorkingTexture Arena
pub struct DeviceWorkingTextureStore<Image: br::Image> {
    planes: Vec<DeviceWorkingTexture2D<Image>>,
    cubes: Vec<DeviceWorkingCubeTexture<Image>>,
    volumes: Vec<DeviceWorkingTexture3D<Image>>,
}
/// DeviceWorkingTexture Reference
pub trait DeviceWorkingTextureRef<Image: br::Image> {
    /// Type of the Texture that this reference referring to
    type TextureT;
    /// Gets texture object from the store
    fn get(self, store: &DeviceWorkingTextureStore<Image>) -> &Self::TextureT;
}
impl<Image: br::Image> DeviceWorkingTextureStore<Image> {
    /// Gets texture object by References
    pub fn get<R: DeviceWorkingTextureRef<Image>>(&self, r: R) -> &R::TextureT {
        r.get(self)
    }
}
impl<Image: br::Image> DeviceWorkingTextureRef<Image> for DeviceWorkingTexture2DRef {
    type TextureT = DeviceWorkingTexture2D<Image>;

    fn get(self, store: &DeviceWorkingTextureStore<Image>) -> &DeviceWorkingTexture2D<Image> {
        &store.planes[self.0]
    }
}
impl<Image: br::Image> DeviceWorkingTextureRef<Image> for DeviceWorkingCubeTextureRef {
    type TextureT = DeviceWorkingCubeTexture<Image>;

    fn get(self, store: &DeviceWorkingTextureStore<Image>) -> &DeviceWorkingCubeTexture<Image> {
        &store.cubes[self.0]
    }
}
impl<Image: br::Image> DeviceWorkingTextureRef<Image> for DeviceWorkingTexture3DRef {
    type TextureT = DeviceWorkingTexture3D<Image>;

    fn get(self, store: &DeviceWorkingTextureStore<Image>) -> &DeviceWorkingTexture3D<Image> {
        &store.volumes[self.0]
    }
}

pub struct BufferWithLength<Object> {
    pub object: Object,
    pub length: u64,
}
impl<Object> BufferWithLength<Object> {
    pub const fn full_range(&self) -> std::ops::Range<u64> {
        0..self.length
    }
}

/// Describing the type that can be used as initializer of `FixedBuffer`s
pub trait FixedBufferInitializer {
    /// Setup memory data in staging buffer
    fn stage_data(
        &mut self,
        m: &br::MappedMemoryRange<impl br::DeviceMemory + br::VkHandleMut + ?Sized>,
    );
    fn buffer_graphics_ready<Device: br::Device + 'static>(
        &self,
        tfb: &mut TransferBatch,
        buf: &SharedRef<
            Buffer<
                impl br::Buffer<ConcreteDevice = Device> + 'static,
                impl br::DeviceMemory<ConcreteDevice = Device> + 'static,
            >,
        >,
        range: Range<u64>,
    );
}
/// The Fix-sized buffers and textures manager
pub struct FixedMemory<Device: br::Device, Buffer: br::Buffer> {
    /// Device accessible buffer object: (buffer object, byte length)
    pub buffer: BufferWithLength<SharedRef<Buffer>>,
    /// Host buffer staging per-frame mutable data: (buffer object, byte length)
    pub mut_buffer: BufferWithLength<SharedRef<DynamicMut<Buffer>>>,
    /// The placement offset of mut_buffer data in buffer
    pub mut_buffer_placement: u64,
    /// Textures
    pub textures:
        Vec<Texture2D<SharedRef<Image<br::ImageObject<Device>, br::DeviceMemoryObject<Device>>>>>,
}
impl
    FixedMemory<
        DeviceObject,
        Buffer<br::BufferObject<DeviceObject>, br::DeviceMemoryObject<DeviceObject>>,
    >
{
    /// Initialize a FixedMemory using preallocation structures
    pub fn new<'g, I: FixedBufferInitializer + ?Sized>(
        g: &'g Graphics,
        mut prealloc: BufferPrealloc<'g>,
        prealloc_mut: BufferPrealloc<'g>,
        textures: TextureInitializationGroup<DeviceObject>,
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

        let (mut mb, mut mb_mut) = (
            MemoryBadget::new(g),
            MemoryBadget::<_, br::ImageObject<DeviceObject>>::new(g),
        );
        mb.add(MemoryBadgetEntry::Buffer(buffer));
        mb_mut.add(MemoryBadgetEntry::Buffer(mut_buffer));
        let (textures, mut bufs) = tex_preallocs.alloc_and_instantiate(mb)?;
        let buffer = SharedRef::new(bufs.pop().expect("objectless").unwrap_buffer());
        let mut_buffer = mb_mut
            .alloc_upload()?
            .pop()
            .expect("objectless")
            .unwrap_buffer();
        let mut mb_stg = MemoryBadget::<_, br::ImageObject<DeviceObject>>::new(g);
        mb_stg.add(MemoryBadgetEntry::Buffer(stg_buffer));
        let mut stg_buffer = mb_stg
            .alloc_upload()?
            .pop()
            .expect("objectless")
            .unwrap_buffer();

        stg_buffer.guard_map(0..stg_buffer_fullsize, |m| {
            textures.stage_data(m);
            initializer.stage_data(m);
        })?;
        let stg_buffer = SharedRef::new(stg_buffer);

        textures.copy_from_stage_batches(tfb, &stg_buffer);
        tfb.add_mirroring_buffer(stg_buffer.clone(), buffer.clone(), 0, imm_buffer_size);
        initializer.buffer_graphics_ready(tfb, &buffer, 0..imm_buffer_size);

        Ok(FixedMemory {
            buffer: BufferWithLength {
                object: buffer,
                length: imm_buffer_size,
            },
            mut_buffer: BufferWithLength {
                object: SharedRef::new(DynamicMut::new(mut_buffer)),
                length: prealloc_mut.total_size(),
            },
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
