use super::*;
use bedrock as br;
use std::ops::{Deref, Range};

mod memory;
pub use self::memory::*;
mod buffer;
pub use self::buffer::*;
mod image;
pub use self::image::*;
use num::Integer;

#[macro_export]
macro_rules! align2 {
    ($v: expr, $a: expr) => {
        ($v + ($a - 1)) & !($a - 1)
    };
}

pub struct BulkedResourceStorageAllocator {
    buffers: Vec<(br::Buffer, u64)>,
    images: Vec<(br::Image, u64)>,
    buffers_top: u64,
    images_top: u64,
    images_align_requirement: u64,
    memory_type_bitmask: u32,
}
impl BulkedResourceStorageAllocator {
    pub fn new() -> Self {
        BulkedResourceStorageAllocator {
            buffers: Vec::new(),
            images: Vec::new(),
            buffers_top: 0,
            images_top: 0,
            images_align_requirement: 0,
            memory_type_bitmask: std::u32::MAX,
        }
    }

    pub fn add_buffer(&mut self, buffer: br::Buffer) -> usize {
        let req = buffer.requirements();
        let new_offset = align2!(self.buffers_top, req.alignment);
        self.buffers.push((buffer, new_offset));
        self.memory_type_bitmask &= req.memoryTypeBits;
        self.buffers_top = new_offset + req.size;

        self.buffers.len() - 1
    }
    pub fn add_image(&mut self, image: br::Image) -> usize {
        let req = image.requirements();
        let new_offset = align2!(self.images_top, req.alignment);
        self.images.push((image, new_offset));
        self.memory_type_bitmask &= req.memoryTypeBits;
        self.images_top = new_offset + req.size;
        self.images_align_requirement = self.images_align_requirement.lcm(&req.alignment);

        self.images.len() - 1
    }
    pub fn add_images(&mut self, images: impl IntoIterator<Item = br::Image>) -> usize {
        let iter = images.into_iter();
        let (min, _) = iter.size_hint();
        if self.images.capacity() < self.images.len() + min {
            self.images.reserve(min);
        }
        let first_id = self.images.len();
        for x in iter {
            self.add_image(x);
        }

        first_id
    }

    pub fn alloc(self, g: &Graphics) -> br::Result<ResourceStorage> {
        let mt = g
            .memory_type_manager
            .device_local_index(self.memory_type_bitmask)
            .expect("No device-local memory")
            .index();
        let images_base = align2!(self.buffers_top, self.images_align_requirement);
        let total_size = images_base + self.images_top;
        info!(
            target: "peridot",
            "Allocating Device Memory: {} bytes in {}(?0x{:x})",
            total_size, mt, self.memory_type_bitmask
        );
        let mem = Rc::new(br::DeviceMemory::allocate(&g.device, total_size as _, mt)?);

        Ok(ResourceStorage {
            buffers: self
                .buffers
                .into_iter()
                .map(|(b, o)| Buffer::bound(b, &mem, o))
                .collect::<Result<_, _>>()?,
            images: self
                .images
                .into_iter()
                .map(|(i, o)| Image::bound(i, &mem, o + images_base))
                .collect::<Result<_, _>>()?,
        })
    }
    pub fn alloc_upload(self, g: &Graphics) -> br::Result<ResourceStorage> {
        let mt = g
            .memory_type_manager
            .exact_host_visible_index(
                self.memory_type_bitmask,
                br::MemoryPropertyFlags::HOST_COHERENT,
            )
            .expect("No host-visible memory")
            .index();
        let images_base = align2!(self.buffers_top, self.images_align_requirement);
        let total_size = images_base + self.images_top;
        info!(
            target: "peridot",
            "Allocating Upload Memory: {} bytes in 0x{:x}(?0x{:x})",
            total_size, mt, self.memory_type_bitmask
        );
        let mem = Rc::new(br::DeviceMemory::allocate(&g.device, total_size as _, mt)?);

        Ok(ResourceStorage {
            buffers: self
                .buffers
                .into_iter()
                .map(|(b, o)| Buffer::bound(b, &mem, o))
                .collect::<Result<_, _>>()?,
            images: self
                .images
                .into_iter()
                .map(|(i, o)| Image::bound(i, &mem, o + images_base))
                .collect::<Result<_, _>>()?,
        })
    }
}
pub struct ResourceStorage {
    buffers: Vec<Buffer>,
    images: Vec<Image>,
}
impl ResourceStorage {
    pub fn get_buffer(&self, index: usize) -> Option<&Buffer> {
        self.buffers.get(index)
    }
    pub fn get_image(&self, index: usize) -> Option<&Image> {
        self.images.get(index)
    }
}

use std::mem::ManuallyDrop;
pub struct AutocloseMappedMemoryRange<'m>(
    &'m br::DeviceMemory,
    ManuallyDrop<br::MappedMemoryRange<'m>>,
);
impl<'m> Deref for AutocloseMappedMemoryRange<'m> {
    type Target = br::MappedMemoryRange<'m>;
    fn deref(&self) -> &Self::Target {
        &self.1
    }
}
impl<'m> Drop for AutocloseMappedMemoryRange<'m> {
    fn drop(&mut self) {
        unsafe {
            // 1を確実に先に破棄したいのでManuallyDropで殺す
            ManuallyDrop::drop(&mut self.1);
            self.0.unmap();
        }
    }
}

/// A view of the buffer in GPU Address, holds object reference count
#[derive(Clone)]
pub struct DeviceBufferViewHold {
    pub buffer: Buffer,
    pub offset: br::vk::VkDeviceSize,
}
impl Buffer {
    pub fn hold_with_dev_offset(&self, offset: br::vk::VkDeviceSize) -> DeviceBufferViewHold {
        DeviceBufferViewHold {
            buffer: self.clone(),
            offset,
        }
    }
}
impl DeviceBufferViewHold {
    pub fn to_unhold_view(&self) -> DeviceBufferView {
        DeviceBufferView {
            buffer: &self.buffer,
            offset: self.offset,
        }
    }
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
        size: &math::Vector2<u32>,
        format: PixelFormat,
        prealloc: &mut BufferPrealloc,
    ) -> br::Result<(br::ImageObject<Device>, u64)> {
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

        idesc.create(g).map(|o| (o, pixels_stg))
    }
}
impl<Image: br::Image> Texture2D<Image> {
    pub fn new(img: Image) -> br::Result<Self> {
        let (fmt, cmap) = match PixelFormat::from(img.format()) {
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
            .map(Texture2D)
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
            let (o, offs) = Texture2D::init(self.0.clone(), &pd.size, pd.format, prealloc)?;
            images.push(o);
            stage_info.push((pd, offs));
        }

        Ok(TexturePreallocatedGroup(stage_info, images))
    }
}
impl TexturePreallocatedGroup<br::ImageObject<DeviceObject>> {
    pub fn alloc_and_instantiate<
        Buffer: br::Buffer<ConcreteDevice = DeviceObject> + br::MemoryBound,
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
    pub fn stage_data(&self, mr: &br::MappedMemoryRange<impl br::DeviceMemory + ?Sized>) {
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
        tb: &mut TransferBatch,
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

    /// Add new mipmapped DeviceWorkingTexture allocation
    pub fn new_cube_mipmapped(
        &mut self,
        size: math::Vector2<u32>,
        format: PixelFormat,
        usage: br::ImageUsage,
        mipmaps: u32,
    ) -> DeviceWorkingCubeTextureRef {
        let mut id = br::ImageDesc::new(&size, format as _, usage, br::ImageLayout::Preinitialized);
        id.flags(br::ImageFlags::CUBE_COMPATIBLE)
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
        let images2 = self.planes.iter().map(|d| d.create(g.device.clone()));
        let images_cube = self.cube.iter().map(|d| d.create(g.device.clone()));
        let images3 = self.volumes.iter().map(|d| d.create(g.device.clone()));
        let images: Vec<_> = images2
            .chain(images_cube)
            .chain(images3)
            .collect::<Result<_, _>>()?;
        let mut storage_alloc = BulkedResourceStorageAllocator::new();
        storage_alloc.add_images(images);
        let ResourceStorage {
            images: mut bound_images,
            ..
        } = storage_alloc.alloc(g)?;

        let mut cs_v3s = bound_images.split_off(self.planes.len());
        let v3s = cs_v3s.split_off(self.cube.len());
        Ok(DeviceWorkingTextureStore {
            planes: self
                .planes
                .into_iter()
                .zip(bound_images.into_iter())
                .map(|(d, res)| {
                    use br::Image;

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
                    })
                })
                .collect::<Result<_, _>>()?,
            cubes: self
                .cube
                .into_iter()
                .zip(cs_v3s.into_iter())
                .map(|(d, res)| {
                    use br::Image;

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
                    })
                })
                .collect::<Result<_, _>>()?,
            volumes: self
                .volumes
                .into_iter()
                .zip(v3s.into_iter())
                .map(|(d, res)| {
                    use br::Image;

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

/// Describing the type that can be used as initializer of `FixedBuffer`s
pub trait FixedBufferInitializer {
    /// Setup memory data in staging buffer
    fn stage_data(&mut self, m: &br::MappedMemoryRange<impl br::DeviceMemory + ?Sized>);
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
    /// Device accessible buffer object
    pub buffer: (SharedRef<Buffer>, u64),
    /// Host buffer staging per-frame mutable data
    pub mut_buffer: (SharedRef<DynamicMut<Buffer>>, u64),
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
            buffer: (buffer, imm_buffer_size),
            mut_buffer: (
                SharedRef::new(DynamicMut::new(mut_buffer)),
                prealloc_mut.total_size(),
            ),
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
