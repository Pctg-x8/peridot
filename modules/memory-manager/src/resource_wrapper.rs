use bedrock::{self as br, DeviceMemoryMut};
use br::{Device, DeviceChild, VkHandle};
#[allow(unused_imports)]
use peridot::mthelper::DynamicMutabilityProvider;
use peridot::mthelper::SharedMutableRef;

use crate::MemoryBlock;

pub(crate) enum BackingMemory {
    Managed(SharedMutableRef<MemoryBlock>),
    Native(br::DeviceMemoryObject<peridot::VulkanGfx>),
    NativeShared(SharedMutableRef<br::DeviceMemoryObject<peridot::VulkanGfx>>),
}
impl BackingMemory {
    #[inline]
    pub(crate) fn vk_handle(&self) -> br::vk::VkDeviceMemory {
        match self {
            Self::Managed(ref m) => m.borrow().handle,
            Self::Native(ref m) => m.native_ptr(),
            Self::NativeShared(ref m) => m.borrow().native_ptr(),
        }
    }
}

pub struct Image {
    pub(crate) handle: br::vk::VkImage,
    pub(crate) device: peridot::VulkanGfx,
    pub(crate) memory_block: BackingMemory,
    pub(crate) format: br::Format,
    pub(crate) size: br::Extent3D,
    pub(crate) image_type: br::vk::VkImageViewType,
    #[allow(dead_code)]
    pub(crate) offset: u64,
    pub(crate) byte_length: usize,
    pub(crate) malloc_offset: u64,
}
impl Drop for Image {
    fn drop(&mut self) {
        if let BackingMemory::Managed(ref b) = self.memory_block {
            b.borrow_mut().free(self.byte_length, self.malloc_offset);
        }

        unsafe {
            br::vkfn_wrapper::destroy_image(self.device.native_ptr(), self.handle, None);
        }
    }
}
impl br::VkHandle for Image {
    type Handle = <br::ImageObject<peridot::DeviceObject> as br::VkHandle>::Handle;

    fn native_ptr(&self) -> Self::Handle {
        self.handle
    }
}
impl br::VkHandleMut for Image {
    fn native_ptr_mut(&mut self) -> Self::Handle {
        self.handle
    }
}
impl br::VkObject for Image {
    const TYPE: br::vk::VkObjectType =
        <br::ImageObject<peridot::DeviceObject> as br::VkObject>::TYPE;
}
impl br::DeviceChildHandle for Image {
    #[inline(always)]
    fn device_handle(&self) -> bedrock::vk::VkDevice {
        self.device.native_ptr()
    }
}
impl br::DeviceChild for Image {
    type ConcreteDevice = peridot::VulkanGfx;

    fn device(&self) -> &Self::ConcreteDevice {
        &self.device
    }
}
impl br::Image for Image {
    fn format(&self) -> br::vk::VkFormat {
        self.format
    }

    fn size(&self) -> &br::vk::VkExtent3D {
        &self.size
    }

    fn dimension(&self) -> br::vk::VkImageViewType {
        self.image_type
    }
}

#[repr(transparent)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Copy, Hash)]
pub struct AnyPointer(pub core::ptr::NonNull<u8>);
impl AnyPointer {
    pub const fn ptr(self) -> core::ptr::NonNull<u8> {
        self.0
    }

    pub unsafe fn get_at<T>(&self, byte_offset: usize) -> &T {
        (self.0.as_ptr().add(byte_offset) as *const T)
            .as_ref()
            .unwrap_unchecked()
    }

    #[allow(clippy::mut_from_ref)]
    pub unsafe fn get_mut_at<T>(&self, byte_offset: usize) -> &mut T {
        (self.0.as_ptr().add(byte_offset) as *mut T)
            .as_mut()
            .unwrap_unchecked()
    }

    pub const unsafe fn slice<T>(&self, byte_offset: usize, len: usize) -> &[T] {
        core::slice::from_raw_parts(self.0.as_ptr().add(byte_offset) as _, len)
    }

    #[allow(clippy::mut_from_ref)]
    pub unsafe fn slice_mut<T>(&self, byte_offset: usize, len: usize) -> &mut [T] {
        core::slice::from_raw_parts_mut(self.0.as_ptr().add(byte_offset) as _, len)
    }

    pub unsafe fn clone_to<T: Clone>(&self, byte_offset: usize, value: &T) {
        self.get_mut_at::<T>(byte_offset).clone_from(value)
    }

    pub unsafe fn clone_slice_to<T: Clone>(&self, byte_offset: usize, values: &[T]) {
        self.slice_mut(byte_offset, values.len())
            .clone_from_slice(values)
    }

    pub unsafe fn copy_slice_to<T: Copy>(&self, byte_offset: usize, values: &[T]) {
        self.slice_mut(byte_offset, values.len())
            .copy_from_slice(values)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BufferMapMode {
    Unspecified,
    Read,
    Write,
    ReadWrite,
}
impl BufferMapMode {
    pub fn is_write(self) -> bool {
        self == Self::Write || self == Self::ReadWrite
    }

    pub fn is_read(self) -> bool {
        self == Self::Read || self == Self::ReadWrite
    }
}

pub struct Buffer {
    pub(crate) handle: br::vk::VkBuffer,
    pub(crate) device: peridot::VulkanGfx,
    pub(crate) memory_block: BackingMemory,
    pub(crate) requires_flushing: bool,
    pub(crate) offset: u64,
    pub(crate) size: usize,
    /// (size, offset)
    pub(crate) malloc: (u64, u64),
}
impl Buffer {
    pub const fn byte_length(&self) -> usize {
        self.size
    }

    /// trueの場合、メモリ上のコンテンツをデバイスと同期するために明示的なFlush(書き込み)/Invalidate(読み取り)が必要
    pub const fn requires_explicit_sync(&self) -> bool {
        self.requires_flushing
    }

    /// very unsafe operation: no guarantees for under resource operations
    pub unsafe fn map_raw(
        &mut self,
        range: core::ops::Range<br::DeviceSize>,
    ) -> br::Result<AnyPointer> {
        let p = match self.memory_block {
            BackingMemory::Managed(ref m) => unsafe {
                br::vkfn_wrapper::map_memory(
                    self.device.native_ptr(),
                    m.borrow_mut().handle,
                    self.offset + range.start,
                    range.end - range.start,
                    0,
                )
            },
            BackingMemory::Native(ref mut m) => {
                m.map_raw(range.start + self.offset..range.end + self.offset)
            }
            BackingMemory::NativeShared(ref m) => m
                .borrow_mut()
                .map_raw(range.start + self.offset..range.end + self.offset),
        }?;

        Ok(AnyPointer(core::ptr::NonNull::new_unchecked(p as _)))
    }

    /// very unsafe operation: no guarantees for under resource operations
    pub unsafe fn unmap_raw(&mut self) {
        match self.memory_block {
            BackingMemory::Managed(ref m) => {
                let locked = m.borrow_mut();

                unsafe {
                    br::vkfn_wrapper::unmap_memory(locked.device.native_ptr(), locked.handle);
                }
            }
            BackingMemory::Native(ref mut m) => m.unmap(),
            BackingMemory::NativeShared(ref m) => m.borrow_mut().unmap(),
        }
    }

    /// very unsafe operation: no guarantees for under resource operations
    pub unsafe fn invalidate_ranges_raw(
        &mut self,
        ranges: &[core::ops::Range<br::DeviceSize>],
    ) -> br::Result<()> {
        match self.memory_block {
            BackingMemory::Managed(ref m) => {
                let locked = m.borrow_mut();

                unsafe {
                    br::vkfn_wrapper::invalidate_mapped_memory_ranges(
                        locked.device.native_ptr(),
                        &ranges
                            .iter()
                            .map(|r| {
                                br::MappedMemoryRange::new_raw(
                                    locked.handle,
                                    r.start,
                                    r.end - r.start,
                                )
                            })
                            .collect::<Vec<_>>(),
                    )
                }
            }
            BackingMemory::Native(ref mut m) => m.device().invalidate_memory_range(
                &ranges
                    .iter()
                    .map(|r| br::MappedMemoryRange::new(m, r.clone()))
                    .collect::<Vec<_>>(),
            ),
            BackingMemory::NativeShared(ref m) => {
                let locked = m.borrow_mut();

                locked.device().invalidate_memory_range(
                    &ranges
                        .iter()
                        .map(|r| br::MappedMemoryRange::new(&locked, r.clone()))
                        .collect::<Vec<_>>(),
                )
            }
        }
    }

    /// very unsafe operation: no guarantees for under resource operations
    pub unsafe fn flush_ranges_raw(
        &mut self,
        ranges: &[core::ops::Range<br::DeviceSize>],
    ) -> br::Result<()> {
        match self.memory_block {
            BackingMemory::Managed(ref m) => {
                let locked = m.borrow_mut();

                unsafe {
                    br::vkfn_wrapper::flush_mapped_memory_ranges(
                        locked.device.native_ptr(),
                        &ranges
                            .iter()
                            .map(|r| {
                                br::MappedMemoryRange::new_raw(
                                    locked.handle,
                                    r.start,
                                    r.end - r.start,
                                )
                            })
                            .collect::<Vec<_>>(),
                    )
                }
            }
            BackingMemory::Native(ref mut m) => m.device().flush_mapped_memory_ranges(
                &ranges
                    .iter()
                    .map(|r| br::MappedMemoryRange::new(m, r.clone()))
                    .collect::<Vec<_>>(),
            ),
            BackingMemory::NativeShared(ref m) => {
                let locked = m.borrow_mut();

                locked.device().flush_mapped_memory_ranges(
                    &ranges
                        .iter()
                        .map(|r| br::MappedMemoryRange::new(&locked, r.clone()))
                        .collect::<Vec<_>>(),
                )
            }
        }
    }

    pub fn guard_map<R>(
        &mut self,
        mode: BufferMapMode,
        op: impl FnOnce(AnyPointer) -> R,
    ) -> br::Result<R> {
        match self.memory_block {
            BackingMemory::Managed(ref m) => {
                let locked = m.borrow_mut();

                let ptr = unsafe {
                    br::vkfn_wrapper::map_memory(
                        locked.device.native_ptr(),
                        locked.handle,
                        self.offset,
                        self.size as _,
                        0,
                    )?
                };
                if self.requires_explicit_sync() && mode.is_read() {
                    unsafe {
                        self.device().invalidate_memory_range(&[
                            br::MappedMemoryRange::new_raw(
                                locked.handle,
                                self.offset,
                                self.size as _,
                            ),
                        ])?;
                    }
                }
                let r = op(AnyPointer(unsafe {
                    core::ptr::NonNull::new_unchecked(ptr as _)
                }));
                if self.requires_explicit_sync() && mode.is_write() {
                    unsafe {
                        self.device().flush_mapped_memory_ranges(&[
                            br::MappedMemoryRange::new_raw(
                                locked.handle,
                                self.offset,
                                self.size as _,
                            ),
                        ])?;
                    }
                }
                unsafe {
                    br::vkfn_wrapper::unmap_memory(locked.device.native_ptr(), locked.handle);
                }

                Ok(r)
            }
            BackingMemory::Native(ref mut m) => {
                let ptr = unsafe {
                    m.map_raw(self.offset..self.offset + self.size as br::vk::VkDeviceSize)?
                };
                if self.requires_flushing && mode.is_read() {
                    unsafe {
                        m.device()
                            .invalidate_memory_range(&[br::MappedMemoryRange::new(
                                m,
                                self.offset..self.offset + self.size as br::DeviceSize,
                            )])?;
                    }
                }
                let r = op(AnyPointer(unsafe {
                    core::ptr::NonNull::new_unchecked(ptr as _)
                }));
                if self.requires_flushing && mode.is_write() {
                    unsafe {
                        m.device()
                            .flush_mapped_memory_ranges(&[br::MappedMemoryRange::new(
                                m,
                                self.offset..self.offset + self.size as br::DeviceSize,
                            )])?;
                    }
                }
                unsafe {
                    m.unmap();
                }

                Ok(r)
            }
            BackingMemory::NativeShared(ref m) => {
                let mut locked = m.borrow_mut();
                let ptr = unsafe {
                    locked.map_raw(self.offset..self.offset + self.size as br::vk::VkDeviceSize)?
                };
                if self.requires_explicit_sync() && mode.is_read() {
                    unsafe {
                        self.device()
                            .invalidate_memory_range(&[br::MappedMemoryRange::new(
                                &locked,
                                self.offset..self.offset + self.size as br::DeviceSize,
                            )])?;
                    }
                }
                let r = op(AnyPointer(unsafe {
                    core::ptr::NonNull::new_unchecked(ptr as _)
                }));
                if self.requires_explicit_sync() && mode.is_write() {
                    unsafe {
                        self.device()
                            .flush_mapped_memory_ranges(&[br::MappedMemoryRange::new(
                                &locked,
                                self.offset..self.offset + self.size as br::DeviceSize,
                            )])?;
                    }
                }
                unsafe {
                    locked.unmap();
                }

                Ok(r)
            }
        }
    }

    /// Writes value as buffer content. checked whether value size and buffer size are equal.
    pub fn write_content<T>(&mut self, value: T) -> br::Result<()> {
        assert_eq!(self.size, core::mem::size_of::<T>());

        unsafe { self.write_content_unchecked(value) }
    }

    pub unsafe fn write_content_unchecked<T>(&mut self, value: T) -> br::Result<()> {
        self.guard_map(BufferMapMode::Write, |ptr| {
            *ptr.get_mut_at(0) = value;
        })
    }

    pub fn clone_content_from_slice<T: Clone>(&mut self, values: &[T]) -> br::Result<()> {
        assert_eq!(self.size, core::mem::size_of::<T>() * values.len());

        self.guard_map(BufferMapMode::Write, |ptr| unsafe {
            ptr.clone_slice_to(0, values);
        })
    }

    pub fn copy_content_from_slice<T: Copy>(&mut self, values: &[T]) -> br::Result<()> {
        assert_eq!(self.size, core::mem::size_of::<T>() * values.len());

        self.guard_map(BufferMapMode::Write, |ptr| unsafe {
            ptr.copy_slice_to(0, values);
        })
    }
}
impl Drop for Buffer {
    fn drop(&mut self) {
        if let BackingMemory::Managed(ref b) = self.memory_block {
            b.borrow_mut().free(self.malloc.0 as _, self.malloc.1);
        }

        unsafe {
            br::vkfn_wrapper::destroy_buffer(self.device.native_ptr(), self.handle, None);
        }
    }
}
impl br::VkHandle for Buffer {
    type Handle = br::vk::VkBuffer;

    fn native_ptr(&self) -> Self::Handle {
        self.handle
    }
}
impl br::VkObject for Buffer {
    const TYPE: br::vk::VkObjectType = br::vk::VK_OBJECT_TYPE_BUFFER;
}
impl br::VkHandleMut for Buffer {
    fn native_ptr_mut(&mut self) -> Self::Handle {
        self.handle
    }
}
impl br::DeviceChildHandle for Buffer {
    #[inline(always)]
    fn device_handle(&self) -> bedrock::vk::VkDevice {
        self.device.native_ptr()
    }
}
impl br::DeviceChild for Buffer {
    type ConcreteDevice = peridot::VulkanGfx;

    fn device(&self) -> &Self::ConcreteDevice {
        &self.device
    }
}
impl br::Buffer for Buffer {}
impl peridot::TransferrableBufferResource for Buffer {
    fn grouping_key(&self) -> u64 {
        unsafe { core::mem::transmute(self.handle) }
    }

    fn raw_handle(&self) -> br::vk::VkBuffer {
        self.handle
    }
}

pub struct LinearImageBuffer {
    pub inner: Buffer,
    pub row_texels: u32,
    pub height: u32,
}
impl std::ops::Deref for LinearImageBuffer {
    type Target = Buffer;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
impl std::ops::DerefMut for LinearImageBuffer {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}
