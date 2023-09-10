use bedrock as br;
use br::{Device, DeviceChild, DeviceMemory, VkHandle, VulkanStructure};
#[allow(unused_imports)]
use peridot::mthelper::DynamicMutabilityProvider;
use peridot::mthelper::SharedMutableRef;

use crate::MemoryBlock;

pub(crate) enum BackingMemory {
    Managed(SharedMutableRef<MemoryBlock<peridot::DeviceObject>>),
    Native(br::DeviceMemoryObject<peridot::DeviceObject>),
    NativeShared(SharedMutableRef<br::DeviceMemoryObject<peridot::DeviceObject>>),
}

pub struct Image {
    pub(crate) object: br::ImageObject<peridot::DeviceObject>,
    pub(crate) memory_block: BackingMemory,
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
    }
}
impl br::VkHandle for Image {
    type Handle = <br::ImageObject<peridot::DeviceObject> as br::VkHandle>::Handle;

    fn native_ptr(&self) -> Self::Handle {
        self.object.native_ptr()
    }
}
impl br::VkHandleMut for Image {
    fn native_ptr_mut(&mut self) -> Self::Handle {
        self.object.native_ptr_mut()
    }
}
impl br::VkObject for Image {
    const TYPE: br::vk::VkObjectType =
        <br::ImageObject<peridot::DeviceObject> as br::VkObject>::TYPE;
}
impl br::DeviceChild for Image {
    type ConcreteDevice =
        <br::ImageObject<peridot::DeviceObject> as br::DeviceChild>::ConcreteDevice;

    fn device(&self) -> &Self::ConcreteDevice {
        self.object.device()
    }
}
impl br::Image for Image {
    fn format(&self) -> br::vk::VkFormat {
        self.object.format()
    }

    fn size(&self) -> &br::vk::VkExtent3D {
        self.object.size()
    }

    fn dimension(&self) -> br::vk::VkImageViewType {
        self.object.dimension()
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

    pub unsafe fn get_mut_at<T>(&self, byte_offset: usize) -> &mut T {
        (self.0.as_ptr().add(byte_offset) as *mut T)
            .as_mut()
            .unwrap_unchecked()
    }

    pub const unsafe fn slice<T>(&self, byte_offset: usize, len: usize) -> &[T] {
        core::slice::from_raw_parts(self.0.as_ptr().add(byte_offset) as _, len)
    }

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
    pub(crate) object: br::BufferObject<peridot::DeviceObject>,
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
        range: core::ops::Range<br::vk::VkDeviceSize>,
    ) -> br::Result<AnyPointer> {
        let p = match self.memory_block {
            BackingMemory::Managed(ref m) => m
                .borrow_mut()
                .object
                .map_raw(range.start + self.offset..range.end + self.offset),
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
            BackingMemory::Managed(ref m) => m.borrow_mut().object.unmap(),
            BackingMemory::Native(ref mut m) => m.unmap(),
            BackingMemory::NativeShared(ref m) => m.borrow_mut().unmap(),
        }
    }

    /// very unsafe operation: no guarantees for under resource operations
    pub unsafe fn invalidate_ranges_raw(
        &mut self,
        ranges: &[core::ops::Range<br::vk::VkDeviceSize>],
    ) -> br::Result<()> {
        match self.memory_block {
            BackingMemory::Managed(ref m) => {
                let locked = m.borrow_mut();

                locked.object.device().invalidate_memory_range(
                    &ranges
                        .iter()
                        .map(|r| br::vk::VkMappedMemoryRange {
                            sType: br::vk::VkMappedMemoryRange::TYPE,
                            pNext: core::ptr::null(),
                            memory: locked.object.native_ptr(),
                            offset: r.start,
                            size: r.end - r.start,
                        })
                        .collect::<Vec<_>>(),
                )
            }
            BackingMemory::Native(ref mut m) => m.device().invalidate_memory_range(
                &ranges
                    .iter()
                    .map(|r| br::vk::VkMappedMemoryRange {
                        sType: br::vk::VkMappedMemoryRange::TYPE,
                        pNext: core::ptr::null(),
                        memory: m.native_ptr(),
                        offset: r.start,
                        size: r.end - r.start,
                    })
                    .collect::<Vec<_>>(),
            ),
            BackingMemory::NativeShared(ref m) => {
                let locked = m.borrow_mut();

                locked.device().invalidate_memory_range(
                    &ranges
                        .iter()
                        .map(|r| br::vk::VkMappedMemoryRange {
                            sType: br::vk::VkMappedMemoryRange::TYPE,
                            pNext: core::ptr::null(),
                            memory: locked.native_ptr(),
                            offset: r.start,
                            size: r.end - r.start,
                        })
                        .collect::<Vec<_>>(),
                )
            }
        }
    }

    /// very unsafe operation: no guarantees for under resource operations
    pub unsafe fn flush_ranges_raw(
        &mut self,
        ranges: &[core::ops::Range<br::vk::VkDeviceSize>],
    ) -> br::Result<()> {
        match self.memory_block {
            BackingMemory::Managed(ref m) => {
                let locked = m.borrow_mut();

                locked.object.device().flush_mapped_memory_ranges(
                    &ranges
                        .iter()
                        .map(|r| br::vk::VkMappedMemoryRange {
                            sType: br::vk::VkMappedMemoryRange::TYPE,
                            pNext: core::ptr::null(),
                            memory: locked.object.native_ptr(),
                            offset: r.start,
                            size: r.end - r.start,
                        })
                        .collect::<Vec<_>>(),
                )
            }
            BackingMemory::Native(ref mut m) => m.device().flush_mapped_memory_ranges(
                &ranges
                    .iter()
                    .map(|r| br::vk::VkMappedMemoryRange {
                        sType: br::vk::VkMappedMemoryRange::TYPE,
                        pNext: core::ptr::null(),
                        memory: m.native_ptr(),
                        offset: r.start,
                        size: r.end - r.start,
                    })
                    .collect::<Vec<_>>(),
            ),
            BackingMemory::NativeShared(ref m) => {
                let locked = m.borrow_mut();

                locked.device().flush_mapped_memory_ranges(
                    &ranges
                        .iter()
                        .map(|r| br::vk::VkMappedMemoryRange {
                            sType: br::vk::VkMappedMemoryRange::TYPE,
                            pNext: core::ptr::null(),
                            memory: locked.native_ptr(),
                            offset: r.start,
                            size: r.end - r.start,
                        })
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
                let mut locked = m.borrow_mut();
                let ptr = unsafe {
                    locked
                        .object
                        .map_raw(self.offset..self.offset + self.size as br::vk::VkDeviceSize)?
                };
                if self.requires_explicit_sync() && mode.is_read() {
                    unsafe {
                        self.device()
                            .invalidate_memory_range(&[br::vk::VkMappedMemoryRange {
                                sType: br::vk::VkMappedMemoryRange::TYPE,
                                pNext: core::ptr::null(),
                                memory: locked.object.native_ptr(),
                                offset: self.offset,
                                size: self.size as _,
                            }])?;
                    }
                }
                let r = op(AnyPointer(unsafe {
                    core::ptr::NonNull::new_unchecked(ptr as _)
                }));
                if self.requires_explicit_sync() && mode.is_write() {
                    unsafe {
                        self.device().flush_mapped_memory_ranges(&[
                            br::vk::VkMappedMemoryRange {
                                sType: br::vk::VkMappedMemoryRange::TYPE,
                                pNext: core::ptr::null(),
                                memory: locked.object.native_ptr(),
                                offset: self.offset,
                                size: self.size as _,
                            },
                        ])?;
                    }
                }
                unsafe {
                    locked.object.unmap();
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
                            .invalidate_memory_range(&[br::vk::VkMappedMemoryRange {
                                sType: br::vk::VkMappedMemoryRange::TYPE,
                                pNext: core::ptr::null(),
                                memory: m.native_ptr(),
                                offset: self.offset,
                                size: self.size as _,
                            }])?;
                    }
                }
                let r = op(AnyPointer(unsafe {
                    core::ptr::NonNull::new_unchecked(ptr as _)
                }));
                if self.requires_flushing && mode.is_write() {
                    unsafe {
                        m.device()
                            .flush_mapped_memory_ranges(&[br::vk::VkMappedMemoryRange {
                                sType: br::vk::VkMappedMemoryRange::TYPE,
                                pNext: core::ptr::null(),
                                memory: m.native_ptr(),
                                offset: self.offset,
                                size: self.size as _,
                            }])?;
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
                            .invalidate_memory_range(&[br::vk::VkMappedMemoryRange {
                                sType: br::vk::VkMappedMemoryRange::TYPE,
                                pNext: core::ptr::null(),
                                memory: locked.native_ptr(),
                                offset: self.offset,
                                size: self.size as _,
                            }])?;
                    }
                }
                let r = op(AnyPointer(unsafe {
                    core::ptr::NonNull::new_unchecked(ptr as _)
                }));
                if self.requires_explicit_sync() && mode.is_write() {
                    unsafe {
                        self.device().flush_mapped_memory_ranges(&[
                            br::vk::VkMappedMemoryRange {
                                sType: br::vk::VkMappedMemoryRange::TYPE,
                                pNext: core::ptr::null(),
                                memory: locked.native_ptr(),
                                offset: self.offset,
                                size: self.size as _,
                            },
                        ])?;
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
    }
}
impl br::VkHandle for Buffer {
    type Handle = <br::BufferObject<peridot::DeviceObject> as br::VkHandle>::Handle;

    fn native_ptr(&self) -> Self::Handle {
        self.object.native_ptr()
    }
}
impl br::VkObject for Buffer {
    const TYPE: br::vk::VkObjectType =
        <br::BufferObject<peridot::DeviceObject> as br::VkObject>::TYPE;
}
impl br::VkHandleMut for Buffer {
    fn native_ptr_mut(&mut self) -> Self::Handle {
        self.object.native_ptr_mut()
    }
}
impl br::DeviceChild for Buffer {
    type ConcreteDevice =
        <br::BufferObject<peridot::DeviceObject> as br::DeviceChild>::ConcreteDevice;

    fn device(&self) -> &Self::ConcreteDevice {
        self.object.device()
    }
}
impl br::Buffer for Buffer {}
impl peridot::TransferrableBufferResource for Buffer {
    fn grouping_key(&self) -> u64 {
        unsafe { core::mem::transmute(self.object.native_ptr()) }
    }

    fn raw_handle(&self) -> br::vk::VkBuffer {
        self.object.native_ptr()
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
