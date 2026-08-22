use std::{
    cell::UnsafeCell,
    collections::{HashMap, HashSet},
    num::NonZero,
    rc::Rc,
};

use bedrock::{
    self as br, CommandBufferMut, DescriptorPoolMut, Device, DeviceMemoryMut, ImageChild,
    MemoryBound, VkHandle,
};
use peridot_math::{Camera, Matrix4, Matrix4F32, One, Vector3};

use crate::{
    graphics::{
        BLEND_STATE_SINGLE_NONE, BLEND_STATE_SINGLE_PREMULTIPLIED, Graphics, IA_STATE_TRILIST,
        IA_STATE_TRISTRIP, MS_STATE_EMPTY, RASTER_STATE_DEFAULT_FILL_NOCULL, VI_STATE_EMPTY,
    },
    rendering::{
        composite::CustomRenderContext,
        preview::handle::{
            HandleVertex, ROTATION_HANDLE_AXES_DRAW_ICOUNT, ROTATION_HANDLE_ICOUNT,
            ROTATION_HANDLE_VCOUNT, SCALE_HANDLE_ICOUNT, SCALE_HANDLE_VCOUNT,
            TRANSLATE_HANDLE_ICOUNT, TRANSLATE_HANDLE_VCOUNT, gen_rotation_handle_mesh,
            gen_scale_handle_mesh, gen_translate_handle_mesh,
        },
    },
    utils::{
        LogicalUnit, PixelsUnit, SafeF32, Size, find_lowest_bit_pos_from_u16, lowest_bit_pos_u16,
        most_top_bit_pos_u64, range_from_len_u64, rup2, rup2_u64,
    },
};

pub mod handle;

pub struct PreviewRenderTargetBuffer {
    memory: br::vk::VkDeviceMemory,
    image: br::vk::VkImage,
    image_view: br::vk::VkImageView,
    depth_image: br::vk::VkImage,
    depth_view: br::vk::VkImageView,
    size: br::Extent2D,
}
impl PreviewRenderTargetBuffer {
    pub unsafe fn drop(self, device: &Graphics) {
        drop(unsafe {
            br::ImageViewObject::manage(
                self.depth_view,
                br::ImageObject::manage(
                    self.depth_image,
                    device,
                    // dropでは使わない情報なので適当に埋める
                    br::vk::VK_IMAGE_TYPE_2D,
                    br::vk::VK_FORMAT_UNDEFINED,
                    br::Extent3D::spread1(1),
                ),
            )
        });
        drop(unsafe {
            br::ImageViewObject::manage(
                self.image_view,
                br::ImageObject::manage(
                    self.image,
                    device,
                    // dropでは使わない情報なので適当に埋める
                    br::vk::VK_IMAGE_TYPE_2D,
                    br::vk::VK_FORMAT_UNDEFINED,
                    br::Extent3D::spread1(1),
                ),
            )
        });
        drop(unsafe { br::DeviceMemoryObject::manage(self.memory, device) });
    }

    // TODO: おそらく本当はDeviceCaps見て選定したほうがいい
    pub const COLOR_FORMAT: br::Format = br::vk::VK_FORMAT_R8G8B8A8_UNORM;
    pub const DEPTH_FORMAT: br::Format = br::vk::VK_FORMAT_D24_UNORM_S8_UINT;

    pub fn new(device: &Graphics, init_size: br::Extent2D) -> Self {
        let mut image = br::ImageObject::new(
            device,
            &br::ImageCreateInfo::new(init_size, Self::COLOR_FORMAT)
                .set_usage(br::ImageUsageFlags::SAMPLED | br::ImageUsageFlags::COLOR_ATTACHMENT),
        )
        .expect("preview_rt.image.create");
        let mut depth_image = br::ImageObject::new(
            device,
            &br::ImageCreateInfo::new(init_size, Self::DEPTH_FORMAT)
                .set_usage(br::ImageUsageFlags::DEPTH_STENCIL_ATTACHMENT),
        )
        .expect("preview_rt.depth_image.create");

        let memreq = image.requirements();
        let depth_memreq = depth_image.requirements();
        // できるだけAlignmentによるPaddingが少なくなるように配置する
        let (image_offset, depth_offset, memory_size);
        if memreq.alignment < depth_memreq.alignment {
            depth_offset = 0;
            image_offset = rup2_u64(depth_memreq.size, memreq.alignment);
            memory_size = image_offset + memreq.size;
        } else {
            image_offset = 0;
            depth_offset = rup2_u64(memreq.size, depth_memreq.alignment);
            memory_size = depth_offset + depth_memreq.size;
        }
        let memory = br::DeviceMemoryObject::new(
            device,
            &br::MemoryAllocateInfo::new(
                memory_size,
                device
                    .find_device_local_memory_index(
                        memreq.memoryTypeBits & depth_memreq.memoryTypeBits,
                    )
                    .expect("preview_rt.memory.index"),
            ),
        )
        .expect("preview_rt.memory.alloc");
        image
            .bind(&memory, image_offset)
            .expect("preview_rt.image.bind");
        depth_image
            .bind(&memory, depth_offset)
            .expect("preview_rt.depth_image.bind");

        let image_view = br::ImageViewBuilder::new(
            image,
            br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
        )
        .create()
        .expect("preview_rt.image_view.create");
        let depth_view = br::ImageViewBuilder::new(
            depth_image,
            br::ImageSubresourceRange::new(
                br::AspectMask::DEPTH | br::AspectMask::STENCIL,
                0..1,
                0..1,
            ),
        )
        .create()
        .expect("preview_rt.depth_view.create");

        device.dbg_set_name(&memory, c"Preview.RenderTarget.BackingMemory");
        device.dbg_set_name(image_view.image(), c"Preview.RenderTarget.ColorBuffer");
        device.dbg_set_name(&image_view, c"Preview.RenderTarget.ColorBuffer.View");
        device.dbg_set_name(depth_view.image(), c"Preview.RenderTarget.DepthBuffer");
        device.dbg_set_name(&depth_view, c"Preview.RenderTarget.DepthBuffer.View");

        let (image_view, image) = image_view.unmanage();
        let (image, _, _, _, _) = image.unmanage();
        let (depth_view, depth_image) = depth_view.unmanage();
        let (depth_image, _, _, _, _) = depth_image.unmanage();
        let (memory, _) = memory.unmanage();
        Self {
            memory,
            image,
            image_view,
            depth_image,
            depth_view,
            size: init_size,
        }
    }

    pub fn validate(&mut self, device: &Graphics, active_size: br::Extent2D) -> bool {
        let mut resource_recreated = false;
        if self.size != active_size {
            drop(unsafe {
                br::ImageViewObject::manage(
                    self.depth_view,
                    br::ImageObject::manage(
                        self.depth_image,
                        device,
                        // dropでは使わない情報なので適当に埋める
                        br::vk::VK_IMAGE_TYPE_2D,
                        br::vk::VK_FORMAT_UNDEFINED,
                        br::Extent3D::spread1(1),
                    ),
                )
            });
            drop(unsafe {
                br::ImageViewObject::manage(
                    self.image_view,
                    br::ImageObject::manage(
                        self.image,
                        device,
                        // dropでは使わない情報なので適当に埋める
                        br::vk::VK_IMAGE_TYPE_2D,
                        br::vk::VK_FORMAT_UNDEFINED,
                        br::Extent3D::spread1(1),
                    ),
                )
            });
            drop(unsafe { br::DeviceMemoryObject::manage(self.memory, device) });

            let mut image = br::ImageObject::new(
                device,
                &br::ImageCreateInfo::new(active_size, Self::COLOR_FORMAT).set_usage(
                    br::ImageUsageFlags::SAMPLED | br::ImageUsageFlags::COLOR_ATTACHMENT,
                ),
            )
            .expect("preview_rt.validate.image.create");
            let mut depth_image = br::ImageObject::new(
                device,
                &br::ImageCreateInfo::new(active_size, Self::DEPTH_FORMAT)
                    .set_usage(br::ImageUsageFlags::DEPTH_STENCIL_ATTACHMENT),
            )
            .expect("preview_rt.validate.depth_image.create");

            let memreq = image.requirements();
            let depth_memreq = depth_image.requirements();
            // できるだけAlignmentによるPaddingが少なくなるように配置する
            let (image_offset, depth_offset, memory_size);
            if memreq.alignment < depth_memreq.alignment {
                depth_offset = 0;
                image_offset = rup2_u64(depth_memreq.size, memreq.alignment);
                memory_size = image_offset + memreq.size;
            } else {
                image_offset = 0;
                depth_offset = rup2_u64(memreq.size, depth_memreq.alignment);
                memory_size = depth_offset + depth_memreq.size;
            }
            let memory = br::DeviceMemoryObject::new(
                device,
                &br::MemoryAllocateInfo::new(
                    memory_size,
                    device
                        .find_device_local_memory_index(
                            memreq.memoryTypeBits & depth_memreq.memoryTypeBits,
                        )
                        .expect("preview_rt.memory.index"),
                ),
            )
            .expect("preview_rt.validate.memory.alloc");
            image
                .bind(&memory, image_offset)
                .expect("preview_rt.validate.image.bind");
            depth_image
                .bind(&memory, depth_offset)
                .expect("preview_rt.validate.depth_image.bind");

            let image_view = br::ImageViewBuilder::new(
                image,
                br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
            )
            .create()
            .expect("preview_rt.validate.image_view.create");
            let depth_view = br::ImageViewBuilder::new(
                depth_image,
                br::ImageSubresourceRange::new(
                    br::AspectMask::DEPTH | br::AspectMask::STENCIL,
                    0..1,
                    0..1,
                ),
            )
            .create()
            .expect("preview_rt.validate.depth_view.create");

            device.dbg_set_name(&memory, c"Preview.RenderTarget.BackingMemory");
            device.dbg_set_name(image_view.image(), c"Preview.RenderTarget.ColorBuffer");
            device.dbg_set_name(&image_view, c"Preview.RenderTarget.ColorBuffer.View");
            device.dbg_set_name(depth_view.image(), c"Preview.RenderTarget.DepthBuffer");
            device.dbg_set_name(&depth_view, c"Preview.RenderTarget.DepthBuffer.View");

            let (image_view, image) = image_view.unmanage();
            let (image, _, _, _, _) = image.unmanage();
            let (depth_view, depth_image) = depth_view.unmanage();
            let (depth_image, _, _, _, _) = depth_image.unmanage();
            let (memory, _) = memory.unmanage();
            self.image_view = image_view;
            self.image = image;
            self.depth_view = depth_view;
            self.depth_image = depth_image;
            self.memory = memory;
            resource_recreated = true;
        }

        self.size = active_size;
        resource_recreated
    }

    pub const fn as_image_view<'a>(&'a self) -> &'a br::VkHandleRef<'a, br::vk::VkImageView> {
        br::VkHandleRef::from_raw_ref(&self.image_view)
    }

    pub const fn image_view_tref<'a>(&'a self) -> br::VkHandleRef<'a, br::vk::VkImageView> {
        unsafe { br::VkHandleRef::dangling(self.image_view) }
    }

    pub const fn depth_view_tref<'a>(&'a self) -> br::VkHandleRef<'a, br::vk::VkImageView> {
        unsafe { br::VkHandleRef::dangling(self.depth_view) }
    }

    pub const fn aspect_wh(&self) -> f32 {
        self.size.width as f32 / self.size.height as f32
    }
}

// std140 layout
#[repr(C)]
pub struct PreviewStreamingBufferContent {
    pub current_sec: f32,
}

pub struct OriginAxesVertex {
    dir: [f32; 4],
    offset: [f32; 4],
}

// std140 layout
#[repr(C)]
pub struct CameraData {
    world_to_clip_space: Matrix4F32,
    world_to_camera_space: Matrix4F32,
    camera_to_clip_space: Matrix4F32,
    camera_pos: [f32; 4],
}
impl CameraData {
    fn new(camera: &peridot_math::Camera, aspect_wh: f32) -> Self {
        Self {
            world_to_clip_space: camera.view_projection_matrix(aspect_wh).transpose(),
            world_to_camera_space: camera.view_matrix().transpose(),
            camera_to_clip_space: camera.projection_matrix(aspect_wh).transpose(),
            camera_pos: [camera.position.0, camera.position.1, camera.position.2, 1.0],
        }
    }
}

// std430 layout
#[repr(C)]
pub struct GridPushConstantData {
    dir: [f32; 4],
    start: [f32; 4],
    altdir: [f32; 4],
    scale: f32,
}

const VS_ORIGIN_AXES: &[OriginAxesVertex] = &[
    OriginAxesVertex {
        dir: [1.0, 0.0, 0.0, 1.0],
        offset: [1000.0, 0.0, 0.0, 0.0],
    },
    OriginAxesVertex {
        dir: [1.0, 0.0, 0.0, 1.0],
        offset: [-1000.0, 0.0, 0.0, 0.0],
    },
    OriginAxesVertex {
        dir: [0.0, 1.0, 0.0, 1.0],
        offset: [0.0, 1000.0, 0.0, 0.0],
    },
    OriginAxesVertex {
        dir: [0.0, 1.0, 0.0, 1.0],
        offset: [0.0, -1000.0, 0.0, 0.0],
    },
    OriginAxesVertex {
        dir: [0.0, 0.0, 1.0, 1.0],
        offset: [0.0, 0.0, 1000.0, 0.0],
    },
    OriginAxesVertex {
        dir: [0.0, 0.0, 1.0, 1.0],
        offset: [0.0, 0.0, -1000.0, 0.0],
    },
];

struct ScratchStagingBuffer {
    buffer: br::vk::VkBuffer,
    memory: br::vk::VkDeviceMemory,
    should_flush: bool,
    mapped_ptr: *mut core::ffi::c_void,
    unused_top: usize,
}
impl ScratchStagingBuffer {
    unsafe fn drop(self, device: &Graphics) {
        unsafe {
            br::vkfn_wrapper::unmap_memory(
                device.as_transparent_ref(),
                br::VkHandleRefMut::dangling(self.memory),
            );
        }

        drop(unsafe { br::BufferObject::manage(self.buffer, device) });
        drop(unsafe { br::DeviceMemoryObject::manage(self.memory, device) });
    }

    const INIT_SIZE: br::DeviceSize = 1024 * 1024;

    fn new(device: &Graphics) -> Self {
        let mut buffer = br::BufferObject::new(
            device,
            &br::BufferCreateInfo::new(Self::INIT_SIZE, br::BufferUsage::TRANSFER_SRC),
        )
        .expect("preview_scratch_staging.buffer.create");
        device.dbg_set_name(&buffer, c"Preview.ScratchStaging.Buffer");
        let memreq = buffer.requirements();
        let memindex = device
            .find_host_visible_memory_index(memreq.memoryTypeBits)
            .expect("preview_scratch_staging.memory.index");
        let should_flush = !device.is_coherent_memory(memindex);
        let memory = br::DeviceMemoryObject::new(
            device,
            &br::MemoryAllocateInfo::new(memreq.size, memindex),
        )
        .expect("preview_scratch_staging.memory.alloc");
        device.dbg_set_name(&memory, c"Preview.ScratchStaging.Memory");
        buffer
            .bind(&memory, 0)
            .expect("preview_scratch_staging.buffer.bind");

        device.dbg_set_name(&memory, c"Preview.ScratchStagingBuffer.BackingMemory");
        device.dbg_set_name(&buffer, c"Preview.ScratchStagingBuffer");

        let (buffer, _) = buffer.unmanage();
        let (memory, _) = memory.unmanage();
        let mapped_ptr = unsafe {
            br::vkfn_wrapper::map_memory(
                device.as_transparent_ref(),
                br::VkHandleRefMut::dangling(memory),
                0..Self::INIT_SIZE,
                0,
            )
            .expect("preview_scratch_staging.map")
        };
        Self {
            buffer,
            memory,
            should_flush,
            mapped_ptr,
            unused_top: 0,
        }
    }

    fn reset(&mut self) {
        self.unused_top = 0;
    }

    fn reserve(&mut self, size: usize) -> usize {
        let r = self.unused_top;
        self.unused_top += size;
        if self.unused_top >= Self::INIT_SIZE as usize {
            todo!("resizing scratch staging buffer");
        }

        r
    }

    fn ops_before_copy(&self, device: &Graphics) {
        if self.should_flush {
            unsafe {
                device
                    .flush_mapped_memory_ranges(&[br::MappedMemoryRange::new_raw(
                        self.memory,
                        0,
                        Self::INIT_SIZE,
                    )])
                    .expect("preview.scratch_staging.flush");
            }
        }
    }
}

#[derive(Debug)]
enum DynamicBufferBlockState {
    Free {
        size: NonZero<br::DeviceSize>,
        prev_block: br::DeviceSize,
        prev_free_block: Option<br::DeviceSize>,
        next_free_block: Option<br::DeviceSize>,
    },
    Used {
        size: NonZero<br::DeviceSize>,
        prev_block: br::DeviceSize,
    },
}

/// 最小アロケーション単位
const DB_TLSF_ALLOC_GRANULARITY: br::DeviceSize = 64; // float4x4
/// 最小アロケーション単位のビット位置
const DB_TLSF_ALLOC_GRANULARITY_BITS: u32 = 6;
/// ページサイズ（一括でDeviceMemory/Bufferとして確保するサイズ）
const DB_TLSF_PAGE_SIZE: br::DeviceSize = 1 * 1024 * 1024;
/// ページサイズのビット位置
const DB_TLSF_PAGE_SIZE_BIT: u32 = 10 + 10;
/// Second Levelの分割数のビット位置
const DB_TLSF_LV2_SUBDIV_BITS: u32 = 4;
/// Second Levelを抽出するためのビットマスク（大きさにからSecond Levelを抽出するのに使う）
const DB_TLSF_LV2_MASK: br::DeviceSize = (1 << DB_TLSF_LV2_SUBDIV_BITS) - 1;
/// First Levelの数
const DB_TLSF_FL_COUNT: usize = (DB_TLSF_PAGE_SIZE_BIT
    - DB_TLSF_ALLOC_GRANULARITY_BITS
    - DB_TLSF_LV2_SUBDIV_BITS
    + 2/* idx0(2^0 based) + idxlast(entire size of page) */)
    as usize;
/// First Level 1段階におけるSecond Levelの数
const DB_TLSF_SL_PER_FL: usize = 1 << DB_TLSF_LV2_SUBDIV_BITS;
/// First Level 0におくサイズの最大値
const DB_TLSF_FL0_MAX_SIZE: br::DeviceSize =
    1 << (DB_TLSF_ALLOC_GRANULARITY_BITS + DB_TLSF_LV2_SUBDIV_BITS);
struct DynamicBufferPage {
    /// 1で空きあり
    first_level_freemap: u16,
    /// 1で空きあり
    second_level_freemap: [u16; DB_TLSF_FL_COUNT],
    block_list_headings: [br::DeviceSize; DB_TLSF_FL_COUNT * DB_TLSF_SL_PER_FL],
    block_states: HashMap<br::DeviceSize, DynamicBufferBlockState>,
    device_memory: br::vk::VkDeviceMemory,
    buffer: br::vk::VkBuffer,
}
impl DynamicBufferPage {
    unsafe fn drop(self, device: &Graphics) {
        drop(unsafe { br::DeviceMemoryObject::manage(self.device_memory, device) });
        drop(unsafe { br::BufferObject::manage(self.buffer, device) });
    }

    fn new(
        device: &Graphics,
        usage: br::BufferUsage,
        dbg_name: &'static str,
        dbg_identifier: usize,
    ) -> Self {
        let mut buffer =
            br::BufferObject::new(device, &br::BufferCreateInfo::new(DB_TLSF_PAGE_SIZE, usage))
                .expect("buffer.create");
        let memreq = buffer.requirements();
        let mem = br::DeviceMemoryObject::new(
            device,
            &br::MemoryAllocateInfo::new(
                memreq.size,
                device
                    .find_device_local_memory_index(memreq.memoryTypeBits)
                    .expect("device_memory.index"),
            ),
        )
        .expect("device_memory.alloc");
        buffer.bind(&mem, 0).expect("buffer.bind");

        device.dbg_set_name(
            &buffer,
            &std::ffi::CString::new(format!(
                "DynamicBuffer[{dbg_name}].Page#{dbg_identifier}.Buffer"
            ))
            .unwrap(),
        );
        device.dbg_set_name(
            &mem,
            &std::ffi::CString::new(format!(
                "DynamicBuffer[{dbg_name}].Page#{dbg_identifier}.Memory"
            ))
            .unwrap(),
        );

        let mut block_states = HashMap::new();
        block_states.insert(
            0,
            DynamicBufferBlockState::Free {
                size: unsafe { NonZero::new_unchecked(DB_TLSF_PAGE_SIZE) },
                prev_block: 0, // self
                next_free_block: None,
                prev_free_block: None,
            },
        );
        let (first_f, first_s) = Self::mapping(DB_TLSF_PAGE_SIZE);
        let mut second_level_freemap = [0; DB_TLSF_FL_COUNT];
        second_level_freemap[first_f as usize] |= 1 << first_s;

        let (mem, _) = mem.unmanage();
        let (buffer, _) = buffer.unmanage();
        Self {
            first_level_freemap: 1 << first_f,
            second_level_freemap,
            block_list_headings: [0; _],
            block_states,
            buffer,
            device_memory: mem,
        }
    }

    /// maps size to (first level index, second level index)
    const fn mapping(size: br::DeviceSize) -> (u32, u32) {
        if size < DB_TLSF_FL0_MAX_SIZE {
            // force level0(2^0..2^(DB_TLSF_ALLOC_GRANULARITY_BIT + DB_TLSF_LV2_SUBDIV_BITS9))
            return (
                0,
                ((size >> DB_TLSF_ALLOC_GRANULARITY_BITS) & DB_TLSF_LV2_MASK) as _,
            );
        }

        let f =
            most_top_bit_pos_u64(size) - DB_TLSF_ALLOC_GRANULARITY_BITS - DB_TLSF_LV2_SUBDIV_BITS
                + 1;
        assert!(f >= 1);
        (
            f,
            ((size >> (f - 1 + DB_TLSF_ALLOC_GRANULARITY_BITS)) & DB_TLSF_LV2_MASK) as _,
        )
    }

    const fn block_list_index(f: u32, s: u32) -> usize {
        f as usize * DB_TLSF_SL_PER_FL + s as usize
    }

    const fn sl_is_fully_occupied(&self, fl: u32) -> bool {
        self.second_level_freemap[fl as usize] == 0
    }
    const fn has_free_block(&self, fl: u32, sl: u32) -> bool {
        (self.second_level_freemap[fl as usize] & (1 << sl)) != 0
    }
    fn mark_free(&mut self, fl: u32, sl: u32) {
        self.second_level_freemap[fl as usize] |= 1 << sl;
        self.first_level_freemap |= 1 << fl;
    }
    fn mark_no_free(&mut self, fl: u32, sl: u32) {
        self.second_level_freemap[fl as usize] &= !(1 << sl);
        if self.sl_is_fully_occupied(fl) {
            // also first level has no free
            self.first_level_freemap &= !(1 << fl);
        }
    }

    fn find_free_at_least(&self, least_f: u32, least_s: u32) -> Option<(u32, u32)> {
        if let Some(usable_bit) =
            find_lowest_bit_pos_from_u16(self.second_level_freemap[least_f as usize], least_s as _)
        {
            // available in this first level
            return Some((least_f, usable_bit as _));
        }

        // use more upper level
        let Some(usable_bit) =
            find_lowest_bit_pos_from_u16(self.first_level_freemap, least_f as u16 + 1)
        else {
            tracing::warn!("no usable block");
            return None;
        };

        let actual_f = usable_bit as _;
        assert!(
            !self.sl_is_fully_occupied(actual_f),
            "selected first-level could not be used?"
        );
        Some((
            actual_f,
            lowest_bit_pos_u16(self.second_level_freemap[actual_f as usize]) as _,
        ))
    }

    #[tracing::instrument(name = "DynamicBufferPage::try_alloc", skip(self), ret(level = tracing::Level::TRACE))]
    fn try_alloc(&mut self, size: br::DeviceSize) -> Option<br::DeviceSize> {
        let size = rup2_u64(size, DB_TLSF_ALLOC_GRANULARITY);
        assert!(0 < size && size <= DB_TLSF_PAGE_SIZE, "size={size}");

        let (f, s) = Self::mapping(size);
        tracing::debug!(f, s, "tlsf level");
        let (f, s) = self.find_free_at_least(f, s)?;
        tracing::debug!(f, s, "free found");

        let head = self.block_list_headings[Self::block_list_index(f, s)];
        let Some(DynamicBufferBlockState::Free {
            size: block_size,
            prev_block,
            prev_free_block,
            next_free_block,
        }) = self.block_states.remove(&head)
        else {
            unreachable!();
        };
        // this should be the first
        assert!(prev_free_block.is_none());

        self.block_states.insert(
            head,
            DynamicBufferBlockState::Used {
                size: unsafe { NonZero::new_unchecked(size) },
                prev_block,
            },
        );
        if let Some(next) = next_free_block {
            // move head ptr to next
            self.block_list_headings[Self::block_list_index(f, s)] = next;
            let Some(&mut DynamicBufferBlockState::Free {
                prev_free_block: ref mut next_prev_free_block,
                ..
            }) = self.block_states.get_mut(&next)
            else {
                unreachable!();
            };
            *next_prev_free_block = None;
        } else {
            // no free block for this size class
            self.mark_no_free(f, s);
        }

        let left_block_size = block_size.get() - size;
        if left_block_size > 0 {
            // subdiv needed
            let left_block_size = unsafe { NonZero::new_unchecked(left_block_size) };
            let left_block_head = head + size;
            let (left_f, left_s) = Self::mapping(left_block_size.get());
            if !self.has_free_block(left_f, left_s) {
                // this is first free block
                self.block_list_headings[Self::block_list_index(left_f, left_s)] = left_block_head;
                self.block_states.insert(
                    left_block_head,
                    DynamicBufferBlockState::Free {
                        size: left_block_size,
                        prev_block: head,
                        prev_free_block: None,
                        next_free_block: None,
                    },
                );
            } else {
                // connect to head of free list
                let old_free_head = core::mem::replace(
                    &mut self.block_list_headings[Self::block_list_index(left_f, left_s)],
                    left_block_head,
                );
                if old_free_head == head {
                    // sipmle replacement
                    self.block_states.insert(
                        left_block_head,
                        DynamicBufferBlockState::Free {
                            size: left_block_size,
                            prev_block: head,
                            prev_free_block: None,
                            next_free_block,
                        },
                    );
                } else {
                    // chaining needed
                    self.block_states.insert(
                        left_block_head,
                        DynamicBufferBlockState::Free {
                            size: left_block_size,
                            prev_block: head,
                            prev_free_block: None,
                            next_free_block: Some(old_free_head),
                        },
                    );
                    let Some(&mut DynamicBufferBlockState::Free {
                        prev_free_block: ref mut old_free_prev_free_block,
                        ..
                    }) = self.block_states.get_mut(&old_free_head)
                    else {
                        unreachable!();
                    };
                    assert!(old_free_prev_free_block.is_none());
                    *old_free_prev_free_block = Some(left_block_head);
                }
            }

            self.mark_free(left_f, left_s);
        }

        Some(head)
    }

    #[tracing::instrument("DynamicBufferPage::free", skip(self))]
    pub fn free(&mut self, offset: br::DeviceSize) {
        let DynamicBufferBlockState::Used { size, prev_block } = self
            .block_states
            .remove(&offset)
            .expect("not a block boundary")
        else {
            unreachable!("freeing non-used offset");
        };

        // try join with adjacent blocks
        let mut new_free_block_prev_block = prev_block;
        let mut new_free_block_offset = offset;
        let mut new_free_block_size = size;
        if prev_block != new_free_block_offset
            && let DynamicBufferBlockState::Free {
                size: prev_size,
                prev_block: prev_prev_block,
                prev_free_block: prev_prev_free_block,
                next_free_block: prev_next_free_block,
            } = self.block_states[&prev_block]
        {
            // join with prev block

            // unlink prev block from freelist first
            match (prev_prev_free_block, prev_next_free_block) {
                (None, None) => {
                    // only one block in the free list
                    let (f, s) = Self::mapping(prev_size.get());
                    self.mark_no_free(f, s);
                }
                (None, Some(next_free)) => {
                    // head of the free list
                    let DynamicBufferBlockState::Free {
                        prev_free_block: next_prev_free_block,
                        ..
                    } = self.block_states.get_mut(&next_free).expect("no block?")
                    else {
                        unreachable!("corrupted chain");
                    };
                    *next_prev_free_block = None;

                    let (f, s) = Self::mapping(prev_size.get());
                    self.block_list_headings[Self::block_list_index(f, s)] = next_free;
                }
                (Some(prev_free), None) => {
                    // tail of the free list
                    let DynamicBufferBlockState::Free {
                        next_free_block: prev_next_free_block,
                        ..
                    } = self.block_states.get_mut(&prev_free).expect("no block?")
                    else {
                        unreachable!("corrupted chain");
                    };
                    *prev_next_free_block = None;
                }
                (Some(prev_free), Some(next_free)) => {
                    // middle of the free list
                    let DynamicBufferBlockState::Free {
                        next_free_block: prev_next_free_block,
                        ..
                    } = self.block_states.get_mut(&prev_free).expect("no block?")
                    else {
                        unreachable!("corrupted chain");
                    };
                    *prev_next_free_block = Some(next_free);

                    let DynamicBufferBlockState::Free {
                        prev_free_block: next_prev_free_block,
                        ..
                    } = self.block_states.get_mut(&next_free).expect("no block?")
                    else {
                        unreachable!("corrupted chain");
                    };
                    *next_prev_free_block = Some(prev_free);
                }
            }

            self.block_states.remove(&prev_block);
            new_free_block_prev_block = prev_prev_block;
            new_free_block_offset = prev_block;
            new_free_block_size = new_free_block_size
                .checked_add(prev_size.get())
                .expect("too long memory block");
        }
        if let DynamicBufferBlockState::Free {
            size: next_size,
            prev_free_block: next_prev_free_block,
            next_free_block: next_next_free_block,
            ..
        } = self.block_states[&(new_free_block_offset + new_free_block_size.get())]
        {
            // join with next block
            match (next_prev_free_block, next_next_free_block) {
                (None, None) => {
                    // only one block in the free list
                    let (f, s) = Self::mapping(next_size.get());
                    self.mark_no_free(f, s);
                }
                (None, Some(next_free)) => {
                    // head of the free list
                    let DynamicBufferBlockState::Free {
                        prev_free_block: next_prev_free_block,
                        ..
                    } = self.block_states.get_mut(&next_free).expect("no block?")
                    else {
                        unreachable!("corrupted chain");
                    };
                    *next_prev_free_block = None;

                    let (f, s) = Self::mapping(next_size.get());
                    self.block_list_headings[Self::block_list_index(f, s)] = next_free;
                }
                (Some(prev_free), None) => {
                    // tail of the free list
                    let DynamicBufferBlockState::Free {
                        next_free_block: prev_next_free_block,
                        ..
                    } = self.block_states.get_mut(&prev_free).expect("no block?")
                    else {
                        unreachable!("corrupted chain");
                    };
                    *prev_next_free_block = None;
                }
                (Some(prev_free), Some(next_free)) => {
                    // middle of the free list
                    let DynamicBufferBlockState::Free {
                        next_free_block: prev_next_free_block,
                        ..
                    } = self.block_states.get_mut(&prev_free).expect("no block?")
                    else {
                        unreachable!("corrupted chain");
                    };
                    *prev_next_free_block = Some(next_free);

                    let DynamicBufferBlockState::Free {
                        prev_free_block: next_prev_free_block,
                        ..
                    } = self.block_states.get_mut(&next_free).expect("no block?")
                    else {
                        unreachable!("corrupted chain");
                    };
                    *next_prev_free_block = Some(prev_free);
                }
            }

            self.block_states
                .remove(&(new_free_block_offset + new_free_block_size.get()));
            new_free_block_size = new_free_block_size
                .checked_add(next_size.get())
                .expect("too long memory block");
        }

        // create free block and join
        let (f, s) = Self::mapping(new_free_block_size.get());
        if self.has_free_block(f, s) {
            // chain to existing free list
            let next_free = core::mem::replace(
                &mut self.block_list_headings[Self::block_list_index(f, s)],
                new_free_block_offset,
            );
            if next_free == new_free_block_offset {
                // simple extending
                let DynamicBufferBlockState::Free { size, .. } = self
                    .block_states
                    .get_mut(&new_free_block_offset)
                    .expect("no block boundary")
                else {
                    unreachable!();
                };

                *size = new_free_block_size;
            } else {
                self.block_states.insert(
                    new_free_block_offset,
                    DynamicBufferBlockState::Free {
                        prev_free_block: None,
                        next_free_block: Some(next_free),
                        prev_block: new_free_block_prev_block,
                        size: new_free_block_size,
                    },
                );
                let DynamicBufferBlockState::Free {
                    prev_free_block, ..
                } = self
                    .block_states
                    .get_mut(&next_free)
                    .expect("no block boundary")
                else {
                    unreachable!();
                };
                *prev_free_block = Some(new_free_block_offset);
            }
        } else {
            // this is the first element of the free list
            self.mark_free(f, s);
            self.block_list_headings[Self::block_list_index(f, s)] = new_free_block_offset;
            self.block_states.insert(
                new_free_block_offset,
                DynamicBufferBlockState::Free {
                    prev_free_block: None,
                    next_free_block: None,
                    prev_block: new_free_block_prev_block,
                    size: new_free_block_size,
                },
            );
        }

        // adjust next block's prev pointer
        let next_block = new_free_block_offset + new_free_block_size.get();
        if next_block < DB_TLSF_PAGE_SIZE {
            match self
                .block_states
                .get_mut(&next_block)
                .expect("no block boundary")
            {
                DynamicBufferBlockState::Free { prev_block, .. } => {
                    *prev_block = new_free_block_offset;
                }
                DynamicBufferBlockState::Used { prev_block, .. } => {
                    *prev_block = new_free_block_offset;
                }
            }
        }
    }

    #[allow(dead_code)]
    fn dump_block_states(&self) {
        let mut x = String::new();
        for (h, st) in self.block_states.iter() {
            use std::fmt::Write;

            writeln!(&mut x, "  {h}: {st:?}").unwrap();
        }

        tracing::debug!("block states\n{x}");
    }
}

struct DynamicBufferPointer {
    // TODO: これ制限した型でwrapしたほうがよさそう（DynamicBufferPointer経由でalloc/freeできないようにする）
    pub source_page: Rc<UnsafeCell<DynamicBufferPage>>,
    pub offset: br::DeviceSize,
}

/// TLSF based dynamic allocatable gpu buffer
struct DynamicBuffer {
    usage: br::BufferUsage,
    page_pools: Vec<Rc<UnsafeCell<DynamicBufferPage>>>,
    dbg_name: &'static str,
}
impl DynamicBuffer {
    unsafe fn drop(self, device: &Graphics) {
        for p in self.page_pools {
            unsafe {
                Rc::try_unwrap(p)
                    .unwrap_or_else(|_| unreachable!("dynamic buffer still referenced"))
                    .into_inner()
                    .drop(device);
            }
        }
    }

    fn new(usage: br::BufferUsage, dbg_name: &'static str) -> Self {
        Self {
            usage,
            page_pools: Vec::new(),
            dbg_name,
        }
    }

    fn alloc(
        &mut self,
        device: &Graphics,
        size: br::DeviceSize,
        mut on_create_page: impl FnMut(usize, &br::VkHandleRef<br::vk::VkBuffer>),
    ) -> DynamicBufferPointer {
        for p in self.page_pools.iter() {
            if let Some(found_offs) = unsafe { &mut *p.get() }.try_alloc(size) {
                // allocated
                return DynamicBufferPointer {
                    source_page: p.clone(),
                    offset: found_offs,
                };
            }
        }

        // allocate new one
        let mut new_page =
            DynamicBufferPage::new(device, self.usage, self.dbg_name, self.page_pools.len());
        let found_offs = unsafe { new_page.try_alloc(size).unwrap_unchecked() };
        on_create_page(
            self.page_pools.len(),
            &br::VkHandleRef::from_raw_ref(&new_page.buffer),
        );
        let new_page = Rc::new(UnsafeCell::new(new_page));
        self.page_pools.push(new_page.clone());
        DynamicBufferPointer {
            source_page: new_page,
            offset: found_offs,
        }
    }

    fn free(&mut self, ptr: DynamicBufferPointer) {
        unsafe { &mut *ptr.source_page.get() }.free(ptr.offset);
    }
}

#[derive(Debug, Clone, Copy)]
pub enum IndexType {
    U16,
    U32,
}

pub struct CommittedMeshData {
    pub vertices: std::sync::Arc<[u8]>,
    pub vertex_stride: usize,
    pub indices: std::sync::Arc<[u8]>,
    pub index_type: IndexType,
    pub sub_mesh_ranges: std::sync::Arc<[core::range::Range<u32>]>,
}

pub struct CommittedRenderData {
    pub object_to_world: Matrix4F32,
    pub mesh_id: usize,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum HandleShape {
    Translation,
    Rotation,
    Scale,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum HandlePointing {
    X,
    Y,
    Z,
    All,
}

pub struct CommittedState {
    pub viewport_size: Size<LogicalUnit>,
    pub main_camera: Camera,
    pub main_camera_dirtified: bool,
    pub pushed_meshes: Vec<CommittedMeshData>,
    pub dirty_meshes: HashMap<usize, CommittedMeshData>,
    pub removed_meshes: HashSet<usize>,
    pub pushed_render_data: Vec<CommittedRenderData>,
    pub dirty_render_data: HashMap<usize, CommittedRenderData>,
    pub removed_render_data: HashSet<usize>,
    pub handle_shape: HandleShape,
    pub handle_to_world_transform: Matrix4F32,
    pub handle_pointing: Option<HandlePointing>,
    pub handle_data_dirtified: bool,
}

struct MeshData {
    vertex_offset: DynamicBufferPointer,
    index_offset: DynamicBufferPointer,
    vertex_size: br::DeviceSize,
    index_size: br::DeviceSize,
    vertex_update_pending: Option<usize>,
    index_update_pending: Option<usize>,
    index_type: IndexType,
    sub_mesh_ranges: Vec<core::range::Range<u32>>,
}

enum RenderData {
    Inactive,
    Active {
        object_uniform_start: DynamicBufferPointer,
        object_uniform_update_pending: Option<usize>,
        mesh_id: usize,
    },
}

pub struct Renderer {
    common_descriptor_set_layout: br::vk::VkDescriptorSetLayout,
    object_descriptor_set_layout: br::vk::VkDescriptorSetLayout,
    descriptor_pool: br::vk::VkDescriptorPool,
    common_descriptor_set: br::DescriptorSet,
    offsettable_object_descriptor_set: br::DescriptorSet,
    dynamic_ubuf_object_descriptor_pool: br::vk::VkDescriptorPool,
    dynamic_ubuf_object_descriptor_sets: Vec<br::DescriptorSet>,
    dynamic_ubuf_object_descriptor_set_index_by_buffer_handle: HashMap<br::vk::VkBuffer, usize>,
    streaming_buffer: br::vk::VkBuffer,
    streaming_memory: br::vk::VkDeviceMemory,
    streaming_memory_should_flush: bool,
    active_rt_size: br::Extent2D,
    active_framebuffer_resource_handle: br::vk::VkImageView,
    render_pass: br::vk::VkRenderPass,
    framebuffer: core::mem::MaybeUninit<br::vk::VkFramebuffer>,
    default_material_pipeline_layout: br::vk::VkPipelineLayout,
    default_material_shader: br::vk::VkShaderModule,
    default_material_pipeline: core::mem::MaybeUninit<br::vk::VkPipeline>,
    origin_axes_pipeline_layout: br::vk::VkPipelineLayout,
    origin_axes_shader: br::vk::VkShaderModule,
    origin_axes_pipeline: core::mem::MaybeUninit<br::vk::VkPipeline>,
    grid_pipeline_layout: br::vk::VkPipelineLayout,
    grid_shader: br::vk::VkShaderModule,
    grid_pipeline: core::mem::MaybeUninit<br::vk::VkPipeline>,
    unlit_colored_shader: br::vk::VkShaderModule,
    rotation_handle_shader: br::vk::VkShaderModule,
    unlit_colored_object_pipeline_layout: br::vk::VkPipelineLayout,
    gizmos_pipeline: core::mem::MaybeUninit<br::vk::VkPipeline>,
    rotation_handle_pipeline: core::mem::MaybeUninit<br::vk::VkPipeline>,
    command_pool: br::vk::VkCommandPool,
    command_buffer: br::vk::VkCommandBuffer,
    update_command_pool: br::vk::VkCommandPool,
    update_command_buffer: br::vk::VkCommandBuffer,
    update_command_pending: bool,
    scratch_staging: ScratchStagingBuffer,
    pending_camera_data_updates: Option<usize>,
    internal_mesh_buffer: br::vk::VkBuffer,
    origin_axes_vbuf_range: core::ops::Range<br::DeviceSize>,
    translate_handle_vbuf_range: core::ops::Range<br::DeviceSize>,
    translate_handle_ibuf_range: core::ops::Range<br::DeviceSize>,
    rotation_handle_vbuf_range: core::ops::Range<br::DeviceSize>,
    rotation_handle_ibuf_range: core::ops::Range<br::DeviceSize>,
    scale_handle_vbuf_range: core::ops::Range<br::DeviceSize>,
    scale_handle_ibuf_range: core::ops::Range<br::DeviceSize>,
    internal_uniform_buffer: br::vk::VkBuffer,
    camera_data_ubuf_range: core::ops::Range<br::DeviceSize>,
    handle_data_ubuf_range: core::ops::Range<br::DeviceSize>,
    internal_data_memory: br::vk::VkDeviceMemory,
    dynamic_buffer: DynamicBuffer,
    dynamic_ubuf: DynamicBuffer,
    user_meshes: Vec<MeshData>,
    user_renders: Vec<RenderData>,
    user_data_update_pending: bool,
    handle_shape: HandleShape,
    handle_pointing: Option<HandlePointing>,
    handle_to_world_transform: Matrix4F32,
    needs_invalidate_render: bool,
    valid: bool,
}
impl Renderer {
    pub unsafe fn drop(mut self, device: &Graphics) {
        self.user_renders.clear();
        self.user_meshes.clear();

        drop(unsafe { br::CommandPoolObject::manage(self.update_command_pool, device) });
        drop(unsafe { br::CommandPoolObject::manage(self.command_pool, device) });

        unsafe {
            self.scratch_staging.drop(device);
            self.dynamic_buffer.drop(device);
            self.dynamic_ubuf.drop(device);
        }

        if self.valid {
            drop(unsafe {
                br::PipelineObject::manage(self.rotation_handle_pipeline.assume_init(), device)
            });
            drop(unsafe { br::PipelineObject::manage(self.gizmos_pipeline.assume_init(), device) });
            drop(unsafe { br::PipelineObject::manage(self.grid_pipeline.assume_init(), device) });
            drop(unsafe {
                br::PipelineObject::manage(self.origin_axes_pipeline.assume_init(), device)
            });
            drop(unsafe {
                br::PipelineObject::manage(self.default_material_pipeline.assume_init(), device)
            });
            drop(unsafe { br::FramebufferObject::manage(self.framebuffer.assume_init(), device) });
        }

        drop(unsafe { br::ShaderModuleObject::manage(self.rotation_handle_shader, device) });
        drop(unsafe { br::ShaderModuleObject::manage(self.unlit_colored_shader, device) });
        drop(unsafe {
            br::PipelineLayoutObject::manage(self.unlit_colored_object_pipeline_layout, device)
        });
        drop(unsafe { br::ShaderModuleObject::manage(self.grid_shader, device) });
        drop(unsafe { br::PipelineLayoutObject::manage(self.grid_pipeline_layout, device) });
        drop(unsafe { br::ShaderModuleObject::manage(self.origin_axes_shader, device) });
        drop(unsafe { br::PipelineLayoutObject::manage(self.origin_axes_pipeline_layout, device) });
        drop(unsafe { br::ShaderModuleObject::manage(self.default_material_shader, device) });
        drop(unsafe {
            br::PipelineLayoutObject::manage(self.default_material_pipeline_layout, device)
        });
        drop(unsafe { br::RenderPassObject::manage(self.render_pass, device) });
        drop(unsafe { br::DeviceMemoryObject::manage(self.streaming_memory, device) });
        drop(unsafe { br::BufferObject::manage(self.streaming_buffer, device) });
        drop(unsafe {
            br::DescriptorPoolObject::manage(self.dynamic_ubuf_object_descriptor_pool, device)
        });
        drop(unsafe { br::DescriptorPoolObject::manage(self.descriptor_pool, device) });
        drop(unsafe {
            br::DescriptorSetLayoutObject::manage(self.object_descriptor_set_layout, device)
        });
        drop(unsafe {
            br::DescriptorSetLayoutObject::manage(self.common_descriptor_set_layout, device)
        });
        drop(unsafe { br::BufferObject::manage(self.internal_mesh_buffer, device) });
        drop(unsafe { br::BufferObject::manage(self.internal_uniform_buffer, device) });
        drop(unsafe { br::DeviceMemoryObject::manage(self.internal_data_memory, device) });
    }

    pub fn new(
        device: &Graphics,
        init_rt: &PreviewRenderTargetBuffer,
        init_state: &CommittedState,
        work_queue_family_index: u32,
        work_queue: &mut (impl br::QueueMut + ?Sized),
    ) -> Self {
        let mut streaming_buffer = br::BufferObject::new(
            device,
            &br::BufferCreateInfo::new_for_type::<PreviewStreamingBufferContent>(
                br::BufferUsage::UNIFORM_BUFFER,
            ),
        )
        .expect("preview.streaming_buffer.create");
        let memreq = streaming_buffer.requirements();
        let memindex = device
            .find_direct_memory_index(memreq.memoryTypeBits)
            .expect("preview.streaming_memory.index");
        let streaming_memory_should_flush = !device.is_coherent_memory(memindex);
        let streaming_memory = br::DeviceMemoryObject::new(
            device,
            &br::MemoryAllocateInfo::new(memreq.size, memindex),
        )
        .expect("preview.streaming_memory.alloc");
        streaming_buffer
            .bind(&streaming_memory, 0)
            .expect("preview_streaming_buffer.bind");

        let render_pass = br::RenderPassObject::new(
            device,
            &br::RenderPassCreateInfo2::new(
                &[
                    br::AttachmentDescription2::new(PreviewRenderTargetBuffer::COLOR_FORMAT)
                        .color_memory_op(br::LoadOp::Clear, br::StoreOp::Store)
                        .with_layout_to(br::ImageLayout::ShaderReadOnlyOpt.from_undefined()),
                    br::AttachmentDescription2::new(PreviewRenderTargetBuffer::DEPTH_FORMAT)
                        .color_memory_op(br::LoadOp::Clear, br::StoreOp::DontCare)
                        .stencil_memory_op(br::LoadOp::Clear, br::StoreOp::DontCare)
                        .with_layout_to(
                            br::ImageLayout::DepthStencilAttachmentOpt.from_undefined(),
                        ),
                ],
                &[br::SubpassDescription2::new()
                    .colors(&[br::AttachmentReference2::color_attachment_opt(0)])
                    .depth_stencil(&br::AttachmentReference2::depth_stencil_attachment_opt(1))],
                &[br::SubpassDependency2::new(
                    br::SubpassIndex::Internal(0),
                    br::SubpassIndex::External,
                )
                .by_region()
                .of_memory(
                    br::AccessFlags::COLOR_ATTACHMENT.write,
                    br::AccessFlags::SHADER.read,
                )
                .of_execution(
                    br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
                    br::PipelineStageFlags::FRAGMENT_SHADER,
                )],
            ),
        )
        .expect("preview.render_pass.create");

        let common_descriptor_set_layout = br::DescriptorSetLayoutObject::new(
            device,
            &br::DescriptorSetLayoutCreateInfo::new(&[
                br::DescriptorType::UniformBuffer.make_binding(0, 1)
            ]),
        )
        .expect("preview.common_descriptor_set_layout.create");
        let object_descriptor_set_layout = br::DescriptorSetLayoutObject::new(
            device,
            &br::DescriptorSetLayoutCreateInfo::new(&[
                br::DescriptorType::UniformBufferDynamic.make_binding(0, 1)
            ]),
        )
        .expect("preview.object_descriptor_set_layout.create");

        let default_material_pipeline_layout = br::PipelineLayoutObject::new(
            device,
            &br::PipelineLayoutCreateInfo::new(
                &[
                    common_descriptor_set_layout.as_transparent_ref(),
                    object_descriptor_set_layout.as_transparent_ref(),
                ],
                &[],
            ),
        )
        .expect("preview.default_material.pipeline_layout.create");
        let origin_axes_pipeline_layout = br::PipelineLayoutObject::new(
            device,
            &br::PipelineLayoutCreateInfo::new(
                &[common_descriptor_set_layout.as_transparent_ref()],
                &[],
            ),
        )
        .expect("preview.origin_axes.pipeline_layout.create");
        let grid_pipeline_layout = br::PipelineLayoutObject::new(
            device,
            &br::PipelineLayoutCreateInfo::new(
                &[common_descriptor_set_layout.as_transparent_ref()],
                &[br::PushConstantRange::for_type::<GridPushConstantData>(
                    br::vk::VK_SHADER_STAGE_VERTEX_BIT | br::vk::VK_SHADER_STAGE_FRAGMENT_BIT,
                    0,
                )],
            ),
        )
        .expect("preview.grid.pipeline_layout.create");
        let unlit_colored_object_pipeline_layout = br::PipelineLayoutObject::new(
            device,
            &br::PipelineLayoutCreateInfo::new(
                &[
                    common_descriptor_set_layout.as_transparent_ref(),
                    object_descriptor_set_layout.as_transparent_ref(),
                ],
                &[br::PushConstantRange::for_type::<[f32; 4 * 5]>(
                    br::vk::VK_SHADER_STAGE_VERTEX_BIT,
                    0,
                )],
            ),
        )
        .expect("preview.unlit_colored_object.pipeline_layout.create");
        let default_material_shader = device.require_shader("preview/default.spv");
        let origin_axes_shader = device.require_shader("preview/origin_axes.spv");
        let grid_shader = device.require_shader("preview/grid.spv");
        let unlit_colored_shader = device.require_shader("preview/unlit_colored.spv");
        let rotation_handle_shader = device.require_shader("preview/rotation_handle.spv");

        let origin_axes_vbuf_range = 0..size_of_val(VS_ORIGIN_AXES) as br::DeviceSize;
        let translate_handle_vbuf_range = range_from_len_u64(
            rup2_u64(origin_axes_vbuf_range.end, align_of::<HandleVertex>() as _),
            (size_of::<HandleVertex>() * TRANSLATE_HANDLE_VCOUNT) as _,
        );
        let rotation_handle_vbuf_range = range_from_len_u64(
            rup2_u64(
                translate_handle_vbuf_range.end,
                align_of::<HandleVertex>() as _,
            ),
            (size_of::<HandleVertex>() * ROTATION_HANDLE_VCOUNT) as _,
        );
        let scale_handle_vbuf_range = range_from_len_u64(
            rup2_u64(
                rotation_handle_vbuf_range.end,
                align_of::<HandleVertex>() as _,
            ),
            (size_of::<HandleVertex>() * SCALE_HANDLE_VCOUNT) as _,
        );
        let translate_handle_ibuf_range = range_from_len_u64(
            rup2_u64(scale_handle_vbuf_range.end, align_of::<u16>() as _),
            (size_of::<u16>() * TRANSLATE_HANDLE_ICOUNT) as _,
        );
        let rotation_handle_ibuf_range = range_from_len_u64(
            rup2_u64(translate_handle_ibuf_range.end, align_of::<u16>() as _),
            (size_of::<u16>() * ROTATION_HANDLE_ICOUNT) as _,
        );
        let scale_handle_ibuf_range = range_from_len_u64(
            rup2_u64(rotation_handle_ibuf_range.end, align_of::<u16>() as _),
            (size_of::<u16>() * SCALE_HANDLE_ICOUNT) as _,
        );
        let mut internal_mesh_buffer = br::BufferObject::new(
            device,
            &br::BufferCreateInfo::new(
                scale_handle_ibuf_range.end,
                br::BufferUsage::VERTEX_BUFFER
                    | br::BufferUsage::INDEX_BUFFER
                    | br::BufferUsage::TRANSFER_DEST,
            ),
        )
        .expect("preview.internal_mesh_buffer.create");
        let camera_data_ubuf_range = 0..size_of::<CameraData>() as br::DeviceSize;
        let gizmos_camera_data_ubuf_range =
            range_from_len_u64(camera_data_ubuf_range.end, size_of::<CameraData>() as _);
        let handle_data_ubuf_range = range_from_len_u64(
            rup2_u64(
                gizmos_camera_data_ubuf_range.end,
                device.min_uniform_buffer_offset_alignment(),
            ),
            size_of::<Matrix4F32>() as _,
        );
        let mut internal_uniform_buffer = br::BufferObject::new(
            device,
            &br::BufferCreateInfo::new(
                handle_data_ubuf_range.end,
                br::BufferUsage::UNIFORM_BUFFER | br::BufferUsage::TRANSFER_DEST,
            ),
        )
        .expect("preview.internal_uniform_buffer.create");
        let internal_mesh_buffer_memreq = internal_mesh_buffer.requirements();
        let internal_uniform_buffer_memreq = internal_uniform_buffer.requirements();
        let internal_mesh_buffer_offset = rup2_u64(
            internal_uniform_buffer_memreq.size,
            internal_mesh_buffer_memreq.alignment,
        );
        let internal_data_memory = device.alloc_device_local_memory(
            internal_mesh_buffer_offset + internal_mesh_buffer_memreq.size,
            internal_mesh_buffer_memreq.memoryTypeBits
                & internal_uniform_buffer_memreq.memoryTypeBits,
        );
        internal_mesh_buffer
            .bind(&internal_data_memory, internal_mesh_buffer_offset)
            .expect("preview.internal_mesh_buffer.bind");
        internal_uniform_buffer
            .bind(&internal_data_memory, 0)
            .expect("preview.internal_uniform_buffer.bind");

        struct UploadBufferData {
            origin_axes_vbuf: [OriginAxesVertex; VS_ORIGIN_AXES.len()],
            camera_data_ubuf: CameraData,
            handle_data_ubuf: Matrix4F32,
        }
        let translate_handle_vbuf_upload_offset =
            rup2(size_of::<UploadBufferData>(), align_of::<HandleVertex>());
        let rotation_handle_vbuf_upload_offset = rup2(
            translate_handle_vbuf_upload_offset
                + size_of::<HandleVertex>() * TRANSLATE_HANDLE_VCOUNT,
            align_of::<HandleVertex>(),
        );
        let scale_handle_vbuf_upload_offset = rup2(
            rotation_handle_vbuf_upload_offset + size_of::<HandleVertex>() * ROTATION_HANDLE_VCOUNT,
            align_of::<HandleVertex>(),
        );
        let translate_handle_ibuf_upload_offset = rup2(
            scale_handle_vbuf_upload_offset + size_of::<HandleVertex>() * SCALE_HANDLE_VCOUNT,
            align_of::<u16>(),
        );
        let rotation_handle_ibuf_upload_offset = rup2(
            translate_handle_ibuf_upload_offset + size_of::<u16>() * TRANSLATE_HANDLE_ICOUNT,
            align_of::<u16>(),
        );
        let scale_handle_ibuf_upload_offset = rup2(
            rotation_handle_ibuf_upload_offset + size_of::<u16>() * ROTATION_HANDLE_ICOUNT,
            align_of::<u16>(),
        );
        let upload_size = scale_handle_ibuf_upload_offset + size_of::<u16>() * SCALE_HANDLE_ICOUNT;
        let mut upload_buffer = br::BufferObject::new(
            device,
            &br::BufferCreateInfo::new(upload_size as _, br::BufferUsage::TRANSFER_SRC),
        )
        .expect("preview.upload_buffer.create");
        let memreq = upload_buffer.requirements();
        let memindex = device
            .find_host_visible_memory_index(memreq.memoryTypeBits)
            .expect("preview.upload_memory.index");
        let should_flush = !device.is_coherent_memory(memindex);
        let mut mem = br::DeviceMemoryObject::new(
            device,
            &br::MemoryAllocateInfo::new(memreq.size, memindex),
        )
        .expect("preview.upload_memory.alloc");
        upload_buffer
            .bind(&mem, 0)
            .expect("preview.upload_buffer.bind");
        let memhandle = mem.native_ptr();
        let ptr = mem
            .map(0..upload_size as _)
            .expect("preview.upload_memory.map");
        unsafe {
            let p = ptr.ptr().cast::<UploadBufferData>();
            core::ptr::copy_nonoverlapping(
                VS_ORIGIN_AXES.as_ptr(),
                (*p).origin_axes_vbuf.as_mut_ptr(),
                VS_ORIGIN_AXES.len(),
            );
            core::ptr::write(
                &raw mut (*p).camera_data_ubuf,
                CameraData::new(&init_state.main_camera, init_rt.aspect_wh()),
            );
            core::ptr::write(
                &raw mut (*p).handle_data_ubuf,
                Matrix4F32::translation(Vector3(1.0, 0.0, 0.0)).transpose(),
            );

            gen_translate_handle_mesh(
                ptr.ptr()
                    .byte_add(translate_handle_vbuf_upload_offset)
                    .cast(),
                ptr.ptr()
                    .byte_add(translate_handle_ibuf_upload_offset)
                    .cast(),
            );
            gen_rotation_handle_mesh(
                ptr.ptr()
                    .byte_add(rotation_handle_vbuf_upload_offset)
                    .cast(),
                ptr.ptr()
                    .byte_add(rotation_handle_ibuf_upload_offset)
                    .cast(),
            );
            gen_scale_handle_mesh(
                ptr.ptr().byte_add(scale_handle_vbuf_upload_offset).cast(),
                ptr.ptr().byte_add(scale_handle_ibuf_upload_offset).cast(),
            );
        }
        if should_flush {
            unsafe {
                device
                    .flush_mapped_memory_ranges(&[br::MappedMemoryRange::new_raw(
                        memhandle,
                        0,
                        upload_size as _,
                    )])
                    .expect("preview.upload_memory.flush");
            }
        }
        drop(ptr);

        let mut init_cp = br::CommandPoolObject::new(
            device,
            &br::CommandPoolCreateInfo::new(work_queue_family_index).transient(),
        )
        .expect("preview.init_cp.create");
        let [mut init_cb] = br::CommandBufferObject::alloc_array(
            device,
            &br::CommandBufferFixedCountAllocateInfo::new(
                &mut init_cp,
                br::CommandBufferLevel::Primary,
            ),
        )
        .expect("preview.init_cb.alloc");
        unsafe {
            init_cb
                .begin(&br::CommandBufferBeginInfo::new().onetime_submit())
                .expect("preview.init_cb.begin")
        }
        .copy_buffer(
            &upload_buffer,
            &internal_mesh_buffer,
            &[
                br::BufferCopy(br::vk::VkBufferCopy {
                    srcOffset: core::mem::offset_of!(UploadBufferData, origin_axes_vbuf) as _,
                    dstOffset: 0,
                    size: size_of_val(VS_ORIGIN_AXES) as _,
                }),
                br::BufferCopy(br::vk::VkBufferCopy {
                    srcOffset: translate_handle_vbuf_upload_offset as _,
                    dstOffset: translate_handle_vbuf_range.start,
                    size: translate_handle_vbuf_range.end - translate_handle_vbuf_range.start,
                }),
                br::BufferCopy(br::vk::VkBufferCopy {
                    srcOffset: translate_handle_ibuf_upload_offset as _,
                    dstOffset: translate_handle_ibuf_range.start,
                    size: translate_handle_ibuf_range.end - translate_handle_ibuf_range.start,
                }),
                br::BufferCopy(br::vk::VkBufferCopy {
                    srcOffset: rotation_handle_vbuf_upload_offset as _,
                    dstOffset: rotation_handle_vbuf_range.start,
                    size: rotation_handle_vbuf_range.end - rotation_handle_vbuf_range.start,
                }),
                br::BufferCopy(br::vk::VkBufferCopy {
                    srcOffset: rotation_handle_ibuf_upload_offset as _,
                    dstOffset: rotation_handle_ibuf_range.start,
                    size: rotation_handle_ibuf_range.end - rotation_handle_ibuf_range.start,
                }),
                br::BufferCopy(br::vk::VkBufferCopy {
                    srcOffset: scale_handle_vbuf_upload_offset as _,
                    dstOffset: scale_handle_vbuf_range.start,
                    size: scale_handle_vbuf_range.end - scale_handle_vbuf_range.start,
                }),
                br::BufferCopy(br::vk::VkBufferCopy {
                    srcOffset: scale_handle_ibuf_upload_offset as _,
                    dstOffset: scale_handle_ibuf_range.start,
                    size: scale_handle_ibuf_range.end - scale_handle_ibuf_range.start,
                }),
            ],
        )
        .copy_buffer(
            &upload_buffer,
            &internal_uniform_buffer,
            &[
                br::BufferCopy::copy_data::<CameraData>(
                    core::mem::offset_of!(UploadBufferData, camera_data_ubuf) as _,
                    camera_data_ubuf_range.start,
                ),
                br::BufferCopy::copy_data::<Matrix4F32>(
                    core::mem::offset_of!(UploadBufferData, handle_data_ubuf) as _,
                    handle_data_ubuf_range.start,
                ),
            ],
        )
        .inject(|r| {
            device.cmd_pipeline_barrier(
                r,
                &br::DependencyInfo::new(
                    &[br::MemoryBarrier2::new()
                        .from(
                            br::PipelineStageFlags2::COPY,
                            br::AccessFlags2::TRANSFER.write,
                        )
                        .to(
                            br::PipelineStageFlags2::VERTEX_ATTRIBUTE_INPUT
                                | br::PipelineStageFlags2::VERTEX_SHADER,
                            br::AccessFlags2::VERTEX_ATTRIBUTE_READ
                                | br::AccessFlags2::UNIFORM_READ,
                        )],
                    &[],
                    &[],
                ),
            )
        })
        .end()
        .expect("preview.init_cb.end");
        work_queue
            .submit(
                &[br::SubmitInfo::new_array(
                    &[],
                    &[],
                    &[init_cb.as_transparent_ref()],
                    &[],
                )],
                None,
            )
            .expect("preview.init_cb.submit");

        let scratch_staging = ScratchStagingBuffer::new(device);

        let mut descriptor_pool = br::DescriptorPoolObject::new(
            device,
            &br::DescriptorPoolCreateInfo::new(
                2,
                &[
                    br::DescriptorType::UniformBuffer.make_size(1),
                    br::DescriptorType::UniformBufferDynamic.make_size(1),
                ],
            ),
        )
        .expect("preview.descriptor_pool.create");
        let [common_descriptor_set, offsettable_object_descriptor_set] = descriptor_pool
            .alloc_array(&[
                common_descriptor_set_layout.as_transparent_ref(),
                object_descriptor_set_layout.as_transparent_ref(),
            ])
            .expect("preview.descriptor.alloc");
        device.update_descriptor_sets(
            &[
                common_descriptor_set
                    .binding_at(0)
                    .write(br::DescriptorContents::uniform_buffer(
                        &internal_uniform_buffer,
                        camera_data_ubuf_range.clone(),
                    )),
                offsettable_object_descriptor_set.binding_at(0).write(
                    br::DescriptorContents::uniform_buffer_dynamic(
                        &internal_uniform_buffer,
                        handle_data_ubuf_range.clone(),
                    ),
                ),
            ],
            &[],
        );

        let mut dynamic_ubuf_object_descriptor_pool = br::DescriptorPoolObject::new(
            device,
            &br::DescriptorPoolCreateInfo::new(
                16,
                &[br::DescriptorType::UniformBufferDynamic.make_size(16)],
            ),
        )
        .expect("preview.dynamic_object_descriptor_pool.create");
        let dynamic_ubuf_object_descriptor_sets = dynamic_ubuf_object_descriptor_pool
            .alloc(&[object_descriptor_set_layout.as_transparent_ref(); 16])
            .expect("preview.dynamic_object_descriptor.alloc");

        let mut command_pool = br::CommandPoolObject::new(
            device,
            &br::CommandPoolCreateInfo::new(device.present_queue_family_index()),
        )
        .expect("preview.command_pool.create");
        let [command_buffer] = br::CommandBufferObject::alloc_array(
            device,
            &br::CommandBufferFixedCountAllocateInfo::new(
                &mut command_pool,
                br::CommandBufferLevel::Primary,
            ),
        )
        .expect("preview.command_buffer.create");
        device.dbg_set_name(&command_pool, c"Preview.RenderCommandPool");
        device.dbg_set_name(&command_buffer, c"Preview.RenderCommandBuffer");

        let mut update_command_pool = br::CommandPoolObject::new(
            device,
            &br::CommandPoolCreateInfo::new(device.present_queue_family_index()),
        )
        .expect("preview.update_command_pool.create");
        let [update_command_buffer] = br::CommandBufferObject::alloc_array(
            device,
            &br::CommandBufferFixedCountAllocateInfo::new(
                &mut update_command_pool,
                br::CommandBufferLevel::Primary,
            ),
        )
        .expect("preview.update_command_buffer.alloc");
        device.dbg_set_name(&update_command_pool, c"Preview.UpdateCommandPool");
        device.dbg_set_name(&update_command_buffer, c"Preview.UpdateCommandBuffer");

        work_queue.wait().expect("preview.init_cb.wait");
        // keep alive
        drop(mem);
        drop(upload_buffer);

        let (update_command_pool, _) = update_command_pool.unmanage();
        let (command_pool, _) = command_pool.unmanage();
        let (internal_data_memory, _) = internal_data_memory.unmanage();
        let (internal_uniform_buffer, _) = internal_uniform_buffer.unmanage();
        let (internal_mesh_buffer, _) = internal_mesh_buffer.unmanage();
        let (default_material_shader, _) = default_material_shader.unmanage();
        let (default_material_pipeline_layout, _) = default_material_pipeline_layout.unmanage();
        let (unlit_colored_shader, _) = unlit_colored_shader.unmanage();
        let (rotation_handle_shader, _) = rotation_handle_shader.unmanage();
        let (unlit_colored_object_pipeline_layout, _) =
            unlit_colored_object_pipeline_layout.unmanage();
        let (grid_shader, _) = grid_shader.unmanage();
        let (grid_pipeline_layout, _) = grid_pipeline_layout.unmanage();
        let (origin_axes_shader, _) = origin_axes_shader.unmanage();
        let (origin_axes_pipeline_layout, _) = origin_axes_pipeline_layout.unmanage();
        let (render_pass, _) = render_pass.unmanage();
        let (streaming_memory, _) = streaming_memory.unmanage();
        let (streaming_buffer, _) = streaming_buffer.unmanage();
        let (dynamic_ubuf_object_descriptor_pool, _) =
            dynamic_ubuf_object_descriptor_pool.unmanage();
        let (descriptor_pool, _) = descriptor_pool.unmanage();
        let (object_descriptor_set_layout, _) = object_descriptor_set_layout.unmanage();
        let (common_descriptor_set_layout, _) = common_descriptor_set_layout.unmanage();
        Self {
            common_descriptor_set_layout,
            object_descriptor_set_layout,
            descriptor_pool,
            common_descriptor_set,
            offsettable_object_descriptor_set,
            dynamic_ubuf_object_descriptor_pool,
            dynamic_ubuf_object_descriptor_sets,
            dynamic_ubuf_object_descriptor_set_index_by_buffer_handle: HashMap::new(),
            streaming_buffer,
            streaming_memory,
            streaming_memory_should_flush,
            active_rt_size: init_rt.size,
            active_framebuffer_resource_handle: init_rt.image_view,
            render_pass,
            framebuffer: core::mem::MaybeUninit::uninit(),
            default_material_pipeline_layout,
            default_material_shader,
            default_material_pipeline: core::mem::MaybeUninit::uninit(),
            origin_axes_pipeline_layout,
            origin_axes_shader,
            origin_axes_pipeline: core::mem::MaybeUninit::uninit(),
            grid_pipeline_layout,
            grid_shader,
            grid_pipeline: core::mem::MaybeUninit::uninit(),
            unlit_colored_object_pipeline_layout,
            unlit_colored_shader,
            rotation_handle_shader,
            gizmos_pipeline: core::mem::MaybeUninit::uninit(),
            rotation_handle_pipeline: core::mem::MaybeUninit::uninit(),
            internal_mesh_buffer,
            origin_axes_vbuf_range,
            translate_handle_vbuf_range,
            translate_handle_ibuf_range,
            rotation_handle_vbuf_range,
            rotation_handle_ibuf_range,
            scale_handle_vbuf_range,
            scale_handle_ibuf_range,
            internal_uniform_buffer,
            camera_data_ubuf_range,
            handle_data_ubuf_range,
            internal_data_memory,
            scratch_staging,
            pending_camera_data_updates: None,
            command_pool,
            command_buffer: command_buffer.native_ptr(),
            update_command_pool,
            update_command_buffer: update_command_buffer.native_ptr(),
            update_command_pending: false,
            dynamic_buffer: DynamicBuffer::new(
                br::BufferUsage::VERTEX_BUFFER
                    | br::BufferUsage::INDEX_BUFFER
                    | br::BufferUsage::TRANSFER_DEST,
                "Preview.Std",
            ),
            dynamic_ubuf: DynamicBuffer::new(
                br::BufferUsage::UNIFORM_BUFFER | br::BufferUsage::TRANSFER_DEST,
                "Preview.Uniform",
            ),
            user_meshes: Vec::new(),
            user_renders: Vec::new(),
            user_data_update_pending: false,
            handle_shape: HandleShape::Translation,
            handle_pointing: None,
            handle_to_world_transform: Matrix4::ONE,
            needs_invalidate_render: false,
            valid: false,
        }
    }

    pub fn update(&mut self, device: &Graphics, committed_state: &mut CommittedState) {
        self.scratch_staging.reset();

        for m in committed_state.pushed_meshes.drain(..) {
            let vbuf = self
                .dynamic_buffer
                .alloc(device, m.vertices.len() as _, |_, _| {});
            let ibuf = self
                .dynamic_buffer
                .alloc(device, m.indices.len() as _, |_, _| {});

            let vertex_update = self.scratch_staging.reserve(m.vertices.len());
            let index_update = self.scratch_staging.reserve(m.indices.len());
            unsafe {
                core::ptr::copy_nonoverlapping(
                    m.vertices.as_ptr(),
                    self.scratch_staging
                        .mapped_ptr
                        .byte_add(vertex_update)
                        .cast(),
                    m.vertices.len(),
                );
                core::ptr::copy_nonoverlapping(
                    m.indices.as_ptr(),
                    self.scratch_staging
                        .mapped_ptr
                        .byte_add(index_update)
                        .cast(),
                    m.indices.len(),
                );
            }
            self.user_data_update_pending = true;

            self.user_meshes.push(MeshData {
                vertex_offset: vbuf,
                index_offset: ibuf,
                vertex_size: m.vertices.len() as _,
                index_size: m.indices.len() as _,
                vertex_update_pending: Some(vertex_update),
                index_update_pending: Some(index_update),
                index_type: m.index_type,
                sub_mesh_ranges: m.sub_mesh_ranges.as_ref().to_owned(),
            });
        }

        for r in committed_state.pushed_render_data.drain(..) {
            let object_uniform_start = self.dynamic_ubuf.alloc(
                device,
                size_of::<Matrix4F32>() as _,
                |new_buffer_index, buffer| {
                    self.dynamic_ubuf_object_descriptor_set_index_by_buffer_handle
                        .insert(buffer.native_ptr(), new_buffer_index);

                    // setup descriptor set for newly created pool
                    if self.dynamic_ubuf_object_descriptor_sets.len() > new_buffer_index {
                        // without extend
                        device.update_descriptor_sets(
                            &[self.dynamic_ubuf_object_descriptor_sets[new_buffer_index]
                                .binding_at(0)
                                .write(br::DescriptorContents::uniform_buffer_dynamic(
                                    buffer,
                                    0..size_of::<Matrix4F32>() as _,
                                ))],
                            &[],
                        );
                        return;
                    }

                    // with extending
                    let mut new_pool = br::DescriptorPoolObject::new(
                        device,
                        &br::DescriptorPoolCreateInfo::new(
                            (self.dynamic_ubuf_object_descriptor_sets.len() * 2) as _,
                            &[br::DescriptorType::UniformBufferDynamic.make_size(
                                (self.dynamic_ubuf_object_descriptor_sets.len() * 2) as _,
                            )],
                        ),
                    )
                    .expect("preview.dynamic_ubuf_object_descriptor_pool.recreate");
                    let new_sets = new_pool
                        .alloc(&vec![
                            unsafe {
                                br::VkHandleRef::dangling(self.object_descriptor_set_layout)
                            };
                            self.dynamic_ubuf_object_descriptor_sets.len() * 2
                        ])
                        .expect("preview.dynamic_ubuf_objet_descriptors.realloc");
                    device.update_descriptor_sets(
                        &[new_sets[new_buffer_index].binding_at(0).write(
                            br::DescriptorContents::uniform_buffer_dynamic(
                                buffer,
                                0..size_of::<Matrix4F32>() as _,
                            ),
                        )],
                        &self
                            .dynamic_ubuf_object_descriptor_sets
                            .iter()
                            .zip(new_sets.iter())
                            .map(|(src, dst)| src.binding_at(0).copy(1, dst.binding_at(0)))
                            .collect::<Vec<_>>(),
                    );
                    let old_pool = core::mem::replace(
                        &mut self.dynamic_ubuf_object_descriptor_pool,
                        new_pool.unmanage().0,
                    );
                    self.dynamic_ubuf_object_descriptor_sets = new_sets;
                    drop(unsafe { br::DescriptorPoolObject::manage(old_pool, device) });
                },
            );

            let object_ubuf = self.scratch_staging.reserve(size_of::<Matrix4F32>());
            unsafe {
                self.scratch_staging
                    .mapped_ptr
                    .byte_add(object_ubuf)
                    .cast::<Matrix4F32>()
                    .write(r.object_to_world);
            }
            self.user_data_update_pending = true;

            self.user_renders.push(RenderData::Active {
                object_uniform_start,
                object_uniform_update_pending: Some(object_ubuf),
                mesh_id: r.mesh_id,
            });
            self.needs_invalidate_render = true;
        }
        for r in committed_state.removed_render_data.drain() {
            let RenderData::Active {
                object_uniform_start,
                ..
            } = core::mem::replace(&mut self.user_renders[r], RenderData::Inactive)
            else {
                // already inactive
                continue;
            };

            self.dynamic_ubuf.free(object_uniform_start);
            self.needs_invalidate_render = true;
        }
        for (rid, d) in committed_state.dirty_render_data.drain() {
            let RenderData::Active {
                object_uniform_update_pending,
                mesh_id,
                ..
            } = &mut self.user_renders[rid]
            else {
                continue;
            };

            let object_ubuf = self.scratch_staging.reserve(size_of::<Matrix4F32>());
            unsafe {
                self.scratch_staging
                    .mapped_ptr
                    .byte_add(object_ubuf)
                    .cast::<Matrix4F32>()
                    .write(d.object_to_world);
            }
            self.user_data_update_pending = true;

            *object_uniform_update_pending = Some(object_ubuf);
            *mesh_id = d.mesh_id;
            self.needs_invalidate_render = true;
        }

        if core::mem::replace(&mut committed_state.handle_data_dirtified, false) {
            self.handle_shape = committed_state.handle_shape;
            self.handle_pointing = committed_state.handle_pointing;
            self.needs_invalidate_render = true;
        }
    }

    pub fn validate(
        &mut self,
        device: &Graphics,
        active_rt: &PreviewRenderTargetBuffer,
        committed_state: &mut CommittedState,
    ) {
        let mut framebuffer_changed = false;
        if !self.valid
            || active_rt.size != self.active_rt_size
            || active_rt.image_view != self.active_framebuffer_resource_handle
        {
            // Note: color bufferとdepth bufferは同時に変わるのでどっちかだけ見ればいい
            if self.valid {
                drop(unsafe {
                    br::FramebufferObject::manage(self.framebuffer.assume_init(), device)
                });
            }

            self.framebuffer.write(
                br::FramebufferObject::new(
                    device,
                    &br::FramebufferCreateInfo::new(
                        br::VkHandleRef::from_raw_ref(&self.render_pass),
                        &[active_rt.image_view_tref(), active_rt.depth_view_tref()],
                        active_rt.size.width,
                        active_rt.size.height,
                    ),
                )
                .expect("preview.validate.framebuffer")
                .unmanage()
                .0,
            );

            framebuffer_changed = true;
        }

        let mut origin_axes_pipeline_changed = false;
        if !self.valid || active_rt.size != self.active_rt_size {
            if self.valid {
                drop(unsafe {
                    br::PipelineObject::manage(self.default_material_pipeline.assume_init(), device)
                });
                drop(unsafe {
                    br::PipelineObject::manage(self.origin_axes_pipeline.assume_init(), device)
                });
                drop(unsafe {
                    br::PipelineObject::manage(self.grid_pipeline.assume_init(), device)
                });
                drop(unsafe {
                    br::PipelineObject::manage(self.gizmos_pipeline.assume_init(), device)
                });
                drop(unsafe {
                    br::PipelineObject::manage(self.rotation_handle_pipeline.assume_init(), device)
                });
            }

            let [
                default_material_pipeline,
                origin_axes_pipeline,
                grid_pipeline,
                gizmos_pipeline,
                rotation_handle_pipeline,
            ] = device
                .create_graphics_pipelines_array(&[
                    br::GraphicsPipelineCreateInfo::new(
                        br::VkHandleRef::from_raw_ref(&self.default_material_pipeline_layout),
                        br::SubpassRef(br::VkHandleRef::from_raw_ref(&self.render_pass), 0),
                        // TODO: from material
                        &[
                            br::PipelineShaderStage::new(
                                br::ShaderStage::Vertex,
                                br::VkHandleRef::from_raw_ref(&self.default_material_shader),
                                c"vertMain",
                            ),
                            br::PipelineShaderStage::new(
                                br::ShaderStage::Fragment,
                                br::VkHandleRef::from_raw_ref(&self.default_material_shader),
                                c"fragMain",
                            ),
                        ],
                        // TODO: from mesh
                        &br::PipelineVertexInputStateCreateInfo::new(
                            &[br::VertexInputBindingDescription::per_vertex_typed::<
                                [peridot_math::Vector4F32; 2],
                            >(0)],
                            &[
                                br::VertexInputAttributeDescription(
                                    br::vk::VkVertexInputAttributeDescription {
                                        location: 0,
                                        binding: 0,
                                        offset: 0,
                                        format: br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
                                    },
                                ),
                                br::VertexInputAttributeDescription(
                                    br::vk::VkVertexInputAttributeDescription {
                                        location: 1,
                                        binding: 0,
                                        offset: size_of::<peridot_math::Vector4F32>() as _,
                                        format: br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
                                    },
                                ),
                            ],
                        ),
                        // TODO: from mesh
                        IA_STATE_TRILIST,
                        &br::PipelineViewportStateCreateInfo::new(
                            &[active_rt
                                .size
                                .into_rect(br::Offset2D::ZERO)
                                .make_viewport(0.0..1.0)],
                            &[active_rt.size.into_rect(br::Offset2D::ZERO)],
                        ),
                        // TODO: from material
                        &br::PipelineRasterizationStateCreateInfo::new(
                            br::PolygonMode::Fill,
                            br::CullModeFlags::BACK,
                            br::FrontFace::CounterClockwise,
                        ),
                        // TODO: from material
                        BLEND_STATE_SINGLE_PREMULTIPLIED,
                    )
                    .set_multisample_state(MS_STATE_EMPTY)
                    // TODO: from material
                    .set_depth_stencil_state(
                        &br::PipelineDepthStencilStateCreateInfo::new()
                            .config_depth(Some(br::CompareOp::Less), true),
                    ),
                    br::GraphicsPipelineCreateInfo::new(
                        br::VkHandleRef::from_raw_ref(&self.origin_axes_pipeline_layout),
                        br::SubpassRef(br::VkHandleRef::from_raw_ref(&self.render_pass), 0),
                        &[
                            br::PipelineShaderStage::new(
                                br::ShaderStage::Vertex,
                                br::VkHandleRef::from_raw_ref(&self.origin_axes_shader),
                                c"vertMain",
                            ),
                            br::PipelineShaderStage::new(
                                br::ShaderStage::Fragment,
                                br::VkHandleRef::from_raw_ref(&self.origin_axes_shader),
                                c"fragMain",
                            ),
                        ],
                        &br::PipelineVertexInputStateCreateInfo::new(
                            &[br::VertexInputBindingDescription(
                                br::vk::VkVertexInputBindingDescription {
                                    binding: 0,
                                    stride: size_of::<OriginAxesVertex>() as _,
                                    inputRate: br::vk::VK_VERTEX_INPUT_RATE_VERTEX,
                                },
                            )],
                            &[
                                br::VertexInputAttributeDescription(
                                    br::vk::VkVertexInputAttributeDescription {
                                        location: 0,
                                        binding: 0,
                                        offset: core::mem::offset_of!(OriginAxesVertex, dir) as _,
                                        format: br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
                                    },
                                ),
                                br::VertexInputAttributeDescription(
                                    br::vk::VkVertexInputAttributeDescription {
                                        location: 1,
                                        binding: 0,
                                        offset: core::mem::offset_of!(OriginAxesVertex, offset)
                                            as _,
                                        format: br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
                                    },
                                ),
                            ],
                        ),
                        &br::PipelineInputAssemblyStateCreateInfo::new(
                            br::PrimitiveTopology::LineList,
                        ),
                        &br::PipelineViewportStateCreateInfo::new(
                            &[active_rt
                                .size
                                .into_rect(br::Offset2D::ZERO)
                                .make_viewport(0.0..1.0)],
                            &[active_rt.size.into_rect(br::Offset2D::ZERO)],
                        ),
                        &br::PipelineRasterizationStateCreateInfo::new(
                            br::PolygonMode::Fill,
                            br::CullModeFlags::NONE,
                            br::FrontFace::CounterClockwise,
                        ),
                        BLEND_STATE_SINGLE_PREMULTIPLIED,
                    )
                    .set_multisample_state(MS_STATE_EMPTY)
                    .set_depth_stencil_state(
                        &br::PipelineDepthStencilStateCreateInfo::new()
                            .config_depth(Some(br::CompareOp::LessOrEqual), false),
                    ),
                    br::GraphicsPipelineCreateInfo::new(
                        br::VkHandleRef::from_raw_ref(&self.grid_pipeline_layout),
                        br::SubpassRef(br::VkHandleRef::from_raw_ref(&self.render_pass), 0),
                        &[
                            br::PipelineShaderStage::new(
                                br::ShaderStage::Vertex,
                                br::VkHandleRef::from_raw_ref(&self.grid_shader),
                                c"vertMain",
                            ),
                            br::PipelineShaderStage::new(
                                br::ShaderStage::Fragment,
                                br::VkHandleRef::from_raw_ref(&self.grid_shader),
                                c"fragMain",
                            ),
                        ],
                        VI_STATE_EMPTY,
                        &br::PipelineInputAssemblyStateCreateInfo::new(
                            br::PrimitiveTopology::LineList,
                        ),
                        &br::PipelineViewportStateCreateInfo::new(
                            &[active_rt
                                .size
                                .into_rect(br::Offset2D::ZERO)
                                .make_viewport(0.0..1.0)],
                            &[active_rt.size.into_rect(br::Offset2D::ZERO)],
                        ),
                        &br::PipelineRasterizationStateCreateInfo::new(
                            br::PolygonMode::Fill,
                            br::CullModeFlags::NONE,
                            br::FrontFace::CounterClockwise,
                        ),
                        BLEND_STATE_SINGLE_PREMULTIPLIED,
                    )
                    .set_multisample_state(MS_STATE_EMPTY)
                    .set_depth_stencil_state(
                        &br::PipelineDepthStencilStateCreateInfo::new()
                            .config_depth(Some(br::CompareOp::Less), false),
                    ),
                    // gizmos
                    br::GraphicsPipelineCreateInfo::new(
                        br::VkHandleRef::from_raw_ref(&self.unlit_colored_object_pipeline_layout),
                        br::SubpassRef(br::VkHandleRef::from_raw_ref(&self.render_pass), 0),
                        &[
                            br::PipelineShaderStage::new(
                                br::ShaderStage::Vertex,
                                br::VkHandleRef::from_raw_ref(&self.unlit_colored_shader),
                                c"vertMain",
                            ),
                            br::PipelineShaderStage::new(
                                br::ShaderStage::Fragment,
                                br::VkHandleRef::from_raw_ref(&self.unlit_colored_shader),
                                c"fragMain",
                            ),
                        ],
                        &br::PipelineVertexInputStateCreateInfo::new(
                            &[br::VertexInputBindingDescription::per_vertex_typed::<
                                HandleVertex,
                            >(0)],
                            &[
                                br::VertexInputAttributeDescription(
                                    br::vk::VkVertexInputAttributeDescription {
                                        location: 0,
                                        binding: 0,
                                        offset: core::mem::offset_of!(HandleVertex, pos) as _,
                                        format: br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
                                    },
                                ),
                                br::VertexInputAttributeDescription(
                                    br::vk::VkVertexInputAttributeDescription {
                                        location: 1,
                                        binding: 0,
                                        offset: core::mem::offset_of!(HandleVertex, col_index) as _,
                                        format: br::vk::VK_FORMAT_R32_UINT,
                                    },
                                ),
                            ],
                        ),
                        IA_STATE_TRILIST,
                        &br::PipelineViewportStateCreateInfo::new(
                            &[active_rt
                                .size
                                .into_rect(br::Offset2D::ZERO)
                                .make_viewport(0.0..1.0)],
                            &[active_rt.size.into_rect(br::Offset2D::ZERO)],
                        ),
                        RASTER_STATE_DEFAULT_FILL_NOCULL,
                        BLEND_STATE_SINGLE_NONE,
                    )
                    .set_multisample_state(MS_STATE_EMPTY)
                    .set_depth_stencil_state(
                        &br::PipelineDepthStencilStateCreateInfo::new()
                            .config_depth(Some(br::CompareOp::Less), false),
                    ),
                    // rotation handle
                    br::GraphicsPipelineCreateInfo::new(
                        br::VkHandleRef::from_raw_ref(&self.unlit_colored_object_pipeline_layout),
                        br::SubpassRef(br::VkHandleRef::from_raw_ref(&self.render_pass), 0),
                        &[
                            br::PipelineShaderStage::new(
                                br::ShaderStage::Vertex,
                                br::VkHandleRef::from_raw_ref(&self.rotation_handle_shader),
                                c"vertMain",
                            ),
                            br::PipelineShaderStage::new(
                                br::ShaderStage::Fragment,
                                br::VkHandleRef::from_raw_ref(&self.rotation_handle_shader),
                                c"fragMain",
                            ),
                        ],
                        &br::PipelineVertexInputStateCreateInfo::new(
                            &[br::VertexInputBindingDescription::per_vertex_typed::<
                                HandleVertex,
                            >(0)],
                            &[
                                br::VertexInputAttributeDescription(
                                    br::vk::VkVertexInputAttributeDescription {
                                        location: 0,
                                        binding: 0,
                                        offset: core::mem::offset_of!(HandleVertex, pos) as _,
                                        format: br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
                                    },
                                ),
                                br::VertexInputAttributeDescription(
                                    br::vk::VkVertexInputAttributeDescription {
                                        location: 1,
                                        binding: 0,
                                        offset: core::mem::offset_of!(HandleVertex, col_index) as _,
                                        format: br::vk::VK_FORMAT_R32_UINT,
                                    },
                                ),
                            ],
                        ),
                        &br::PipelineInputAssemblyStateCreateInfo::new(
                            br::PrimitiveTopology::LineList,
                        ),
                        &br::PipelineViewportStateCreateInfo::new(
                            &[active_rt
                                .size
                                .into_rect(br::Offset2D::ZERO)
                                .make_viewport(0.0..1.0)],
                            &[active_rt.size.into_rect(br::Offset2D::ZERO)],
                        ),
                        &&br::PipelineRasterizationStateCreateInfo::new(
                            br::PolygonMode::Fill,
                            br::CullModeFlags::NONE,
                            br::FrontFace::CounterClockwise,
                        )
                        .line_width(2.0),
                        BLEND_STATE_SINGLE_NONE,
                    )
                    .set_multisample_state(MS_STATE_EMPTY)
                    .set_depth_stencil_state(
                        &br::PipelineDepthStencilStateCreateInfo::new()
                            .config_depth(Some(br::CompareOp::Less), false),
                    ),
                ])
                .expect("preview.validate.origin_axes.pipelines.create");
            self.default_material_pipeline
                .write(default_material_pipeline.unmanage().0);
            self.origin_axes_pipeline
                .write(origin_axes_pipeline.unmanage().0);
            self.grid_pipeline.write(grid_pipeline.unmanage().0);
            self.gizmos_pipeline.write(gizmos_pipeline.unmanage().0);
            self.rotation_handle_pipeline
                .write(rotation_handle_pipeline.unmanage().0);

            origin_axes_pipeline_changed = true;
        }

        let main_camera_dirtified =
            core::mem::replace(&mut committed_state.main_camera_dirtified, false);
        if main_camera_dirtified || active_rt.size != self.active_rt_size {
            let buffer_offset = *self
                .pending_camera_data_updates
                .get_or_insert_with(|| self.scratch_staging.reserve(size_of::<CameraData>()));
            unsafe {
                core::ptr::write(
                    self.scratch_staging
                        .mapped_ptr
                        .byte_add(buffer_offset)
                        .cast::<CameraData>(),
                    CameraData::new(&committed_state.main_camera, active_rt.aspect_wh()),
                );
            }
        }

        self.update_command_pending = false;
        let user_data_update_pending =
            core::mem::replace(&mut self.user_data_update_pending, false);
        if self.pending_camera_data_updates.is_some() || user_data_update_pending {
            // needs update device data
            self.scratch_staging.ops_before_copy(device);

            unsafe {
                br::vkfn_wrapper::reset_command_pool(
                    device.as_transparent_ref(),
                    br::VkHandleRefMut::dangling(self.update_command_pool),
                    br::CommandPoolResetFlags::EMPTY,
                )
                .expect("preview.validate.update_command_pool.reset");
            }
            unsafe {
                br::vkfn_wrapper::begin_command_buffer(
                    br::VkHandleRefMut::dangling(self.update_command_buffer),
                    &br::CommandBufferBeginInfo::new(),
                )
                .expect("preview.validate.update_command_buffer.begin");
            }
            br::CmdRecord::new(unsafe { br::VkHandleRefMut::dangling(self.update_command_buffer) })
                .inject(|r| match self.pending_camera_data_updates.take() {
                    None => r,
                    Some(bo) => r.copy_buffer(
                        br::VkHandleRef::from_raw_ref(&self.scratch_staging.buffer),
                        br::VkHandleRef::from_raw_ref(&self.internal_uniform_buffer),
                        &[br::BufferCopy::copy_data::<CameraData>(
                            bo as _,
                            self.camera_data_ubuf_range.start,
                        )],
                    ),
                })
                .inject(|r| {
                    let mut std_buffer_copies = HashMap::new();
                    for m in self.user_meshes.iter_mut() {
                        if let Some(o) = m.vertex_update_pending.take() {
                            std_buffer_copies
                                .entry(unsafe { &*m.vertex_offset.source_page.get() }.buffer)
                                .or_insert_with(Vec::new)
                                .push(br::BufferCopy(br::vk::VkBufferCopy {
                                    srcOffset: o as _,
                                    dstOffset: m.vertex_offset.offset,
                                    size: m.vertex_size,
                                }));
                        }
                        if let Some(o) = m.index_update_pending.take() {
                            std_buffer_copies
                                .entry(unsafe { &*m.index_offset.source_page.get() }.buffer)
                                .or_insert_with(Vec::new)
                                .push(br::BufferCopy(br::vk::VkBufferCopy {
                                    srcOffset: o as _,
                                    dstOffset: m.index_offset.offset,
                                    size: m.index_size,
                                }));
                        }
                    }
                    for r in self.user_renders.iter_mut() {
                        let RenderData::Active {
                            object_uniform_start,
                            object_uniform_update_pending,
                            ..
                        } = r
                        else {
                            continue;
                        };

                        if let Some(o) = object_uniform_update_pending.take() {
                            std_buffer_copies
                                .entry(unsafe { &*object_uniform_start.source_page.get() }.buffer)
                                .or_insert_with(Vec::new)
                                .push(br::BufferCopy(br::vk::VkBufferCopy {
                                    srcOffset: o as _,
                                    dstOffset: object_uniform_start.offset,
                                    size: size_of::<peridot_math::Matrix4F32>() as _,
                                }));
                        }
                    }

                    if std_buffer_copies.is_empty() {
                        // no copies
                        r
                    } else {
                        std_buffer_copies
                            .into_iter()
                            .fold(r, |r, (dest_buffer, copies)| {
                                r.copy_buffer(
                                    br::VkHandleRef::from_raw_ref(&self.scratch_staging.buffer),
                                    br::VkHandleRef::from_raw_ref(&dest_buffer),
                                    &copies,
                                )
                            })
                    }
                })
                .inject(|r| {
                    device.cmd_pipeline_barrier(
                        r,
                        &br::DependencyInfo::new(
                            &[br::MemoryBarrier2::new()
                                .from(
                                    br::PipelineStageFlags2::COPY,
                                    br::AccessFlags2::TRANSFER.write,
                                )
                                .to(
                                    br::PipelineStageFlags2::VERTEX_SHADER,
                                    br::AccessFlags2::UNIFORM_READ,
                                )],
                            &[],
                            &[],
                        ),
                    )
                })
                .end()
                .expect("preview.validate.update_command_buffer.end");
            self.update_command_pending = true;
        }

        let needs_invalidate_render = core::mem::replace(&mut self.needs_invalidate_render, false);
        if framebuffer_changed
            || origin_axes_pipeline_changed
            || active_rt.size != self.active_rt_size
            || needs_invalidate_render
        {
            unsafe {
                br::vkfn_wrapper::reset_command_pool(
                    device.as_transparent_ref(),
                    br::VkHandleRefMut::dangling(self.command_pool),
                    br::CommandPoolResetFlags::RELEASE_RESOURCES,
                )
                .expect("preview.validate.command_pool.reset");
            }

            unsafe {
                br::vkfn_wrapper::begin_command_buffer(
                    br::VkHandleRefMut::dangling(self.command_buffer),
                    &br::CommandBufferBeginInfo::new(),
                )
                .expect("preview.validate.command_buffer.begin");
            }
            br::CmdRecord::new(unsafe { br::VkHandleRefMut::dangling(self.command_buffer) })
                .begin_render_pass(
                    &br::RenderPassBeginInfo::new(
                        br::VkHandleRef::from_raw_ref(&self.render_pass),
                        br::VkHandleRef::from_raw_ref(unsafe {
                            self.framebuffer.assume_init_ref()
                        }),
                        active_rt.size.into_rect(br::Offset2D::ZERO),
                        &[
                            br::ClearValue::color_f32([0.0, 0.0, 0.0, 1.0]),
                            br::ClearValue::depth_stencil(1.0, 0),
                        ],
                    ),
                    br::SubpassContents::Inline,
                )
                .inject(|mut r| {
                    // TODO: needs approprivate batching
                    for x in self.user_renders.iter() {
                        let &RenderData::Active {
                            ref object_uniform_start,
                            mesh_id,
                            ..
                        } = x
                        else {
                            continue;
                        };

                        let mesh = &self.user_meshes[mesh_id];
                        r = r
                            .bind_pipeline(
                                br::PipelineBindPoint::Graphics,
                                br::VkHandleRef::from_raw_ref(unsafe {
                                    self.default_material_pipeline.assume_init_ref()
                                }),
                            )
                            .bind_descriptor_sets(
                                br::PipelineBindPoint::Graphics,
                                br::VkHandleRef::from_raw_ref(
                                    &self.default_material_pipeline_layout,
                                ),
                                0,
                                &[
                                    self.common_descriptor_set,
                                    self.dynamic_ubuf_object_descriptor_sets[self
                                        .dynamic_ubuf_object_descriptor_set_index_by_buffer_handle
                                        [&unsafe { &*object_uniform_start.source_page.get() }
                                            .buffer]],
                                ],
                                &[object_uniform_start.offset as _],
                            )
                            .bind_vertex_buffer_array(
                                0,
                                &[unsafe {
                                    br::VkHandleRef::dangling(
                                        (&*mesh.vertex_offset.source_page.get()).buffer,
                                    )
                                }],
                                &[mesh.vertex_offset.offset],
                            )
                            .bind_index_buffer(
                                br::VkHandleRef::from_raw_ref(
                                    &unsafe { &*mesh.index_offset.source_page.get() }.buffer,
                                ),
                                mesh.index_offset.offset as _,
                                match mesh.index_type {
                                    IndexType::U16 => br::IndexType::U16,
                                    IndexType::U32 => br::IndexType::U32,
                                },
                            );
                        for sub in mesh.sub_mesh_ranges.iter() {
                            r = r.draw_indexed(sub.end - sub.start, 1, sub.start, 0, 0);
                        }
                    }

                    r
                })
                .bind_pipeline(
                    br::PipelineBindPoint::Graphics,
                    br::VkHandleRef::from_raw_ref(unsafe { self.grid_pipeline.assume_init_ref() }),
                )
                .bind_descriptor_sets(
                    br::PipelineBindPoint::Graphics,
                    br::VkHandleRef::from_raw_ref(&self.grid_pipeline_layout),
                    0,
                    &[self.common_descriptor_set],
                    &[],
                )
                .push_constant(
                    br::VkHandleRef::from_raw_ref(&self.grid_pipeline_layout),
                    br::vk::VK_SHADER_STAGE_VERTEX_BIT | br::vk::VK_SHADER_STAGE_FRAGMENT_BIT,
                    0,
                    &GridPushConstantData {
                        dir: [1.0, 0.0, 0.0, 0.0],
                        start: [0.0, 0.0, -250.0, 1.0],
                        altdir: [0.0, 0.0, 1.0, 0.0],
                        scale: 1.0,
                    },
                )
                .draw(2, 500, 0, 0)
                .push_constant(
                    br::VkHandleRef::from_raw_ref(&self.grid_pipeline_layout),
                    br::vk::VK_SHADER_STAGE_VERTEX_BIT | br::vk::VK_SHADER_STAGE_FRAGMENT_BIT,
                    0,
                    &GridPushConstantData {
                        dir: [0.0, 0.0, 1.0, 0.0],
                        start: [-250.0, 0.0, 0.0, 1.0],
                        altdir: [1.0, 0.0, 0.0, 0.0],
                        scale: 1.0,
                    },
                )
                .draw(2, 500, 0, 0)
                .push_constant(
                    br::VkHandleRef::from_raw_ref(&self.grid_pipeline_layout),
                    br::vk::VK_SHADER_STAGE_VERTEX_BIT | br::vk::VK_SHADER_STAGE_FRAGMENT_BIT,
                    0,
                    &GridPushConstantData {
                        dir: [1.0, 0.0, 0.0, 0.0],
                        start: [0.0, 0.0, -250.0, 1.0],
                        altdir: [0.0, 0.0, 1.0, 0.0],
                        scale: 0.1,
                    },
                )
                .draw(2, 500, 0, 0)
                .push_constant(
                    br::VkHandleRef::from_raw_ref(&self.grid_pipeline_layout),
                    br::vk::VK_SHADER_STAGE_VERTEX_BIT | br::vk::VK_SHADER_STAGE_FRAGMENT_BIT,
                    0,
                    &GridPushConstantData {
                        dir: [0.0, 0.0, 1.0, 0.0],
                        start: [-250.0, 0.0, 0.0, 1.0],
                        altdir: [1.0, 0.0, 0.0, 0.0],
                        scale: 0.1,
                    },
                )
                .draw(2, 500, 0, 0)
                .bind_pipeline(
                    br::PipelineBindPoint::Graphics,
                    br::VkHandleRef::from_raw_ref(unsafe {
                        self.origin_axes_pipeline.assume_init_ref()
                    }),
                )
                .bind_descriptor_sets(
                    br::PipelineBindPoint::Graphics,
                    br::VkHandleRef::from_raw_ref(&self.origin_axes_pipeline_layout),
                    0,
                    &[self.common_descriptor_set],
                    &[],
                )
                .bind_vertex_buffer_array(
                    0,
                    &[unsafe { br::VkHandleRef::dangling(self.internal_mesh_buffer) }],
                    &[self.origin_axes_vbuf_range.start],
                )
                .draw(VS_ORIGIN_AXES.len() as _, 1, 0, 0)
                // clear depth for gizmos rendering
                .clear_attachments(
                    &[br::vk::VkClearAttachment {
                        aspectMask: (br::AspectMask::DEPTH | br::AspectMask::STENCIL).bits(),
                        colorAttachment: 0,
                        clearValue: br::ClearValue::depth_stencil(1.0, 0).0,
                    }],
                    &[br::vk::VkClearRect {
                        rect: active_rt.size.into_rect(br::Offset2D::ZERO),
                        baseArrayLayer: 0,
                        layerCount: 1,
                    }],
                )
                // render gizmos
                .inject(|r| match self.handle_shape {
                    HandleShape::Translation => r
                        .bind_pipeline(
                            br::PipelineBindPoint::Graphics,
                            br::VkHandleRef::from_raw_ref(unsafe {
                                self.gizmos_pipeline.assume_init_ref()
                            }),
                        )
                        .bind_descriptor_sets(
                            br::PipelineBindPoint::Graphics,
                            br::VkHandleRef::from_raw_ref(
                                &self.unlit_colored_object_pipeline_layout,
                            ),
                            0,
                            &[
                                self.common_descriptor_set,
                                self.offsettable_object_descriptor_set,
                            ],
                            &[0],
                        )
                        .push_constant_slice::<[f32; 4]>(
                            br::VkHandleRef::from_raw_ref(
                                &self.unlit_colored_object_pipeline_layout,
                            ),
                            br::vk::VK_SHADER_STAGE_VERTEX_BIT,
                            0,
                            &[
                                if self.handle_pointing == Some(HandlePointing::X) {
                                    [1.0, 1.0, 0.0, 1.0]
                                } else {
                                    [1.0, 0.0, 0.0, 1.0]
                                },
                                if self.handle_pointing == Some(HandlePointing::Y) {
                                    [1.0, 1.0, 0.0, 1.0]
                                } else {
                                    [0.0, 1.0, 0.0, 1.0]
                                },
                                if self.handle_pointing == Some(HandlePointing::Z) {
                                    [1.0, 1.0, 0.0, 1.0]
                                } else {
                                    [0.0, 0.0, 1.0, 1.0]
                                },
                                if self.handle_pointing == Some(HandlePointing::All) {
                                    [1.0, 1.0, 0.0, 1.0]
                                } else {
                                    [1.0, 1.0, 1.0, 1.0]
                                },
                                [1.0, 1.0, 1.0, 1.0],
                            ],
                        )
                        .bind_vertex_buffer_array(
                            0,
                            &[unsafe { br::VkHandleRef::dangling(self.internal_mesh_buffer) }],
                            &[self.translate_handle_vbuf_range.start],
                        )
                        .bind_index_buffer(
                            br::VkHandleRef::from_raw_ref(&self.internal_mesh_buffer),
                            self.translate_handle_ibuf_range.start as _,
                            br::IndexType::U16,
                        )
                        .draw_indexed(TRANSLATE_HANDLE_ICOUNT as _, 1, 0, 0, 0),
                    HandleShape::Rotation => r
                        .bind_pipeline(
                            br::PipelineBindPoint::Graphics,
                            br::VkHandleRef::from_raw_ref(unsafe {
                                self.rotation_handle_pipeline.assume_init_ref()
                            }),
                        )
                        .bind_descriptor_sets(
                            br::PipelineBindPoint::Graphics,
                            br::VkHandleRef::from_raw_ref(
                                &self.unlit_colored_object_pipeline_layout,
                            ),
                            0,
                            &[
                                self.common_descriptor_set,
                                self.offsettable_object_descriptor_set,
                            ],
                            &[0],
                        )
                        .push_constant_slice::<[f32; 4]>(
                            br::VkHandleRef::from_raw_ref(
                                &self.unlit_colored_object_pipeline_layout,
                            ),
                            br::vk::VK_SHADER_STAGE_VERTEX_BIT,
                            0,
                            &[
                                if self.handle_pointing == Some(HandlePointing::X) {
                                    [1.0, 1.0, 0.0, 1.0]
                                } else {
                                    [1.0, 0.0, 0.0, 1.0]
                                },
                                if self.handle_pointing == Some(HandlePointing::Y) {
                                    [1.0, 1.0, 0.0, 1.0]
                                } else {
                                    [0.0, 1.0, 0.0, 1.0]
                                },
                                if self.handle_pointing == Some(HandlePointing::Z) {
                                    [1.0, 1.0, 0.0, 1.0]
                                } else {
                                    [0.0, 0.0, 1.0, 1.0]
                                },
                                if self.handle_pointing == Some(HandlePointing::All) {
                                    [1.0, 1.0, 0.0, 1.0]
                                } else {
                                    [1.0, 1.0, 1.0, 1.0]
                                },
                                [1.0, 1.0, 1.0, 1.0],
                            ],
                        )
                        .bind_vertex_buffer_array(
                            0,
                            &[unsafe { br::VkHandleRef::dangling(self.internal_mesh_buffer) }],
                            &[self.rotation_handle_vbuf_range.start],
                        )
                        .bind_index_buffer(
                            br::VkHandleRef::from_raw_ref(&self.internal_mesh_buffer),
                            self.rotation_handle_ibuf_range.start as _,
                            br::IndexType::U16,
                        )
                        .draw_indexed(ROTATION_HANDLE_AXES_DRAW_ICOUNT, 1, 0, 0, 0),
                    HandleShape::Scale => r
                        .bind_pipeline(
                            br::PipelineBindPoint::Graphics,
                            br::VkHandleRef::from_raw_ref(unsafe {
                                self.gizmos_pipeline.assume_init_ref()
                            }),
                        )
                        .bind_descriptor_sets(
                            br::PipelineBindPoint::Graphics,
                            br::VkHandleRef::from_raw_ref(
                                &self.unlit_colored_object_pipeline_layout,
                            ),
                            0,
                            &[
                                self.common_descriptor_set,
                                self.offsettable_object_descriptor_set,
                            ],
                            &[0],
                        )
                        .push_constant_slice::<[f32; 4]>(
                            br::VkHandleRef::from_raw_ref(
                                &self.unlit_colored_object_pipeline_layout,
                            ),
                            br::vk::VK_SHADER_STAGE_VERTEX_BIT,
                            0,
                            &[
                                if self.handle_pointing == Some(HandlePointing::X) {
                                    [1.0, 1.0, 0.0, 1.0]
                                } else {
                                    [1.0, 0.0, 0.0, 1.0]
                                },
                                if self.handle_pointing == Some(HandlePointing::Y) {
                                    [1.0, 1.0, 0.0, 1.0]
                                } else {
                                    [0.0, 1.0, 0.0, 1.0]
                                },
                                if self.handle_pointing == Some(HandlePointing::Z) {
                                    [1.0, 1.0, 0.0, 1.0]
                                } else {
                                    [0.0, 0.0, 1.0, 1.0]
                                },
                                if self.handle_pointing == Some(HandlePointing::All) {
                                    [1.0, 1.0, 0.0, 1.0]
                                } else {
                                    [1.0, 1.0, 1.0, 1.0]
                                },
                                [1.0, 1.0, 1.0, 1.0],
                            ],
                        )
                        .bind_vertex_buffer_array(
                            0,
                            &[unsafe { br::VkHandleRef::dangling(self.internal_mesh_buffer) }],
                            &[self.scale_handle_vbuf_range.start],
                        )
                        .bind_index_buffer(
                            br::VkHandleRef::from_raw_ref(&self.internal_mesh_buffer),
                            self.scale_handle_ibuf_range.start as _,
                            br::IndexType::U16,
                        )
                        .draw_indexed(SCALE_HANDLE_ICOUNT as _, 1, 0, 0, 0),
                })
                .end_render_pass()
                .end()
                .expect("preview.validate.command_buffer.end");
        }

        self.active_rt_size = active_rt.size;
        self.active_framebuffer_resource_handle = active_rt.image_view;
        self.valid = true;
    }

    #[inline(always)]
    pub fn take_pending_update_command_buffer<'a>(
        &'a mut self,
    ) -> Option<br::VkHandleRef<'a, br::vk::VkCommandBuffer>> {
        if core::mem::replace(&mut self.update_command_pending, false) {
            Some(unsafe { br::VkHandleRef::dangling(self.update_command_buffer) })
        } else {
            None
        }
    }

    #[inline(always)]
    pub const fn main_subpass<'a>(
        &'a self,
    ) -> br::SubpassRef<'a, br::VkHandleRef<'a, br::vk::VkRenderPass>> {
        br::SubpassRef(br::VkHandleRef::from_raw_ref(&self.render_pass), 0)
    }

    #[inline(always)]
    pub const fn update_command_buffer<'a>(
        &'a self,
    ) -> br::VkHandleRef<'a, br::vk::VkCommandBuffer> {
        unsafe { br::VkHandleRef::dangling(self.update_command_buffer) }
    }

    #[inline(always)]
    pub const fn command_buffer<'a>(&'a self) -> br::VkHandleRef<'a, br::vk::VkCommandBuffer> {
        unsafe { br::VkHandleRef::dangling(self.command_buffer) }
    }

    // pub fn write_streaming_buffer_content(
    //     &mut self,
    //     device: &VulkanDevice,
    //     data: PreviewStreamingBufferContent,
    // ) {
    //     let ptr = unsafe {
    //         br::vkfn_wrapper::map_memory(
    //             device.as_transparent_ref(),
    //             br::VkHandleRefMut::dangling(self.streaming_memory),
    //             0..size_of::<PreviewStreamingBufferContent>() as _,
    //             0,
    //         )
    //         .expect("preview.write_streaming_buffer_content.map")
    //     };
    //     unsafe {
    //         ptr.cast::<PreviewStreamingBufferContent>().write(data);
    //     }
    //     if self.streaming_memory_should_flush {
    //         br::vkfn_wrapper::flush_mapped_memory_ranges(
    //             device.as_transparent_ref(),
    //             &[br::MappedMemoryRange::new(
    //                 br::VkHandleRef::from_raw_ref(&self.streaming_memory),
    //                 0..size_of::<PreviewStreamingBufferContent>() as _,
    //             )],
    //         )
    //         .expect("preview.write_streaming_buffer_content.flush");
    //     }
    //     unsafe {
    //         br::vkfn_wrapper::unmap_memory(
    //             device.as_transparent_ref(),
    //             br::VkHandleRefMut::dangling(self.streaming_memory),
    //         );
    //     }
    // }
}

pub struct Composite {
    descriptor_set_layout: br::vk::VkDescriptorSetLayout,
    descriptor_pool: br::vk::VkDescriptorPool,
    descriptor_set: br::DescriptorSet,
    descriptor_bound_resource_handle: br::vk::VkImageView,
    pipeline_layout: br::vk::VkPipelineLayout,
    shader: br::vk::VkShaderModule,
    pipeline: core::mem::MaybeUninit<br::vk::VkPipeline>,
    pipeline_target_rt_size: br::Extent2D,
    pipeline_target_render_pass_handle: br::vk::VkRenderPass,
    pipeline_target_subpass: u32,
    valid: bool,
}
impl Composite {
    pub unsafe fn drop(self, vk_device: &Graphics) {
        if self.valid {
            drop(unsafe { br::PipelineObject::manage(self.pipeline.assume_init(), vk_device) });
        }

        drop(unsafe { br::DescriptorPoolObject::manage(self.descriptor_pool, vk_device) });
        drop(unsafe { br::ShaderModuleObject::manage(self.shader, vk_device) });
        drop(unsafe { br::PipelineLayoutObject::manage(self.pipeline_layout, vk_device) });
        drop(unsafe {
            br::DescriptorSetLayoutObject::manage(self.descriptor_set_layout, vk_device)
        });
    }

    pub fn new(
        vk_device: &Graphics,
        init_render_tex: &(impl br::VkHandle<Handle = br::vk::VkImageView> + ?Sized),
        smp: &(impl br::VkHandle<Handle = br::vk::VkSampler> + ?Sized),
        target_pass: br::SubpassRef<impl br::VkHandle<Handle = br::vk::VkRenderPass> + ?Sized>,
        init_screen_size: br::Extent2D,
    ) -> Self {
        let descriptor_set_layout = br::DescriptorSetLayoutObject::new(
            vk_device,
            &br::DescriptorSetLayoutCreateInfo::new(&[br::DescriptorType::CombinedImageSampler
                .make_binding(0, 1)
                .with_immutable_samplers(&[smp.as_transparent_ref()])]),
        )
        .expect("preview_composite.descriptor_set_layout.create");
        let pipeline_layout = br::PipelineLayoutObject::new(
            vk_device,
            &br::PipelineLayoutCreateInfo::new(
                &[descriptor_set_layout.as_transparent_ref()],
                &[br::PushConstantRange::for_type::<CompositePushConstants>(
                    br::vk::VK_SHADER_STAGE_VERTEX_BIT | br::vk::VK_SHADER_STAGE_FRAGMENT_BIT,
                    0,
                )],
            ),
        )
        .expect("preview_composite.pipeline_layout.create");
        let shader = vk_device.require_shader("simple_blit.spv");

        let mut descriptor_pool = br::DescriptorPoolObject::new(
            vk_device,
            &br::DescriptorPoolCreateInfo::new(
                1,
                &[br::DescriptorType::CombinedImageSampler.make_size(1)],
            ),
        )
        .expect("preview_composite.descriptor_pool.create");
        let [descriptor_set] = descriptor_pool
            .alloc_array(&[descriptor_set_layout.as_transparent_ref()])
            .expect("preview_composite.descriptor_set.alloc");
        vk_device.update_descriptor_sets(
            &[descriptor_set
                .binding_at(0)
                .write(br::DescriptorContents::combined_image_sampler(
                    init_render_tex,
                    br::ImageLayout::ShaderReadOnlyOpt,
                ))],
            &[],
        );

        let (descriptor_pool, _) = descriptor_pool.unmanage();
        let (shader, _) = shader.unmanage();
        let (pipeline_layout, _) = pipeline_layout.unmanage();
        let (descriptor_set_layout, _) = descriptor_set_layout.unmanage();
        Self {
            descriptor_set_layout,
            descriptor_pool,
            descriptor_set,
            descriptor_bound_resource_handle: init_render_tex.native_ptr(),
            pipeline_layout,
            shader,
            pipeline_target_rt_size: init_screen_size,
            pipeline_target_render_pass_handle: target_pass.0.native_ptr(),
            pipeline_target_subpass: target_pass.1,
            pipeline: core::mem::MaybeUninit::uninit(),
            valid: false,
        }
    }

    pub fn force_invalidate_descriptor_set_state(&mut self) {
        #[allow(invalid_value)]
        {
            self.descriptor_bound_resource_handle = unsafe { core::mem::transmute(0u64) };
        }
    }

    pub fn validate(
        &mut self,
        device: &Graphics,
        content_rt: &PreviewRenderTargetBuffer,
        new_rt_size: br::Extent2D,
        new_target_render_pass_handle: br::vk::VkRenderPass,
        new_target_subpass: u32,
    ) {
        if !self.valid
            || self.pipeline_target_rt_size != new_rt_size
            || self.pipeline_target_render_pass_handle != new_target_render_pass_handle
            || self.pipeline_target_subpass != new_target_subpass
        {
            if self.valid {
                drop(unsafe { br::PipelineObject::manage(self.pipeline.assume_init(), device) });
            }

            let [pipeline] = device
                .create_graphics_pipelines_array(&[br::GraphicsPipelineCreateInfo::new(
                    br::VkHandleRef::from_raw_ref(&self.pipeline_layout),
                    br::SubpassRef(
                        br::VkHandleRef::from_raw_ref(&new_target_render_pass_handle),
                        new_target_subpass,
                    ),
                    &[
                        br::PipelineShaderStage::new(
                            br::ShaderStage::Vertex,
                            br::VkHandleRef::from_raw_ref(&self.shader),
                            c"vertMain",
                        ),
                        br::PipelineShaderStage::new(
                            br::ShaderStage::Fragment,
                            br::VkHandleRef::from_raw_ref(&self.shader),
                            c"fragMain",
                        ),
                    ],
                    VI_STATE_EMPTY,
                    IA_STATE_TRISTRIP,
                    &br::PipelineViewportStateCreateInfo::new(
                        &[new_rt_size
                            .into_rect(br::Offset2D::ZERO)
                            .make_viewport(0.0..1.0)],
                        &[new_rt_size.into_rect(br::Offset2D::ZERO)],
                    ),
                    RASTER_STATE_DEFAULT_FILL_NOCULL,
                    BLEND_STATE_SINGLE_NONE,
                )
                .set_multisample_state(MS_STATE_EMPTY)])
                .expect("preview_composite.pipeline.create");

            self.pipeline.write(pipeline.unmanage().0);
            self.pipeline_target_rt_size = new_rt_size;
            self.pipeline_target_render_pass_handle = new_target_render_pass_handle;
            self.pipeline_target_subpass = new_target_subpass;
        }

        if self.descriptor_bound_resource_handle != content_rt.image_view {
            device.update_descriptor_sets(
                &[self.descriptor_set.binding_at(0).write(
                    br::DescriptorContents::combined_image_sampler(
                        br::VkHandleRef::from_raw_ref(&content_rt.image_view),
                        br::ImageLayout::ShaderReadOnlyOpt,
                    ),
                )],
                &[],
            );

            self.descriptor_bound_resource_handle = content_rt.image_view;
        }

        self.valid = true;
    }

    pub fn populate_commands<'r>(
        &self,
        size: Size<PixelsUnit>,
        position_modifier_matrix: Matrix4<SafeF32>,
        ctx: &CustomRenderContext,
        rec: br::CmdRecord<'r>,
    ) -> br::CmdRecord<'r> {
        debug_assert!(self.valid);

        rec.bind_pipeline(
            br::PipelineBindPoint::Graphics,
            br::VkHandleRef::from_raw_ref(unsafe { self.pipeline.assume_init_ref() }),
        )
        .push_constant(
            br::VkHandleRef::from_raw_ref(&self.pipeline_layout),
            br::vk::VK_SHADER_STAGE_VERTEX_BIT | br::vk::VK_SHADER_STAGE_FRAGMENT_BIT,
            0,
            &CompositePushConstants {
                position_modifier_matrix: position_modifier_matrix.transpose(),
                element_size: [size.width as _, size.height as _],
                screen_size: [ctx.rt_size.width as _, ctx.rt_size.height as _],
            },
        )
        .bind_descriptor_sets(
            br::PipelineBindPoint::Graphics,
            &br::VkHandleRef::from_raw_ref(&self.pipeline_layout),
            0,
            &[self.descriptor_set],
            &[],
        )
        .draw(4, 1, 0, 0)
    }
}

#[repr(C)]
struct CompositePushConstants {
    pub position_modifier_matrix: Matrix4<SafeF32>,
    pub element_size: [f32; 2],
    pub screen_size: [f32; 2],
}
