use bedrock::{self as br, CommandBufferMut, DescriptorPoolMut, Fence, RenderPass, VkHandle};
use br::resources::Image;
use br::Device;
use log::*;
use parking_lot::RwLock;
use peridot::math::{Camera, Matrix4, Matrix4F32, One, ProjectionMethod, Quaternion, Vector3};
use peridot::PlatformPresenter;
use peridot::{
    audio::StreamingPlayableWav, CBSubmissionType, CommandBundle, SubpassDependencyTemplates,
};
use peridot_math::Zero;
use peridot_memory_manager::{BufferMapMode, MemoryManager};
use peridot_semantic_shader::{ShaderPackAsset, VertexInputSemantic};
use std::sync::Arc;

use peridot_command_object::{
    BeginRenderPass, BindGraphicsPipeline, BufferImageDataDesc, BufferUsage,
    ColorAttachmentBlending, CopyBufferToImage, DescriptorSets, EndRenderPass, GraphicsCommand,
    GraphicsCommandCombiner, ImageResourceRange, PipelineBarrier, RangedBuffer, RangedImage,
    StandardMesh,
};

struct LocalImageView {
    handle: br::vk::VkImageView,
    device: peridot::VulkanGfx,
}
impl Drop for LocalImageView {
    fn drop(&mut self) {
        unsafe {
            br::vkfn_wrapper::destroy_image_view(self.device.native_ptr(), self.handle, None);
        }
    }
}
impl br::VkHandle for LocalImageView {
    type Handle = br::vk::VkImageView;

    fn native_ptr(&self) -> Self::Handle {
        self.handle
    }
}

enum MeshDataBuffer {
    Staged {
        device_buffer: peridot_memory_manager::Buffer,
        staging_buffer: peridot_memory_manager::Buffer,
        staging_mapped_ptr: Option<core::ptr::NonNull<u8>>,
        is_dirty: bool,
        byte_length: usize,
        element_count: usize,
    },
    Streamed {
        direct_buffer: peridot_memory_manager::Buffer,
        mapped_ptr: Option<core::ptr::NonNull<u8>>,
        byte_length: usize,
        element_count: usize,
    },
}
impl MeshDataBuffer {
    pub fn bound_buffer_object(&self) -> br::VkHandleRef<br::vk::VkBuffer> {
        match self {
            Self::Staged {
                ref device_buffer, ..
            } => device_buffer.as_transparent_ref(),
            Self::Streamed {
                ref direct_buffer, ..
            } => direct_buffer.as_transparent_ref(),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum MeshDataBufferType {
    Staged,
    Streamed,
}
impl Default for MeshDataBufferType {
    #[inline(always)]
    fn default() -> Self {
        Self::Staged
    }
}

struct VertexAttribute {
    pub semantic: peridot_semantic_shader::VertexInputSemantic,
    pub buffer_index: usize,
    pub format: br::Format,
}

const fn align(x: usize, a: usize) -> usize {
    ((x + a - 1) / a) * a
}

struct Mesh {
    vertex_buffers: Vec<MeshDataBuffer>,
    index_buffer: Option<MeshDataBuffer>,
    vertex_layout: Vec<(VertexAttribute, usize)>,
    vk_vertex_input_attributes: Vec<br::VertexInputAttributeDescription>,
    vk_vertex_input_bindings: Vec<br::VertexInputBindingDescription>,
    index_layout: Option<br::IndexType>,
    submesh_ranges: Vec<core::ops::Range<usize>>,
    is_dirty: bool,
}
impl Drop for Mesh {
    fn drop(&mut self) {
        unsafe {
            self.unmap_if_mapped();
        }
    }
}
impl Mesh {
    pub fn new() -> Self {
        Self {
            vertex_buffers: Vec::new(),
            index_buffer: None,
            vertex_layout: Vec::new(),
            vk_vertex_input_attributes: Vec::new(),
            vk_vertex_input_bindings: Vec::new(),
            index_layout: None,
            submesh_ranges: Vec::new(),
            is_dirty: false,
        }
    }

    pub fn set_vertex_layout(
        &mut self,
        g: &peridot::Graphics,
        mm: &mut peridot_memory_manager::MemoryManager,
        layout: Vec<VertexAttribute>,
        element_count: usize,
    ) {
        let mut byte_size_per_buffer = Vec::with_capacity(4);
        let mut layout_offset = Vec::with_capacity(layout.len());
        for x in layout.iter() {
            while byte_size_per_buffer.len() <= x.buffer_index {
                byte_size_per_buffer.push((0, 1));
            }

            const fn align_size<T>() -> (usize, usize) {
                (core::mem::align_of::<T>(), core::mem::size_of::<T>())
            }

            let (alignment, size) = match x.format {
                br::vk::VK_FORMAT_R8_UNORM
                | br::vk::VK_FORMAT_R8_USCALED
                | br::vk::VK_FORMAT_R8_UINT
                | br::vk::VK_FORMAT_R8_SRGB => align_size::<u8>(),
                br::vk::VK_FORMAT_R8_SNORM
                | br::vk::VK_FORMAT_R8_SSCALED
                | br::vk::VK_FORMAT_R8_SINT => align_size::<i8>(),
                br::vk::VK_FORMAT_R8G8_UNORM
                | br::vk::VK_FORMAT_R8G8_USCALED
                | br::vk::VK_FORMAT_R8G8_UINT
                | br::vk::VK_FORMAT_R8G8_SRGB => align_size::<[u8; 2]>(),
                br::vk::VK_FORMAT_R8G8_SNORM
                | br::vk::VK_FORMAT_R8G8_SSCALED
                | br::vk::VK_FORMAT_R8G8_SINT => align_size::<[i8; 2]>(),
                br::vk::VK_FORMAT_R8G8B8A8_UNORM
                | br::vk::VK_FORMAT_R8G8B8A8_USCALED
                | br::vk::VK_FORMAT_R8G8B8A8_UINT
                | br::vk::VK_FORMAT_R8G8B8A8_SRGB => align_size::<[u8; 4]>(),
                br::vk::VK_FORMAT_R8G8B8A8_SNORM
                | br::vk::VK_FORMAT_R8G8B8A8_SSCALED
                | br::vk::VK_FORMAT_R8G8B8A8_SINT => align_size::<[i8; 4]>(),
                br::vk::VK_FORMAT_R32_SFLOAT => align_size::<f32>(),
                br::vk::VK_FORMAT_R32_SINT => align_size::<i32>(),
                br::vk::VK_FORMAT_R32_UINT => align_size::<u32>(),
                br::vk::VK_FORMAT_R32G32_SFLOAT => align_size::<[f32; 2]>(),
                br::vk::VK_FORMAT_R32G32_SINT => align_size::<[i32; 2]>(),
                br::vk::VK_FORMAT_R32G32_UINT => align_size::<[u32; 2]>(),
                br::vk::VK_FORMAT_R32G32B32_SFLOAT => align_size::<[f32; 3]>(),
                br::vk::VK_FORMAT_R32G32B32_SINT => align_size::<[i32; 3]>(),
                br::vk::VK_FORMAT_R32G32B32_UINT => align_size::<[u32; 3]>(),
                br::vk::VK_FORMAT_R32G32B32A32_SFLOAT => align_size::<[f32; 4]>(),
                br::vk::VK_FORMAT_R32G32B32A32_SINT => align_size::<[i32; 4]>(),
                br::vk::VK_FORMAT_R32G32B32A32_UINT => align_size::<[u32; 4]>(),
                _ => unimplemented!("vertex attribute format"),
            };

            let top = align(byte_size_per_buffer[x.buffer_index].0, alignment);
            byte_size_per_buffer[x.buffer_index] = (
                top + size,
                num_integer::lcm(byte_size_per_buffer[x.buffer_index].1, alignment),
            );
            layout_offset.push(top);
        }

        self.vk_vertex_input_bindings = byte_size_per_buffer
            .iter()
            .enumerate()
            .map(|(n, (x, a))| {
                br::VertexInputBindingDescription::per_vertex(n as _, align(*x, *a) as _)
            })
            .collect();
        self.vertex_layout = layout.into_iter().zip(layout_offset.into_iter()).collect();
        for x in self.vertex_buffers.drain(..) {
            match x {
                MeshDataBuffer::Staged {
                    mut staging_buffer,
                    staging_mapped_ptr: Some(_),
                    ..
                } => unsafe {
                    staging_buffer.unmap_raw();
                },
                MeshDataBuffer::Streamed {
                    mut direct_buffer,
                    mapped_ptr: Some(_),
                    ..
                } => unsafe {
                    direct_buffer.unmap_raw();
                },
                _ => (),
            }
        }
        self.vertex_buffers.reserve(
            byte_size_per_buffer
                .len()
                .saturating_sub(self.vertex_buffers.len()),
        );
        for (n, (x, a)) in byte_size_per_buffer.into_iter().enumerate() {
            if x == 0 {
                eprintln!("buffer #{n} is zero-sized stride");
            }

            let element_size = align(x, a);

            self.vertex_buffers.push(MeshDataBuffer::Staged {
                device_buffer: mm
                    .allocate_device_local_buffer(
                        g,
                        br::BufferCreateInfo::new(
                            element_size * element_count,
                            br::BufferUsage::VERTEX_BUFFER.transfer_dest(),
                        ),
                    )
                    .expect("Failed to create device vertex buffer"),
                staging_buffer: mm
                    .allocate_upload_buffer(
                        g,
                        br::BufferCreateInfo::new(
                            element_size * element_count,
                            br::BufferUsage::TRANSFER_SRC,
                        ),
                    )
                    .expect("Failed to create staging vertex buffer"),
                staging_mapped_ptr: None,
                is_dirty: false,
                byte_length: element_size * element_count,
                element_count,
            });
        }
        self.vertex_buffers.shrink_to_fit();
    }

    pub fn set_index_layout(
        &mut self,
        g: &peridot::Graphics,
        mm: &mut peridot_memory_manager::MemoryManager,
        layout: br::IndexType,
        element_count: usize,
    ) {
        let byte_size = match layout {
            br::IndexType::U16 => element_count * 2,
            br::IndexType::U32 => element_count * 4,
        };

        self.index_layout = Some(layout);
        let old_index_buffer = self.index_buffer.replace(MeshDataBuffer::Staged {
            device_buffer: mm
                .allocate_device_local_buffer(
                    g,
                    br::BufferCreateInfo::new(
                        byte_size,
                        br::BufferUsage::INDEX_BUFFER.transfer_dest(),
                    ),
                )
                .expect("Failed to create device index buffer"),
            staging_buffer: mm
                .allocate_upload_buffer(
                    g,
                    br::BufferCreateInfo::new(byte_size, br::BufferUsage::TRANSFER_SRC),
                )
                .expect("Failed to create staging index buffer"),
            staging_mapped_ptr: None,
            is_dirty: false,
            byte_length: byte_size,
            element_count,
        });
        // pre-drop old buffers
        match old_index_buffer {
            Some(MeshDataBuffer::Staged {
                mut staging_buffer,
                staging_mapped_ptr,
                ..
            }) if staging_mapped_ptr.is_some() => unsafe {
                staging_buffer.unmap_raw();
            },
            Some(MeshDataBuffer::Streamed {
                mut direct_buffer,
                mapped_ptr,
                ..
            }) if mapped_ptr.is_some() => unsafe {
                direct_buffer.unmap_raw();
            },
            _ => (),
        }
    }

    pub fn set_submesh_ranges(&mut self, ranges: Vec<core::ops::Range<usize>>) {
        self.submesh_ranges = ranges;
    }

    pub fn vk_vertex_input_attributes(
        &self,
        semantic_resolver: &peridot_semantic_shader::ShaderPack<peridot::VulkanGfx>,
    ) -> Vec<br::VertexInputAttributeDescription> {
        self.vertex_layout
            .iter()
            .map(|&(ref x, o)| br::VertexInputAttributeDescription {
                location: semantic_resolver
                    .resolve_input_semantic_location(x.semantic.clone())
                    .expect("Cannot resolve semantic to index"),
                binding: x.buffer_index as _,
                format: x.format,
                offset: o as _,
            })
            .collect()
    }

    pub fn modify_vertex_buffer<T>(
        &mut self,
        index: usize,
        allow_readback: bool,
        op: impl FnOnce(core::ptr::NonNull<T>),
    ) {
        unsafe {
            self.unmap_if_mapped();
        }

        match self.vertex_buffers[index] {
            MeshDataBuffer::Staged {
                ref mut staging_buffer,
                ref mut is_dirty,
                ..
            } => {
                staging_buffer
                    .guard_map(
                        if allow_readback {
                            peridot_memory_manager::BufferMapMode::ReadWrite
                        } else {
                            peridot_memory_manager::BufferMapMode::Write
                        },
                        move |p| op(p.ptr().cast()),
                    )
                    .expect("Failed to write contents");

                *is_dirty = true;
                self.is_dirty = true;
            }
            MeshDataBuffer::Streamed {
                ref mut direct_buffer,
                ..
            } => direct_buffer
                .guard_map(
                    if allow_readback {
                        peridot_memory_manager::BufferMapMode::ReadWrite
                    } else {
                        peridot_memory_manager::BufferMapMode::Write
                    },
                    move |p| op(p.ptr().cast()),
                )
                .expect("Failed to write contents"),
        }
    }

    pub unsafe fn get_vertex_buffer_pointer<T>(
        &mut self,
        index: usize,
        allow_readback: bool,
    ) -> core::ptr::NonNull<T> {
        match self.vertex_buffers[index] {
            MeshDataBuffer::Staged {
                staging_mapped_ptr: Some(ptr),
                ..
            } => ptr.cast(),
            MeshDataBuffer::Staged {
                ref mut staging_buffer,
                ref mut staging_mapped_ptr,
                byte_length,
                ..
            } => {
                let p = unsafe {
                    staging_buffer
                        .map_raw(0..byte_length as _)
                        .expect("Failed to map buffer")
                };
                if allow_readback && staging_buffer.requires_explicit_sync() {
                    unsafe {
                        staging_buffer
                            .invalidate_ranges_raw(&[0..byte_length as _])
                            .expect("Failed to invalidate mapped contents");
                    }
                }

                *staging_mapped_ptr = Some(p.ptr());
                p.ptr().cast()
            }
            MeshDataBuffer::Streamed {
                mapped_ptr: Some(ptr),
                ..
            } => ptr.cast(),
            MeshDataBuffer::Streamed {
                ref mut direct_buffer,
                ref mut mapped_ptr,
                byte_length,
                ..
            } => {
                let p = unsafe {
                    direct_buffer
                        .map_raw(0..byte_length as _)
                        .expect("Faield to map buffer")
                };
                if allow_readback && direct_buffer.requires_explicit_sync() {
                    unsafe {
                        direct_buffer
                            .invalidate_ranges_raw(&[0..byte_length as _])
                            .expect("Faield to invalidate mapped contents");
                    }
                }

                *mapped_ptr = Some(p.ptr());
                p.ptr().cast()
            }
        }
    }

    pub fn mark_vertex_buffer_dirty(&mut self, index: usize) {
        match self.vertex_buffers[index] {
            MeshDataBuffer::Staged {
                ref mut is_dirty, ..
            } => {
                *is_dirty = true;
            }
            _ => (),
        }

        self.is_dirty = true;
    }

    pub fn sync_contents(&mut self, e: &mut peridot::Graphics) {
        if !self.is_dirty {
            return;
        }

        unsafe {
            self.unmap_if_mapped();
        }

        // TODO: あとでcommand bufferを個別に生成しなくても良いようにする（Engine側の改修が必要になる かうまい仕組みをここで考えたい）
        e.submit_commands(|mut rec| {
            for x in self.vertex_buffers.iter_mut() {
                match x {
                    &mut MeshDataBuffer::Staged {
                        ref mut is_dirty,
                        ref device_buffer,
                        ref staging_buffer,
                        byte_length,
                        ..
                    } if *is_dirty => {
                        rec = rec.copy_buffer(
                            staging_buffer,
                            device_buffer,
                            &[br::BufferCopy::mirror(0, byte_length as _)],
                        );

                        *is_dirty = false;
                    }
                    _ => (),
                }
            }
            match self.index_buffer {
                Some(MeshDataBuffer::Staged {
                    ref mut is_dirty,
                    ref device_buffer,
                    ref staging_buffer,
                    byte_length,
                    ..
                }) if *is_dirty => {
                    rec = rec.copy_buffer(
                        staging_buffer,
                        device_buffer,
                        &[br::BufferCopy::mirror(0, byte_length as _)],
                    );

                    *is_dirty = false;
                }
                _ => (),
            }

            rec.pipeline_barrier(
                br::PipelineStageFlags::TRANSFER,
                br::PipelineStageFlags::VERTEX_INPUT,
                0,
                &[br::vk::VkMemoryBarrier {
                    sType: br::vk::VK_STRUCTURE_TYPE_MEMORY_BARRIER,
                    pNext: core::ptr::null(),
                    srcAccessMask: br::AccessFlags::TRANSFER.write,
                    dstAccessMask: br::AccessFlags::VERTEX_ATTRIBUTE_READ,
                }],
                &[],
                &[],
            )
        })
        .expect("Failed to sync mesh contents");

        self.is_dirty = false;
    }

    pub unsafe fn unmap_if_mapped(&mut self) {
        for x in self.vertex_buffers.iter_mut() {
            match x {
                &mut MeshDataBuffer::Staged {
                    ref mut staging_buffer,
                    ref mut staging_mapped_ptr,
                    byte_length,
                    ..
                } => {
                    if staging_mapped_ptr.is_some() {
                        if staging_buffer.requires_explicit_sync() {
                            staging_buffer
                                .flush_ranges_raw(&[0..byte_length as _])
                                .expect("Failed to flush contents");
                        }
                        unsafe {
                            staging_buffer.unmap_raw();
                        }
                        *staging_mapped_ptr = None;
                    }
                }
                &mut MeshDataBuffer::Streamed {
                    ref mut direct_buffer,
                    ref mut mapped_ptr,
                    byte_length,
                    ..
                } => {
                    if mapped_ptr.is_some() {
                        if direct_buffer.requires_explicit_sync() {
                            direct_buffer
                                .flush_ranges_raw(&[0..byte_length as _])
                                .expect("Failed to flush contents");
                        }
                        unsafe {
                            direct_buffer.unmap_raw();
                        }

                        *mapped_ptr = None;
                    }
                }
            }
        }

        match self.index_buffer {
            Some(MeshDataBuffer::Staged {
                ref mut staging_buffer,
                ref mut staging_mapped_ptr,
                ..
            }) => {
                if staging_mapped_ptr.is_some() {
                    unsafe {
                        staging_buffer.unmap_raw();
                    }
                    *staging_mapped_ptr = None;
                }
            }
            Some(MeshDataBuffer::Streamed {
                ref mut direct_buffer,
                ref mut mapped_ptr,
                ..
            }) => {
                if mapped_ptr.is_some() {
                    unsafe {
                        direct_buffer.unmap_raw();
                    }
                    *mapped_ptr = None;
                }
            }
            _ => (),
        }
    }
}

pub async fn game_main<'q>(e: &mut peridot::Engine<'q, impl peridot::NativeLinker>) {
    let screen_size = e.back_buffer_size();
    let screen_aspect = screen_size.0 as f32 / screen_size.1 as f32;

    let image_data: peridot_image::PNG = e.load("images.example").expect("No image found");
    debug!("image: {}x{}", image_data.0.size.x(), image_data.0.size.y());
    debug!("ImageFormat: {:?}", image_data.0.format);
    debug!("ImageStride: {} bytes", image_data.0.stride);

    let bgm = Arc::new(RwLock::new(
        e.streaming::<StreamingPlayableWav>("bgm")
            .expect("Loading BGM"),
    ));
    e.audio_mixer().write().add_process(bgm.clone());
    e.audio_mixer().write().set_master_volume(0.5);

    let mut memory_manager = MemoryManager::new(e.graphics());

    let plane_mesh = peridot::Primitive::uv_plane_centric_xy(1.0, 0.0);
    let mut cam = Camera {
        projection: Some(ProjectionMethod::Perspective {
            fov: 75.0f32.to_radians(),
        }),
        position: Vector3(-4.0, -1.0, -3.0),
        rotation: Quaternion::ONE,
        depth_range: 1.0..10.0,
    };
    cam.look_at(Vector3::ZERO);

    let mut plane_mesh_object = Mesh::new();
    plane_mesh_object.set_vertex_layout(
        e.graphics(),
        &mut memory_manager,
        vec![
            VertexAttribute {
                semantic: peridot_semantic_shader::VertexInputSemantic::Position(0),
                buffer_index: 0,
                format: br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
            },
            VertexAttribute {
                semantic: peridot_semantic_shader::VertexInputSemantic::Texcoord(0),
                buffer_index: 1,
                format: br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
            },
        ],
        plane_mesh.vertices.len(),
    );
    plane_mesh_object.modify_vertex_buffer(0, false, |p| {
        for (n, x) in plane_mesh.vertices.iter().enumerate() {
            unsafe {
                p.add(n).write(x.pos);
            }
        }
    });
    plane_mesh_object.modify_vertex_buffer(1, false, |p| {
        for (n, x) in plane_mesh.vertices.iter().enumerate() {
            unsafe {
                p.add(n).write(x.uv);
            }
        }
    });
    plane_mesh_object.set_submesh_ranges(vec![0..plane_mesh.vertices.len()]);

    let [vertex_buffer, cam_uniform_buffer, obj_uniform_buffer] = memory_manager
        .allocate_device_local_buffer_array(
            e.graphics(),
            [
                br::BufferCreateInfo::new(
                    plane_mesh.byte_length(),
                    br::BufferUsage::VERTEX_BUFFER.transfer_dest(),
                ),
                br::BufferCreateInfo::new_for_type::<UniformCameraParameters>(
                    br::BufferUsage::UNIFORM_BUFFER.transfer_dest(),
                ),
                br::BufferCreateInfo::new_for_type::<UniformObjectParameters>(
                    br::BufferUsage::UNIFORM_BUFFER.transfer_dest(),
                ),
            ],
        )
        .expect("Failed to allocate buffers");
    let vertex_buffer = RangedBuffer::from(vertex_buffer);
    let cam_uniform_buffer = RangedBuffer::from(cam_uniform_buffer);
    let obj_uniform_buffer = RangedBuffer::from(obj_uniform_buffer);
    #[cfg(feature = "debug")]
    e.graphics_device()
        .set_object_name(&vertex_buffer.0, c"Vertex Buffer")
        .expect("Failed to set object name");
    #[cfg(feature = "debug")]
    e.graphics_device()
        .set_object_name(&cam_uniform_buffer.0, c"Uniform Buffer[CameraParameters]")
        .expect("Failed to set object name");
    #[cfg(feature = "debug")]
    e.graphics_device()
        .set_object_name(&obj_uniform_buffer.0, c"Uniform Buffer")
        .expect("Faield to set object name");

    let [vertex_buffer_stg, cam_uniform_buffer_stg, obj_uniform_mut_buffer] = memory_manager
        .allocate_upload_buffer_array(
            e.graphics(),
            [
                br::BufferCreateInfo::new(
                    vertex_buffer.byte_length() as _,
                    br::BufferUsage::TRANSFER_SRC,
                ),
                br::BufferCreateInfo::new(
                    cam_uniform_buffer.byte_length() as _,
                    br::BufferUsage::TRANSFER_SRC,
                ),
                br::BufferCreateInfo::new(
                    obj_uniform_buffer.byte_length() as _,
                    br::BufferUsage::TRANSFER_SRC,
                ),
            ],
        )
        .expect("Failed to allocate upload buffer");
    let mut vertex_buffer_stg = RangedBuffer::from(vertex_buffer_stg);
    let mut cam_uniform_buffer_stg = RangedBuffer::from(cam_uniform_buffer_stg);
    let mut obj_uniform_mut_buffer = RangedBuffer::from(obj_uniform_mut_buffer);
    vertex_buffer_stg
        .0
        .clone_content_from_slice(&plane_mesh.vertices)
        .expect("Failed to set upload content");
    cam_uniform_buffer_stg
        .0
        .write_content(UniformCameraParameters {
            camera: cam.view_projection_matrix(screen_aspect),
        })
        .expect("Failed to set initial data of camera uniform buffer");
    obj_uniform_mut_buffer
        .0
        .write_content(UniformObjectParameters {
            object: Matrix4::ONE,
        })
        .expect("Failed to set initial data of object uniform buffer");

    let image = memory_manager
        .allocate_device_local_image(
            e.graphics(),
            br::ImageCreateInfo::new(image_data.0.size, image_data.0.format as _)
                .sampled()
                .transfer_dest()
                .init_layout(br::ImageLayout::Preinitialized),
        )
        .expect("Failed to allocate main image");
    let mut image_data_stg_buffer = memory_manager
        .allocate_upload_linear_image_buffer(
            e.graphics(),
            *image_data.0.size.x(),
            *image_data.0.size.y(),
            image_data.0.format,
            br::BufferUsage::TRANSFER_SRC,
        )
        .expect("Failed to allocate linear image buffer");
    image_data_stg_buffer
        .copy_content_from_slice(image_data.0.u8_pixels())
        .expect("Failed to set image data");

    let pre_configure_awaiter = e
        .submit_commands_async(|r| {
            let texture = RangedImage::single_color_plane(&image);
            let image_data_stg_buffer_ranged = RangedBuffer::from(&image_data_stg_buffer.inner);

            let [mut_uniform_in_barrier, mut_uniform_out_barrier] = obj_uniform_mut_buffer
                .make_ref()
                .usage_barrier3_switching(BufferUsage::HOST_RW, BufferUsage::TRANSFER_SRC);
            let [tex_init_barrier, tex_ready_barrier] = texture.barrier3(
                br::ImageLayout::Preinitialized,
                br::ImageLayout::TransferDestOpt,
                br::ImageLayout::ShaderReadOnlyOpt,
            );

            let in_barriers = PipelineBarrier::new()
                .with_barriers([
                    mut_uniform_in_barrier,
                    obj_uniform_buffer
                        .make_ref()
                        .usage_barrier(BufferUsage::UNUSED, BufferUsage::TRANSFER_DST),
                    cam_uniform_buffer_stg
                        .make_ref()
                        .usage_barrier(BufferUsage::HOST_RW, BufferUsage::TRANSFER_SRC),
                    vertex_buffer_stg
                        .make_ref()
                        .usage_barrier(BufferUsage::HOST_RW, BufferUsage::TRANSFER_SRC),
                    vertex_buffer
                        .make_ref()
                        .usage_barrier(BufferUsage::UNUSED, BufferUsage::TRANSFER_DST),
                    image_data_stg_buffer_ranged
                        .usage_barrier(BufferUsage::HOST_RW, BufferUsage::TRANSFER_SRC),
                ])
                .with_barrier(tex_init_barrier)
                .by_region();
            let out_barriers = PipelineBarrier::new()
                .with_barriers([
                    vertex_buffer
                        .make_ref()
                        .usage_barrier(BufferUsage::TRANSFER_DST, BufferUsage::VERTEX_BUFFER),
                    cam_uniform_buffer_stg
                        .make_ref()
                        .usage_barrier(BufferUsage::TRANSFER_DST, BufferUsage::VERTEX_UNIFORM),
                    mut_uniform_out_barrier,
                    obj_uniform_buffer
                        .make_ref()
                        .usage_barrier(BufferUsage::TRANSFER_DST, BufferUsage::VERTEX_UNIFORM),
                ])
                .with_barrier(tex_ready_barrier)
                .by_region();
            let init_vertex = vertex_buffer.byref_mirror_from(&vertex_buffer_stg);
            let init_cam_uniform = cam_uniform_buffer.byref_mirror_from(&cam_uniform_buffer_stg);
            let init_obj_uniform = obj_uniform_buffer.byref_mirror_from(&obj_uniform_mut_buffer);
            let init_tex = CopyBufferToImage::new(&image_data_stg_buffer.inner, &image).with_range(
                BufferImageDataDesc::new(0, image_data_stg_buffer.row_texels),
                ImageResourceRange::for_single_color_from_rect2d(
                    image.size().wh().into_rect(br::vk::VkOffset2D::ZERO),
                ),
            );
            let copies = (init_vertex, init_cam_uniform, init_obj_uniform, init_tex);

            copies.between(in_barriers, out_barriers).execute(r)
        })
        .expect("Failed to submit pre-configure commands");

    let mut update_cb =
        CommandBundle::new(&e.graphics(), CBSubmissionType::Graphics, 1).expect("Alloc UpdateCB");
    {
        let uniform_buffer_ref = obj_uniform_buffer.make_ref();
        let uniform_mut_buffer_ref = obj_uniform_mut_buffer.make_ref();

        let [uniform_in_barrier, uniform_out_barrier] = uniform_buffer_ref
            .usage_barrier3_switching(BufferUsage::VERTEX_UNIFORM, BufferUsage::TRANSFER_DST);
        let [staging_uniform_in_barrier, staging_uniform_out_barrier] = uniform_mut_buffer_ref
            .usage_barrier3_switching(BufferUsage::HOST_RW, BufferUsage::TRANSFER_SRC);

        let in_barriers = [uniform_in_barrier, staging_uniform_in_barrier];
        let out_barriers = [uniform_out_barrier, staging_uniform_out_barrier];
        let copy_uniform = obj_uniform_buffer.byref_mirror_from(&obj_uniform_mut_buffer);

        copy_uniform
            .between(in_barriers, out_barriers)
            .execute_and_finish(
                update_cb
                    .synchronized_nth(0)
                    .begin(&br::CommandBufferBeginInfo::new(), e.graphics().device())
                    .expect("Failed to begin recording update command"),
            )
            .expect("Failed to record update commands");
    }

    let back_buffer_attachment = e
        .back_buffer_attachment_desc()
        .color_memory_op(br::LoadOp::Clear, br::StoreOp::Store);
    let color_outputs = [br::vk::VkAttachmentReference::new(
        0,
        br::ImageLayout::ColorAttachmentOpt,
    )];
    let color_render_subpass = br::SubpassDescription::new().color_attachments(&color_outputs, &[]);
    let renderpass = br::RenderPassObject::new(
        e.graphics().device().clone(),
        &br::RenderPassCreateInfo::new(
            &[back_buffer_attachment],
            &[color_render_subpass],
            &[SubpassDependencyTemplates::to_color_attachment_in(
                None, 0, true,
            )],
        ),
    )
    .expect("Create RenderPass");
    let mut backbuffer_resources = e
        .iter_back_buffers()
        .map(|x| LocalImageView {
            handle: unsafe {
                br::vkfn_wrapper::create_image_view(
                    e.graphics().device().native_ptr(),
                    &br::ImageViewCreateInfo::new(
                        &x,
                        br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
                        br::vk::VK_IMAGE_VIEW_TYPE_2D,
                        e.back_buffer_format(),
                    ),
                    None,
                )
                .expect("Failed to create backbuffer view")
            },
            device: e.graphics().device().clone(),
        })
        .collect::<Vec<_>>();
    let mut framebuffers = backbuffer_resources
        .iter()
        .map(|b| {
            br::FramebufferObject::new(
                e.graphics_device().clone(),
                &br::FramebufferCreateInfo::new(
                    &renderpass,
                    &[b.as_transparent_ref()],
                    screen_size.0,
                    screen_size.1,
                ),
            )
        })
        .collect::<Result<Vec<_>, _>>()
        .expect("Bind Framebuffer");

    let smp = br::SamplerObject::new(e.graphics().device().clone(), &br::SamplerCreateInfo::new())
        .expect("Creating Sampler");
    let dsl_ub1 = br::DescriptorSetLayoutObject::new(
        e.graphics().device().clone(),
        &br::DescriptorSetLayoutCreateInfo::new(&[br::DescriptorType::UniformBuffer
            .make_binding(0, 1)
            .only_for_vertex()]),
    )
    .expect("Create DescriptorSetLayout with UniformBuffer(x1)");
    let descriptor_layout = br::DescriptorSetLayoutObject::new(
        e.graphics().device().clone(),
        &br::DescriptorSetLayoutCreateInfo::new(&[
            br::DescriptorType::UniformBuffer
                .make_binding(0, 1)
                .only_for_vertex(),
            br::DescriptorType::CombinedImageSampler
                .make_binding(1, 1)
                .only_for_fragment()
                .with_immutable_samplers(&[smp.as_transparent_ref()]),
        ]),
    )
    .expect("Create DescriptorSetLayout");
    let mut descriptor_pool = br::DescriptorPoolObject::new(
        e.graphics().device().clone(),
        &br::DescriptorPoolCreateInfo::new(
            2,
            &[
                br::DescriptorType::UniformBuffer.make_size(2),
                br::DescriptorType::CombinedImageSampler.make_size(1),
            ],
        ),
    )
    .expect("Create DescriptorPool");

    let pl = br::PipelineLayoutObject::new(
        e.graphics().device().clone(),
        &br::PipelineLayoutCreateInfo::new(
            &[
                dsl_ub1.as_transparent_ref(),
                descriptor_layout.as_transparent_ref(),
            ],
            &[],
        ),
    )
    .expect("Create PipelineLayout");
    let shader = e
        .load::<ShaderPackAsset>("builtin.semantic_shaders.unlit_image")
        .expect("Loading shader")
        .instantiate(e.graphics().device().clone())
        .expect("Instantiate Shaders");
    let sc = [br::Extent2D::from(screen_size).into_rect(br::Offset2D::ZERO)];
    let vp = [sc[0].make_viewport(0.0..1.0)];
    let [gp] = e
        .graphics()
        .device()
        .new_graphics_pipeline_array(
            &[br::GraphicsPipelineCreateInfo::new(
                &pl,
                renderpass.subpass(0),
                &[
                    shader.pipeline_vertex_shader(),
                    shader.pipeline_fragment_shader().expect("no fsh?"),
                ],
                &br::PipelineVertexInputStateCreateInfo::new(
                    &plane_mesh_object.vk_vertex_input_bindings,
                    &plane_mesh_object.vk_vertex_input_attributes(&shader),
                ),
                &br::PipelineInputAssemblyStateCreateInfo::new(
                    br::PrimitiveTopology::TriangleStrip,
                ),
                &br::PipelineViewportStateCreateInfo::new_array(&vp, &sc),
                &br::PipelineRasterizationStateCreateInfo::new(
                    br::PolygonMode::Fill,
                    br::CullModeFlags::NONE,
                    br::FrontFace::CounterClockwise,
                ),
                &br::PipelineColorBlendStateCreateInfo::new(&[
                    ColorAttachmentBlending::Disabled.into_vk()
                ]),
            )
            .multisample_state(&br::PipelineMultisampleStateCreateInfo::new())],
            None::<&br::PipelineCacheObject<peridot::DeviceObject>>,
        )
        .expect("Create GraphicsPipeline");
    let gp = gp.clone_parent();
    #[cfg(feature = "debug")]
    e.graphics_device()
        .set_object_name(&gp, c"Main Pipeline")
        .expect("Failed to set pipeline name");

    pre_configure_awaiter
        .await
        .expect("Failed to pre-configure resources");

    let image_view = br::ImageViewBuilder::new(
        image,
        br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
    )
    .create()
    .expect("Failed to create main image view");
    let [descriptor_cam, descriptor_main] = descriptor_pool
        .alloc_array(&[
            dsl_ub1.as_transparent_ref(),
            descriptor_layout.as_transparent_ref(),
        ])
        .expect("Create main Descriptor");
    {
        let mut descriptor_writes = Vec::with_capacity(3);
        descriptor_writes.push(descriptor_cam.binding_at(0).write(
            br::DescriptorContents::UniformBuffer(vec![
                cam_uniform_buffer.make_descriptor_buffer_ref(),
            ]),
        ));
        descriptor_writes.extend(
            br::DescriptorPointer::new(descriptor_main.into(), 0).write_continuous_bindings([
                br::DescriptorContents::UniformBuffer(vec![
                    obj_uniform_buffer.make_descriptor_buffer_ref()
                ]),
                br::DescriptorContents::CombinedImageSampler(vec![br::DescriptorImageInfo::new(
                    &image_view,
                    br::ImageLayout::ShaderReadOnlyOpt,
                )]),
            ]),
        );
        e.graphics()
            .device()
            .update_descriptor_sets(&descriptor_writes, &[]);
    }

    struct BufferedFrameRenderingState {
        cb: CommandBundle<peridot::VulkanGfx>,
        completion: br::FenceObject<peridot::VulkanGfx>,
        rendering: bool,
    }
    let mut frame_render_states = (0..e.back_buffer_count())
        .map(|_| {
            let cb = CommandBundle::new(e.graphics(), CBSubmissionType::Graphics, 1)
                .expect("Alloc RenderCB");
            #[cfg(feature = "debug")]
            e.graphics()
                .device()
                .set_object_name(
                    &cb.nth_ref(0),
                    &std::ffi::CString::new(format!("Primary Render Commands #{n}"))
                        .expect("invalid sequence?"),
                )
                .expect("Failed to set render cb name");

            BufferedFrameRenderingState {
                cb,
                completion: br::FenceObject::new(
                    e.graphics().device().clone(),
                    &br::FenceCreateInfo::new(0),
                )
                .expect("Completion Fence creation"),
                rendering: false,
            }
        })
        .collect::<Vec<_>>();

    bgm.write().play();

    let mut frame_sec_samples = [0.0; 640];
    let mut frame_sec_sample_pos = 0;
    let mut frame_sec_collect_timer = std::time::Instant::now();
    let mut rot = 0.0f32;
    loop {
        match e.next_event().await {
            peridot::Event::Shutdown => break,
            peridot::Event::NextFrame => {
                let t0 = std::time::Instant::now();
                let fd = match e.prepare_frame() {
                    Ok(fd) => fd,
                    Err(peridot::PrepareFrameError::FramebufferOutOfDate) => {
                        // resize and do nothing
                        let new_size = e.back_buffer_size();

                        for x in frame_render_states.iter_mut() {
                            if x.rendering {
                                x.completion
                                    .wait()
                                    .expect("Failed to wait previous rendering work");
                                unsafe {
                                    x.cb.reset()
                                        .expect("Failed to reset previous rendering commands");
                                }
                                x.rendering = false;
                            }
                        }
                        drop(framebuffers);
                        drop(backbuffer_resources);

                        e.resize_presenter_backbuffers(new_size);

                        backbuffer_resources = e
                            .iter_back_buffers()
                            .map(|x| LocalImageView {
                                handle: unsafe {
                                    br::vkfn_wrapper::create_image_view(
                                        e.graphics().device().native_ptr(),
                                        &br::ImageViewCreateInfo::new(
                                            &x,
                                            br::ImageSubresourceRange::new(
                                                br::AspectMask::COLOR,
                                                0..1,
                                                0..1,
                                            ),
                                            br::vk::VK_IMAGE_VIEW_TYPE_2D,
                                            e.back_buffer_format(),
                                        ),
                                        None,
                                    )
                                    .expect("Failed to create backbuffer view")
                                },
                                device: e.graphics().device().clone(),
                            })
                            .collect();
                        framebuffers = backbuffer_resources
                            .iter()
                            .map(|b| {
                                br::FramebufferObject::new(
                                    e.graphics_device().clone(),
                                    &br::FramebufferCreateInfo::new(
                                        &renderpass,
                                        &[b.as_transparent_ref()],
                                        new_size.0,
                                        new_size.1,
                                    ),
                                )
                            })
                            .collect::<Result<Vec<_>, _>>()
                            .expect("Bind Framebuffers");

                        continue;
                    }
                };

                let current_render_frame_state =
                    &mut frame_render_states[fd.backbuffer_index as usize];

                if current_render_frame_state.rendering {
                    current_render_frame_state
                        .completion
                        .wait()
                        .expect("Failed to wait previous rendering work");
                    unsafe {
                        current_render_frame_state
                            .cb
                            .reset()
                            .expect("Failed to reset previous rendering commands");
                    }
                    current_render_frame_state.rendering = false;
                }

                let dtsec = fd.delta_time.as_secs() as f32
                    + fd.delta_time.subsec_micros() as f32 / 1000_0000.0;
                rot += dtsec * 15.0;
                let rot = rot;
                obj_uniform_mut_buffer
                    .0
                    .guard_map(BufferMapMode::Write, |ptr| unsafe {
                        ptr.get_mut_at::<UniformObjectParameters>(0).object =
                            Quaternion::new(rot, Vector3::up()).into();
                    })
                    .expect("Update DynamicStgBuffer");

                plane_mesh_object.sync_contents(e.graphics_mut());

                unsafe {
                    current_render_frame_state
                        .cb
                        .nth_ref_mut(0)
                        .begin(&br::CommandBufferBeginInfo::new(), e.graphics().device())
                        .expect("Failed to begin command recording")
                }
                .begin_render_pass(
                    &br::RenderPassBeginInfo::new(
                        &renderpass,
                        &framebuffers[fd.backbuffer_index as usize],
                        br::Extent2D::from(screen_size).into_rect(br::Offset2D::ZERO),
                        &[br::ClearValue::color([0.0; 4])],
                    ),
                    br::SubpassContents::Inline,
                )
                .bind_pipeline(br::PipelineBindPoint::Graphics, &gp)
                .bind_descriptor_sets(
                    br::PipelineBindPoint::Graphics,
                    &pl,
                    0,
                    &[descriptor_cam, descriptor_main],
                    &[],
                )
                .bind_vertex_buffer_array(
                    0,
                    &[
                        plane_mesh_object.vertex_buffers[0].bound_buffer_object(),
                        plane_mesh_object.vertex_buffers[1].bound_buffer_object(),
                    ],
                    &[0, 0],
                )
                .draw(
                    plane_mesh_object.submesh_ranges[0].len() as _,
                    1,
                    plane_mesh_object.submesh_ranges[0].start as _,
                    0,
                )
                .end_render_pass()
                .end()
                .expect("Failed to record render commands");

                let update_cb = update_cb.nth_ref(0);
                let render_cb = current_render_frame_state.cb.nth_ref(0);
                let mut update_batch = peridot::SubmissionBatchBuilder::new();
                update_batch.add_command_buffers([update_cb.as_transparent_ref()]);
                let mut render_batch = peridot::SubmissionBatchBuilder::new();
                render_batch.add_command_buffers([render_cb.as_transparent_ref()]);

                unsafe {
                    e.do_render_to_custom_fence(
                        &mut current_render_frame_state.completion,
                        fd.backbuffer_index,
                        Some(update_batch),
                        render_batch,
                    )
                    .expect("Failed to present");
                }
                current_render_frame_state.rendering = true;

                frame_sec_samples[frame_sec_sample_pos] = t0.elapsed().as_secs_f32();
                frame_sec_sample_pos += 1;

                if frame_sec_collect_timer.elapsed() >= std::time::Duration::from_secs(1) {
                    let avg = frame_sec_samples[..frame_sec_sample_pos]
                        .iter()
                        .sum::<f32>()
                        / frame_sec_sample_pos as f32;
                    println!("frame sec avg: {avg}");

                    frame_sec_collect_timer = std::time::Instant::now();
                    frame_sec_sample_pos = 0;
                }
            }
            peridot::Event::Resize(new_size) => {
                for x in frame_render_states.iter_mut() {
                    if x.rendering {
                        x.completion
                            .wait()
                            .expect("Failed to wait previous rendering work");
                        unsafe {
                            x.cb.reset()
                                .expect("Failed to reset previous rendering commands");
                        }
                        x.rendering = false;
                    }
                }
                drop(framebuffers);
                drop(backbuffer_resources);

                e.resize_presenter_backbuffers(new_size);

                backbuffer_resources = e
                    .iter_back_buffers()
                    .map(|x| LocalImageView {
                        handle: unsafe {
                            br::vkfn_wrapper::create_image_view(
                                e.graphics().device().native_ptr(),
                                &br::ImageViewCreateInfo::new(
                                    &x,
                                    br::vk::VkImageSubresourceRange {
                                        aspectMask: br::AspectMask::COLOR.bits(),
                                        baseMipLevel: 0,
                                        levelCount: 1,
                                        baseArrayLayer: 0,
                                        layerCount: 1,
                                    },
                                    br::vk::VK_IMAGE_VIEW_TYPE_2D,
                                    e.back_buffer_format(),
                                ),
                                None,
                            )
                            .expect("Failed to create backbuffer view")
                        },
                        device: e.graphics().device().clone(),
                    })
                    .collect();
                framebuffers = backbuffer_resources
                    .iter()
                    .map(|b| {
                        br::FramebufferObject::new(
                            e.graphics_device().clone(),
                            &br::FramebufferCreateInfo::new(
                                &renderpass,
                                &[b.as_transparent_ref()],
                                new_size.0,
                                new_size.1,
                            ),
                        )
                    })
                    .collect::<Result<Vec<_>, _>>()
                    .expect("Bind Framebuffers");
            }
        }
    }

    unsafe {
        e.graphics_device().wait().expect("Failed to wait for work");
    }
}

#[repr(C)]
struct UniformCameraParameters {
    pub camera: Matrix4F32,
}

#[repr(C)]
struct UniformObjectParameters {
    pub object: Matrix4F32,
}
