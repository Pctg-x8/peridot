//! Peridot Standard Mesh

use bedrock::{self as br, VkHandle};

enum MeshDataBuffer {
    Staged {
        device_buffer: peridot_memory_manager::Buffer,
        staging_buffer: peridot_memory_manager::Buffer,
        staging_mapped_ptr: Option<core::ptr::NonNull<u8>>,
        is_dirty: bool,
        byte_length: usize,
    },
    Streamed {
        direct_buffer: peridot_memory_manager::Buffer,
        mapped_ptr: Option<core::ptr::NonNull<u8>>,
        byte_length: usize,
    },
}
impl MeshDataBuffer {
    pub fn bound_buffer_object(&self) -> br::VkHandleRef<br::vk::VkBuffer> {
        match self {
            Self::Staged { device_buffer, .. } => device_buffer.as_transparent_ref(),
            Self::Streamed { direct_buffer, .. } => direct_buffer.as_transparent_ref(),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum MeshDataBufferType {
    Staged,
    Streaming,
}
impl Default for MeshDataBufferType {
    #[inline(always)]
    fn default() -> Self {
        Self::Staged
    }
}

pub struct VertexAttribute {
    pub semantic: peridot_semantic_shader::VertexInputSemantic,
    pub buffer_index: usize,
    pub format: br::Format,
}

const fn align(x: usize, a: usize) -> usize {
    ((x + a - 1) / a) * a
}

struct MeshIndexBufferState {
    buffer: MeshDataBuffer,
    layout: br::IndexType,
}

pub struct MeshVertexConfig {
    pub layout: Vec<VertexAttribute>,
    pub buffer_types: Vec<MeshDataBufferType>,
    pub primitive_topology: br::PrimitiveTopology,
    pub element_count: usize,
}
pub struct MeshIndexConfig {
    pub layout: br::IndexType,
    pub buffer_type: MeshDataBufferType,
    pub element_count: usize,
}

/// An generic mesh data. combined Vertex Buffers and Index Buffer.
pub struct Mesh {
    vertex_buffers: Vec<MeshDataBuffer>,
    vk_vertex_buffers: Vec<br::VkHandleRef<'static, br::vk::VkBuffer>>,
    vk_vertex_buffer_offsets: Vec<br::DeviceSize>,
    primitive_topology: br::PrimitiveTopology,
    vertex_layout: Vec<(VertexAttribute, usize)>,
    vk_vertex_input_bindings: Vec<br::VertexInputBindingDescription>,
    index: Option<MeshIndexBufferState>,
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
    pub fn new(
        g: &peridot::Graphics,
        mm: &mut peridot_memory_manager::MemoryManager,
        init_vertex_config: MeshVertexConfig,
        init_index_config: Option<MeshIndexConfig>,
    ) -> Self {
        let mut this = Self {
            vertex_buffers: Vec::new(),
            vk_vertex_buffers: Vec::new(),
            vk_vertex_buffer_offsets: Vec::new(),
            primitive_topology: br::PrimitiveTopology::TriangleList,
            vertex_layout: Vec::new(),
            vk_vertex_input_bindings: Vec::new(),
            index: None,
            submesh_ranges: Vec::new(),
            is_dirty: false,
        };

        this.configure_vertex(
            g,
            mm,
            init_vertex_config.layout,
            init_vertex_config.buffer_types,
            init_vertex_config.primitive_topology,
            init_vertex_config.element_count,
        );
        if let Some(x) = init_index_config {
            this.configure_index(g, mm, x.layout, x.buffer_type, x.element_count);
        }

        this
    }

    pub fn configure_vertex(
        &mut self,
        g: &peridot::Graphics,
        mm: &mut peridot_memory_manager::MemoryManager,
        layout: Vec<VertexAttribute>,
        buffer_types: Vec<MeshDataBufferType>,
        primitive_topology: br::PrimitiveTopology,
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
        self.vk_vertex_buffers.clear();
        self.vk_vertex_buffers.reserve(
            byte_size_per_buffer
                .len()
                .saturating_sub(self.vk_vertex_buffers.len()),
        );
        self.vk_vertex_buffer_offsets.clear();
        self.vk_vertex_buffer_offsets.reserve(
            byte_size_per_buffer
                .len()
                .saturating_sub(self.vk_vertex_buffer_offsets.len()),
        );
        for (n, (x, a)) in byte_size_per_buffer.into_iter().enumerate() {
            if x == 0 {
                eprintln!("buffer #{n} is zero-sized stride");
            }

            let _buffer_type = buffer_types.get(n).copied().unwrap_or_default();

            let element_size = align(x, a);
            // TODO: streaming(direct buffer) support
            let device_buffer = mm
                .allocate_device_local_buffer(
                    g,
                    br::BufferCreateInfo::new(
                        element_size * element_count,
                        br::BufferUsage::VERTEX_BUFFER | br::BufferUsage::TRANSFER_DEST,
                    ),
                )
                .expect("Failed to create device vertex buffer");

            self.vk_vertex_buffers
                .push(unsafe { br::VkHandleRef::dangling(device_buffer.native_ptr()) });
            self.vk_vertex_buffer_offsets.push(0);
            self.vertex_buffers.push(MeshDataBuffer::Staged {
                device_buffer,
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
            });
        }
        self.vertex_buffers.shrink_to_fit();
        self.vk_vertex_buffers.shrink_to_fit();
        self.vk_vertex_buffer_offsets.shrink_to_fit();

        self.primitive_topology = primitive_topology;
    }

    pub fn configure_index(
        &mut self,
        g: &peridot::Graphics,
        mm: &mut peridot_memory_manager::MemoryManager,
        layout: br::IndexType,
        _buffer_type: MeshDataBufferType,
        element_count: usize,
    ) {
        let byte_size = match layout {
            br::IndexType::U16 => element_count * 2,
            br::IndexType::U32 => element_count * 4,
        };

        // TODO: streaming(direct buffer) support
        let old_index = self.index.replace(MeshIndexBufferState {
            buffer: MeshDataBuffer::Staged {
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
            },
            layout,
        });
        // pre-drop old buffers
        match old_index {
            Some(MeshIndexBufferState {
                buffer:
                    MeshDataBuffer::Staged {
                        mut staging_buffer,
                        staging_mapped_ptr,
                        ..
                    },
                ..
            }) if staging_mapped_ptr.is_some() => unsafe {
                staging_buffer.unmap_raw();
            },
            Some(MeshIndexBufferState {
                buffer:
                    MeshDataBuffer::Streamed {
                        mut direct_buffer,
                        mapped_ptr,
                        ..
                    },
                ..
            }) if mapped_ptr.is_some() => unsafe {
                direct_buffer.unmap_raw();
            },
            _ => (),
        }
    }

    pub fn configure_submesh(&mut self, ranges: Vec<core::ops::Range<usize>>) {
        self.submesh_ranges = ranges;
    }

    pub fn vk_vertex_input_bindings(&self) -> &[br::VertexInputBindingDescription] {
        &self.vk_vertex_input_bindings
    }

    pub fn vk_vertex_input_attributes(
        &self,
        semantic_resolver: &peridot_semantic_shader::ShaderPack<impl br::Device>,
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

    pub fn vk_primitive_topology(&self) -> br::PrimitiveTopology {
        self.primitive_topology
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
            match self.index {
                Some(MeshIndexBufferState {
                    buffer:
                        MeshDataBuffer::Staged {
                            ref mut is_dirty,
                            ref device_buffer,
                            ref staging_buffer,
                            byte_length,
                            ..
                        },
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

    pub fn prepare_draw_buffers<'c, E>(&self, rec: br::CmdRecord<'c, E>) -> br::CmdRecord<'c, E> {
        rec.bind_vertex_buffers(0, &self.vk_vertex_buffers, &self.vk_vertex_buffer_offsets)
            .inject(|r| match self.index {
                Some(MeshIndexBufferState { ref buffer, layout }) => {
                    r.bind_index_buffer(&buffer.bound_buffer_object(), 0, layout)
                }
                None => r,
            })
    }

    pub fn draw<'c, E>(
        &self,
        rec: br::CmdRecord<'c, E>,
        submesh_index: usize,
        instance_count: u32,
    ) -> br::CmdRecord<'c, E> {
        rec.draw(
            self.submesh_ranges[submesh_index].len() as _,
            instance_count,
            self.submesh_ranges[submesh_index].start as _,
            0,
        )
    }

    unsafe fn unmap_if_mapped(&mut self) {
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
                            unsafe {
                                staging_buffer
                                    .flush_ranges_raw(&[0..byte_length as _])
                                    .expect("Failed to flush contents");
                            }
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
                            unsafe {
                                direct_buffer
                                    .flush_ranges_raw(&[0..byte_length as _])
                                    .expect("Failed to flush contents");
                            }
                        }
                        unsafe {
                            direct_buffer.unmap_raw();
                        }

                        *mapped_ptr = None;
                    }
                }
            }
        }

        match self.index {
            Some(MeshIndexBufferState {
                buffer:
                    MeshDataBuffer::Staged {
                        ref mut staging_buffer,
                        ref mut staging_mapped_ptr,
                        ..
                    },
                ..
            }) => {
                if staging_mapped_ptr.is_some() {
                    unsafe {
                        staging_buffer.unmap_raw();
                    }
                    *staging_mapped_ptr = None;
                }
            }
            Some(MeshIndexBufferState {
                buffer:
                    MeshDataBuffer::Streamed {
                        ref mut direct_buffer,
                        ref mut mapped_ptr,
                        ..
                    },
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

/// templates
impl Mesh {
    /// -size to size squared plane with normalized uv, rendered as triangle strip
    pub fn uv_plane_centric_xy(
        g: &peridot::Graphics,
        mm: &mut peridot_memory_manager::MemoryManager,
        size: f32,
        z: f32,
    ) -> Self {
        let mut this = Self::new(
            g,
            mm,
            MeshVertexConfig {
                layout: vec![
                    VertexAttribute {
                        semantic: peridot_semantic_shader::VertexInputSemantic::Position(0),
                        buffer_index: 0,
                        format: br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
                    },
                    VertexAttribute {
                        semantic: peridot_semantic_shader::VertexInputSemantic::Texcoord(0),
                        buffer_index: 1,
                        format: br::vk::VK_FORMAT_R32G32_SFLOAT,
                    },
                ],
                buffer_types: vec![MeshDataBufferType::default()],
                primitive_topology: br::PrimitiveTopology::TriangleStrip,
                element_count: 4,
            },
            None,
        );
        this.modify_vertex_buffer(0, false, |p| unsafe {
            p.write(peridot::math::Vector4(-size, size, z, 1.0));
            p.add(1).write(peridot::math::Vector4(-size, -size, z, 1.0));
            p.add(2).write(peridot::math::Vector4(size, size, z, 1.0));
            p.add(3).write(peridot::math::Vector4(size, -size, z, 1.0));
        });
        this.modify_vertex_buffer(1, false, |p| unsafe {
            p.write(peridot::math::Vector2(0.0f32, 0.0f32));
            p.add(1).write(peridot::math::Vector2(0.0f32, 1.0f32));
            p.add(2).write(peridot::math::Vector2(1.0f32, 0.0f32));
            p.add(3).write(peridot::math::Vector2(1.0f32, 1.0f32));
        });
        this.configure_submesh(vec![0..4]);

        this
    }
}
