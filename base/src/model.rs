//! ModelData Traits and Impls

use super::*;
use bedrock as br;

// 仮定義
pub trait ModelData {
    type PreallocOffsetType;
    type RendererParams;

    fn prealloc(&self, alloc: &mut BufferPrealloc) -> Self::PreallocOffsetType;
    fn stage_data_into(
        &self,
        mem: &br::MappedMemoryRange<impl br::DeviceMemory + br::VkHandleMut + ?Sized>,
        offsets: Self::PreallocOffsetType,
    ) -> Self::RendererParams;
}
pub trait DefaultRenderCommands<'e, Device: br::Device> {
    type Extras: 'e;

    fn default_render_commands<NL: NativeLinker>(
        &self,
        e: &Engine<NL>,
        cmd: &mut br::CmdRecord<impl br::VkHandleMut<Handle = br::vk::VkCommandBuffer> + ?Sized>,
        buffer: &(impl br::Buffer<ConcreteDevice = Device> + ?Sized),
        extras: Self::Extras,
    );
}

#[derive(Debug, Clone)]
pub struct Primitive<VT> {
    pub vertices: Vec<VT>,
}
impl<VT: Clone> ModelData for Primitive<VT> {
    type PreallocOffsetType = u64;
    type RendererParams = ();

    fn prealloc(&self, alloc: &mut BufferPrealloc) -> u64 {
        alloc.add(BufferContent::vertices::<VT>(self.vertices.len()))
    }
    fn stage_data_into(
        &self,
        mem: &br::MappedMemoryRange<impl br::DeviceMemory + br::VkHandleMut + ?Sized>,
        vo: u64,
    ) {
        unsafe {
            mem.slice_mut::<VT>(vo as _, self.vertices.len())
                .clone_from_slice(&self.vertices);
        }
    }
}
#[derive(Debug, Clone)]
pub struct IndexedPrimitive<VT> {
    pub vertices: Vec<VT>,
    pub indices: Vec<u16>,
}
impl<VT: Clone> ModelData for IndexedPrimitive<VT> {
    type PreallocOffsetType = (u64, u64);
    type RendererParams = ();

    fn prealloc(&self, alloc: &mut BufferPrealloc) -> (u64, u64) {
        let v = alloc.add(BufferContent::vertices::<VT>(self.vertices.len()));
        let i = alloc.add(BufferContent::indices::<u16>(self.indices.len()));

        (v, i)
    }
    fn stage_data_into(
        &self,
        mem: &br::MappedMemoryRange<impl br::DeviceMemory + br::VkHandleMut + ?Sized>,
        (vo, io): (u64, u64),
    ) {
        unsafe {
            mem.slice_mut::<VT>(vo as _, self.vertices.len())
                .clone_from_slice(&self.vertices);
            mem.slice_mut::<u16>(io as _, self.indices.len())
                .copy_from_slice(&self.indices);
        }
    }
}
impl<VT> Primitive<VT> {
    pub fn byte_length(&self) -> usize {
        core::mem::size_of::<VT>() * self.vertices.len()
    }

    pub fn with_indices(self, indices: Vec<u16>) -> IndexedPrimitive<VT> {
        IndexedPrimitive {
            vertices: self.vertices,
            indices,
        }
    }
}

impl<VT> IndexedPrimitive<VT> {
    pub fn vertices_byte_length(&self) -> usize {
        core::mem::size_of::<VT>() * self.vertices.len()
    }

    pub fn indices_byte_length(&self) -> usize {
        core::mem::size_of::<u16>() * self.indices.len()
    }
}

impl Primitive<math::Vector4F32> {
    /// 0.0 to size squared 2d plane, rendered as triangle strip
    pub fn plane(size: f32) -> Self {
        Self {
            vertices: vec![
                math::Vector4(0.0, 0.0, 0.0, 1.0),
                math::Vector4(0.0, size, 0.0, 1.0),
                math::Vector4(size, 0.0, 0.0, 1.0),
                math::Vector4(size, size, 0.0, 1.0),
            ],
        }
    }

    /// -size to size squared 2d plane, rendered as triangle strip
    pub fn plane_centric(size: f32) -> Self {
        Self {
            vertices: vec![
                math::Vector4(-size, -size, 0.0, 1.0),
                math::Vector4(-size, size, 0.0, 1.0),
                math::Vector4(size, -size, 0.0, 1.0),
                math::Vector4(size, size, 0.0, 1.0),
            ],
        }
    }
}
impl IndexedPrimitive<math::Vector4F32> {
    /// 0.0 to size squared 2d plane, vertices for rendered as triangle strip, indices for rendered as triangle list
    pub fn plane(size: f32) -> Self {
        Primitive::plane(size).with_indices(vec![0, 1, 2, 1, 2, 3])
    }

    /// -size to size squared 2d plane, vertices for rendered as triangle strip, indices for rendered as triangle list
    pub fn plane_centric(size: f32) -> Self {
        Primitive::plane_centric(size).with_indices(vec![0, 1, 2, 1, 2, 3])
    }
}

impl Primitive<math::Vector4F32> {
    /// limited xz grid lines, rendered as line list
    pub fn limited_xz_grid(limit: u32) -> Self {
        let xlines = (-(limit as i64)..=limit as i64).flat_map(|x| {
            vec![
                math::Vector4(x as f32, 0.0, -(limit as f32), 1.0),
                math::Vector4(x as f32, 0.0, limit as f32, 1.0),
            ]
        });
        let zlines = (-(limit as i64)..=limit as i64).flat_map(|z| {
            vec![
                math::Vector4(-(limit as f32), 0.0, z as f32, 1.0),
                math::Vector4(limit as f32, 0.0, z as f32, 1.0),
            ]
        });

        Self {
            vertices: xlines.chain(zlines).collect(),
        }
    }
}

#[repr(C, align(16))]
#[derive(Debug, Clone, PartialEq)]
pub struct VertexUV2D {
    pub pos: math::Vector2F32,
    pub uv: math::Vector2F32,
}
impl Primitive<VertexUV2D> {
    /// 0.0 to size squared 2d plane with normalized uv, rendered as triangle strip
    pub fn uv_plane(size: f32) -> Self {
        Primitive {
            vertices: vec![
                VertexUV2D {
                    pos: math::Vector2(0.0, 0.0),
                    uv: math::Vector2(0.0, 0.0),
                },
                VertexUV2D {
                    pos: math::Vector2(0.0, size),
                    uv: math::Vector2(0.0, 1.0),
                },
                VertexUV2D {
                    pos: math::Vector2(size, 0.0),
                    uv: math::Vector2(1.0, 0.0),
                },
                VertexUV2D {
                    pos: math::Vector2(size, size),
                    uv: math::Vector2(1.0, 1.0),
                },
            ],
        }
    }

    /// -size to size squared 2d plane with normalized uv, rendered as triangle strip
    pub fn uv_plane_centric(size: f32) -> Self {
        Primitive {
            vertices: vec![
                VertexUV2D {
                    pos: math::Vector2(-size, -size),
                    uv: math::Vector2(0.0, 0.0),
                },
                VertexUV2D {
                    pos: math::Vector2(-size, size),
                    uv: math::Vector2(0.0, 1.0),
                },
                VertexUV2D {
                    pos: math::Vector2(size, -size),
                    uv: math::Vector2(1.0, 0.0),
                },
                VertexUV2D {
                    pos: math::Vector2(size, size),
                    uv: math::Vector2(1.0, 1.0),
                },
            ],
        }
    }

    /// Primitive covers entire of the viewport area
    pub fn fill_rect() -> Self {
        Self {
            vertices: vec![
                VertexUV2D {
                    pos: math::Vector2(-1.0, 1.0),
                    uv: math::Vector2(0.0, 0.0),
                },
                VertexUV2D {
                    pos: math::Vector2(1.0, 1.0),
                    uv: math::Vector2(1.0, 0.0),
                },
                VertexUV2D {
                    pos: math::Vector2(-1.0, -1.0),
                    uv: math::Vector2(0.0, 1.0),
                },
                VertexUV2D {
                    pos: math::Vector2(1.0, -1.0),
                    uv: math::Vector2(1.0, 1.0),
                },
            ],
        }
    }
}
impl IndexedPrimitive<VertexUV2D> {
    /// 0.0 to size squared 2d plane with normalized uv,
    /// vertices for rendered as triangle strip, indices for rendered as triangle list
    pub fn uv_plane(size: f32) -> Self {
        Primitive::uv_plane(size).with_indices(vec![0, 1, 2, 1, 2, 3])
    }

    /// -size to size squared 2d plane with normalized uv,
    /// vertices for rendered as triangle strip, indices for rendered as triangle list
    pub fn uv_plane_centric(size: f32) -> Self {
        Primitive::uv_plane_centric(size).with_indices(vec![0, 1, 2, 1, 2, 3])
    }
}

#[repr(C, align(16))]
#[derive(Debug, Clone, PartialEq)]
pub struct VertexUV {
    pub pos: math::Vector4F32,
    pub uv: math::Vector4F32,
}
impl Primitive<VertexUV> {
    /// 0.0 to size squared plane with normalized uv, rendered as triangle strip
    pub fn uv_plane_xy(size: f32, z: f32) -> Self {
        Self {
            vertices: vec![
                VertexUV {
                    pos: math::Vector4(0.0, 0.0, z, 1.0),
                    uv: math::Vector4(0.0, 0.0, 0.0, 1.0),
                },
                VertexUV {
                    pos: math::Vector4(0.0, size, z, 1.0),
                    uv: math::Vector4(0.0, 1.0, 0.0, 1.0),
                },
                VertexUV {
                    pos: math::Vector4(size, 0.0, z, 1.0),
                    uv: math::Vector4(1.0, 0.0, 0.0, 1.0),
                },
                VertexUV {
                    pos: math::Vector4(size, size, z, 1.0),
                    uv: math::Vector4(1.0, 1.0, 0.0, 1.0),
                },
            ],
        }
    }

    /// -size to size squared plane with normalized uv, rendered as triangle strip
    pub fn uv_plane_centric_xy(size: f32, z: f32) -> Self {
        Self {
            vertices: vec![
                VertexUV {
                    pos: math::Vector4(-size, size, z, 1.0),
                    uv: math::Vector4(0.0, 0.0, 0.0, 1.0),
                },
                VertexUV {
                    pos: math::Vector4(-size, -size, z, 1.0),
                    uv: math::Vector4(0.0, 1.0, 0.0, 1.0),
                },
                VertexUV {
                    pos: math::Vector4(size, size, z, 1.0),
                    uv: math::Vector4(1.0, 0.0, 0.0, 1.0),
                },
                VertexUV {
                    pos: math::Vector4(size, -size, z, 1.0),
                    uv: math::Vector4(1.0, 1.0, 0.0, 1.0),
                },
            ],
        }
    }
}

#[repr(C, align(16))]
#[derive(Debug, Clone, PartialEq)]
pub struct ColoredVertex {
    pub pos: math::Vector4F32,
    pub color: math::Vector4F32,
}
impl Primitive<ColoredVertex> {
    /// colored coordinate axis lines, rendered as line list
    pub fn limited_coordinate_axis(limit: u32) -> Self {
        Primitive {
            vertices: vec![
                ColoredVertex {
                    pos: math::Vector4(0.0, 0.0, 0.0, 1.0),
                    color: math::Vector4(1.0, 0.0, 0.0, 1.0),
                },
                ColoredVertex {
                    pos: math::Vector4(limit as _, 0.0, 0.0, 1.0),
                    color: math::Vector4(1.0, 0.0, 0.0, 1.0),
                },
                ColoredVertex {
                    pos: math::Vector4(0.0, 0.0, 0.0, 1.0),
                    color: math::Vector4(0.0, 1.0, 0.0, 1.0),
                },
                ColoredVertex {
                    pos: math::Vector4(0.0, limit as _, 0.0, 1.0),
                    color: math::Vector4(0.0, 1.0, 0.0, 1.0),
                },
                ColoredVertex {
                    pos: math::Vector4(0.0, 0.0, 0.0, 1.0),
                    color: math::Vector4(0.0, 0.0, 1.0, 1.0),
                },
                ColoredVertex {
                    pos: math::Vector4(0.0, 0.0, limit as _, 1.0),
                    color: math::Vector4(0.0, 0.0, 1.0, 1.0),
                },
            ],
        }
    }
}
