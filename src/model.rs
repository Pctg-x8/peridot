//! ModelData Traits and Impls

use super::*;
use bedrock as br;
use pathfinder_partitioner::BQuadVertexPositions;
use std::ops::Range;

// 仮定義
pub trait ModelData {
    type PreallocOffsetType;
    type AdditionalRenderingInfo;

    fn prealloc(&self, alloc: &mut BufferPrealloc) -> Self::PreallocOffsetType;
    fn stage_data_into(&self, mem: &br::MappedMemoryRange, offsets: &Self::PreallocOffsetType)
        -> Self::AdditionalRenderingInfo;
    fn default_render_commands(&self, _cmd: &mut br::CmdRecord,
        _buffer: &Buffer, _buffer_offsets: &Self::PreallocOffsetType,
        _rinfo: &Self::AdditionalRenderingInfo) {}
}

pub struct VgContextPreallocOffsets {
    transforms: usize, interior_positions: usize, interior_indices: usize
}
impl VgContextPreallocOffsets {
    pub fn transforms_byterange(&self) -> Range<u64> { self.transforms as u64 .. self.interior_positions as u64 }
}
pub struct VgContextRenderInfo {
    interior_index_range_per_mesh: Vec<Range<u64>>
}
impl ModelData for vg::Context {
    type PreallocOffsetType = VgContextPreallocOffsets;
    type AdditionalRenderingInfo = VgContextRenderInfo;

    fn prealloc(&self, alloc: &mut BufferPrealloc) -> VgContextPreallocOffsets {
        let interior_positions_count = self.meshes().iter().map(|x| x.0.b_quad_vertex_positions.len()).sum();
        let interior_indices_count = self.meshes().iter().map(|x| x.0.b_quad_vertex_interior_indices.len()).sum();
        
        let transforms = alloc.add(BufferContent::uniform_texel_dynarray::<math::Matrix4F32>(self.meshes().len()));
        let interior_positions = alloc.add(BufferContent::vertices::<BQuadVertexPositions>(interior_positions_count));
        let interior_indices = alloc.add(BufferContent::indices::<u32>(interior_indices_count));
        VgContextPreallocOffsets { transforms, interior_positions, interior_indices }
    }
    fn stage_data_into(&self, mem: &br::MappedMemoryRange, offsets: &VgContextPreallocOffsets) -> VgContextRenderInfo {
        let transforms_stg = unsafe { mem.slice_mut(offsets.transforms, self.meshes().len()) };
        let (mut vofs, mut iofs) = (offsets.interior_positions, offsets.interior_indices);
        let mut interior_index_range_per_mesh = Vec::new();
        let mut interior_index_current = 0;
        for (n, (v, t)) in self.meshes().iter().enumerate() {
            transforms_stg[n] = t.clone();
            let ii_start = interior_index_current;
            interior_index_current += v.b_quad_vertex_interior_indices.len() as u64;
            interior_index_range_per_mesh.push(ii_start .. interior_index_current);
            unsafe {
                mem.slice_mut(vofs, v.b_quad_vertex_positions.len()).clone_from_slice(&v.b_quad_vertex_positions);
                mem.slice_mut(iofs, v.b_quad_vertex_interior_indices.len())
                    .clone_from_slice(&v.b_quad_vertex_interior_indices);
            }
            vofs += v.b_quad_vertex_positions.len();
            iofs = v.b_quad_vertex_interior_indices.len();
        }

        return VgContextRenderInfo { interior_index_range_per_mesh }
    }
    fn default_render_commands(&self, cmd: &mut br::CmdRecord, 
            buffer: &Buffer, buffer_offsets: &VgContextPreallocOffsets, rinfo: &VgContextRenderInfo) {
        cmd.bind_vertex_buffers(0, &[(buffer, buffer_offsets.interior_positions)]);
        cmd.bind_index_buffer(buffer, buffer_offsets.interior_indices, br::IndexType::U32);
        for (n, ir) in rinfo.interior_index_range_per_mesh.iter().enumerate() {
            // skip if there is no indices
            if ir.end == ir.start { continue; }
            
            cmd.push_graphics_constant(br::ShaderStage::VERTEX, 0, &(n as u32));
            cmd.draw_indexed((ir.end - ir.start) as _, 1, ir.start as _, 0, 0);
        }
    }
}
