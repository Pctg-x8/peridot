//! ModelData Traits and Impls

use super::*;
use bedrock as br;
use pathfinder_partitioner::{BQuadVertexPositions, BVertexLoopBlinnData};
use std::ops::Range;
use std::mem::size_of;
use self::math::Vector2;

// 仮定義
pub trait ModelData {
    type PreallocOffsetType;
    type RendererParams;

    fn prealloc(&self, alloc: &mut BufferPrealloc) -> Self::PreallocOffsetType;
    fn stage_data_into(&self, mem: &br::MappedMemoryRange, offsets: Self::PreallocOffsetType)
        -> Self::RendererParams;
}
pub trait DefaultRenderCommands {
    type Extras;

    fn default_render_commands<EH: EngineEvents<NL>, NL: NativeLinker>(&self, e: &Engine<EH, NL>,
        cmd: &mut br::CmdRecord, buffer: &Buffer, extras: &Self::Extras);
}

#[repr(C)] struct GlyphTransform { st: [f32; 4], ext: [f32; 2], pad: [f32; 2] }

#[derive(Clone, Debug)]
pub struct VgContextPreallocOffsets {
    transforms: usize, interior_positions: usize, interior_indices: usize,
    curve_positions: usize, curve_helper_coords: usize, curve_indices: usize
}
impl VgContextPreallocOffsets {
    pub fn transforms_byterange(&self) -> Range<u64> { self.transforms as u64 .. self.interior_positions as u64 }
}
pub struct VgContextRenderInfo {
    interior_index_range_per_mesh: Vec<Range<u32>>, curve_index_range_per_mesh: Vec<Range<u32>>
}
pub struct VgRendererParams {
    buffer_offsets: VgContextPreallocOffsets, render_info: VgContextRenderInfo
}
impl VgRendererParams {
    pub fn transforms_byterange(&self) -> Range<u64> { self.buffer_offsets.transforms_byterange() }
}
pub struct VgRendererExternalInstances {
    pub interior_pipeline: LayoutedPipeline, pub curve_pipeline: LayoutedPipeline,
    pub transform_buffer_descriptor_set: br::vk::VkDescriptorSet,
    pub target_pixels: Vector2<f32>
}
impl ModelData for vg::Context {
    type PreallocOffsetType = VgContextPreallocOffsets;
    type RendererParams = VgRendererParams;

    fn prealloc(&self, alloc: &mut BufferPrealloc) -> VgContextPreallocOffsets {
        let interior_positions_count = self.meshes().iter().map(|x| x.0.b_quad_vertex_positions.len()).sum();
        let interior_indices_count = self.meshes().iter().map(|x| x.0.b_quad_vertex_interior_indices.len()).sum();
        let curve_positions_count = self.meshes().iter().map(|x| x.0.b_vertex_positions.len()).sum();
        let curve_helper_coords_count = self.meshes().iter().map(|x| x.0.b_vertex_loop_blinn_data.len()).sum();
        let curve_indices_count = self.meshes().iter().flat_map(|x| x.0.b_quads.iter().map(|xq| {
            (if xq.upper_control_point_vertex_index != 0xffff_ffff { 3 } else { 0 }) +
            if xq.lower_control_point_vertex_index != 0xffff_ffff { 3 } else { 0 }
        })).sum();
        
        let transforms = alloc.add(BufferContent::uniform_texel_dynarray::<GlyphTransform>(self.meshes().len()));
        let interior_positions = alloc.add(BufferContent::vertices::<BQuadVertexPositions>(interior_positions_count));
        let interior_indices = alloc.add(BufferContent::indices::<u32>(interior_indices_count));
        let curve_positions = alloc.add(BufferContent::vertices::<[f32; 2]>(curve_positions_count));
        let curve_helper_coords = alloc.add(BufferContent::vertices::<BVertexLoopBlinnData>(curve_helper_coords_count));
        let curve_indices = alloc.add(BufferContent::indices::<u32>(curve_indices_count));
        VgContextPreallocOffsets {
            transforms, interior_positions, interior_indices, curve_positions, curve_helper_coords, curve_indices
        }
    }
    fn stage_data_into(&self, mem: &br::MappedMemoryRange, offsets: VgContextPreallocOffsets) -> VgRendererParams {
        let transforms_stg = unsafe { mem.slice_mut(offsets.transforms, self.meshes().len()) };
        let mut vofs = offsets.interior_positions;
        let (mut cpofs, mut chofs) = (offsets.curve_positions, offsets.curve_helper_coords);
        let mut interior_index_range_per_mesh = Vec::new();
        let mut curve_index_range_per_mesh = Vec::new();
        let (mut vindex_offset, mut curve_vindex_offset) = (0, 0);
        let (mut interior_index_offset, mut curve_index_offset) = (0u32, 0u32);
        for (n, (v, st, ext)) in self.meshes().iter().enumerate() {
            transforms_stg[n] = GlyphTransform { st: st.clone(), ext: ext.clone(), pad: [0.0; 2] };
            let ii_start = interior_index_offset;
            interior_index_offset += v.b_quad_vertex_interior_indices.len() as u32;
            interior_index_range_per_mesh.push(ii_start .. interior_index_offset);
            unsafe {
                mem.slice_mut(vofs, v.b_quad_vertex_positions.len()).clone_from_slice(&v.b_quad_vertex_positions);
                mem.slice_mut(cpofs, v.b_vertex_positions.len()).clone_from_slice(&v.b_vertex_positions);
                mem.slice_mut(chofs, v.b_vertex_loop_blinn_data.len()).clone_from_slice(&v.b_vertex_loop_blinn_data);

                let idxslice = mem.slice_mut(offsets.interior_indices + ii_start as usize * size_of::<u32>(),
                    v.b_quad_vertex_interior_indices.len());
                for (i, ib) in v.b_quad_vertex_interior_indices.iter().zip(idxslice.iter_mut()) {
                    *ib = i + vindex_offset;
                }
                let ci_start = curve_index_offset;
                for bq in &v.b_quads {
                    if bq.upper_control_point_vertex_index != 0xffff_ffff {
                        let idx = mem.slice_mut(offsets.curve_indices +
                            curve_index_offset as usize * size_of::<u32>(), 3);
                        idx[0] = curve_vindex_offset + bq.upper_control_point_vertex_index;
                        idx[1] = curve_vindex_offset + bq.upper_right_vertex_index;
                        idx[2] = curve_vindex_offset + bq.upper_left_vertex_index;
                        curve_index_offset += 3;
                    }
                    if bq.lower_control_point_vertex_index != 0xffff_ffff {
                        let idx = mem.slice_mut(offsets.curve_indices +
                            curve_index_offset as usize * size_of::<u32>(), 3);
                        idx[0] = curve_vindex_offset + bq.lower_control_point_vertex_index;
                        idx[1] = curve_vindex_offset + bq.lower_right_vertex_index;
                        idx[2] = curve_vindex_offset + bq.lower_left_vertex_index;
                        curve_index_offset += 3;
                    }
                }
                curve_index_range_per_mesh.push(ci_start .. curve_index_offset);
            }
            vofs += v.b_quad_vertex_positions.len() * size_of::<BQuadVertexPositions>();
            cpofs += v.b_vertex_positions.len() * size_of::<[f32; 2]>();
            chofs += v.b_vertex_loop_blinn_data.len() * size_of::<BVertexLoopBlinnData>();
            vindex_offset += (v.b_quad_vertex_positions.len() as u32) * 6;
            curve_vindex_offset += v.b_vertex_positions.len() as u32;
        }

        return VgRendererParams {
            buffer_offsets: offsets, render_info: VgContextRenderInfo {
                interior_index_range_per_mesh, curve_index_range_per_mesh
            }
        };
    }
}
impl DefaultRenderCommands for VgRendererParams {
    type Extras = VgRendererExternalInstances;

    fn default_render_commands<EH: EngineEvents<NL>, NL: NativeLinker>(&self, e: &Engine<EH, NL>,
        cmd: &mut br::CmdRecord, buffer: &Buffer, extras: &Self::Extras) {
        let renderscale = extras.target_pixels.clone() * e.rendering_precision().recip();
        extras.interior_pipeline.bind(cmd);
        cmd.push_graphics_constant(br::ShaderStage::VERTEX, 0, &renderscale);
        cmd.push_graphics_constant(br::ShaderStage::VERTEX, 4 * 3, &0u32);
        cmd.bind_graphics_descriptor_sets(0, &[extras.transform_buffer_descriptor_set], &[]);

        cmd.bind_vertex_buffers(0, &[(buffer, self.buffer_offsets.interior_positions)]);
        cmd.bind_index_buffer(buffer, self.buffer_offsets.interior_indices, br::IndexType::U32);
        for (n, ir) in self.render_info.interior_index_range_per_mesh.iter().enumerate() {
            // skip if there is no indices
            if ir.end == ir.start { continue; }

            cmd.push_graphics_constant(br::ShaderStage::VERTEX, 4 * 2, &(n as u32));
            cmd.draw_indexed((ir.end - ir.start) as _, 1, ir.start as _, 0, 0);
        }
        extras.curve_pipeline.bind(cmd);
        cmd.bind_vertex_buffers(0,
            &[(buffer, self.buffer_offsets.curve_positions), (buffer, self.buffer_offsets.curve_helper_coords)]);
        cmd.bind_index_buffer(buffer, self.buffer_offsets.curve_indices, br::IndexType::U32);
        for (n, ir) in self.render_info.curve_index_range_per_mesh.iter().enumerate() {
            if ir.end == ir.start { continue; }
            cmd.push_graphics_constant(br::ShaderStage::VERTEX, 4 * 2, &(n as u32));
            cmd.draw_indexed(ir.end - ir.start, 1, ir.start, 0, 0);
        }
    }
}