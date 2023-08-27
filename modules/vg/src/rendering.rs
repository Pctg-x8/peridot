//! ModelData Traits and Impls

use super::*;
use bedrock as br;
use pathfinder_partitioner::{BQuadVertexPositions, BVertexLoopBlinnData};
use peridot::math::Vector2;
use peridot::mthelper::SharedRef;
use peridot::{
    BufferContent, BufferPrealloc, DefaultRenderCommands, Engine, LayoutedPipeline, ModelData,
    NativeLinker,
};
use peridot_command_object::{
    DescriptorSets, GraphicsCommand, GraphicsCommandCombiner, PreConfigureDrawIndexed,
    PushConstant, RangedBuffer, SimpleDrawIndexed, StandardIndexedMesh,
};
use std::mem::size_of;
use std::ops::Range;

#[repr(C)]
struct GlyphTransform {
    st: [f32; 4],
    ext: [f32; 2],
    pad: [f32; 2],
}

#[derive(Clone, Debug)]
pub struct ContextPreallocOffsets {
    transforms: usize,
    interior_positions: usize,
    interior_indices: usize,
    curve_positions: usize,
    curve_helper_coords: usize,
    curve_indices: usize,
}
impl ContextPreallocOffsets {
    pub fn transforms_byterange(&self) -> Range<u64> {
        self.transforms as u64..self.interior_positions as u64
    }
}
pub struct ContextRenderInfo {
    interior_index_range_per_mesh: Vec<Range<u32>>,
    curve_index_range_per_mesh: Vec<Range<u32>>,
}
pub struct RendererParams {
    buffer_offsets: ContextPreallocOffsets,
    render_info: ContextRenderInfo,
}
impl RendererParams {
    pub fn transforms_byterange(&self) -> Range<u64> {
        self.buffer_offsets.transforms_byterange()
    }
}
pub struct RendererExternalInstances<'r, Device: br::Device> {
    // TODO: SharedRef強制はなんとか剥がしたい
    pub interior_pipeline: &'r LayoutedPipeline<
        br::PipelineObject<Device>,
        SharedRef<br::PipelineLayoutObject<Device>>,
    >,
    pub curve_pipeline: &'r LayoutedPipeline<
        br::PipelineObject<Device>,
        SharedRef<br::PipelineLayoutObject<Device>>,
    >,
    pub transform_buffer_descriptor_set: br::DescriptorSet,
    pub target_pixels: Vector2<f32>,
}
impl ModelData for Context {
    type PreallocOffsetType = ContextPreallocOffsets;
    type RendererParams = RendererParams;

    fn prealloc(&self, alloc: &mut BufferPrealloc) -> ContextPreallocOffsets {
        let (
            mut interior_positions_count,
            mut interior_indices_count,
            mut curve_positions_count,
            mut curve_helper_coords_count,
            mut curve_indices_count,
        ) = (0, 0, 0, 0, 0);
        for (m, _, _) in &self.meshes {
            interior_positions_count += m.b_quad_vertex_positions.len();
            interior_indices_count += m.b_quad_vertex_interior_indices.len();
            curve_positions_count += m.b_vertex_positions.len();
            curve_helper_coords_count += m.b_vertex_loop_blinn_data.len();
            curve_indices_count += m
                .b_quads
                .iter()
                .map(|xq| {
                    let upper = if xq.upper_control_point_vertex_index != 0xffff_ffff {
                        3
                    } else {
                        0
                    };
                    let lower = if xq.lower_control_point_vertex_index != 0xffff_ffff {
                        3
                    } else {
                        0
                    };

                    upper + lower
                })
                .sum::<usize>();
        }

        let transforms = alloc.add(BufferContent::uniform_texel_dynarray::<GlyphTransform>(
            self.meshes().len(),
        )) as _;
        let interior_positions = alloc.add(BufferContent::vertices::<BQuadVertexPositions>(
            interior_positions_count,
        )) as _;
        let interior_indices =
            alloc.add(BufferContent::indices::<u32>(interior_indices_count)) as _;
        let curve_positions =
            alloc.add(BufferContent::vertices::<[f32; 2]>(curve_positions_count)) as _;
        let curve_helper_coords = alloc.add(BufferContent::vertices::<BVertexLoopBlinnData>(
            curve_helper_coords_count,
        )) as _;
        let curve_indices = alloc.add(BufferContent::indices::<u32>(curve_indices_count)) as _;

        ContextPreallocOffsets {
            transforms,
            interior_positions,
            interior_indices,
            curve_positions,
            curve_helper_coords,
            curve_indices,
        }
    }

    fn stage_data_into(
        &self,
        mem: &br::MappedMemoryRange<impl br::DeviceMemory + br::VkHandleMut + ?Sized>,
        offsets: ContextPreallocOffsets,
    ) -> RendererParams {
        let transforms_stg = unsafe { mem.slice_mut(offsets.transforms, self.meshes().len()) };
        let mut vofs = offsets.interior_positions;
        let (mut cpofs, mut chofs) = (offsets.curve_positions, offsets.curve_helper_coords);
        let mut interior_index_range_per_mesh = Vec::new();
        let mut curve_index_range_per_mesh = Vec::new();
        let (mut vindex_offset, mut curve_vindex_offset) = (0, 0);
        let (mut interior_index_offset, mut curve_index_offset) = (0u32, 0u32);
        for (n, (v, st, ext)) in self.meshes().iter().enumerate() {
            transforms_stg[n] = GlyphTransform {
                st: st.clone(),
                ext: ext.clone(),
                pad: [0.0; 2],
            };
            let ii_start = interior_index_offset;
            interior_index_offset += v.b_quad_vertex_interior_indices.len() as u32;
            interior_index_range_per_mesh.push(ii_start..interior_index_offset);
            unsafe {
                mem.slice_mut(vofs, v.b_quad_vertex_positions.len())
                    .clone_from_slice(&v.b_quad_vertex_positions);
                mem.slice_mut(cpofs, v.b_vertex_positions.len())
                    .clone_from_slice(&v.b_vertex_positions);
                mem.slice_mut(chofs, v.b_vertex_loop_blinn_data.len())
                    .clone_from_slice(&v.b_vertex_loop_blinn_data);

                let idxslice = mem.slice_mut(
                    offsets.interior_indices + ii_start as usize * size_of::<u32>(),
                    v.b_quad_vertex_interior_indices.len(),
                );
                for (i, ib) in v
                    .b_quad_vertex_interior_indices
                    .iter()
                    .zip(idxslice.iter_mut())
                {
                    *ib = i + vindex_offset;
                }
                let ci_start = curve_index_offset;
                for bq in &v.b_quads {
                    if bq.upper_control_point_vertex_index != 0xffff_ffff {
                        let idx = mem.slice_mut(
                            offsets.curve_indices + curve_index_offset as usize * size_of::<u32>(),
                            3,
                        );
                        idx[0] = curve_vindex_offset + bq.upper_control_point_vertex_index;
                        idx[1] = curve_vindex_offset + bq.upper_right_vertex_index;
                        idx[2] = curve_vindex_offset + bq.upper_left_vertex_index;
                        curve_index_offset += 3;
                    }
                    if bq.lower_control_point_vertex_index != 0xffff_ffff {
                        let idx = mem.slice_mut(
                            offsets.curve_indices + curve_index_offset as usize * size_of::<u32>(),
                            3,
                        );
                        idx[0] = curve_vindex_offset + bq.lower_control_point_vertex_index;
                        idx[1] = curve_vindex_offset + bq.lower_right_vertex_index;
                        idx[2] = curve_vindex_offset + bq.lower_left_vertex_index;
                        curve_index_offset += 3;
                    }
                }
                curve_index_range_per_mesh.push(ci_start..curve_index_offset);
            }
            vofs += v.b_quad_vertex_positions.len() * size_of::<BQuadVertexPositions>();
            cpofs += v.b_vertex_positions.len() * size_of::<[f32; 2]>();
            chofs += v.b_vertex_loop_blinn_data.len() * size_of::<BVertexLoopBlinnData>();
            vindex_offset += (v.b_quad_vertex_positions.len() as u32) * 6;
            curve_vindex_offset += v.b_vertex_positions.len() as u32;
        }

        RendererParams {
            buffer_offsets: offsets,
            render_info: ContextRenderInfo {
                interior_index_range_per_mesh,
                curve_index_range_per_mesh,
            },
        }
    }
}
impl<'e, Device: br::Device + 'e> DefaultRenderCommands<'e, Device> for RendererParams {
    type Extras = RendererExternalInstances<'e, Device>;

    fn default_render_commands<NL: NativeLinker>(
        &self,
        e: &Engine<NL>,
        cmd: &mut br::CmdRecord<impl br::VkHandleMut<Handle = br::vk::VkCommandBuffer> + ?Sized>,
        buffer: &(impl br::Buffer<ConcreteDevice = Device> + ?Sized),
        extras: Self::Extras,
    ) {
        let renderscale = extras.target_pixels.clone() * e.rendering_precision().recip();
        extras.interior_pipeline.bind(cmd);
        let _ = cmd
            .push_graphics_constant(br::ShaderStage::VERTEX, 0, &renderscale)
            .push_graphics_constant(br::ShaderStage::VERTEX, 4 * 3, &0u32)
            .bind_graphics_descriptor_sets(
                0,
                &[extras.transform_buffer_descriptor_set.into()],
                &[],
            );

        let _ = cmd
            .bind_vertex_buffers(0, &[(buffer, self.buffer_offsets.interior_positions)])
            .bind_index_buffer(
                buffer,
                self.buffer_offsets.interior_indices,
                br::IndexType::U32,
            );
        for (n, ir) in self
            .render_info
            .interior_index_range_per_mesh
            .iter()
            .enumerate()
        {
            // skip if there is no indices
            if ir.end == ir.start {
                continue;
            }

            let _ = cmd
                .push_graphics_constant(br::ShaderStage::VERTEX, 4 * 2, &(n as u32))
                .draw_indexed((ir.end - ir.start) as _, 1, ir.start as _, 0, 0);
        }
        extras.curve_pipeline.bind(cmd);
        let _ = cmd
            .bind_vertex_buffers(
                0,
                &[
                    (buffer, self.buffer_offsets.curve_positions),
                    (buffer, self.buffer_offsets.curve_helper_coords),
                ],
            )
            .bind_index_buffer(
                buffer,
                self.buffer_offsets.curve_indices,
                br::IndexType::U32,
            );
        for (n, ir) in self
            .render_info
            .curve_index_range_per_mesh
            .iter()
            .enumerate()
        {
            if ir.end == ir.start {
                continue;
            }

            let _ = cmd
                .push_graphics_constant(br::ShaderStage::VERTEX, 4 * 2, &(n as u32))
                .draw_indexed(ir.end - ir.start, 1, ir.start, 0, 0);
        }
    }
}

pub struct RenderVG<Device: br::Device, Buffer: br::Buffer<ConcreteDevice = Device>> {
    pub params: RendererParams,
    pub interior_pipeline:
        LayoutedPipeline<br::PipelineObject<Device>, SharedRef<br::PipelineLayoutObject<Device>>>,
    pub curve_pipeline:
        LayoutedPipeline<br::PipelineObject<Device>, SharedRef<br::PipelineLayoutObject<Device>>>,
    pub transform_buffer_descriptor_set: br::DescriptorSet,
    pub target_pixels: Vector2<f32>,
    pub buffer: Buffer,
    pub rendering_precision: f32,
}
impl<Device: br::Device, Buffer: br::Buffer<ConcreteDevice = Device>> RenderVG<Device, Buffer> {
    pub fn set_target_pixels(&mut self, new_target_pixels: Vector2<f32>) {
        self.target_pixels = new_target_pixels;
    }
}
impl<Device: br::Device, Buffer: br::Buffer<ConcreteDevice = Device>> GraphicsCommand
    for RenderVG<Device, Buffer>
{
    fn execute(
        &self,
        cb: &mut br::CmdRecord<'_, dyn br::VkHandleMut<Handle = br::vk::VkCommandBuffer>>,
    ) {
        let render_scale = self.target_pixels.clone() * self.rendering_precision.recip();

        let common_configs = (
            PushConstant::for_vertex(0, render_scale),
            PushConstant::for_vertex(4 * 3, 0u32),
            DescriptorSets(vec![self.transform_buffer_descriptor_set.into()]).into_bind_graphics(),
        );

        let interior_mesh = StandardIndexedMesh {
            vertex_buffers: vec![RangedBuffer::from_offset_length(
                &self.buffer,
                self.params.buffer_offsets.interior_positions as _,
                1,
            )],
            index_buffer: RangedBuffer::from_offset_length(
                &self.buffer,
                self.params.buffer_offsets.interior_indices as _,
                1,
            ),
            index_type: br::IndexType::U32,
            vertex_count: 0,
        };
        let curve_mesh = StandardIndexedMesh {
            vertex_buffers: vec![
                RangedBuffer::from_offset_length(
                    &self.buffer,
                    self.params.buffer_offsets.curve_positions as _,
                    1,
                ),
                RangedBuffer::from_offset_length(
                    &self.buffer,
                    self.params.buffer_offsets.curve_helper_coords as _,
                    1,
                ),
            ],
            index_buffer: RangedBuffer::from_offset_length(
                &self.buffer,
                self.params.buffer_offsets.curve_indices as _,
                1,
            ),
            index_type: br::IndexType::U32,
            vertex_count: 0,
        };

        let interior_render = interior_mesh.ref_pre_configure_for_draw().then(
            self.params
                .render_info
                .interior_index_range_per_mesh
                .iter()
                .enumerate()
                // skip if there is no indices
                .filter(|(_, ir)| ir.end != ir.start)
                .map(|(n, ir)| {
                    (
                        PushConstant::for_vertex(4 * 2, n as u32),
                        SimpleDrawIndexed::new(ir.end - ir.start, 1).from_index(ir.start),
                    )
                })
                .collect::<Vec<_>>(),
        );
        let curve_render = curve_mesh.ref_pre_configure_for_draw().then(
            self.params
                .render_info
                .curve_index_range_per_mesh
                .iter()
                .enumerate()
                // skip if there is no indices
                .filter(|(_, ir)| ir.end != ir.start)
                .map(|(n, ir)| {
                    (
                        PushConstant::for_vertex(4 * 2, n as u32),
                        SimpleDrawIndexed::new(ir.end - ir.start, 1).from_index(ir.start),
                    )
                })
                .collect::<Vec<_>>(),
        );

        (
            interior_render.after_of((&self.interior_pipeline).then(common_configs)),
            curve_render.after_of(&self.curve_pipeline),
        )
            .execute(cb);
    }
}
