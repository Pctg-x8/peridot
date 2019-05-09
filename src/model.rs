//! ModelData Traits and Impls

use super::*;
use bedrock as br;
use pathfinder_partitioner::{BQuadVertexPositions, BVertexLoopBlinnData};
use std::ops::Range;
use std::mem::size_of;
use self::math::{Vector2, Vector2F32, Vector4F32, Vector4};
use rayon::prelude::*;
use std::sync::RwLock;

// 仮定義
pub trait ModelData
{
    type PreallocOffsetType;
    type RendererParams;

    fn prealloc<EH: EngineEvents<NL>, NL: NativeLinker>(&self, e: &Engine<EH, NL>, alloc: &mut BufferPrealloc,
        textures: &mut TextureInitializationGroup) -> Self::PreallocOffsetType;
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

    fn prealloc<EH: EngineEvents<NL>, NL: NativeLinker>(&self, _e: &Engine<EH, NL>, alloc: &mut BufferPrealloc,
            _: &mut TextureInitializationGroup) -> VgContextPreallocOffsets {
        let interior_positions_count = self.meshes().iter().map(|x| x.0.b_quad_vertex_positions.len()).sum();
        let interior_indices_count = self.meshes().iter().map(|x| x.0.b_quad_vertex_interior_indices.len()).sum();
        let curve_positions_count = self.meshes().iter().map(|x| x.0.b_vertex_positions.len()).sum();
        let curve_helper_coords_count = self.meshes().iter().map(|x| x.0.b_vertex_loop_blinn_data.len()).sum();
        let curve_indices_count = self.meshes().iter().flat_map(|x| x.0.b_quads.iter().map(|xq| {
            (if xq.upper_control_point_vertex_index != 0xffff_ffff { 3 } else { 0 }) +
            if xq.lower_control_point_vertex_index != 0xffff_ffff { 3 } else { 0 }
        })).sum();
        
        let transforms = alloc.add(BufferContent::uniform_texel_dynarray::<GlyphTransform>(self.meshes().len())) as _;
        let interior_positions =
            alloc.add(BufferContent::vertices::<BQuadVertexPositions>(interior_positions_count)) as _;
        let interior_indices = alloc.add(BufferContent::indices::<u32>(interior_indices_count)) as _;
        let curve_positions = alloc.add(BufferContent::vertices::<[f32; 2]>(curve_positions_count)) as _;
        let curve_helper_coords =
            alloc.add(BufferContent::vertices::<BVertexLoopBlinnData>(curve_helper_coords_count)) as _;
        let curve_indices = alloc.add(BufferContent::indices::<u32>(curve_indices_count)) as _;
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

pub struct PMXDataPlacementOffsets {
    pub vbuf_suballoc_positions: usize, pub ibuf_offset: usize,
    pub vbuf_suballoc_normals: usize, pub vbuf_suballoc_uvs: usize,
    pub texture_slot_numbers: Vec<usize>
}
pub struct PMXRenderingParams {
    index_size: br::IndexType, offsets: PMXDataPlacementOffsets, index_count: u32,
    textured_surface_ranges: Vec<(usize, Range<u32>)>,
    untextured_surface_ranges: Vec<(usize, Range<u32>)>
}
impl ModelData for super::PolygonModelExtended {
    type PreallocOffsetType = PMXDataPlacementOffsets;
    type RendererParams = PMXRenderingParams;

    fn prealloc<EH: EngineEvents<NL>, NL: NativeLinker>(&self, e: &Engine<EH, NL>, alloc: &mut BufferPrealloc,
            textures: &mut TextureInitializationGroup) -> Self::PreallocOffsetType {
        let vbuf_suballoc_positions = alloc.add(BufferContent::vertices::<Vector4F32>(self.vertices.len())) as _;
        let vbuf_suballoc_normals = alloc.add(BufferContent::vertices::<Vector4F32>(self.vertices.len())) as _;
        let vbuf_suballoc_uvs = alloc.add(BufferContent::vertices::<Vector2F32>(self.vertices.len())) as _;
        let ibuf_offset = if self.header.index_sizes.vertex == mmdloader::pmx::IndexSize::Long {
            // use 32bit
            alloc.add(BufferContent::indices::<u32>(self.surfaces.len() * 3)) as _
        }
        else {
            alloc.add(BufferContent::indices::<u16>(self.surfaces.len() * 3)) as _
        };

        let al_ref = e.async_asset_loader();
        let mut texture_slot_numbers = Vec::with_capacity(self.textures.len());
        let textures_ref = RwLock::new(textures);
        let ref cmp = self.base_components;
        let loaded_textures = self.textures.par_iter().map(|tex|
        {
            let mut asset_components = cmp.iter().map(|x| x as _).collect::<Vec<&str>>();
            if let Some(p) = tex.parent() {
                asset_components.extend(p.components().map(|c| c.as_os_str().to_str().expect("Decoding path")));
            }
            asset_components.push(tex.file_stem().expect("Address not points to a file?")
                .to_str().expect("Decoding path"));
            let asset_path = asset_components.join(".");
            trace!("Loading Asset in MMD: {}", asset_path);
            // switch loader by extension
            match tex.extension().and_then(std::ffi::OsStr::to_str) {
                Some("bmp") => al_ref.load::<BMP>(&asset_path).and_then(|l| {
                    trace!("Texture Loaded! {}", asset_path);
                    Ok(textures_ref.write().expect("Poisoning?").add(l))
                }).expect("Loading Texture"),
                Some("png") => al_ref.load::<PNG>(&asset_path).and_then(|l| {
                    trace!("Texture Loaded! {}", asset_path);
                    Ok(textures_ref.write().expect("Poisoning?").add(l))
                }).expect("Loading Texture"),
                Some("tiff") => al_ref.load::<TIFF>(&asset_path).and_then(|l| {
                    trace!("Texture Loaded! {}", asset_path);
                    Ok(textures_ref.write().expect("Poisoning?").add(l))
                }).expect("Loading Texture"),
                Some("tga") => al_ref.load::<TGA>(&asset_path).and_then(|l| {
                    trace!("Texture Loaded! {}", asset_path);
                    Ok(textures_ref.write().expect("Poisoning?").add(l))
                }).expect("Loading Texture"),
                Some("webp") => al_ref.load::<WebP>(&asset_path).and_then(|l| {
                    trace!("Texture Loaded! {}", asset_path);
                    Ok(textures_ref.write().expect("Poisoning?").add(l))
                }).expect("Loading Texture"),
                t => panic!("Unsupported Texture: {:?}", t)
            }
        }).collect::<Vec<_>>();
        for texindex in loaded_textures
        {
            texture_slot_numbers.push(texindex);
        }

        PMXDataPlacementOffsets {
            vbuf_suballoc_positions, vbuf_suballoc_normals, vbuf_suballoc_uvs, ibuf_offset,
            texture_slot_numbers
        }
    }
    fn stage_data_into(&self, mem: &br::MappedMemoryRange, offsets: Self::PreallocOffsetType) -> PMXRenderingParams {
        let positions_stg = unsafe { mem.slice_mut(offsets.vbuf_suballoc_positions, self.vertices.len()) };
        let normals_stg = unsafe { mem.slice_mut(offsets.vbuf_suballoc_normals, self.vertices.len()) };
        let uvs_stg = unsafe { mem.slice_mut(offsets.vbuf_suballoc_uvs, self.vertices.len()) };
        for (i, v) in self.vertices.iter().enumerate() {
            positions_stg[i] = Vector4(v.position.0, v.position.1, v.position.2, 1.0);
            normals_stg[i] = Vector4(v.normal.0, v.normal.1, v.normal.2, 0.0);
            uvs_stg[i] = Vector2(v.uv.0, v.uv.1);
        }

        let index_size = match self.surfaces {
            mmdloader::pmx::SurfaceSection::Long(ref lv) => unsafe {
                // 32bit indices
                mem.slice_mut(offsets.ibuf_offset, self.surfaces.len()).clone_from_slice(&lv);
                br::IndexType::U32
            },
            mmdloader::pmx::SurfaceSection::Short(ref lv) => unsafe {
                // 16bit indices
                mem.slice_mut(offsets.ibuf_offset, self.surfaces.len()).clone_from_slice(&lv);
                br::IndexType::U16
            },
            mmdloader::pmx::SurfaceSection::Byte(ref lv) => unsafe {
                // 16bit indices with extending
                for (d, s) in mem.slice_mut(offsets.ibuf_offset, self.surfaces.len()).iter_mut().zip(lv) {
                    *d = [s[0] as u16, s[1] as u16, s[2] as u16];
                }
                br::IndexType::U16
            }
        };

        let mut textured_surface_ranges = Vec::new();
        let mut untextured_surface_ranges = Vec::new();
        let mut processed_surfaces = 0;
        for (n, mat) in self.materials.iter().enumerate() {
            let ps2 = processed_surfaces + mat.surface_affects;
            if mat.texture_index.is_some() {
                textured_surface_ranges.push((n, processed_surfaces as u32 .. ps2 as u32));
            }
            else {
                untextured_surface_ranges.push((n, processed_surfaces as u32 .. ps2 as u32));
            }
            processed_surfaces = ps2;
        }

        PMXRenderingParams {
            index_size, offsets, index_count: (self.surfaces.len() * 3) as _,
            textured_surface_ranges, untextured_surface_ranges
        }
    }
}
impl PMXRenderingParams {
    pub fn set_vertex_buffer(&self, cmd: &mut br::CmdRecord, buffer: &Buffer) {
        cmd.bind_vertex_buffers(0, &[
            (buffer, self.offsets.vbuf_suballoc_positions),
            (buffer, self.offsets.vbuf_suballoc_normals),
            (buffer, self.offsets.vbuf_suballoc_uvs),
        ]);
    }
    pub fn untextured_render(&self, cmd: &mut br::CmdRecord, buffer: &Buffer, model: &PolygonModelExtended) {
        let index_multiplier = if self.index_size == br::IndexType::U16 { 1 } else { 2 };
        for &(nmat, ref sr) in &self.untextured_surface_ranges {
            cmd.push_graphics_constant(br::ShaderStage::FRAGMENT, 0, &model.materials[nmat].diffuse_color);
            cmd.bind_index_buffer(buffer,
                self.offsets.ibuf_offset + ((sr.start as usize) << index_multiplier),
                self.index_size);
            cmd.draw_indexed(sr.len() as _, 1, 0, 0, 0);
        }
    }
    pub fn textured_render(&self, cmd: &mut br::CmdRecord, buffer: &Buffer, model: &PolygonModelExtended,
            texture_descs: &[br::vk::VkDescriptorSet]) {
        let index_multiplier = if self.index_size == br::IndexType::U16 { 1 } else { 2 };
        for &(nmat, ref sr) in &self.textured_surface_ranges {
            let texture_slot_index = self.offsets.texture_slot_numbers[
                model.materials[nmat].texture_index.as_ref().expect("No Texture?").as_index() as usize];
            cmd.push_graphics_constant(br::ShaderStage::FRAGMENT, 0, &model.materials[nmat].diffuse_color);
            cmd.bind_graphics_descriptor_sets(2, &texture_descs[texture_slot_index .. texture_slot_index + 1], &[]);
            cmd.bind_index_buffer(buffer,
                self.offsets.ibuf_offset + ((sr.start as usize) << index_multiplier),
                self.index_size);
            cmd.draw_indexed(sr.len() as _, 1, 0, 0, 0);
        }
    }
}

impl ModelData for super::GLTFBinary
{
    type PreallocOffsetType = (Vec<usize>, Option<usize>);
    type RendererParams = Vec<usize>;

    fn prealloc<EH: EngineEvents<NL>, NL: NativeLinker>(&self, e: &Engine<EH, NL>, alloc: &mut BufferPrealloc,
        _: &mut TextureInitializationGroup) -> (Vec<usize>, Option<usize>)
    {
        let mut offsets = Vec::with_capacity(self.buffers().len());
        for b in self.buffers()
        {
            offsets.push(alloc.add(BufferContent::RawPair(b.data.len() as _, b.usage)) as _);
        }
        let wb_offs = self.u8_to_u16_buffer().map(|b|
        {
            alloc.add(BufferContent::RawPair((b.copied_bytes << 1) as u64, b.usage)) as _
        });
        
        (offsets, wb_offs)
    }
    fn stage_data_into(&self, mem: &br::MappedMemoryRange, (mut offsets, wbuf_offset): (Vec<usize>, Option<usize>))
        -> Vec<usize>
    {
        for (&o, b) in offsets.iter().zip(self.buffers())
        {
            unsafe { mem.slice_mut(o, b.data.len()).copy_from_slice(&b.data); }
        }
        if let Some(wbuf_offs) = wbuf_offset
        {
            offsets.push(wbuf_offs);

            let mut copied = 0;
            for &(o, ref brange) in &self.u8_to_u16_buffer().expect("inconsistent state").buffer_slices
            {
                let slice = unsafe { mem.slice_mut::<u16>(wbuf_offs + copied, brange.len()) };
                for (&b8, dest) in self.buffers()[o].data[brange.clone()].iter().zip(slice)
                {
                    *dest = b8 as u16;
                }
                copied += brange.len() << 1;
            }
        }
        offsets
    }
}
