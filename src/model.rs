//! ModelData Traits and Impls

use super::*;
use bedrock as br;

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
pub trait DefaultRenderCommands
{
    type Extras;

    fn default_render_commands<EH: EngineEvents<NL>, NL: NativeLinker>(&self, e: &Engine<EH, NL>,
        cmd: &mut br::CmdRecord, buffer: &Buffer, extras: &Self::Extras);
}

#[derive(Debug, Clone)]
pub struct Primitive<VT>
{
    pub vertices: Vec<VT>
}
impl<VT: Clone> ModelData for Primitive<VT>
{
    type PreallocOffsetType = u64;
    type RendererParams = ();

    fn prealloc(&self, alloc: &mut BufferPrealloc) -> u64
    {
        alloc.add(BufferContent::vertices::<VT>(self.vertices.len()))
    }
    fn stage_data_into(&self, mem: &br::MappedMemoryRange, vo: u64)
    {
        unsafe
        {
            mem.slice_mut::<VT>(vo as _, self.vertices.len()).clone_from_slice(&self.vertices);
        }
    }
}
#[derive(Debug, Clone)]
pub struct IndexedPrimitive<VT>
{
    pub vertices: Vec<VT>, pub indices: Vec<u16>
}
impl<VT: Clone> ModelData for IndexedPrimitive<VT>
{
    type PreallocOffsetType = (u64, u64);
    type RendererParams = ();

    fn prealloc(&self, alloc: &mut BufferPrealloc) -> (u64, u64)
    {
        let v = alloc.add(BufferContent::vertices::<VT>(self.vertices.len()));
        let i = alloc.add(BufferContent::indices::<u16>(self.indices.len()));

        (v, i)
    }
    fn stage_data_into(&self, mem: &br::MappedMemoryRange, (vo, io): (u64, u64))
    {
        unsafe
        {
            mem.slice_mut::<VT>(vo as _, self.vertices.len()).clone_from_slice(&self.vertices);
            mem.slice_mut::<u16>(io as _, self.indices.len()).copy_from_slice(&self.indices);
        }
    }
}
impl<VT> Primitive<VT>
{
    pub fn with_indices(self, indices: Vec<u16>) -> IndexedPrimitive<VT>
    {
        IndexedPrimitive { vertices: self.vertices, indices }
    }
}

impl Primitive<math::Vector4F32>
{
    /// 0.0 to size squared 2d plane, rendered as triangle strip
    pub fn plane(size: f32) -> Self
    {
        Primitive
        {
            vertices: vec![
                math::Vector4(0.0, 0.0, 0.0, 1.0),
                math::Vector4(0.0, size, 0.0, 1.0),
                math::Vector4(size, 0.0, 0.0, 1.0),
                math::Vector4(size, size, 0.0, 1.0)
            ]
        }
    }
    /// -size to size squared 2d plane, rendered as triangle strip
    pub fn plane_centric(size: f32) -> Self
    {
        Primitive
        {
            vertices: vec![
                math::Vector4(-size, -size, 0.0, 1.0),
                math::Vector4(-size,  size, 0.0, 1.0),
                math::Vector4( size, -size, 0.0, 1.0),
                math::Vector4( size,  size, 0.0, 1.0)
            ]
        }
    }
}
impl IndexedPrimitive<math::Vector4F32>
{
    /// 0.0 to size squared 2d plane, vertices for rendered as triangle strip, indices for rendered as triangle list
    pub fn plane(size: f32) -> Self
    {
        Primitive::plane(size).with_indices(vec![0, 1, 2, 1, 2, 3])
    }
    /// -size to size squared 2d plane, vertices for rendered as triangle strip, indices for rendered as triangle list
    pub fn plane_centric(size: f32) -> Self
    {
        Primitive::plane_centric(size).with_indices(vec![0, 1, 2, 1, 2, 3])
    }
}

impl Primitive<math::Vector4F32>
{
    /// limited xz grid lines, rendered as line list
    pub fn limited_xz_grid(limit: u32) -> Self
    {
        let xlines = (-(limit as i64) ..= limit as i64).flat_map(|x| vec![
            math::Vector4(x as f32, 0.0, -(limit as f32), 1.0), math::Vector4(x as f32, 0.0, limit as f32, 1.0)
        ]);
        let zlines = (-(limit as i64) ..= limit as i64).flat_map(|z| vec![
            math::Vector4(-(limit as f32), 0.0, z as f32, 1.0), math::Vector4(limit as f32, 0.0, z as f32, 1.0)
        ]);

        Primitive { vertices: xlines.chain(zlines).collect() }
    }
}

#[repr(C, align(16))]
#[derive(Debug, Clone, PartialEq)]
pub struct VertexUV2D
{
    pub pos: math::Vector2F32,
    pub uv: math::Vector2F32
}
impl Primitive<VertexUV2D>
{
    /// 0.0 to size squared 2d plane with normalized uv, rendered as triangle strip
    pub fn uv_plane(size: f32) -> Self
    {
        Primitive
        {
            vertices: vec![
                VertexUV2D { pos: math::Vector2( 0.0,  0.0), uv: math::Vector2(0.0, 0.0) },
                VertexUV2D { pos: math::Vector2( 0.0, size), uv: math::Vector2(0.0, 1.0) },
                VertexUV2D { pos: math::Vector2(size,  0.0), uv: math::Vector2(1.0, 0.0) },
                VertexUV2D { pos: math::Vector2(size, size), uv: math::Vector2(1.0, 1.0) }
            ]
        }
    }
    /// -size to size squared 2d plane with normalized uv, rendered as triangle strip
    pub fn uv_plane_centric(size: f32) -> Self
    {
        Primitive
        {
            vertices: vec![
                VertexUV2D { pos: math::Vector2(-size, -size), uv: math::Vector2(0.0, 0.0) },
                VertexUV2D { pos: math::Vector2(-size,  size), uv: math::Vector2(0.0, 1.0) },
                VertexUV2D { pos: math::Vector2( size, -size), uv: math::Vector2(1.0, 0.0) },
                VertexUV2D { pos: math::Vector2( size,  size), uv: math::Vector2(1.0, 1.0) }
            ]
        }
    }
}
impl IndexedPrimitive<VertexUV2D>
{
    /// 0.0 to size squared 2d plane with normalized uv,
    /// vertices for rendered as triangle strip, indices for rendered as triangle list
    pub fn uv_plane(size: f32) -> Self
    {
        Primitive::uv_plane(size).with_indices(vec![0, 1, 2, 1, 2, 3])
    }
    /// -size to size squared 2d plane with normalized uv,
    /// vertices for rendered as triangle strip, indices for rendered as triangle list
    pub fn uv_plane_centric(size: f32) -> Self
    {
        Primitive::uv_plane_centric(size).with_indices(vec![0, 1, 2, 1, 2, 3])
    }
}
#[repr(C, align(16))]
#[derive(Debug, Clone, PartialEq)]
pub struct ColoredVertex
{
    pub pos: math::Vector4F32,
    pub color: math::Vector4F32
}
impl Primitive<ColoredVertex>
{
    /// colored coordinate axis lines, rendered as line list
    pub fn limited_coordinate_axis(limit: u32) -> Self
    {
        Primitive
        {
            vertices: vec![
                ColoredVertex
                {
                    pos: math::Vector4(0.0, 0.0, 0.0, 1.0), color: math::Vector4(1.0, 0.0, 0.0, 1.0)
                },
                ColoredVertex
                {
                    pos: math::Vector4(limit as _, 0.0, 0.0, 1.0), color: math::Vector4(1.0, 0.0, 0.0, 1.0)
                },
                ColoredVertex
                {
                    pos: math::Vector4(0.0, 0.0, 0.0, 1.0), color: math::Vector4(0.0, 1.0, 0.0, 1.0)
                },
                ColoredVertex
                {
                    pos: math::Vector4(0.0, limit as _, 0.0, 1.0), color: math::Vector4(0.0, 1.0, 0.0, 1.0)
                },
                ColoredVertex
                {
                    pos: math::Vector4(0.0, 0.0, 0.0, 1.0), color: math::Vector4(0.0, 0.0, 1.0, 1.0)
                },
                ColoredVertex
                {
                    pos: math::Vector4(0.0, 0.0, limit as _, 1.0), color: math::Vector4(0.0, 0.0, 1.0, 1.0)
                }
            ]
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

    fn prealloc<EH: EngineEvents<NL>, NL: NativeLinker>(&self, _e: &Engine<EH, NL>, alloc: &mut BufferPrealloc,
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
