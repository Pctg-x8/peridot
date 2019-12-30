#![warn(clippy::all)]

//! Peridot MMD(Polygon Model eXtended/Vocaloid(!?) Motion Data) Loader

extern crate encoding;

use std::fs::File;
use std::io::{Read, BufReader, Seek};
use std::path::{Path, PathBuf};
use std::ops::Range;
use std::sync::RwLock;
use rayon::prelude::*;
use bedrock as br;

#[macro_use] extern crate log;

pub mod pmx;
pub mod vmd;
pub use vmd::MotionData;

pub struct PolygonModelExtended {
    pub base_components: Vec<String>,
    pub header: pmx::Header, pub vertices: Vec<pmx::Vertex>, pub surfaces: pmx::SurfaceSection,
    pub textures: Vec<PathBuf>, pub materials: Vec<pmx::Material>, pub bones: Vec<pmx::Bone>,
    pub morphs: Vec<pmx::Morph>, pub display_frames: Vec<pmx::DisplayFrame>, rigid_bodies: Vec<pmx::RigidBody>,
    joints: Vec<pmx::Joint>, softbodies: Vec<pmx::Softbody>
}
impl PolygonModelExtended
{
    pub fn from_file<P: AsRef<Path> + ?Sized>(filepath: &P) -> Result<Self, pmx::LoadingError>
    {
        let mut base_components = filepath.as_ref().components()
            .map(|x| x.as_os_str().to_string_lossy().into_owned()).collect::<Vec<_>>();
        base_components.pop();
        File::open(filepath).map(BufReader::new).map_err(From::from).and_then(|r| Self::load(base_components, r))
    }
    pub fn load<R: Read>(base_components: Vec<String>, mut reader: R) -> Result<Self, pmx::LoadingError>
    {
        let header = pmx::Header::load(&mut reader)?;
        let vertices = pmx::read_array(&mut reader,
            |r| pmx::Vertex::read(r, header.additional_vec4_count as _, header.index_sizes.bone))?;
        let surfaces = pmx::SurfaceSection::read(&mut reader, header.index_sizes.vertex)?;
        let textures = pmx::read_array(&mut reader,
            |r| header.string_reader.read_string(r).map(PathBuf::from).map_err(From::from))?;
        let materials = pmx::read_array(&mut reader, |r| pmx::Material::read(r, &header))?;
        let bones = pmx::read_array(&mut reader, |r| pmx::Bone::read(r, &header))?;
        let morphs = pmx::read_array(&mut reader, |r| pmx::Morph::read(r, &header))?;
        let display_frames = pmx::read_array(&mut reader, |r| pmx::DisplayFrame::read(r, &header))?;
        let rigid_bodies = pmx::read_array(&mut reader, |r| pmx::RigidBody::read(r, &header))?;
        let joints = pmx::read_array(&mut reader, |r| pmx::Joint::read(r, &header))?;
        let softbodies = if header.version > 2.0 {
            pmx::read_array(&mut reader, |r| pmx::Softbody::read(r, &header))?
        }
        else { Vec::new() };

        Ok(PolygonModelExtended
        {
            base_components,
            header, vertices, surfaces, textures, materials, bones, morphs, display_frames, rigid_bodies,
            joints, softbodies
        })
    }
}
/// Exports
impl PolygonModelExtended
{
    /// Name of the model, in Japanese.
    pub fn name_jp(&self) -> &str { &self.header.model_name.jp }
    /// Name of the model
    pub fn name(&self) -> &str { &self.header.model_name.univ }
}

// Asset Impl //
impl peridot::LogicalAssetData for PolygonModelExtended { const EXT: &'static str = "pmx"; }
impl peridot::FromAsset for PolygonModelExtended
{
    type Error = pmx::LoadingError;

    fn from_asset<Asset: Read + Seek + 'static>(path: &str, asset: Asset) -> Result<Self, Self::Error>
    {
        let mut base = path.split('.').map(|c| c.to_owned()).collect::<Vec<_>>(); base.pop();
        PolygonModelExtended::load(base, BufReader::new(asset))
    }
}
impl peridot::LogicalAssetData for vmd::MotionData { const EXT: &'static str = "vmd"; }
impl peridot::FromAsset for vmd::MotionData
{
    type Error = vmd::LoadingError;

    fn from_asset<Asset: Read + Seek + 'static>(_path: &str, asset: Asset) -> Result<Self, Self::Error>
    {
        vmd::MotionData::read(&mut BufReader::new(asset))
    }
}

use peridot::{
    Engine, EngineEvents, NativeLinker, BufferPrealloc, TextureInitializationGroup, BufferContent,
    BMP, PNG, TIFF, TGA, WebP, AssetLoaderService, Buffer
};
use peridot::math::{Vector4F32, Vector2F32, Vector4, Vector2};

// Rendering Impl //
pub struct PMXDataPlacementOffsets
{
    pub vbuf_suballoc_positions: usize, pub ibuf_offset: usize,
    pub vbuf_suballoc_normals: usize, pub vbuf_suballoc_uvs: usize,
    pub texture_slot_numbers: Vec<usize>
}
pub struct PMXRenderingParams
{
    index_size: br::IndexType, offsets: PMXDataPlacementOffsets,
    index_count: u32,
    textured_surface_ranges: Vec<(usize, Range<u32>)>,
    untextured_surface_ranges: Vec<(usize, Range<u32>)>
}
impl peridot::ModelData for PolygonModelExtended
{
    type PreallocOffsetType = PMXDataPlacementOffsets;
    type RendererParams = PMXRenderingParams;

    fn prealloc<EH: EngineEvents<NL>, NL: NativeLinker>(&self, e: &Engine<EH, NL>, alloc: &mut BufferPrealloc,
        textures: &mut TextureInitializationGroup) -> Self::PreallocOffsetType
    {
        let vbuf_suballoc_positions = alloc.add(BufferContent::vertices::<Vector4F32>(self.vertices.len())) as _;
        let vbuf_suballoc_normals = alloc.add(BufferContent::vertices::<Vector4F32>(self.vertices.len())) as _;
        let vbuf_suballoc_uvs = alloc.add(BufferContent::vertices::<Vector2F32>(self.vertices.len())) as _;
        let ibuf_offset = if self.header.index_sizes.vertex == pmx::IndexSize::Long
        {
            // use 32bit
            alloc.add(BufferContent::indices::<u32>(self.surfaces.len() * 3)) as _
        }
        else {
            alloc.add(BufferContent::indices::<u16>(self.surfaces.len() * 3)) as _
        };

        let al_ref = e.async_asset_loader();
        let mut texture_slot_numbers = Vec::with_capacity(self.textures.len());
        let textures_ref = RwLock::new(textures);
        let cmp = &self.base_components;
        let loaded_textures = self.textures.par_iter().map(|tex|
        {
            let mut asset_components = cmp.iter().map(|x| x as _).collect::<Vec<&str>>();
            if let Some(p) = tex.parent()
            {
                asset_components.extend(p.components().map(|c| c.as_os_str().to_str().expect("Decoding path")));
            }
            asset_components.push(tex.file_stem().expect("Address not points to a file?")
                .to_str().expect("Decoding path"));
            let asset_path = asset_components.join(".");
            trace!("Loading Asset in MMD: {}", asset_path);
            // switch loader by extension
            match tex.extension().and_then(std::ffi::OsStr::to_str)
            {
                Some("bmp") => al_ref.load::<BMP>(&asset_path).and_then(|l|
                {
                    trace!("Texture Loaded! {}", asset_path);
                    Ok(textures_ref.write().expect("Poisoning?").add(l))
                }).expect("Loading Texture"),
                Some("png") => al_ref.load::<PNG>(&asset_path).and_then(|l|
                {
                    trace!("Texture Loaded! {}", asset_path);
                    Ok(textures_ref.write().expect("Poisoning?").add(l))
                }).expect("Loading Texture"),
                Some("tiff") => al_ref.load::<TIFF>(&asset_path).and_then(|l|
                {
                    trace!("Texture Loaded! {}", asset_path);
                    Ok(textures_ref.write().expect("Poisoning?").add(l))
                }).expect("Loading Texture"),
                Some("tga") => al_ref.load::<TGA>(&asset_path).and_then(|l|
                {
                    trace!("Texture Loaded! {}", asset_path);
                    Ok(textures_ref.write().expect("Poisoning?").add(l))
                }).expect("Loading Texture"),
                Some("webp") => al_ref.load::<WebP>(&asset_path).and_then(|l| 
                {
                    trace!("Texture Loaded! {}", asset_path);
                    Ok(textures_ref.write().expect("Poisoning?").add(l))
                }).expect("Loading Texture"),
                t => panic!("Unsupported Texture: {:?}", t)
            }
        }).collect::<Vec<_>>();
        for texindex in loaded_textures { texture_slot_numbers.push(texindex); }

        PMXDataPlacementOffsets
        {
            vbuf_suballoc_positions, vbuf_suballoc_normals, vbuf_suballoc_uvs, ibuf_offset,
            texture_slot_numbers
        }
    }
    fn stage_data_into(&self, mem: &br::MappedMemoryRange, offsets: Self::PreallocOffsetType) -> PMXRenderingParams
    {
        let positions_stg = unsafe { mem.slice_mut(offsets.vbuf_suballoc_positions, self.vertices.len()) };
        let normals_stg = unsafe { mem.slice_mut(offsets.vbuf_suballoc_normals, self.vertices.len()) };
        let uvs_stg = unsafe { mem.slice_mut(offsets.vbuf_suballoc_uvs, self.vertices.len()) };
        for (i, v) in self.vertices.iter().enumerate()
        {
            positions_stg[i] = Vector4(v.position.0, v.position.1, v.position.2, 1.0);
            normals_stg[i] = Vector4(v.normal.0, v.normal.1, v.normal.2, 0.0);
            uvs_stg[i] = Vector2(v.uv.0, v.uv.1);
        }

        let index_size = match self.surfaces
        {
            pmx::SurfaceSection::Long(ref lv) => unsafe
            {
                // 32bit indices
                mem.slice_mut(offsets.ibuf_offset, self.surfaces.len()).clone_from_slice(&lv);

                br::IndexType::U32
            },
            pmx::SurfaceSection::Short(ref lv) => unsafe
            {
                // 16bit indices
                mem.slice_mut(offsets.ibuf_offset, self.surfaces.len()).clone_from_slice(&lv);

                br::IndexType::U16
            },
            pmx::SurfaceSection::Byte(ref lv) => unsafe
            {
                // 16bit indices with extending
                #[allow(clippy::cast_lossless)]
                for (d, s) in mem.slice_mut(offsets.ibuf_offset, self.surfaces.len()).iter_mut().zip(lv)
                {
                    *d = [s[0] as u16, s[1] as u16, s[2] as u16];
                }

                br::IndexType::U16
            }
        };

        let mut textured_surface_ranges = Vec::new();
        let mut untextured_surface_ranges = Vec::new();
        let mut processed_surfaces = 0;
        for (n, mat) in self.materials.iter().enumerate()
        {
            let ps2 = processed_surfaces + mat.surface_affects;
            if mat.texture_index.is_some()
            {
                textured_surface_ranges.push((n, processed_surfaces as u32 .. ps2 as u32));
            }
            else {
                untextured_surface_ranges.push((n, processed_surfaces as u32 .. ps2 as u32));
            }
            processed_surfaces = ps2;
        }

        PMXRenderingParams
        {
            index_size, offsets, index_count: (self.surfaces.len() * 3) as _,
            textured_surface_ranges, untextured_surface_ranges
        }
    }
}
impl PMXRenderingParams
{
    pub fn set_vertex_buffer(&self, cmd: &mut br::CmdRecord, buffer: &Buffer)
    {
        cmd.bind_vertex_buffers(0, &[
            (buffer, self.offsets.vbuf_suballoc_positions),
            (buffer, self.offsets.vbuf_suballoc_normals),
            (buffer, self.offsets.vbuf_suballoc_uvs),
        ]);
    }
    pub fn untextured_render(&self, cmd: &mut br::CmdRecord, buffer: &Buffer, model: &PolygonModelExtended)
    {
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
        texture_descs: &[br::vk::VkDescriptorSet])
    {
        let index_multiplier = if self.index_size == br::IndexType::U16 { 1 } else { 2 };
        for &(nmat, ref sr) in &self.textured_surface_ranges
        {
            let texture_slot_index = self.offsets.texture_slot_numbers[
                model.materials[nmat].texture_index.as_ref().expect("No Texture?").as_index() as usize];
            cmd.push_graphics_constant(br::ShaderStage::FRAGMENT, 0, &model.materials[nmat].diffuse_color);
            cmd.bind_graphics_descriptor_sets(2, &texture_descs[texture_slot_index ..= texture_slot_index], &[]);
            cmd.bind_index_buffer(buffer,
                self.offsets.ibuf_offset + ((sr.start as usize) << index_multiplier),
                self.index_size);
            cmd.draw_indexed(sr.len() as _, 1, 0, 0, 0);
        }
    }
}
