
use std::io::{Read, Result as IOResult};
use bedrock as br;
use std::collections::BTreeMap;
#[macro_use] extern crate serde_derive;
#[macro_use] extern crate serde_repr;

#[derive(Debug)]
pub enum Chunk { JSON(Vec<u8>), Bin(Vec<u8>) }
impl Chunk
{
    pub fn read<R: Read + ?Sized>(reader: &mut R) -> IOResult<Self>
    {
        let mut header = [0u32; 2];
        reader.read_exact(unsafe { &mut *(&mut header as *mut _ as *mut [u8; 8]) })?;

        let mut data = Vec::with_capacity(header[0] as _);
        unsafe { data.set_len(header[0] as _); }
        reader.read_exact(&mut data)?;

        if header[1] == 0x4e4f534a { Ok(Chunk::JSON(data)) } else { Ok(Chunk::Bin(data)) }
    }

    pub fn bytes(&self) -> &[u8]
    {
        match self { Chunk::JSON(ref b) | Chunk::Bin(ref b) => b as _ }
    }
    pub fn unwrap_json(self) -> Vec<u8>
    {
        if let Chunk::JSON(j) = self { j } else { panic!("Not a JSON Chunk") }
    }
    pub fn unwrap_bin(self) -> Vec<u8>
    {
        if let Chunk::Bin(j) = self { j } else { panic!("Not a Binary Chunk") }
    }
}

pub fn read_glb<R: Read + ?Sized>(reader: &mut R) -> IOResult<GLBChunkReader<R>>
{
    let mut header = [0u32; 3];
    reader.read_exact(unsafe { &mut *(&mut header as *mut _ as *mut [u8; 12]) })?;
    if header[0] != 0x46546c67 { panic!("Signature Mismatch"); }

    println!("glTF Version: {}", header[1]);
    println!("File Length: {}", header[2]);

    let chunks = GLBChunkReader(reader, (header[2] as usize) - 12);
    Ok(chunks)
}
pub fn deserialize_info_json(v: &[u8]) -> Root { serde_json::from_slice(v).expect("Deserializing glTF Info") }
pub struct GLBChunkReader<'r, R: 'r + Read + ?Sized>(&'r mut R, usize);
impl<'r, R: 'r + Read + ?Sized> Iterator for GLBChunkReader<'r, R>
{
    type Item = IOResult<Chunk>;

    fn next(&mut self) -> Option<IOResult<Chunk>>
    {
        if self.1 <= 0 { return None; }

        Some(Chunk::read(self.0).map(|c|
        {
            self.1 -= 8 + c.bytes().len();
            c
        }))
    }
}

use std::collections::HashMap;

#[derive(Deserialize_repr, Debug, Clone, Copy)]
#[repr(u8)] pub enum PrimitiveMode
{
    Points = 0, Lines, LineLoop, LineStrip, Triangles, TriangleStrip, TriangleFan
}
impl Default for PrimitiveMode
{
    fn default() -> Self { PrimitiveMode::Triangles }
}
impl PrimitiveMode
{
    pub fn as_vk_topology(self) -> br::vk::VkPrimitiveTopology
    {
        match self
        {
            PrimitiveMode::Points => br::vk::VK_PRIMITIVE_TOPOLOGY_POINT_LIST,
            PrimitiveMode::Lines => br::vk::VK_PRIMITIVE_TOPOLOGY_LINE_LIST,
            PrimitiveMode::Triangles => br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST,
            PrimitiveMode::LineStrip => br::vk::VK_PRIMITIVE_TOPOLOGY_LINE_STRIP,
            PrimitiveMode::TriangleStrip => br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP,
            PrimitiveMode::TriangleFan => br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN,
            PrimitiveMode::LineLoop => panic!("Unable to handle LineLoop")
        }
    }
}

#[derive(Deserialize, Debug)]
pub struct Primitive<'d>
{
    #[serde(borrow = "'d")]
    attributes: HashMap<&'d str, usize>,
    #[serde(default)]
    mode: PrimitiveMode,
    indices: Option<usize>,
    material: Option<usize>
}
#[derive(Deserialize, Debug)]
pub struct Mesh<'d>
{
    #[serde(borrow = "'d")]
    primitives: Vec<Primitive<'d>>
}

#[derive(Deserialize_repr, Debug, PartialEq, Eq)] #[repr(u16)]
pub enum ComponentType
{
    Byte = 5120, UnsignedByte, Short, UnsignedShort,
    UnsignedInt = 5125, Float
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Accessor<'d>
{
    component_type: ComponentType,
    #[serde(default)]
    normalized: bool,
    count: u32,
    #[serde(rename = "type")]
    type_: &'d str,
    buffer_view: Option<usize>
}

#[derive(Deserialize_repr, Debug)] #[repr(u16)]
pub enum BufferBoundTarget
{
    VertexBuffer = 34962, IndexBuffer
}
#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct BufferView
{
    buffer: usize, byte_length: usize,
    #[serde(default)]
    byte_offset: usize,
    byte_stride: Option<usize>,
    target: Option<BufferBoundTarget>
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct PBRMetallicRoughness
{
    #[serde(default = "PBRMetallicRoughness::default_base_color")]
    base_color_factor: [f32; 4],
    #[serde(default = "PBRMetallicRoughness::default_one")]
    metallic_factor: f32,
    #[serde(default = "PBRMetallicRoughness::default_one")]
    roughness_factor: f32
}
impl PBRMetallicRoughness
{
    pub fn default_base_color() -> [f32; 4] { [1.0; 4] }
    pub fn default_one() -> f32 { 1.0 }
}
#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Material
{
    pbr_metallic_roughness: Option<PBRMetallicRoughness>
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Buffer<'d>
{
    byte_length: usize,
    uri: Option<&'d str>
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Root<'d>
{
    #[serde(borrow = "'d")]
    accessors: Vec<Accessor<'d>>,
    #[serde(borrow = "'d")]
    meshes: Vec<Mesh<'d>>,
    buffer_views: Vec<BufferView>,
    materials: Vec<Material>,
    #[serde(borrow = "'d")]
    buffers: Vec<Buffer<'d>>
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub struct HashableF32(pub f32);
impl Eq for HashableF32 {}
impl std::hash::Hash for HashableF32
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) { i32::hash(unsafe { &std::mem::transmute(self.0) }, state) }
}
impl From<f32> for HashableF32 { fn from(v: f32) -> Self { HashableF32(v) } }

use std::ops::Range;
#[derive(PartialEq, Eq, Hash, Debug, Clone)] pub enum AlphaMode { Opaque, Masked(HashableF32), Blended }
#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct PipelineSetting
{
    pub topo: br::vk::VkPrimitiveTopology, pub alpha_mode: AlphaMode, pub enable_back_culling: bool
}
#[derive(Debug)]
pub struct MeshRenderingParam
{
    pub vertex_buffers: HashMap<String, (usize, Range<u64>)>,
    pub index_buffer: Option<(usize, Range<u64>)>,
    pub vertex_count: u32
}
#[derive(Debug, PartialEq)]
pub struct PBRMaterialData
{
    pub albedo: [f32; 4], pub metallic: f32, pub roughness: f32
}
impl Eq for PBRMaterialData { /* assuming there is no NaN values */ }
impl std::hash::Hash for PBRMaterialData
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H)
    {
        unsafe
        {
            std::mem::transmute::<_, &[i32; 4]>(&self.albedo).hash(state);
            std::mem::transmute::<_, &i32>(&self.metallic).hash(state);
            std::mem::transmute::<_, &i32>(&self.roughness).hash(state);
        }
    }
}
pub struct BufferObject { pub data: Vec<u8>, pub usage: br::BufferUsage }
pub struct WideCastedBuffer
{
    pub buffer_slices: Vec<(usize, Range<usize>)>, pub usage: br::BufferUsage, pub copied_bytes: usize
}
pub struct GLTFRenderableObject
{
    buffer_objects: Vec<BufferObject>, wide_casted_buffer: Option<WideCastedBuffer>,
    pipelines: Vec<PipelineSetting>,
    rendering_groups: Vec<(PBRMaterialData, Vec<(usize, Vec<MeshRenderingParam>)>)>
}
impl GLTFRenderableObject
{
    pub fn new(info: &Root, chunks: impl Iterator<Item = Chunk>) -> Self
    {
        let mut buffer_objects: Vec<_> = chunks.take(info.buffers.len())
            .map(|d| BufferObject { data: d.unwrap_bin(), usage: br::BufferUsage(0) }).collect();

        let mut wide_casted_buffer = None;
        let mut rendering_groups = HashMap::new();
        let mut pipeline_cache = HashMap::new();
        let mut pipelines = Vec::new();
        for p in info.meshes.iter().flat_map(|m| &m.primitives)
        {
            let material_ref = p.material.and_then(|i| info.materials[i].pbr_metallic_roughness.as_ref());
            let pbr_material_param = if let Some(ref pbr) = material_ref
            {
                PBRMaterialData
                {
                    albedo: pbr.base_color_factor, metallic: pbr.metallic_factor, roughness: pbr.roughness_factor
                }
            }
            else
            {
                PBRMaterialData
                {
                    albedo: [1.0; 4], metallic: 1.0, roughness: 1.0
                }
            };
            let pipeline_state = PipelineSetting
            {
                topo: p.mode.as_vk_topology(), alpha_mode: AlphaMode::Opaque, enable_back_culling: true
            };
            let pipeline_index = if let Some(&pi) = pipeline_cache.get(&pipeline_state) { pi } else
            {
                let pi = pipelines.len();
                pipelines.push(pipeline_state.clone());
                pipeline_cache.insert(pipeline_state, pi);
                pi
            };

            let mut vertex_buffers = HashMap::with_capacity(p.attributes.len());
            for (&k, &v) in &p.attributes
            {
                let buffer_a = &info.accessors[v];
                let buffer_view = &info.buffer_views[
                    *buffer_a.buffer_view.as_ref().expect("BufferView required for inputs of vertices")
                ];
                buffer_objects[buffer_view.buffer].usage = buffer_objects[buffer_view.buffer].usage.vertex_buffer();

                vertex_buffers.insert(k.to_owned(), (
                    buffer_view.buffer,
                    buffer_view.byte_offset as _ .. buffer_view.byte_offset as u64 + buffer_view.byte_length as u64
                ));
            }
            let mrp = if let Some(i) = p.indices
            {
                let index_buffer_accessor = &info.accessors[i];
                let ib_index = index_buffer_accessor.buffer_view.expect("BufferView required for inputs of indices");
                let index_buffer_view = &info.buffer_views[ib_index];
                let ib_pair = if index_buffer_accessor.component_type == ComponentType::Byte ||
                    index_buffer_accessor.component_type == ComponentType::UnsignedByte
                {
                    // require converting u8 -> u16
                    if wide_casted_buffer.is_none()
                    {
                        wide_casted_buffer = Some(WideCastedBuffer
                        {
                            buffer_slices: Vec::new(), usage: br::BufferUsage(0), copied_bytes: 0
                        });
                    }
                    let mref = wide_casted_buffer.as_mut().expect("bug");
                    mref.buffer_slices.push((ib_index,
                        index_buffer_view.byte_offset .. index_buffer_view.byte_offset + index_buffer_view.byte_length
                    ));
                    let oldbase = (mref.copied_bytes << 1) as u64;
                    mref.copied_bytes += index_buffer_view.byte_length;
                    mref.usage = mref.usage.index_buffer();

                    (buffer_objects.len(), oldbase .. oldbase + (index_buffer_view.byte_length << 1) as u64)
                }
                else
                {
                    buffer_objects[index_buffer_view.buffer].usage =
                        buffer_objects[index_buffer_view.buffer].usage.index_buffer();
                    
                    (index_buffer_view.buffer, index_buffer_view.byte_offset as _ ..
                        index_buffer_view.byte_offset as u64 + index_buffer_view.byte_length as u64)
                };

                MeshRenderingParam
                {
                    vertex_buffers, index_buffer: Some(ib_pair),
                    vertex_count: index_buffer_accessor.count
                }
            }
            else
            {
                MeshRenderingParam
                {
                    vertex_buffers, index_buffer: None,
                    vertex_count: info.accessors[
                        *p.attributes.get("POSITION").expect("Required POSITION attribute for rendering")
                    ].count
                }
            };

            rendering_groups.entry(pbr_material_param).or_insert_with(BTreeMap::new)
                .entry(pipeline_index).or_insert_with(Vec::new).push(mrp);
        }

        GLTFRenderableObject
        {
            buffer_objects, wide_casted_buffer, pipelines,
            rendering_groups: rendering_groups.into_iter().map(|(k, v)| (k, v.into_iter().collect())).collect()
        }
    }

    pub fn buffers(&self) -> &[BufferObject] { &self.buffer_objects }
    pub fn u8_to_u16_buffer(&self) -> Option<&WideCastedBuffer> { self.wide_casted_buffer.as_ref() }
    pub fn required_pipelines(&self) -> &[PipelineSetting] { &self.pipelines }
    /// Iterate this to render the glTF file
    pub fn render_params(&self) -> &[(PBRMaterialData, Vec<(usize, Vec<MeshRenderingParam>)>)]
    {
        &self.rendering_groups
    }
}
