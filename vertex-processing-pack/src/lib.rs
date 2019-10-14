//! Vertex Processing Stage Container

extern crate bedrock;
extern crate peridot_serialization_utils; use peridot_serialization_utils::*;

use bedrock as br;
use std::io::{
    Read, Write, BufRead, BufReader, Seek, SeekFrom, Result as IOResult, Error as IOError, ErrorKind, Cursor
};
use std::fs::File;
use std::path::Path;
use std::ffi::CString;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PvpContainer {
    pub vertex_bindings: Vec<br::vk::VkVertexInputBindingDescription>,
    pub vertex_attributes: Vec<br::vk::VkVertexInputAttributeDescription>,
    pub vertex_shader: Vec<u8>,
    pub fragment_shader: Option<Vec<u8>>,
}
impl PvpContainer {
    pub fn empty() -> Self {
        PvpContainer {
            vertex_bindings: Vec::new(), vertex_attributes: Vec::new(), vertex_shader: Vec::new(),
            fragment_shader: None
        }
    }

    pub fn write<W: Write>(&self, writer: &mut W) -> IOResult<()> {
        writer.write(b"PVP\x01")?;  // ヘッダ(シグネチャとバージョン)

        // バイナリを裏で構築しつつオフセット値を書き出す
        let mut blob = Cursor::new(Vec::new());
        self.vertex_bindings.binary_serialize(&mut blob)?;
        VariableUInt((blob.seek(SeekFrom::Current(0))?) as _).write(writer)?;
        self.vertex_attributes.binary_serialize(&mut blob)?;
        VariableUInt((blob.seek(SeekFrom::Current(0))?) as _).write(writer)?;
        self.vertex_shader.binary_serialize(&mut blob)?;
        if let Some(ref b) = self.fragment_shader {
            VariableUInt((blob.seek(SeekFrom::Current(0))?) as _).write(writer)?;
            b.binary_serialize(&mut blob)?;
        }
        else { VariableUInt(0).write(writer)?; }

        writer.write(&blob.into_inner()).map(drop)
    }
}

impl peridot::LogicalAssetData for PvpContainer { const EXT: &'static str = "pvp"; }
impl peridot::FromAsset for PvpContainer
{
    type Error = IOError;

    fn from_asset<Asset: Read + Seek + 'static>(_path: &str, asset: Asset) -> IOResult<Self> {
        PvpContainerReader::new(BufReader::new(asset)).and_then(PvpContainerReader::into_container)
    }
}
pub struct PvpShaderModules<'d>
{
    bindings: Vec<br::vk::VkVertexInputBindingDescription>, attributes: Vec<br::vk::VkVertexInputAttributeDescription>,
    vertex: br::ShaderModule, fragment: Option<br::ShaderModule>,
    vertex_spec_constants: Option<(Vec<br::vk::VkSpecializationMapEntry>, br::DynamicDataCell<'d>)>,
    fragment_spec_constants: Option<(Vec<br::vk::VkSpecializationMapEntry>, br::DynamicDataCell<'d>)>,
}
impl<'d> PvpShaderModules<'d>
{
    pub fn new(device: &br::Device, container: PvpContainer) -> br::Result<Self>
    {
        let fragment = container.fragment_shader.map(|b| br::ShaderModule::from_memory(device, &b)).transpose()?;

        Ok(PvpShaderModules
        {
            vertex: br::ShaderModule::from_memory(device, &container.vertex_shader)?, fragment,
            bindings: container.vertex_bindings, attributes: container.vertex_attributes,
            vertex_spec_constants: None, fragment_spec_constants: None
        })
    }
    pub fn generate_vps(&'d self, primitive_topo: br::vk::VkPrimitiveTopology) -> br::VertexProcessingStages<'d>
    {
        let mut r = br::VertexProcessingStages::new(br::PipelineShader
        {
            module: &self.vertex, entry_name: CString::new("main").expect("unreachable"),
            specinfo: self.vertex_spec_constants.clone()
        }, &self.bindings, &self.attributes, primitive_topo);
        if let Some(ref f) = self.fragment
        {
            r.fragment_shader(br::PipelineShader
            {
                module: f, entry_name: CString::new("main").expect("unreachable"),
                specinfo: self.fragment_spec_constants.clone()
            });
        }
        return r;
    }
}

pub struct PvpContainerReader<R: BufRead + Seek> {
    vb_offset: usize, va_offset: usize, vsh_offset: usize, fsh_offset: Option<usize>,
    reader: R
}
impl<R: BufRead + Seek> PvpContainerReader<R> {
    pub fn new(mut reader: R) -> IOResult<Self> {
        let mut signature = [0u8; 4];
        reader.read_exact(&mut signature)?;
        if &signature != b"PVP\x01" {
            return Err(IOError::new(ErrorKind::Other,
                "Signature mismatch: Invalid or corrupted Peridot Vertex Processing file"));
        }

        let VariableUInt(va_offset) = VariableUInt::read(&mut reader)?;
        let VariableUInt(vsh_offset) = VariableUInt::read(&mut reader)?;
        let VariableUInt(fsh_offset_0) = VariableUInt::read(&mut reader)?;
        let blob_offset = reader.seek(SeekFrom::Current(0))? as usize;

        return Ok(PvpContainerReader {
            vb_offset: blob_offset as _, va_offset: (va_offset + blob_offset as u32) as _,
            vsh_offset: (vsh_offset + blob_offset as u32) as _,
            fsh_offset: if fsh_offset_0 == 0 { None } else { Some((fsh_offset_0 + blob_offset as u32) as _) },
            reader
        });
    }

    pub fn read_vertex_bindings(&mut self) -> IOResult<Vec<br::vk::VkVertexInputBindingDescription>> {
        self.reader.seek(SeekFrom::Start(self.vb_offset as _))?;
        Vec::<_>::binary_unserialize(&mut self.reader)
    }
    pub fn read_vertex_attributes(&mut self) -> IOResult<Vec<br::vk::VkVertexInputAttributeDescription>> {
        self.reader.seek(SeekFrom::Start(self.va_offset as _))?;
        Vec::<_>::binary_unserialize(&mut self.reader)
    }
    pub fn read_vertex_shader(&mut self) -> IOResult<Vec<u8>> {
        self.reader.seek(SeekFrom::Start(self.vsh_offset as _))?;
        Vec::<u8>::binary_unserialize(&mut self.reader)
    }
    pub fn is_fragment_stage_provided(&mut self) -> bool { self.fsh_offset.is_some() }
    pub fn read_fragment_shader(&mut self) -> IOResult<Vec<u8>> {
        self.reader.seek(SeekFrom::Start(self.fsh_offset.expect("no fsh") as _))?;
        Vec::<u8>::binary_unserialize(&mut self.reader)
    }

    pub fn into_container(mut self) -> IOResult<PvpContainer> {
        Ok(PvpContainer {
            vertex_bindings: self.read_vertex_bindings()?,
            vertex_attributes: self.read_vertex_attributes()?,
            vertex_shader: self.read_vertex_shader()?,
            fragment_shader: if self.is_fragment_stage_provided() { Some(self.read_fragment_shader()?) } else { None }
        })
    }
}
impl PvpContainerReader<BufReader<File>> {
    pub fn from_file<P: AsRef<Path>>(path: P) -> IOResult<Self> {
        File::open(path).and_then(|fp| Self::new(BufReader::new(fp)))
    }
}

trait BinarySerializeVkStructures {
    fn binary_serialize<W: Write>(&self, sink: &mut W) -> IOResult<usize>;
    fn binary_unserialize<R: BufRead>(source: &mut R) -> IOResult<Self> where Self: Sized;
    fn serialize_into_memory(&self) -> IOResult<Vec<u8>> {
        let mut sink = Cursor::new(Vec::new());
        self.binary_serialize(&mut sink).map(|_| sink.into_inner())
    }
}
impl BinarySerializeVkStructures for br::vk::VkVertexInputBindingDescription {
    fn binary_serialize<W: Write>(&self, sink: &mut W) -> IOResult<usize> {
        VariableUInt(self.inputRate as _).write(sink)
            .and_then(|w0| VariableUInt(self.binding as _).write(sink).map(move |w1| w1 + w0))
            .and_then(|w0| VariableUInt(self.stride as _).write(sink).map(move |w1| w1 + w0))
    }
    fn binary_unserialize<R: BufRead>(source: &mut R) -> IOResult<Self> where Self: Sized {
        let VariableUInt(input_rate) = VariableUInt::read(source)?;
        let VariableUInt(binding) = VariableUInt::read(source)?;
        let VariableUInt(stride) = VariableUInt::read(source)?;
        return Ok(br::vk::VkVertexInputBindingDescription {
            inputRate: input_rate as _, binding: binding as _, stride: stride as _
        });
    }
}
impl BinarySerializeVkStructures for br::vk::VkVertexInputAttributeDescription {
    fn binary_serialize<W: Write>(&self, sink: &mut W) -> IOResult<usize> {
        VariableUInt(self.location as _).write(sink)
            .and_then(|w0| VariableUInt(self.binding as _).write(sink).map(move |w1| w1 + w0))
            .and_then(|w0| VariableUInt(self.offset as _).write(sink).map(move |w1| w1 + w0))
            .and_then(|w0| VariableUInt(self.format as _).write(sink).map(move |w1| w1 + w0))
    }
    fn binary_unserialize<R: BufRead>(source: &mut R) -> IOResult<Self> where Self: Sized {
        let VariableUInt(location) = VariableUInt::read(source)?;
        let VariableUInt(binding) = VariableUInt::read(source)?;
        let VariableUInt(offset) = VariableUInt::read(source)?;
        let VariableUInt(format) = VariableUInt::read(source)?;
        return Ok(br::vk::VkVertexInputAttributeDescription {
            location: location as _, binding: binding as _, offset: offset as _,
            format: format as _
        });
    }
}
impl<T: BinarySerializeVkStructures> BinarySerializeVkStructures for Vec<T> {
    fn binary_serialize<W: Write>(&self, sink: &mut W) -> IOResult<usize> {
        let mut write_bytes = VariableUInt(self.len() as _).write(sink)?;
        for x in self { write_bytes += x.binary_serialize(sink)?; } return Ok(write_bytes);
    }
    fn binary_unserialize<R: BufRead>(source: &mut R) -> IOResult<Self> where Self: Sized {
        let VariableUInt(element_count) = VariableUInt::read(source)?;
        let mut vs = Vec::with_capacity(element_count as _);
        for _ in 0 .. element_count { vs.push(T::binary_unserialize(source)?); }
        return Ok(vs);
    }
}
impl BinarySerializeVkStructures for Vec<u8> {
    fn binary_serialize<W: Write>(&self, sink: &mut W) -> IOResult<usize> {
        VariableUInt(self.len() as _).write(sink)
            .and_then(|w0| sink.write_all(self).map(move |_| self.len() + w0))
    }
    fn binary_unserialize<R: BufRead>(source: &mut R) -> IOResult<Self> where Self: Sized {
        let VariableUInt(element_count) = VariableUInt::read(source)?;
        let mut buf = vec![0u8; element_count as usize];
        source.read_exact(&mut buf).map(|_| buf)
    }
}
