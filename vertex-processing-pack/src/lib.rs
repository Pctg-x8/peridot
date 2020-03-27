//! Vertex Processing Stage Container

extern crate bedrock;
extern crate peridot_serialization_utils; use peridot_serialization_utils::*;

use bedrock as br;
use async_std::io::{
    Write as AsyncWrite, BufRead as AsyncBufRead, Seek as AsyncSeek,
    BufReader, Result as IOResult, Error as IOError, ErrorKind, Cursor, SeekFrom,
    prelude::{WriteExt, SeekExt, ReadExt}
};
#[cfg(feature = "with-loader-impl")]
use std::io::Read;
use async_std::fs::File;
use async_std::path::Path;
#[cfg(feature = "with-loader-impl")]
use std::ffi::CString;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PvpContainer
{
    pub vertex_bindings: Vec<br::vk::VkVertexInputBindingDescription>,
    pub vertex_attributes: Vec<br::vk::VkVertexInputAttributeDescription>,
    pub vertex_shader: Vec<u8>,
    pub fragment_shader: Option<Vec<u8>>,
}
impl PvpContainer
{
    pub fn empty() -> Self
    {
        PvpContainer
        {
            vertex_bindings: Vec::new(), vertex_attributes: Vec::new(), vertex_shader: Vec::new(),
            fragment_shader: None
        }
    }

    pub async fn write<W: AsyncWrite + Unpin>(&self, writer: &mut W) -> IOResult<()>
    {
        writer.write(b"PVP\x01").await?;  // ヘッダ(シグネチャとバージョン)

        // バイナリを裏で構築しつつオフセット値を書き出す
        let mut blob = Cursor::new(Vec::new());
        binary_serialize_bindings(&self.vertex_bindings, &mut blob).await?;
        let vertex_attribute_offs = blob.seek(SeekFrom::Current(0i64)).await?;
        binary_serialize_attributes(&self.vertex_attributes, &mut blob).await?;
        let vshader_offs = blob.seek(SeekFrom::Current(0i64)).await?;
        write_u8s(&self.vertex_shader, &mut blob).await?;
        VariableUInt(vertex_attribute_offs as _).write(writer).await?;
        VariableUInt(vshader_offs as _).write(writer).await?;
        if let Some(ref b) = self.fragment_shader
        {
            VariableUInt(blob.seek(SeekFrom::Current(0i64)).await? as _).write(writer).await?;
            write_u8s(b, &mut blob).await?;
        }
        else { VariableUInt(0).write(writer).await?; }

        writer.write(&blob.into_inner()).await.map(drop)
    }
}

#[cfg(feature = "with-loader-impl")]
impl peridot::LogicalAssetData for PvpContainer { const EXT: &'static str = "pvp"; }
#[cfg(feature = "with-loader-impl")]
impl peridot::FromAsset for PvpContainer
{
    type Error = IOError;

    fn from_asset<Asset: Read + Seek + 'static>(asset: Asset) -> IOResult<Self>
    {
        PvpContainerReader::new(BufReader::new(asset)).and_then(PvpContainerReader::into_container)
    }
}

#[cfg(feature = "with-loader-impl")]
pub struct PvpShaderModules<'d>
{
    bindings: Vec<br::vk::VkVertexInputBindingDescription>, attributes: Vec<br::vk::VkVertexInputAttributeDescription>,
    vertex: br::ShaderModule, fragment: Option<br::ShaderModule>,
    vertex_spec_constants: Option<(Vec<br::vk::VkSpecializationMapEntry>, br::DynamicDataCell<'d>)>,
    fragment_spec_constants: Option<(Vec<br::vk::VkSpecializationMapEntry>, br::DynamicDataCell<'d>)>,
}
#[cfg(feature = "with-loader-impl")]
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

pub struct PvpContainerReader<R>
{
    vb_offset: usize, va_offset: usize, vsh_offset: usize, fsh_offset: Option<usize>,
    reader: R
}
impl<R: AsyncBufRead + AsyncSeek + Unpin> PvpContainerReader<R>
{
    pub async fn new(mut reader: R) -> IOResult<Self>
    {
        let mut signature = [0u8; 4];
        reader.read_exact(&mut signature).await?;
        if &signature != b"PVP\x01"
        {
            return Err(IOError::new(ErrorKind::Other,
                "Signature mismatch: Invalid or corrupted Peridot Vertex Processing file"));
        }

        let VariableUInt(va_offset) = VariableUInt::read(&mut reader).await?;
        let VariableUInt(vsh_offset) = VariableUInt::read(&mut reader).await?;
        let VariableUInt(fsh_offset_0) = VariableUInt::read(&mut reader).await?;
        let blob_offset = reader.seek(SeekFrom::Current(0i64)).await? as usize;

        return Ok(PvpContainerReader
        {
            vb_offset: blob_offset as _, va_offset: (va_offset + blob_offset as u32) as _,
            vsh_offset: (vsh_offset + blob_offset as u32) as _,
            fsh_offset: if fsh_offset_0 == 0 { None } else { Some((fsh_offset_0 + blob_offset as u32) as _) },
            reader
        });
    }

    pub async fn read_vertex_bindings(&mut self) -> IOResult<Vec<br::vk::VkVertexInputBindingDescription>>
    {
        self.reader.seek(SeekFrom::Start(self.vb_offset as _)).await?;
        binary_deserialize_bindings(&mut self.reader).await
    }
    pub async fn read_vertex_attributes(&mut self) -> IOResult<Vec<br::vk::VkVertexInputAttributeDescription>>
    {
        self.reader.seek(SeekFrom::Start(self.va_offset as _)).await?;
        binary_deserialize_attributes(&mut self.reader).await
    }
    pub async fn read_vertex_shader(&mut self) -> IOResult<Vec<u8>>
    {
        self.reader.seek(SeekFrom::Start(self.vsh_offset as _)).await?;
        read_u8s(&mut self.reader).await
    }
    pub fn is_fragment_stage_provided(&mut self) -> bool { self.fsh_offset.is_some() }
    pub async fn read_fragment_shader(&mut self) -> IOResult<Vec<u8>>
    {
        self.reader.seek(SeekFrom::Start(self.fsh_offset.expect("no fsh") as _)).await?;
        read_u8s(&mut self.reader).await
    }

    pub async fn into_container(mut self) -> IOResult<PvpContainer>
    {
        Ok(PvpContainer
        {
            vertex_bindings: self.read_vertex_bindings().await?,
            vertex_attributes: self.read_vertex_attributes().await?,
            vertex_shader: self.read_vertex_shader().await?,
            fragment_shader: if self.is_fragment_stage_provided() { Some(self.read_fragment_shader().await?) } else { None }
        })
    }
}
impl PvpContainerReader<BufReader<File>>
{
    pub async fn from_file<P: AsRef<Path>>(path: P) -> IOResult<Self>
    {
        Self::new(BufReader::new(File::open(path).await?)).await
    }
}

// fixme: traitでasync fn使えるようになったら復活させたいね
/*trait BinarySerializeVkStructures
{
    fn binary_serialize<W: Write>(&self, sink: &mut W) -> IOResult<usize>;
    fn binary_unserialize<R: BufRead>(source: &mut R) -> IOResult<Self> where Self: Sized;
    fn serialize_into_memory(&self) -> IOResult<Vec<u8>> {
        let mut sink = Cursor::new(Vec::new());
        self.binary_serialize(&mut sink).map(|_| sink.into_inner())
    }
}*/
async fn binary_serialize_binding<W: AsyncWrite + Unpin>(v: &br::vk::VkVertexInputBindingDescription, sink: &mut W) -> IOResult<usize>
{
    let w = VariableUInt(v.inputRate as _).write(sink).await?;
    let w = w + VariableUInt(v.binding as _).write(sink).await?;
    Ok(w + VariableUInt(v.stride as _).write(sink).await?)
}
async fn binary_deserialize_binding<R: AsyncBufRead + Unpin>(source: &mut R) -> IOResult<br::vk::VkVertexInputBindingDescription>
{
    let VariableUInt(input_rate) = VariableUInt::read(source).await?;
    let VariableUInt(binding) = VariableUInt::read(source).await?;
    let VariableUInt(stride) = VariableUInt::read(source).await?;
    
    Ok(br::vk::VkVertexInputBindingDescription
    {
        inputRate: input_rate as _, binding: binding as _, stride: stride as _
    })
}

async fn binary_serialize_bindings<W: AsyncWrite + Unpin>(entries: &[br::vk::VkVertexInputBindingDescription], sink: &mut W) -> IOResult<usize>
{
    let mut write_bytes = VariableUInt(entries.len() as _).write(sink).await?;
    for x in entries { write_bytes += binary_serialize_binding(x, sink).await?; }
    
    Ok(write_bytes)
}
async fn binary_deserialize_bindings<R: AsyncBufRead + Unpin>(source: &mut R) -> IOResult<Vec<br::vk::VkVertexInputBindingDescription>>
{
    let VariableUInt(entry_count) = VariableUInt::read(source).await?;
    let mut vs = Vec::with_capacity(entry_count as _);
    for _ in 0 .. entry_count { vs.push(binary_deserialize_binding(source).await?); }

    Ok(vs)
}

async fn binary_serialize_attribute<W: AsyncWrite + Unpin>(v: &br::vk::VkVertexInputAttributeDescription, sink: &mut W) -> IOResult<usize>
{
    let w = VariableUInt(v.location as _).write(sink).await?;
    let w = w + VariableUInt(v.binding as _).write(sink).await?;
    let w = w + VariableUInt(v.offset as _).write(sink).await?;
    Ok(w + VariableUInt(v.format as _).write(sink).await?)
}
async fn binary_deserialize_attribute<R: AsyncBufRead + Unpin>(source: &mut R) -> IOResult<br::vk::VkVertexInputAttributeDescription>
{
    let VariableUInt(location) = VariableUInt::read(source).await?;
    let VariableUInt(binding) = VariableUInt::read(source).await?;
    let VariableUInt(offset) = VariableUInt::read(source).await?;
    let VariableUInt(format) = VariableUInt::read(source).await?;

    Ok(br::vk::VkVertexInputAttributeDescription
    {
        location: location as _, binding: binding as _, offset: offset as _, format: format as _
    })
}

async fn binary_serialize_attributes<W: AsyncWrite + Unpin>(entries: &[br::vk::VkVertexInputAttributeDescription], sink: &mut W) -> IOResult<usize>
{
    let mut write_bytes = VariableUInt(entries.len() as _).write(sink).await?;
    for x in entries { write_bytes += binary_serialize_attribute(x, sink).await?; }
    
    Ok(write_bytes)
}
async fn binary_deserialize_attributes<R: AsyncBufRead + Unpin>(source: &mut R) -> IOResult<Vec<br::vk::VkVertexInputAttributeDescription>>
{
    let VariableUInt(entry_count) = VariableUInt::read(source).await?;
    let mut vs = Vec::with_capacity(entry_count as _);
    for _ in 0 .. entry_count { vs.push(binary_deserialize_attribute(source).await?); }

    Ok(vs)
}

async fn write_u8s<W: AsyncWrite + Unpin>(buf: &[u8], sink: &mut W) -> IOResult<usize>
{
    let v = VariableUInt(buf.len() as _).write(sink).await?;
    
    Ok(v + sink.write_all(buf).await.map(|_| buf.len())?)
}
async fn read_u8s<R: AsyncBufRead + Unpin>(source: &mut R) -> IOResult<Vec<u8>>
{
    let VariableUInt(size) = VariableUInt::read(source).await?;
    let mut buf = Vec::with_capacity(size as _);
    unsafe { buf.set_len(size as _); }
    source.read_exact(&mut buf).await.map(move |_| buf)
}
