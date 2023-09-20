//! Vertex Processing Stage Container

extern crate bedrock;
extern crate peridot_serialization_utils;
use peridot_serialization_utils::*;

use bedrock::{self as br, VulkanStructure, VkHandle};
#[cfg(feature = "with-loader-impl")]
use std::borrow::Cow;
use std::fs::File;
#[cfg(feature = "with-loader-impl")]
use std::io::Read;
use std::io::{
    BufRead, BufReader, Cursor, Error as IOError, ErrorKind, Result as IOResult, Seek, SeekFrom,
    Write,
};
use std::path::Path;

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
            vertex_bindings: Vec::new(),
            vertex_attributes: Vec::new(),
            vertex_shader: Vec::new(),
            fragment_shader: None,
        }
    }

    pub fn write<W: Write>(&self, writer: &mut W) -> IOResult<()> {
        writer.write(b"PVP\x01")?; // ヘッダ(シグネチャとバージョン)

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
        } else {
            VariableUInt(0).write(writer)?;
        }

        writer.write(&blob.into_inner()).map(drop)
    }
}

#[cfg(feature = "with-loader-impl")]
impl peridot::LogicalAssetData for PvpContainer {
    const EXT: &'static str = "pvp";
}
#[cfg(feature = "with-loader-impl")]
impl peridot::FromAsset for PvpContainer {
    type Error = IOError;

    fn from_asset<Asset: Read + Seek + 'static>(asset: Asset) -> IOResult<Self> {
        PvpContainerReader::new(BufReader::new(asset)).and_then(PvpContainerReader::into_container)
    }
}

#[cfg(feature = "with-loader-impl")]
pub struct PvpShaderModules<'d, Device: br::Device> {
    bindings: Vec<br::vk::VkVertexInputBindingDescription>,
    attributes: Vec<br::vk::VkVertexInputAttributeDescription>,
    vertex: br::ShaderModuleObject<Device>,
    fragment: Option<br::ShaderModuleObject<Device>>,
    vertex_spec_constants: Option<(
        Vec<br::vk::VkSpecializationMapEntry>,
        Cow<'d, [u8]>,
    )>,
    fragment_spec_constants: Option<(
        Vec<br::vk::VkSpecializationMapEntry>,
        Cow<'d, [u8]>,
    )>,
}
#[cfg(feature = "with-loader-impl")]
impl<'d, Device: br::Device + Clone> PvpShaderModules<'d, Device> {
    pub fn new(device: &Device, container: PvpContainer) -> br::Result<Self> {
        let fragment = container
            .fragment_shader
            .map(|b| device.clone().new_shader_module(&b))
            .transpose()?;

        Ok(Self {
            vertex: device.clone().new_shader_module(&container.vertex_shader)?,
            fragment,
            bindings: container.vertex_bindings,
            attributes: container.vertex_attributes,
            vertex_spec_constants: None,
            fragment_spec_constants: None,
        })
    }

    pub fn generate_vps(&'d self, primitive_topo: br::vk::VkPrimitiveTopology) -> br::VertexProcessingStages<'d, &'d Self> {
        let bindings = unsafe {
            // Transparentなのでok
            std::slice::from_raw_parts(
                self.bindings.as_ptr() as *const br::VertexInputBindingDescription,
                self.bindings.len(),
            )
        };
        
        br::VertexProcessingStages::new(self, bindings, &self.attributes, primitive_topo)
    }
}
impl<Device: br::Device> br::PipelineShaderStageProvider for PvpShaderModules<'_, Device> {
    type ExtraStorage = (Option<br::vk::VkSpecializationInfo>, Option<br::vk::VkSpecializationInfo>);

    fn base_struct(&self, extra_storage: &Self::ExtraStorage) -> Vec<br::vk::VkPipelineShaderStageCreateInfo> {
        let mut v = vec![br::vk::VkPipelineShaderStageCreateInfo {
            sType: br::vk::VkPipelineShaderStageCreateInfo::TYPE,
            pNext: core::ptr::null(),
            flags: 0,
            stage: br::ShaderStage::VERTEX.0,
            module: self.vertex.native_ptr(),
            pName: unsafe { core::ffi::CStr::from_bytes_with_nul_unchecked(b"main\0").as_ptr() },
            pSpecializationInfo: extra_storage.0.as_ref().map_or_else(core::ptr::null, |x| x as _)
        }];

        if let Some(ref f) = self.fragment {
            v.push(br::vk::VkPipelineShaderStageCreateInfo {
                sType: br::vk::VkPipelineShaderStageCreateInfo::TYPE,
                pNext: core::ptr::null(),
                flags: 0,
                stage: br::ShaderStage::FRAGMENT.0,
                module: f.native_ptr(),
                pName: unsafe { core::ffi::CStr::from_bytes_with_nul_unchecked(b"main\0").as_ptr() },
                pSpecializationInfo: extra_storage.1.as_ref().map_or_else(core::ptr::null, |x| x as _)
            });
        }

        v
    }
    fn make_extras(&self) -> Self::ExtraStorage {
        (self.vertex_spec_constants.as_ref().map(|c| br::vk::VkSpecializationInfo {
            mapEntryCount: c.0.len() as _,
            pMapEntries: c.0.as_ptr(),
            dataSize: c.1.len(),
            pData: c.1.as_ptr() as *const _ as _
        }), self.fragment_spec_constants.as_ref().map(|c| br::vk::VkSpecializationInfo {
            mapEntryCount: c.0.len() as _,
            pMapEntries: c.0.as_ptr(),
            dataSize: c.1.len(),
            pData: c.1.as_ptr() as *const _ as _
        }), )
    }
}

pub struct PvpContainerReader<R: BufRead + Seek> {
    vb_offset: usize,
    va_offset: usize,
    vsh_offset: usize,
    fsh_offset: Option<usize>,
    reader: R,
}
impl<R: BufRead + Seek> PvpContainerReader<R> {
    pub fn new(mut reader: R) -> IOResult<Self> {
        let mut signature = [0u8; 4];
        reader.read_exact(&mut signature)?;
        if &signature != b"PVP\x01" {
            return Err(IOError::new(
                ErrorKind::Other,
                "Signature mismatch: Invalid or corrupted Peridot Vertex Processing file",
            ));
        }

        let VariableUInt(va_offset) = VariableUInt::read(&mut reader)?;
        let VariableUInt(vsh_offset) = VariableUInt::read(&mut reader)?;
        let VariableUInt(fsh_offset_0) = VariableUInt::read(&mut reader)?;
        let blob_offset = reader.seek(SeekFrom::Current(0))? as usize;

        return Ok(PvpContainerReader {
            vb_offset: blob_offset as _,
            va_offset: (va_offset + blob_offset as u32) as _,
            vsh_offset: (vsh_offset + blob_offset as u32) as _,
            fsh_offset: if fsh_offset_0 == 0 {
                None
            } else {
                Some((fsh_offset_0 + blob_offset as u32) as _)
            },
            reader,
        });
    }

    pub fn read_vertex_bindings(
        &mut self,
    ) -> IOResult<Vec<br::vk::VkVertexInputBindingDescription>> {
        self.reader.seek(SeekFrom::Start(self.vb_offset as _))?;
        Vec::<_>::binary_unserialize(&mut self.reader)
    }
    pub fn read_vertex_attributes(
        &mut self,
    ) -> IOResult<Vec<br::vk::VkVertexInputAttributeDescription>> {
        self.reader.seek(SeekFrom::Start(self.va_offset as _))?;
        Vec::<_>::binary_unserialize(&mut self.reader)
    }
    pub fn read_vertex_shader(&mut self) -> IOResult<Vec<u8>> {
        self.reader.seek(SeekFrom::Start(self.vsh_offset as _))?;
        Vec::<u8>::binary_unserialize(&mut self.reader)
    }
    pub fn is_fragment_stage_provided(&mut self) -> bool {
        self.fsh_offset.is_some()
    }
    pub fn read_fragment_shader(&mut self) -> IOResult<Vec<u8>> {
        self.reader
            .seek(SeekFrom::Start(self.fsh_offset.expect("no fsh") as _))?;
        Vec::<u8>::binary_unserialize(&mut self.reader)
    }

    pub fn into_container(mut self) -> IOResult<PvpContainer> {
        Ok(PvpContainer {
            vertex_bindings: self.read_vertex_bindings()?,
            vertex_attributes: self.read_vertex_attributes()?,
            vertex_shader: self.read_vertex_shader()?,
            fragment_shader: if self.is_fragment_stage_provided() {
                Some(self.read_fragment_shader()?)
            } else {
                None
            },
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
    fn binary_unserialize<R: BufRead>(source: &mut R) -> IOResult<Self>
    where
        Self: Sized;
    fn serialize_into_memory(&self) -> IOResult<Vec<u8>> {
        let mut sink = Cursor::new(Vec::new());
        self.binary_serialize(&mut sink).map(|_| sink.into_inner())
    }
}
impl BinarySerializeVkStructures for br::vk::VkVertexInputBindingDescription {
    fn binary_serialize<W: Write>(&self, sink: &mut W) -> IOResult<usize> {
        VariableUInt(self.inputRate as _)
            .write(sink)
            .and_then(|w0| {
                VariableUInt(self.binding as _)
                    .write(sink)
                    .map(move |w1| w1 + w0)
            })
            .and_then(|w0| {
                VariableUInt(self.stride as _)
                    .write(sink)
                    .map(move |w1| w1 + w0)
            })
    }
    fn binary_unserialize<R: BufRead>(source: &mut R) -> IOResult<Self>
    where
        Self: Sized,
    {
        let VariableUInt(input_rate) = VariableUInt::read(source)?;
        let VariableUInt(binding) = VariableUInt::read(source)?;
        let VariableUInt(stride) = VariableUInt::read(source)?;
        return Ok(br::vk::VkVertexInputBindingDescription {
            inputRate: input_rate as _,
            binding: binding as _,
            stride: stride as _,
        });
    }
}
impl BinarySerializeVkStructures for br::vk::VkVertexInputAttributeDescription {
    fn binary_serialize<W: Write>(&self, sink: &mut W) -> IOResult<usize> {
        VariableUInt(self.location as _)
            .write(sink)
            .and_then(|w0| {
                VariableUInt(self.binding as _)
                    .write(sink)
                    .map(move |w1| w1 + w0)
            })
            .and_then(|w0| {
                VariableUInt(self.offset as _)
                    .write(sink)
                    .map(move |w1| w1 + w0)
            })
            .and_then(|w0| {
                VariableUInt(self.format as _)
                    .write(sink)
                    .map(move |w1| w1 + w0)
            })
    }
    fn binary_unserialize<R: BufRead>(source: &mut R) -> IOResult<Self>
    where
        Self: Sized,
    {
        let VariableUInt(location) = VariableUInt::read(source)?;
        let VariableUInt(binding) = VariableUInt::read(source)?;
        let VariableUInt(offset) = VariableUInt::read(source)?;
        let VariableUInt(format) = VariableUInt::read(source)?;
        return Ok(br::vk::VkVertexInputAttributeDescription {
            location: location as _,
            binding: binding as _,
            offset: offset as _,
            format: format as _,
        });
    }
}
impl<T: BinarySerializeVkStructures> BinarySerializeVkStructures for Vec<T> {
    fn binary_serialize<W: Write>(&self, sink: &mut W) -> IOResult<usize> {
        let mut write_bytes = VariableUInt(self.len() as _).write(sink)?;
        for x in self {
            write_bytes += x.binary_serialize(sink)?;
        }
        return Ok(write_bytes);
    }
    fn binary_unserialize<R: BufRead>(source: &mut R) -> IOResult<Self>
    where
        Self: Sized,
    {
        let VariableUInt(element_count) = VariableUInt::read(source)?;
        let mut vs = Vec::with_capacity(element_count as _);
        for _ in 0..element_count {
            vs.push(T::binary_unserialize(source)?);
        }
        return Ok(vs);
    }
}
impl BinarySerializeVkStructures for Vec<u8> {
    fn binary_serialize<W: Write>(&self, sink: &mut W) -> IOResult<usize> {
        VariableUInt(self.len() as _)
            .write(sink)
            .and_then(|w0| sink.write_all(self).map(move |_| self.len() + w0))
    }
    fn binary_unserialize<R: BufRead>(source: &mut R) -> IOResult<Self>
    where
        Self: Sized,
    {
        let VariableUInt(element_count) = VariableUInt::read(source)?;
        let mut buf = vec![0u8; element_count as usize];
        source.read_exact(&mut buf).map(|_| buf)
    }
}
