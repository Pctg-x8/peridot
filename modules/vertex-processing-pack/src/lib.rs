//! Vertex Processing Stage Container

extern crate bedrock;
extern crate peridot_serialization_utils;
use peridot_serialization_utils::*;

use bedrock as br;
use std::fs::File;
#[cfg(feature = "with-loader-impl")]
use std::io::Read;
use std::io::{
    BufRead, BufReader, Cursor, Error as IOError, Result as IOResult, Seek, SeekFrom, Write,
};
use std::path::Path;

#[derive(Debug, Clone)]
pub struct PvpContainer {
    pub vertex_bindings: Vec<br::vk::VkVertexInputBindingDescription>,
    pub vertex_attributes: Vec<br::vk::VkVertexInputAttributeDescription>,
    pub vertex_shader: Vec<u32>,
    pub fragment_shader: Option<Vec<u32>>,
}
impl PartialEq for PvpContainer {
    fn eq(&self, other: &Self) -> bool {
        if self.vertex_shader != other.vertex_shader {
            return false;
        }

        if self.fragment_shader != other.fragment_shader {
            return false;
        }

        if !self
            .vertex_bindings
            .iter()
            .zip(other.vertex_bindings.iter())
            .all(|(a, b)| {
                a.binding == b.binding && a.inputRate == b.inputRate && a.stride == b.stride
            })
        {
            return false;
        }

        if !self
            .vertex_attributes
            .iter()
            .zip(other.vertex_attributes.iter())
            .all(|(a, b)| {
                a.binding == b.binding
                    && a.location == b.location
                    && a.format == b.format
                    && a.offset == b.offset
            })
        {
            return false;
        }

        true
    }
}
impl Eq for PvpContainer {}
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
        SpvBinary::from_ref(&self.vertex_shader).binary_serialize(&mut blob)?;
        if let Some(ref b) = self.fragment_shader {
            VariableUInt((blob.seek(SeekFrom::Current(0))?) as _).write(writer)?;
            SpvBinary::from_ref(b).binary_serialize(&mut blob)?;
        } else {
            VariableUInt(0).write(writer)?;
        }

        writer.write(&blob.into_inner()).map(drop)
    }

    #[cfg(feature = "async-rt-async-std")]
    pub async fn write_async(
        &self,
        writer: &mut (impl async_std::io::Write + Unpin + ?Sized),
    ) -> IOResult<()> {
        // ヘッダ(シグネチャとバージョン)
        async_std::io::WriteExt::write_all(writer, b"PVP\x01").await?;

        // バイナリを裏で構築しつつオフセット値を書き出す
        let mut blob = Vec::new();
        self.vertex_bindings.binary_serialize(&mut blob)?;
        VariableUInt(blob.len() as _).write_async(writer).await?;
        self.vertex_attributes.binary_serialize(&mut blob)?;
        VariableUInt(blob.len() as _).write_async(writer).await?;
        SpvBinary::from_ref(&self.vertex_shader).binary_serialize(&mut blob)?;
        if let Some(ref b) = self.fragment_shader {
            VariableUInt(blob.len() as _).write_async(writer).await?;
            SpvBinary::from_ref(b).binary_serialize(&mut blob)?;
        } else {
            VariableUInt(0).write_async(writer).await?;
        }

        async_std::io::WriteExt::write_all(writer, &blob).await?;
        Ok(())
    }
}

#[cfg(feature = "with-loader-impl")]
impl peridot::LogicalAssetData for PvpContainer {
    const EXT: &'static str = "pvp";
}
#[cfg(feature = "with-loader-impl")]
impl peridot::FromAsset for PvpContainer {
    type Error = PvpContainerReadError;

    fn from_asset<Asset: Read + Seek + 'static>(
        asset: Asset,
    ) -> Result<Self, PvpContainerReadError> {
        PvpContainerReader::new(BufReader::new(asset))?
            .into_container()
            .map_err(From::from)
    }
}

#[cfg(feature = "with-loader-impl")]
pub struct PvpShaderModules<Device: br::Device> {
    vertex: br::ShaderModuleObject<Device>,
    fragment: Option<br::ShaderModuleObject<Device>>,
}
#[cfg(feature = "with-loader-impl")]
impl<Device: br::Device + Clone> PvpShaderModules<Device> {
    pub fn new(device: &Device, container: &PvpContainer) -> br::Result<Self> {
        Ok(Self {
            vertex: br::ShaderModuleObject::new(
                device.clone(),
                &br::ShaderModuleCreateInfo::new(&container.vertex_shader),
            )?,
            fragment: container
                .fragment_shader
                .as_ref()
                .map(|b| {
                    br::ShaderModuleObject::new(device.clone(), &br::ShaderModuleCreateInfo::new(b))
                })
                .transpose()?,
        })
    }

    pub fn pipeline_vertex_shader_stage<'d, 's>(&'d self) -> br::PipelineShaderStage<'d, 's> {
        use br::ShaderModule;

        self.vertex.on_stage(br::ShaderStage::Vertex, c"main")
    }

    pub fn pipeline_fragment_shader_stage<'d, 's>(
        &'d self,
    ) -> Option<br::PipelineShaderStage<'d, 's>> {
        use br::ShaderModule;

        self.fragment
            .as_ref()
            .map(|x| x.on_stage(br::ShaderStage::Fragment, c"main"))
    }
}

#[derive(Debug)]
pub enum PvpContainerReadError {
    IOError(IOError),
    InvalidSignature,
}
impl From<IOError> for PvpContainerReadError {
    #[inline(always)]
    fn from(value: IOError) -> Self {
        Self::IOError(value)
    }
}
impl core::fmt::Display for PvpContainerReadError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IOError(e) => e.fmt(f),
            Self::InvalidSignature => f.write_str(
                "Signature mismatch: Invalid or corrupted Peridot Vertex Processing file",
            ),
        }
    }
}

#[cfg(feature = "async-rt-async-std")]
pub struct PvpContainerReaderAsync<R: async_std::io::BufRead + async_std::io::Seek> {
    vb_offset: u64,
    va_offset: u64,
    vsh_offset: u64,
    fsh_offset: Option<u64>,
    reader: R,
}
#[cfg(feature = "async-rt-async-std")]
impl<R: async_std::io::BufRead + async_std::io::Seek + Unpin> PvpContainerReaderAsync<R> {
    pub async fn new(mut reader: R) -> Result<Self, PvpContainerReadError> {
        let mut signature = [0u8; 4];
        async_std::io::ReadExt::read_exact(&mut reader, &mut signature).await?;
        if &signature != b"PVP\x01" {
            return Err(PvpContainerReadError::InvalidSignature);
        }

        let VariableUInt(va_offset) = VariableUInt::read_async(&mut reader).await?;
        let VariableUInt(vsh_offset) = VariableUInt::read_async(&mut reader).await?;
        let VariableUInt(fsh_offset_0) = VariableUInt::read_async(&mut reader).await?;
        let blob_offset = async_std::io::SeekExt::seek(&mut reader, SeekFrom::Current(0)).await?;

        Ok(Self {
            vb_offset: blob_offset,
            va_offset: va_offset as u64 + blob_offset,
            vsh_offset: vsh_offset as u64 + blob_offset,
            fsh_offset: if fsh_offset_0 == 0 {
                None
            } else {
                Some(fsh_offset_0 as u64 + blob_offset)
            },
            reader,
        })
    }

    pub async fn read_vertex_bindings(
        &mut self,
    ) -> IOResult<Vec<br::vk::VkVertexInputBindingDescription>> {
        async_std::io::SeekExt::seek(&mut self.reader, SeekFrom::Start(self.vb_offset)).await?;
        Vec::binary_deserialize_async(&mut self.reader).await
    }

    pub async fn read_vertex_attributes(
        &mut self,
    ) -> IOResult<Vec<br::vk::VkVertexInputAttributeDescription>> {
        async_std::io::SeekExt::seek(&mut self.reader, SeekFrom::Start(self.va_offset)).await?;
        Vec::binary_deserialize_async(&mut self.reader).await
    }

    pub async fn read_vertex_shader(&mut self) -> IOResult<Vec<u32>> {
        async_std::io::SeekExt::seek(&mut self.reader, SeekFrom::Start(self.vsh_offset)).await?;
        SpvBinary::binary_deserialize_async(&mut self.reader)
            .await
            .map(|x| x.0)
    }

    pub fn is_fragment_stage_provided(&self) -> bool {
        self.fsh_offset.is_some()
    }

    pub async fn read_fragment_shader(&mut self) -> IOResult<Option<Vec<u32>>> {
        let Some(o) = self.fsh_offset else {
            return Ok(None);
        };

        async_std::io::SeekExt::seek(&mut self.reader, SeekFrom::Start(o)).await?;
        SpvBinary::binary_deserialize_async(&mut self.reader)
            .await
            .map(|x| Some(x.0))
    }

    pub async fn into_container(mut self) -> IOResult<PvpContainer> {
        Ok(PvpContainer {
            vertex_bindings: self.read_vertex_bindings().await?,
            vertex_attributes: self.read_vertex_attributes().await?,
            vertex_shader: self.read_vertex_shader().await?,
            fragment_shader: self.read_fragment_shader().await?,
        })
    }
}
#[cfg(feature = "async-rt-async-std")]
impl PvpContainerReaderAsync<async_std::io::BufReader<async_std::fs::File>> {
    pub async fn from_file(
        path: impl AsRef<async_std::path::Path>,
    ) -> Result<Self, PvpContainerReadError> {
        Self::new(async_std::io::BufReader::new(
            async_std::fs::File::open(path).await?,
        ))
        .await
    }
}

pub struct PvpContainerReader<R: BufRead + Seek> {
    vb_offset: u64,
    va_offset: u64,
    vsh_offset: u64,
    fsh_offset: Option<u64>,
    reader: R,
}
impl<R: BufRead + Seek> PvpContainerReader<R> {
    pub fn new(mut reader: R) -> Result<Self, PvpContainerReadError> {
        let mut signature = [0u8; 4];
        reader.read_exact(&mut signature)?;
        if &signature != b"PVP\x01" {
            return Err(PvpContainerReadError::InvalidSignature);
        }

        let VariableUInt(va_offset) = VariableUInt::read(&mut reader)?;
        let VariableUInt(vsh_offset) = VariableUInt::read(&mut reader)?;
        let VariableUInt(fsh_offset_0) = VariableUInt::read(&mut reader)?;
        let blob_offset = reader.seek(SeekFrom::Current(0))? as u64;

        return Ok(PvpContainerReader {
            vb_offset: blob_offset,
            va_offset: va_offset as u64 + blob_offset,
            vsh_offset: vsh_offset as u64 + blob_offset,
            fsh_offset: if fsh_offset_0 == 0 {
                None
            } else {
                Some(fsh_offset_0 as u64 + blob_offset)
            },
            reader,
        });
    }

    pub fn read_vertex_bindings(
        &mut self,
    ) -> IOResult<Vec<br::vk::VkVertexInputBindingDescription>> {
        self.reader.seek(SeekFrom::Start(self.vb_offset))?;
        Vec::<_>::binary_unserialize(&mut self.reader)
    }
    pub fn read_vertex_attributes(
        &mut self,
    ) -> IOResult<Vec<br::vk::VkVertexInputAttributeDescription>> {
        self.reader.seek(SeekFrom::Start(self.va_offset))?;
        Vec::<_>::binary_unserialize(&mut self.reader)
    }
    pub fn read_vertex_shader(&mut self) -> IOResult<Vec<u32>> {
        self.reader.seek(SeekFrom::Start(self.vsh_offset))?;
        SpvBinary::binary_unserialize(&mut self.reader).map(|x| x.0)
    }
    pub fn is_fragment_stage_provided(&mut self) -> bool {
        self.fsh_offset.is_some()
    }
    pub fn read_fragment_shader(&mut self) -> IOResult<Option<Vec<u32>>> {
        let Some(o) = self.fsh_offset else {
            return Ok(None);
        };

        self.reader.seek(SeekFrom::Start(o))?;
        SpvBinary::binary_unserialize(&mut self.reader).map(|x| Some(x.0))
    }

    pub fn into_container(mut self) -> IOResult<PvpContainer> {
        Ok(PvpContainer {
            vertex_bindings: self.read_vertex_bindings()?,
            vertex_attributes: self.read_vertex_attributes()?,
            vertex_shader: self.read_vertex_shader()?,
            fragment_shader: self.read_fragment_shader()?,
        })
    }
}
impl PvpContainerReader<BufReader<File>> {
    pub fn from_file<P: AsRef<Path>>(path: P) -> Result<Self, PvpContainerReadError> {
        Self::new(BufReader::new(File::open(path)?))
    }
}

trait BinarySerializeVkStructures {
    fn binary_serialize<W: Write>(&self, sink: &mut W) -> IOResult<usize>;
    fn binary_unserialize<R: BufRead>(source: &mut R) -> IOResult<Self>
    where
        Self: Sized;
}
#[cfg(feature = "async-rt-async-std")]
trait AsyncBinarySerializeVkStructures {
    // つかってないやつ
    // fn binary_serialize_async<'s>(
    //     &'s self,
    //     sink: &'s mut (impl async_std::io::Write + Unpin + ?Sized),
    // ) -> impl std::future::Future<Output = IOResult<usize>> + 's;
    fn binary_deserialize_async<'r>(
        source: &'r mut (impl async_std::io::BufRead + Unpin + ?Sized),
    ) -> impl std::future::Future<Output = IOResult<Self>> + 'r
    where
        Self: Sized;
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
#[cfg(feature = "async-rt-async-std")]
impl AsyncBinarySerializeVkStructures for br::vk::VkVertexInputBindingDescription {
    // fn binary_serialize_async<'s>(
    //     &'s self,
    //     sink: &'s mut (impl async_std::io::Write + Unpin + ?Sized),
    // ) -> impl std::future::Future<Output = IOResult<usize>> + 's {
    //     async move {
    //         let w1 = VariableUInt(self.inputRate as _).write_async(sink).await?;
    //         let w2 = VariableUInt(self.binding as _).write_async(sink).await?;
    //         let w3 = VariableUInt(self.stride as _).write_async(sink).await?;

    //         Ok(w1 + w2 + w3)
    //     }
    // }

    fn binary_deserialize_async<'r>(
        source: &'r mut (impl async_std::io::BufRead + Unpin + ?Sized),
    ) -> impl std::future::Future<Output = IOResult<Self>> + 'r
    where
        Self: Sized,
    {
        async move {
            let VariableUInt(input_rate) = VariableUInt::read_async(source).await?;
            let VariableUInt(binding) = VariableUInt::read_async(source).await?;
            let VariableUInt(stride) = VariableUInt::read_async(source).await?;

            Ok(Self {
                inputRate: input_rate as _,
                binding: binding as _,
                stride: stride as _,
            })
        }
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
#[cfg(feature = "async-rt-async-std")]
impl AsyncBinarySerializeVkStructures for br::vk::VkVertexInputAttributeDescription {
    // fn binary_serialize_async<'s>(
    //     &'s self,
    //     sink: &'s mut (impl async_std::io::Write + Unpin + ?Sized),
    // ) -> impl std::future::Future<Output = IOResult<usize>> + 's {
    //     async move {
    //         let w1 = VariableUInt(self.location as _).write_async(sink).await?;
    //         let w2 = VariableUInt(self.binding as _).write_async(sink).await?;
    //         let w3 = VariableUInt(self.offset as _).write_async(sink).await?;
    //         let w4 = VariableUInt(self.format as _).write_async(sink).await?;

    //         Ok(w1 + w2 + w3 + w4)
    //     }
    // }

    fn binary_deserialize_async<'r>(
        source: &'r mut (impl async_std::io::BufRead + Unpin + ?Sized),
    ) -> impl std::future::Future<Output = IOResult<Self>> + 'r
    where
        Self: Sized,
    {
        async move {
            let VariableUInt(location) = VariableUInt::read_async(source).await?;
            let VariableUInt(binding) = VariableUInt::read_async(source).await?;
            let VariableUInt(offset) = VariableUInt::read_async(source).await?;
            let VariableUInt(format) = VariableUInt::read_async(source).await?;

            Ok(Self {
                location: location as _,
                binding: binding as _,
                offset: offset as _,
                format: format as _,
            })
        }
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
#[cfg(feature = "async-rt-async-std")]
impl<T: AsyncBinarySerializeVkStructures> AsyncBinarySerializeVkStructures for Vec<T> {
    // fn binary_serialize_async<'s>(
    //     &'s self,
    //     sink: &'s mut (impl async_std::io::Write + Unpin + ?Sized),
    // ) -> impl std::future::Future<Output = IOResult<usize>> + 's {
    //     async move {
    //         let mut write_bytes = VariableUInt(self.len() as _).write_async(sink).await?;
    //         for x in self.iter() {
    //             write_bytes += x.binary_serialize_async(sink).await?;
    //         }

    //         Ok(write_bytes)
    //     }
    // }

    fn binary_deserialize_async<'r>(
        source: &'r mut (impl async_std::io::BufRead + Unpin + ?Sized),
    ) -> impl std::future::Future<Output = IOResult<Self>> + 'r
    where
        Self: Sized,
    {
        async move {
            let VariableUInt(element_count) = VariableUInt::read_async(source).await?;

            let mut xs = Vec::with_capacity(element_count as _);
            for _ in 0..element_count {
                xs.push(T::binary_deserialize_async(source).await?);
            }

            Ok(xs)
        }
    }
}

#[repr(transparent)]
pub struct SpvBinary(pub Vec<u32>);
impl SpvBinary {
    pub const fn from_ref(b: &Vec<u32>) -> &Self {
        unsafe { core::mem::transmute(b) }
    }
}

impl BinarySerializeVkStructures for SpvBinary {
    fn binary_serialize<W: Write>(&self, sink: &mut W) -> IOResult<usize> {
        let w0 = VariableUInt(self.0.len() as _).write(sink)?;
        sink.write_all(unsafe {
            core::slice::from_raw_parts(self.0.as_ptr() as *const u8, self.0.len() << 2)
        })?;

        Ok(w0 + self.0.len() << 2)
    }

    fn binary_unserialize<R: BufRead>(source: &mut R) -> IOResult<Self>
    where
        Self: Sized,
    {
        let VariableUInt(element_count) = VariableUInt::read(source)?;
        let mut buf = vec![0u32; element_count as usize];
        source
            .read_exact(unsafe {
                core::slice::from_raw_parts_mut(buf.as_mut_ptr() as *mut u8, buf.len() << 2)
            })
            .map(|_| Self(buf))
    }
}
#[cfg(feature = "async-rt-async-std")]
impl AsyncBinarySerializeVkStructures for SpvBinary {
    // fn binary_serialize_async<'s>(
    //     &'s self,
    //     sink: &'s mut (impl async_std::io::Write + Unpin + ?Sized),
    // ) -> impl std::future::Future<Output = IOResult<usize>> + 's {
    //     async move {
    //         let l = VariableUInt(self.0.len() as _).write_async(sink).await?;
    //         async_std::io::WriteExt::write_all(sink, unsafe {
    //             core::slice::from_raw_parts(self.0.as_ptr() as *const u8, self.0.len() << 2)
    //         })
    //         .await?;

    //         Ok(l + self.0.len())
    //     }
    // }

    fn binary_deserialize_async<'r>(
        source: &'r mut (impl async_std::io::BufRead + Unpin + ?Sized),
    ) -> impl std::future::Future<Output = IOResult<Self>> + 'r
    where
        Self: Sized,
    {
        async move {
            let VariableUInt(len) = VariableUInt::read_async(source).await?;
            let mut buf = Vec::with_capacity(len as _);
            unsafe {
                buf.set_len(len as _);
            }
            async_std::io::ReadExt::read_exact(source, unsafe {
                core::slice::from_raw_parts_mut(buf.as_mut_ptr() as *mut u8, buf.len() << 2)
            })
            .await?;

            Ok(Self(buf))
        }
    }
}
