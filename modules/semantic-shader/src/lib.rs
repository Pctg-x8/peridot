use std::collections::HashMap;

use bedrock::{self as br, ShaderModule};
use peridot_serialization_utils::VariableUInt;

/// Represents the semantic of a vertex shader input.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum VertexInputSemantic {
    /// Position input
    Position(u8),
    /// Normal input
    Normal(u8),
    /// Tangent input
    Tangent(u8),
    /// Binormal input
    Binormal(u8),
    /// Texture coordinate(UV) input
    Texcoord(u8),
    /// Color input
    Color(u8),
    /// Miscellaneous(Application-defined) input
    Misc(u8),
}
impl VertexInputSemantic {
    pub fn write(&self, writer: &mut (impl std::io::Write + ?Sized)) -> std::io::Result<usize> {
        match self {
            &Self::Misc(n) => writer.write_all(&[0, n]).map(|_| 2),
            &Self::Position(n) => writer.write_all(&[1, n]).map(|_| 2),
            &Self::Normal(n) => writer.write_all(&[2, n]).map(|_| 2),
            &Self::Tangent(n) => writer.write_all(&[3, n]).map(|_| 2),
            &Self::Binormal(n) => writer.write_all(&[4, n]).map(|_| 2),
            &Self::Texcoord(n) => writer.write_all(&[5, n]).map(|_| 2),
            &Self::Color(n) => writer.write_all(&[6, n]).map(|_| 2),
        }
    }

    pub fn read(reader: &mut (impl std::io::Read + ?Sized)) -> std::io::Result<Self> {
        let mut buf = [0u8; 2];
        reader.read_exact(&mut buf)?;

        match buf[0] {
            0 => Ok(Self::Misc(buf[1])),
            1 => Ok(Self::Position(buf[1])),
            2 => Ok(Self::Normal(buf[1])),
            3 => Ok(Self::Tangent(buf[1])),
            4 => Ok(Self::Binormal(buf[1])),
            5 => Ok(Self::Texcoord(buf[1])),
            6 => Ok(Self::Color(buf[1])),
            _ => Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                "unknown tag for VertexInputSemantic",
            )),
        }
    }
}

/// API-Native Shader Module Package
pub struct ShaderPack<Device: br::Device> {
    vertex_module: br::ShaderModuleObject<Device>,
    fragment_module: Option<br::ShaderModuleObject<Device>>,
    input_semantic_location_map: HashMap<VertexInputSemantic, u32>,
}
impl<Device: br::Device> ShaderPack<Device> {
    /// Creates a wrapper object for GraphicsPipeline's shader stage.
    pub fn pipeline_vertex_shader(&self) -> br::PipelineShaderStage {
        self.vertex_module
            .on_stage(br::ShaderStage::Vertex, c"main")
    }

    /// Creates a wrapper object for GraphicsPipeline's shader stage.
    pub fn pipeline_fragment_shader(&self) -> Option<br::PipelineShaderStage> {
        self.fragment_module
            .as_ref()
            .map(|m| m.on_stage(br::ShaderStage::Fragment, c"main"))
    }

    /// Resolves the location of shader input variable for the semantic.
    pub fn resolve_input_semantic_location(&self, semantic: VertexInputSemantic) -> Option<u32> {
        self.input_semantic_location_map.get(&semantic).copied()
    }
}

/// Error occured in loading a ShaderPack asset.
#[derive(Debug, thiserror::Error)]
pub enum AssetReadError {
    /// IO Error
    #[error(transparent)]
    IO(#[from] std::io::Error),
    /// The reader cannot recognize the valid signature in the asset.
    #[error("invalid siganture")]
    InvalidSignature,
}

/// An asset representation of the `ShaderPack` object.
pub struct ShaderPackAsset {
    pub vertex_shader_code: Vec<u32>,
    pub fragment_shader_code: Option<Vec<u32>>,
    pub input_semantic_location_map: HashMap<VertexInputSemantic, u32>,
}
impl ShaderPackAsset {
    /// Writes an asset's binary representation to the stream.
    pub fn write(&self, writer: &mut (impl std::io::Write + ?Sized)) -> std::io::Result<()> {
        // signature(PSS+version)
        writer.write_all(b"PSS\x01")?;

        let data_flags = if self.fragment_shader_code.is_some() {
            1 << 0
        } else {
            0
        };
        writer.write_all(&[data_flags])?;

        // 裏でdata部分を書き出して、メイン（ヘッダ）にはoffsetを追記していく
        let mut data = Vec::new();
        InputSemanticMap::from_ref(&self.input_semantic_location_map).write(&mut data)?;
        VariableUInt(data.len() as _).write(writer)?;
        SpirvBinary::from_ref(&self.vertex_shader_code).write(&mut data)?;
        if let Some(ref f) = self.fragment_shader_code {
            VariableUInt(data.len() as _).write(writer)?;
            SpirvBinary::from_ref(f).write(&mut data)?;
        }

        // data部分を末尾にがっちゃんこ
        writer.write_all(&data)?;

        Ok(())
    }

    /// Reads an asset from the stream.
    pub fn read(
        reader: &mut (impl std::io::BufRead + std::io::Seek + ?Sized),
    ) -> Result<Self, AssetReadError> {
        let mut signature = [0u8; 4];
        reader.read_exact(&mut signature)?;
        if &signature != b"PSS\x01" {
            return Err(AssetReadError::InvalidSignature);
        }

        let mut data_flags = [0u8; 1];
        reader.read_exact(&mut data_flags)?;
        let [data_flags] = data_flags;
        let has_fragment_shader = (data_flags & (1 << 0)) != 0;

        let VariableUInt(vertex_shader_offset) = VariableUInt::read(reader)?;
        let fragment_shader_offset = if has_fragment_shader {
            Some(VariableUInt::read(reader)?.0)
        } else {
            None
        };
        let data_base_offset = reader.seek(std::io::SeekFrom::Current(0))?;

        let InputSemanticMap(input_semantic_location_map) = InputSemanticMap::read(reader)?;
        reader.seek(std::io::SeekFrom::Start(
            data_base_offset + vertex_shader_offset as u64,
        ))?;
        let SpirvBinary(vertex_shader_code) = SpirvBinary::read(reader)?;
        let fragment_shader_code = if let Some(o) = fragment_shader_offset {
            reader.seek(std::io::SeekFrom::Start(data_base_offset + o as u64))?;
            Some(SpirvBinary::read(reader)?.0)
        } else {
            None
        };

        Ok(Self {
            vertex_shader_code,
            fragment_shader_code,
            input_semantic_location_map,
        })
    }

    /// Instantiates the native-api objects in this asset.
    #[cfg(feature = "bedrock-implements")]
    pub fn instantiate<Device: br::Device + Clone>(
        self,
        device: Device,
    ) -> br::Result<ShaderPack<Device>> {
        Ok(ShaderPack {
            vertex_module: br::ShaderModuleObject::new(
                device.clone(),
                &br::ShaderModuleCreateInfo::new(&self.vertex_shader_code),
            )?,
            fragment_module: self
                .fragment_shader_code
                .as_ref()
                .map(|b| {
                    br::ShaderModuleObject::new(device.clone(), &br::ShaderModuleCreateInfo::new(b))
                })
                .transpose()?,
            input_semantic_location_map: self.input_semantic_location_map,
        })
    }
}
#[cfg(feature = "with-loader-impl")]
impl peridot::LogicalAssetData for ShaderPackAsset {
    const EXT: &'static str = "pss";
}
#[cfg(feature = "with-loader-impl")]
impl peridot::FromAsset for ShaderPackAsset {
    type Error = AssetReadError;

    #[inline(always)]
    fn from_asset<Asset: std::io::Read + std::io::Seek + 'static>(
        asset: Asset,
    ) -> Result<Self, Self::Error> {
        ShaderPackAsset::read(&mut std::io::BufReader::new(asset))
    }
}

#[repr(transparent)]
struct SpirvBinary(pub Vec<u32>);
impl SpirvBinary {
    const fn from_ref(r: &Vec<u32>) -> &Self {
        unsafe { core::mem::transmute(r) }
    }

    fn write(&self, writer: &mut (impl std::io::Write + ?Sized)) -> std::io::Result<usize> {
        let blen = VariableUInt(self.0.len() as _).write(writer)?;
        writer.write_all(unsafe {
            core::slice::from_raw_parts(self.0.as_ptr() as *const u8, self.0.len() << 2)
        })?;

        Ok(blen + self.0.len() << 2)
    }

    fn read(reader: &mut (impl std::io::BufRead + ?Sized)) -> std::io::Result<Self> {
        let VariableUInt(len) = VariableUInt::read(reader)?;
        let mut buf = Vec::with_capacity(len as _);
        reader.read_exact(unsafe {
            core::slice::from_raw_parts_mut(buf.as_mut_ptr() as *mut u8, buf.capacity() << 2)
        })?;
        unsafe {
            buf.set_len(buf.capacity());
        }

        Ok(Self(buf))
    }
}

#[repr(transparent)]
struct InputSemanticMap(pub HashMap<VertexInputSemantic, u32>);
impl InputSemanticMap {
    const fn from_ref(r: &HashMap<VertexInputSemantic, u32>) -> &Self {
        unsafe { core::mem::transmute(r) }
    }

    fn write(&self, writer: &mut (impl std::io::Write + ?Sized)) -> std::io::Result<usize> {
        let mut wlen = VariableUInt(self.0.len() as _).write(writer)?;
        for (k, v) in self.0.iter() {
            wlen += k.write(writer)?;
            wlen += VariableUInt(*v).write(writer)?;
        }

        Ok(wlen)
    }

    fn read(reader: &mut (impl std::io::BufRead + ?Sized)) -> std::io::Result<Self> {
        let VariableUInt(clen) = VariableUInt::read(reader)?;
        let mut sink = HashMap::with_capacity(clen as _);
        for _ in 0..clen {
            let k = VertexInputSemantic::read(reader)?;
            let VariableUInt(v) = VariableUInt::read(reader)?;

            sink.insert(k, v);
        }

        Ok(Self(sink))
    }
}
