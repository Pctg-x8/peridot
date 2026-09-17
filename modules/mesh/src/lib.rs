//! Standard Mesh

use std::io::{Read, Write};

use bedrock as br;

/// ファイルシグネチャ
pub const SIGNATURE: u32 = u32::from_ne_bytes(*b"pa1m");

/// ヘッダ情報
#[derive(Debug)]
pub struct Header {
    /// プリミティブの形状
    pub primitive_topology: PrimitiveTopology,
    /// 頂点ストリームの数
    pub vertex_stream_count: u8,
}
impl Header {
    pub const fn serialize_size() -> usize {
        1 + 1
    }

    pub fn serialize(&self, w: &mut (impl Write + ?Sized)) -> std::io::Result<()> {
        w.write_all(&[self.primitive_topology as u8, self.vertex_stream_count])
    }

    pub fn deserialize(r: &mut (impl Read + ?Sized)) -> std::io::Result<Self> {
        let mut primitive_topology = 0u8;
        let mut vertex_stream_count = 0u8;
        readva(
            r,
            &mut [
                io_slice_mut_u8(&mut primitive_topology),
                io_slice_mut_u8(&mut vertex_stream_count),
            ],
        )?;

        Ok(Self {
            primitive_topology: PrimitiveTopology::try_from(primitive_topology)
                .expect("invalid topology value"),
            vertex_stream_count,
        })
    }
}

/// バッファ情報
#[derive(Debug)]
pub struct StreamBuffer {
    /// ファイル内の内容物の位置
    pub content_location: u64,
    /// バッファのバイト長
    pub byte_length: u32,
    /// デバイスでのアライメント要求（バイト数）
    pub device_alignment_requirement: u32,
}
impl StreamBuffer {
    pub const fn serialize_size() -> usize {
        8 + 4 + 4
    }

    pub fn serialize(&self, w: &mut (impl Write + ?Sized)) -> std::io::Result<()> {
        w.write_all(&self.content_location.to_ne_bytes())?;
        w.write_all(&self.byte_length.to_ne_bytes())?;
        w.write_all(&self.device_alignment_requirement.to_ne_bytes())
    }

    pub fn deserialize(r: &mut (impl Read + ?Sized), needs_swap: bool) -> std::io::Result<Self> {
        let mut content_location = 0u64;
        let mut byte_length = 0u32;
        let mut device_alignment_requirement = 0u32;
        readva(
            r,
            &mut [
                io_slice_mut_u64(&mut content_location),
                io_slice_mut_u32(&mut byte_length),
                io_slice_mut_u32(&mut device_alignment_requirement),
            ],
        )?;
        if needs_swap {
            content_location = content_location.swap_bytes();
            byte_length = byte_length.swap_bytes();
            device_alignment_requirement = device_alignment_requirement.swap_bytes();
        }

        Ok(Self {
            content_location,
            byte_length,
            device_alignment_requirement,
        })
    }
}

/// プリミティブの形状
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum PrimitiveTopology {
    /// 点群
    Points = 0,
    /// 線
    Lines = 1,
    /// 線ループ
    LineLoop = 2,
    /// 線ストリップ
    LineStrip = 3,
    /// 三角形
    Triangles = 4,
    /// 三角形ストリップ
    TriangleStrip = 5,
    /// 三角形ファン
    TriangleFan = 6,
}
impl TryFrom<u8> for PrimitiveTopology {
    type Error = u8;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        if Self::Points as u8 <= value && value <= Self::TriangleFan as u8 {
            Ok(unsafe { core::mem::transmute(value) })
        } else {
            Err(value)
        }
    }
}
impl PrimitiveTopology {
    pub fn into_vk(self) -> br::PrimitiveTopology {
        match self {
            Self::Points => br::PrimitiveTopology::PointList,
            Self::Lines => br::PrimitiveTopology::LineList,
            Self::LineLoop => todo!("line loop support?"),
            Self::LineStrip => br::PrimitiveTopology::LineStrip,
            Self::Triangles => br::PrimitiveTopology::TriangleList,
            Self::TriangleStrip => br::PrimitiveTopology::TriangleStrip,
            Self::TriangleFan => br::PrimitiveTopology::TriangleFan,
        }
    }
}

/// インデックスストリーム
#[derive(Debug)]
pub enum IndexStream {
    /// インデックスバッファなし
    None,
    /// インデックスバッファあり
    Stream {
        /// インデックスの型
        index_type: IndexType,
        /// バッファ情報
        buffer: StreamBuffer,
    },
}
impl IndexStream {
    pub const fn serialize_size(&self) -> usize {
        match self {
            Self::None => 1,
            Self::Stream { .. } => 1 + StreamBuffer::serialize_size(),
        }
    }

    pub fn serialize(&self, w: &mut (impl Write + ?Sized)) -> std::io::Result<()> {
        match self {
            Self::None => w.write_all(&[0]),
            Self::Stream { index_type, buffer } => {
                w.write_all(&[*index_type as u8])?;
                buffer.serialize(w)
            }
        }
    }

    pub fn deserialize(r: &mut (impl Read + ?Sized), needs_swap: bool) -> std::io::Result<Self> {
        let mut index_type_buf = [0u8];
        r.read_exact(&mut index_type_buf)?;
        if index_type_buf[0] == 0 {
            return Ok(Self::None);
        }

        let index_type = IndexType::try_from(index_type_buf[0]).expect("invalid index type");
        let buffer = StreamBuffer::deserialize(r, needs_swap)?;
        Ok(Self::Stream { index_type, buffer })
    }
}

/// インデックスの型
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum IndexType {
    /// 16ビット
    UInt16 = 1,
    /// 32ビット
    UInt32 = 2,
}
impl TryFrom<u8> for IndexType {
    type Error = u8;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        if Self::UInt16 as u8 <= value && value <= Self::UInt32 as u8 {
            Ok(unsafe { core::mem::transmute(value) })
        } else {
            Err(value)
        }
    }
}
impl IndexType {
    pub const fn into_vk(self) -> br::IndexType {
        match self {
            IndexType::UInt16 => br::IndexType::U16,
            IndexType::UInt32 => br::IndexType::U32,
        }
    }
}

/// 頂点ストリーム
#[derive(Debug)]
pub struct VertexStream {
    /// バッファ情報
    pub buffer: StreamBuffer,
    /// アトリビュートの数
    pub attribute_count: u8,
}
impl VertexStream {
    pub const fn serialize_size() -> usize {
        StreamBuffer::serialize_size() + 1
    }

    pub fn serialize(&self, w: &mut (impl Write + ?Sized)) -> std::io::Result<()> {
        self.buffer.serialize(w)?;
        w.write_all(&[self.attribute_count])
    }

    pub fn deserialize(r: &mut (impl Read + ?Sized), needs_swap: bool) -> std::io::Result<Self> {
        let buffer = StreamBuffer::deserialize(r, needs_swap)?;
        let mut attribute_count = 0u8;
        readva(r, &mut [io_slice_mut_u8(&mut attribute_count)])?;

        Ok(Self {
            buffer,
            attribute_count,
        })
    }
}

/// 頂点アトリビュート
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Attribute {
    /// 位置
    Position,
    /// 法線
    Normal,
    /// 接線
    Tangent,
    /// テクスチャ座標
    Texcoord(u8),
    /// 頂点色
    Color(u8),
    /// ボーンインデックス
    Joints(u8),
    /// ボーンウェイト
    Weights(u8),
}
impl Attribute {
    pub fn assert_validate(&self) {
        match self {
            Self::Position | Self::Normal | Self::Tangent => (),
            &Self::Texcoord(n) => assert!(n < 8, "too many texcoords"),
            &Self::Color(n) => assert!(n < 8, "too many colors"),
            &Self::Joints(n) => assert!(n < 8, "too many joints"),
            &Self::Weights(n) => assert!(n < 8, "too many weights"),
        }
    }

    pub const fn order(&self) -> u8 {
        match self {
            Self::Position => 0,
            Self::Normal => 1,
            Self::Tangent => 2,
            // up to 8 items
            &Self::Texcoord(n) => 0x08 + n,
            &Self::Color(n) => 0x10 + n,
            &Self::Joints(n) => 0x18 + n,
            &Self::Weights(n) => 0x20 + n,
        }
    }

    #[cfg(feature = "with-rendering-configuration")]
    pub fn into_semantic(&self) -> peridot_rendering_configuration::VertexInputSemantic {
        match self {
            Self::Position => peridot_rendering_configuration::VertexInputSemantic::Position(0),
            Self::Normal => peridot_rendering_configuration::VertexInputSemantic::Normal(0),
            Self::Tangent => peridot_rendering_configuration::VertexInputSemantic::Tangent(0),
            &Self::Texcoord(n) => peridot_rendering_configuration::VertexInputSemantic::Texcoord(n),
            &Self::Color(n) => peridot_rendering_configuration::VertexInputSemantic::Color(n),
            &Self::Joints(n) => todo!("joints"),
            &Self::Weights(n) => todo!("weights"),
        }
    }

    pub const fn serialize_size() -> usize {
        1
    }

    pub fn serialize(&self, w: &mut (impl Write + ?Sized)) -> std::io::Result<()> {
        match self {
            Self::Position => w.write_all(&[0]),
            Self::Normal => w.write_all(&[1]),
            Self::Tangent => w.write_all(&[2]),
            // up to 8 items
            Self::Texcoord(n) => w.write_all(&[0x08 + n]),
            Self::Color(n) => w.write_all(&[0x10 + n]),
            Self::Joints(n) => w.write_all(&[0x18 + n]),
            Self::Weights(n) => w.write_all(&[0x20 + n]),
        }
    }

    pub fn deserialize(r: &mut (impl Read + ?Sized)) -> std::io::Result<Self> {
        let mut buf = [0u8];
        r.read_exact(&mut buf)?;

        match buf[0] {
            0 => Ok(Self::Position),
            1 => Ok(Self::Normal),
            2 => Ok(Self::Tangent),
            0x08..0x10 => Ok(Self::Texcoord(buf[0] - 0x08)),
            0x10..0x18 => Ok(Self::Color(buf[0] - 0x10)),
            0x18..0x20 => Ok(Self::Joints(buf[0] - 0x18)),
            0x20..0x28 => Ok(Self::Weights(buf[0] - 0x20)),
            _ => Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "invalid attribute",
            )),
        }
    }
}

/// 頂点アトリビュートの付随データ
#[derive(Debug)]
pub struct AttributeData {
    /// バッファストリーム内の1要素あたりでのオフセット
    pub offset: u16,
    /// 要素型
    pub element_type: BufferElementType,
}
impl AttributeData {
    pub const fn serialize_size() -> usize {
        2 + 2
    }

    pub fn serialize(&self, w: &mut (impl Write + ?Sized)) -> std::io::Result<()> {
        w.write_all(&self.offset.to_ne_bytes())?;
        w.write_all(&(self.element_type as u16).to_ne_bytes())
    }

    pub fn deserialize(r: &mut (impl Read + ?Sized), needs_swap: bool) -> std::io::Result<Self> {
        let mut offset = 0u16;
        let mut element_type = 0u16;
        readva(
            r,
            &mut [
                io_slice_mut_u16(&mut offset),
                io_slice_mut_u16(&mut element_type),
            ],
        )?;
        if needs_swap {
            offset = offset.swap_bytes();
            element_type = element_type.swap_bytes();
        }
        let element_type =
            BufferElementType::try_from(element_type).expect("invalid buffer element type");

        Ok(Self {
            offset,
            element_type,
        })
    }
}

/// バッファの要素型
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u16)]
pub enum BufferElementType {
    /// u16
    Ushort = 0,
    /// vec2 f32
    Float2 = 1,
    /// vec3 f32
    Float3 = 2,
    /// vec4 f32
    Float4 = 3,
}
impl TryFrom<u16> for BufferElementType {
    type Error = u16;

    fn try_from(value: u16) -> Result<Self, Self::Error> {
        if Self::Ushort as u16 <= value && value <= Self::Float4 as u16 {
            Ok(unsafe { std::mem::transmute(value) })
        } else {
            Err(value)
        }
    }
}
impl BufferElementType {
    /// 要素のサイズ（バイト数）
    pub const fn size(&self) -> usize {
        match self {
            Self::Ushort => 2,
            Self::Float2 => 2 * 4,
            Self::Float3 => 3 * 4,
            Self::Float4 => 4 * 4,
        }
    }

    /// デバイスのアライメント要件（バイト数）
    pub const fn device_alignment_requirement(&self) -> u32 {
        match self {
            Self::Ushort => 2,
            Self::Float2 => 4,
            Self::Float3 => 4,
            Self::Float4 => 4,
        }
    }

    pub const fn into_vk_format(self) -> br::Format {
        match self {
            Self::Ushort => br::vk::VK_FORMAT_R16_UINT,
            Self::Float2 => br::vk::VK_FORMAT_R32G32_SFLOAT,
            Self::Float3 => br::vk::VK_FORMAT_R32G32B32_SFLOAT,
            Self::Float4 => br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
        }
    }
}

#[inline(always)]
fn io_slice_mut_u8<'a>(sink: &'a mut u8) -> std::io::IoSliceMut<'a> {
    std::io::IoSliceMut::new(unsafe { core::mem::transmute::<&'a mut u8, &'a mut [u8; 1]>(sink) })
}

#[inline(always)]
fn io_slice_mut_u16<'a>(sink: &'a mut u16) -> std::io::IoSliceMut<'a> {
    std::io::IoSliceMut::new(unsafe { core::mem::transmute::<&'a mut u16, &'a mut [u8; 2]>(sink) })
}

#[inline(always)]
fn io_slice_mut_u32<'a>(sink: &'a mut u32) -> std::io::IoSliceMut<'a> {
    std::io::IoSliceMut::new(unsafe { core::mem::transmute::<&'a mut u32, &'a mut [u8; 4]>(sink) })
}

#[inline(always)]
fn io_slice_mut_u64<'a>(sink: &'a mut u64) -> std::io::IoSliceMut<'a> {
    std::io::IoSliceMut::new(unsafe { core::mem::transmute::<&'a mut u64, &'a mut [u8; 8]>(sink) })
}

fn readva(
    r: &mut (impl Read + ?Sized),
    mut vec: &mut [std::io::IoSliceMut],
) -> std::io::Result<()> {
    std::io::IoSliceMut::advance_slices(&mut vec, 0);

    while !vec.is_empty() {
        let b = r.read_vectored(vec)?;
        std::io::IoSliceMut::advance_slices(&mut vec, b);
    }

    Ok(())
}
