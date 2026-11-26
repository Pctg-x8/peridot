use core::pin::Pin;

use futures_io::AsyncRead;
use pinned_futures_helper::read_exact_async_pinned;

/// Represents the semantic of a vertex shader input.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
    /// Blend Index input
    BlendIndex(u8),
    /// Blend Weights input
    BlendWeight(u8),
    /// Miscellaneous(Application-defined) input
    Misc(u8),
}
impl VertexInputSemantic {
    pub fn write(&self, writer: &mut (impl std::io::Write + ?Sized)) -> std::io::Result<usize> {
        match *self {
            Self::Misc(n) => writer.write_all(&[0, n]).map(|_| 2),
            Self::Position(n) => writer.write_all(&[1, n]).map(|_| 2),
            Self::Normal(n) => writer.write_all(&[2, n]).map(|_| 2),
            Self::Tangent(n) => writer.write_all(&[3, n]).map(|_| 2),
            Self::Binormal(n) => writer.write_all(&[4, n]).map(|_| 2),
            Self::Texcoord(n) => writer.write_all(&[5, n]).map(|_| 2),
            Self::Color(n) => writer.write_all(&[6, n]).map(|_| 2),
            Self::BlendIndex(n) => writer.write_all(&[7, n]).map(|_| 2),
            Self::BlendWeight(n) => writer.write_all(&[8, n]).map(|_| 2),
        }
    }

    #[inline(always)]
    fn read_core(bytes: [u8; 2]) -> std::io::Result<Self> {
        match bytes[0] {
            0 => Ok(Self::Misc(bytes[1])),
            1 => Ok(Self::Position(bytes[1])),
            2 => Ok(Self::Normal(bytes[1])),
            3 => Ok(Self::Tangent(bytes[1])),
            4 => Ok(Self::Binormal(bytes[1])),
            5 => Ok(Self::Texcoord(bytes[1])),
            6 => Ok(Self::Color(bytes[1])),
            7 => Ok(Self::BlendIndex(bytes[1])),
            8 => Ok(Self::BlendWeight(bytes[1])),
            _ => Err(std::io::Error::other("unknown tag for VertexInputSemantic")),
        }
    }

    pub fn read(reader: &mut (impl std::io::Read + ?Sized)) -> std::io::Result<Self> {
        let mut buf = [0u8; 2];
        reader.read_exact(&mut buf)?;
        Self::read_core(buf)
    }

    pub async fn read_async(reader: Pin<&mut (impl AsyncRead + ?Sized)>) -> std::io::Result<Self> {
        let mut buf = [core::mem::MaybeUninit::uninit(); 2];
        read_exact_async_pinned(reader, &mut buf).await?;
        Self::read_core(unsafe {
            core::mem::transmute::<[core::mem::MaybeUninit<_>; _], [_; _]>(buf)
        })
    }
}
