use euclid::Rect;
use lyon_path::builder::PathBuilder;
use peridot::{math::Vector2, AssetBlob};

#[cfg(all(target_os = "macos", not(feature = "use-freetype")))]
mod core_text;
#[cfg(all(windows, not(feature = "use-freetype")))]
mod dwrite;
#[cfg(feature = "use-freetype")]
mod freetype;

mod provider;
pub use self::provider::*;

pub struct GlyphBound<T> {
    left: T,
    top: T,
    right: T,
    bottom: T,
}
impl<T: Copy> GlyphBound<T> {
    pub fn offset(&self) -> Vector2<T> {
        Vector2(self.left, self.top)
    }
    pub fn size(&self) -> Vector2<T>
    where
        T: std::ops::Sub<Output = T>,
    {
        Vector2(self.right - self.left, self.bottom - self.top)
    }
}
#[repr(C)]
#[derive(Clone, Debug)]
pub(crate) struct GlyphTransform {
    pub st: [f32; 4],
    pub ext: [f32; 2],
    pub pad: [f32; 2],
}

pub struct FontProperties {
    pub italic: bool,
    pub weight: u16,
}
impl Default for FontProperties {
    fn default() -> Self {
        FontProperties {
            italic: false,
            weight: 400,
        }
    }
}
#[cfg(target_os = "macos")]
impl FontProperties {
    pub fn native_weight(&self) -> f32 {
        2.0 * self.weight as f32 / 1000.0 - 1.0
    }
}

#[derive(Debug, thiserror::Error)]
pub enum FontConstructionError {
    #[error("No matcher available")]
    MatcherUnavailable,
    #[error("Unsupported Font File")]
    UnsupportedFontFile,
    #[error("System API Call Error: {0}")]
    SysAPICallError(&'static str),
    #[error("IO Error: {0}")]
    IO(#[from] std::io::Error),
    #[cfg(feature = "use-freetype")]
    #[error("FreeType2 Error: {0}")]
    FT2(#[from] peridot_tp_freetype::Error),
    #[cfg(target_os = "windows")]
    #[error("Windows System Error: {0}")]
    WindowsSysError(#[from] windows::core::Error),
}

#[derive(Debug, thiserror::Error)]
pub enum GlyphLoadingError {
    #[error("System API Call Error: {0}")]
    SysAPICallError(&'static str),
    #[error("IO Error: {0}")]
    IO(#[from] std::io::Error),
    #[cfg(feature = "use-freetype")]
    #[error("FreeType2 Error: {0}")]
    FT2(#[from] peridot_tp_freetype::Error),
    #[cfg(target_os = "windows")]
    #[error("Windows System Call Error: {0}")]
    WindowsSysError(#[from] windows::core::Error),
}

/// Represents a font(layered on DirectWrite FontFace / FreeType Face)
///
/// For default implementation type, use `DefaultFont` type in some platforms.
pub trait Font {
    type GlyphID;

    fn set_em_size(&mut self, size: f32);
    fn size(&self) -> f32;

    fn ascent(&self) -> f32;
    fn units_per_em(&self) -> u32;

    fn glyph_id(&self, c: char) -> Option<Self::GlyphID>;
    fn advance_h(&self, glyph: &Self::GlyphID) -> Result<f32, GlyphLoadingError>;
    /// in dip
    fn bounds(&self, glyph: &Self::GlyphID) -> Result<Rect<f32>, GlyphLoadingError>;
    fn outline<B: PathBuilder>(
        &self,
        glyph: &Self::GlyphID,
        transform: &euclid::Transform2D<f32>,
        builder: &mut B,
    ) -> Result<(), GlyphLoadingError>;
}

#[cfg(not(doc))]
#[cfg(not(feature = "ci-nolib"))]
pub type DefaultFont = <DefaultFontProvider as FontProvider>::Font;

/// An asset represents ttf blob
pub struct TTFBlob(pub(crate) Vec<u8>);
impl peridot::LogicalAssetData for TTFBlob {
    const EXT: &'static str = "ttf";
}
impl peridot::FromAssetBlob for TTFBlob {
    type Error = std::io::Error;

    #[inline(always)]
    fn from_asset_blob<'a, Blob: AssetBlob + 'a>(blob: Blob) -> Result<Self, Self::Error> {
        Ok(Self(blob.read_to_end(0)?))
    }
}
impl peridot::FromAssetBlobAsync for TTFBlob {
    type Error = std::io::Error;

    #[inline(always)]
    fn from_asset_blob_async<'a, Blob: peridot::AssetBlobAsync + 'a>(
        blob: Blob,
    ) -> impl core::future::Future<Output = Result<Self, Self::Error>> {
        async move { Ok(Self(blob.read_to_end_async(0).await?)) }
    }
}
