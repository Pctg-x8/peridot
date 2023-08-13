use euclid::Rect;
use lyon_path::builder::PathBuilder;
use peridot::math::Vector2;

#[cfg(all(target_os = "macos", not(feature = "use-freetype")))]
use objc_ext::ObjcObject;
#[cfg(all(target_os = "macos", not(feature = "use-freetype")))]
mod core_text;
#[cfg(all(target_os = "windows", not(feature = "use-freetype")))]
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

#[derive(Debug)]
pub enum FontConstructionError {
    SysAPICallError(&'static str),
    MatcherUnavailable,
    IO(std::io::Error),
    UnsupportedFontFile,
    #[cfg(feature = "use-freetype")]
    FT2(freetype2::FT_Error),
    #[cfg(target_os = "windows")]
    WindowsSysError(windows::core::Error),
}
impl From<std::io::Error> for FontConstructionError {
    fn from(v: std::io::Error) -> Self {
        Self::IO(v)
    }
}
#[cfg(target_os = "windows")]
impl From<windows::core::Error> for FontConstructionError {
    fn from(value: windows::core::Error) -> Self {
        Self::WindowsSysError(value)
    }
}

#[derive(Debug)]
pub enum GlyphLoadingError {
    SysAPICallError(&'static str),
    IO(std::io::Error),
    #[cfg(feature = "use-freetype")]
    FT2(freetype2::FT_Error),
    #[cfg(target_os = "windows")]
    WindowsSysError(windows::core::Error),
}
impl From<std::io::Error> for GlyphLoadingError {
    fn from(v: std::io::Error) -> Self {
        Self::IO(v)
    }
}
#[cfg(feature = "use-freetype")]
impl From<freetype2::FT_Error> for GlyphLoadingError {
    fn from(v: freetype2::FT_Error) -> Self {
        Self::FT2(v)
    }
}
#[cfg(target_os = "windows")]
impl From<windows::core::Error> for GlyphLoadingError {
    fn from(value: windows::core::Error) -> Self {
        Self::WindowsSysError(value)
    }
}

pub trait Font {
    type GlyphID;

    fn set_em_size(&mut self, size: f32);
    fn size(&self) -> f32;

    fn scale_value(&self) -> f32;
    fn ascent(&self) -> f32;
    fn units_per_em(&self) -> u32;

    fn glyph_id(&self, c: char) -> Option<Self::GlyphID>;
    fn advance_h(&self, glyph: &Self::GlyphID) -> Result<f32, GlyphLoadingError>;
    /// in dip
    fn bounds(&self, glyph: &Self::GlyphID) -> Result<Rect<f32>, GlyphLoadingError>;
    fn outline<B: PathBuilder>(
        &self,
        glyph: &Self::GlyphID,
        builder: &mut B,
    ) -> Result<(), GlyphLoadingError>;
}

pub type DefaultFont = <DefaultFontProvider as FontProvider>::Font;

/// An asset represents ttf blob
pub struct TTFBlob(pub(crate) Vec<u8>);
impl peridot::LogicalAssetData for TTFBlob {
    const EXT: &'static str = "ttf";
}
impl peridot::FromAsset for TTFBlob {
    type Error = std::io::Error;

    fn from_asset<Asset: std::io::Read + std::io::Seek + 'static>(
        mut asset: Asset,
    ) -> Result<Self, Self::Error> {
        let mut bin = Vec::new();
        asset.read_to_end(&mut bin).map(move |_| TTFBlob(bin))
    }
}
