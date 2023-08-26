use cfg_if::cfg_if;

use crate::{FontConstructionError, FontProperties};

pub trait FontProviderConstruct: Sized + FontProvider {
    /// Creates font provider
    fn new() -> Result<Self, FontConstructionError>;
}
pub trait FontProvider {
    /// Associated font type for this provider
    type Font: crate::Font;

    /// Create a best-matching font for family name and provided properties
    fn best_match(
        &self,
        family_name: &str,
        properties: &FontProperties,
        size: f32,
    ) -> Result<Self::Font, FontConstructionError>;

    /// Load a font from a specific asset
    fn load<NL: peridot::NativeLinker>(
        &self,
        e: &peridot::Engine<NL>,
        asset_path: &str,
        size: f32,
    ) -> Result<Self::Font, FontConstructionError>;
}

cfg_if! {
    if #[cfg(all(windows, not(feature = "use-freetype")))] {
        // activate DirectWrite backend
        mod dwrite;
        pub use self::dwrite::*;
    }
}
cfg_if! {
    if #[cfg(feature = "use-freetype")] {
        if #[cfg(feature = "use-fontconfig")] {
            // activate FreeType/Fontconfig backend
            mod fontconfig;
            pub use self::fontconfig::*;
        } else {
            // activate freetype-only backend
            mod freetype_only;
            pub use self::freetype_only::*;
        }
    }
}
cfg_if! {
    if #[cfg(all(target_os = "macos", not(feature = "use-freetype")))] {
        // activate CoreText backend
        mod core_text;
        pub use self::core_text::*;
    }
}

cfg_if! {
    if #[cfg(feature = "use-freetype")] {
        if #[cfg(feature = "use-fontconfig")] {
            pub type DefaultFontProvider = FontconfigFontProvider;
        } else {
            pub type DefaultFontProvider = FreetypeOnlyFontProvider;
        }
    } else if #[cfg(windows)] {
        pub type DefaultFontProvider = DirectWriteFontProvider;
    } else if #[cfg(targeT_os = "macos")] {
        pub type DefaultFontProvider = CoreTextFontProvider;
    }
}
