//! DirectWrite Font Provider impl

use std::cell::RefCell;

use widestring::WideCString;
use windows::Win32::Graphics::DirectWrite::{
    DWriteCreateFactory, IDWriteFactory, IDWriteFontFileLoader, IDWriteFontFileLoader_Impl,
    IDWriteFontFileStream, IDWriteFontFileStream_Impl, DWRITE_FACTORY_TYPE_SHARED,
    DWRITE_FONT_SIMULATIONS_NONE, DWRITE_FONT_STRETCH_NORMAL, DWRITE_FONT_STYLE_ITALIC,
    DWRITE_FONT_STYLE_NORMAL, DWRITE_FONT_WEIGHT,
};

use crate::{Font, FontConstructionError, FontProperties, FontProvider, TTFBlob};

pub struct DirectWriteFontProvider(IDWriteFactory);
impl FontProvider for DirectWriteFontProvider {
    fn new() -> Result<Self, FontConstructionError> {
        unsafe {
            DWriteCreateFactory(DWRITE_FACTORY_TYPE_SHARED)
                .map(Self)
                .map_err(From::from)
        }
    }

    fn best_match(
        &self,
        family_name: &str,
        properties: &FontProperties,
        size: f32,
    ) -> Result<Font, FontConstructionError> {
        let mut collection = None;
        unsafe { self.0.GetSystemFontCollection(&mut collection, false)? };
        let collection = collection.expect("no system font collection");

        let mut family_index = 0;
        let mut exists = Default::default();
        let family_name = WideCString::from_str(family_name).expect("invalid sequence");
        unsafe {
            collection.FindFamilyName(
                windows::core::PCWSTR(family_name.as_ptr()),
                &mut family_index,
                &mut exists,
            )?;
        }
        let family_index = if exists.as_bool() { family_index } else { 0 };
        let family = unsafe { collection.GetFontFamily(family_index)? };
        let font_style = if properties.italic {
            DWRITE_FONT_STYLE_ITALIC
        } else {
            DWRITE_FONT_STYLE_NORMAL
        };
        let font = unsafe {
            family.GetFirstMatchingFont(
                DWRITE_FONT_WEIGHT(properties.weight as _),
                DWRITE_FONT_STRETCH_NORMAL,
                font_style,
            )?
        };

        unsafe {
            font.CreateFontFace()
                .map(|x| Font(x, size))
                .map_err(From::from)
        }
    }

    fn load<NL: peridot::NativeLinker>(
        &self,
        e: &peridot::Engine<NL>,
        asset_path: &str,
        size: f32,
    ) -> Result<Font, FontConstructionError> {
        let a: TTFBlob = e.load(asset_path)?;
        let converter =
            FontFileLoaderRegisterScope::register(&self.0, AssetToFontConverter::new(a).into())?;
        let fnt_file = unsafe {
            self.0.CreateCustomFontFileReference(
                &1u32 as *const u32 as _,
                std::mem::size_of::<u32>() as _,
                converter.loader(),
            )?
        };
        let (mut is_supported, mut file_type, mut face_type, mut face_count) = Default::default();
        unsafe {
            fnt_file.Analyze(
                &mut is_supported,
                &mut file_type,
                Some(&mut face_type),
                &mut face_count,
            )?
        };
        if !is_supported.as_bool() {
            return Err(FontConstructionError::UnsupportedFontFile);
        }

        unsafe {
            self.0
                .CreateFontFace(
                    face_type,
                    &[Some(fnt_file)],
                    0,
                    DWRITE_FONT_SIMULATIONS_NONE,
                )
                .map(|x| Font(x, size))
                .map_err(From::from)
        }
    }
}

struct FontFileLoaderRegisterScope<'a>(&'a IDWriteFactory, IDWriteFontFileLoader);
impl<'a> FontFileLoaderRegisterScope<'a> {
    fn register(
        factory: &'a IDWriteFactory,
        atf: IDWriteFontFileLoader,
    ) -> windows::core::Result<Self> {
        unsafe {
            factory
                .RegisterFontFileLoader(&atf)
                .map(|_| Self(factory, atf))
        }
    }

    fn loader(&self) -> &IDWriteFontFileLoader {
        &self.1
    }
}
impl Drop for FontFileLoaderRegisterScope<'_> {
    fn drop(&mut self) {
        unsafe {
            self.0
                .UnregisterFontFileLoader(&self.1)
                .expect("Failed to unregister FontFileLoader")
        };
    }
}

#[windows::core::implement(IDWriteFontFileLoader)]
pub struct AssetToFontConverter {
    asset: RefCell<Option<TTFBlob>>,
}
impl AssetToFontConverter {
    pub const fn new(asset: TTFBlob) -> Self {
        Self {
            asset: RefCell::new(Some(asset)),
        }
    }
}
#[allow(non_snake_case)]
impl IDWriteFontFileLoader_Impl for AssetToFontConverter {
    fn CreateStreamFromKey(
        &self,
        _font_file_reference_key: *const core::ffi::c_void,
        _font_file_reference_key_size: u32,
    ) -> windows::core::Result<windows::Win32::Graphics::DirectWrite::IDWriteFontFileStream> {
        unsafe {
            AssetStreamBridge::new(
                self.asset
                    .borrow_mut()
                    .take()
                    .expect("ATF create stream called twice?"),
            )
            .cast()
        }
    }
}

#[windows::core::implement(IDWriteFontFileStream)]
pub struct AssetStreamBridge {
    asset: TTFBlob,
}
impl AssetStreamBridge {
    const fn new(asset: TTFBlob) -> Self {
        Self { asset }
    }
}
#[allow(non_snake_case)]
impl IDWriteFontFileStream_Impl for AssetStreamBridge {
    fn GetFileSize(&self) -> windows::core::Result<u64> {
        Ok(self.asset.0.len() as _)
    }

    fn GetLastWriteTime(&self) -> windows::core::Result<u64> {
        Ok(0)
    }

    fn ReadFileFragment(
        &self,
        fragment_start: *mut *mut core::ffi::c_void,
        file_offset: u64,
        _fragment_size: u64,
        fragment_context: *mut *mut core::ffi::c_void,
    ) -> windows::core::Result<()> {
        unsafe {
            *fragment_context = std::ptr::null_mut();
            *fragment_start = self.asset.0.as_ptr().add(file_offset as _) as *mut _;
        }

        Ok(())
    }

    fn ReleaseFileFragment(&self, _fragment_context: *mut core::ffi::c_void) {}
}
