#[cfg(feature = "fontconfig")]
use peridot_tp_fontconfig as fc;
#[cfg(feature = "freetype")]
use peridot_tp_freetype as ft;
#[cfg(feature = "harfbuzz")]
use peridot_tp_harfbuzz as hb;

#[cfg(feature = "freetype")]
pub struct FreeType(ft::Library);
#[cfg(feature = "freetype")]
impl Drop for FreeType {
    #[inline(always)]
    fn drop(&mut self) {
        if let Err(e) = unsafe { ft::done_freetype(self.0) } {
            tracing::error!(reason = ?e, "FreeType.done");
        }
    }
}
#[cfg(feature = "freetype")]
unsafe impl Sync for FreeType {}
#[cfg(feature = "freetype")]
unsafe impl Send for FreeType {}
#[cfg(feature = "freetype")]
impl FreeType {
    #[inline(always)]
    pub fn init() -> ft::Result<Self> {
        ft::init_freetype().map(Self)
    }
}

#[derive(Default, Clone, Copy, Debug)]
#[repr(usize)]
pub enum FontID {
    #[default]
    UIDefault,
    UITitleProjectName,
}

#[cfg(feature = "freetype")]
pub struct FaceSet {
    pub faces: Vec<ft::Face>,
}
#[cfg(feature = "freetype")]
impl Drop for FaceSet {
    fn drop(&mut self) {
        for x in self.faces.drain(..) {
            if let Err(e) = unsafe { ft::done_face(x) } {
                tracing::error!(reason = %e, "ft.done_face");
            }
        }
    }
}

#[cfg(all(feature = "freetype", feature = "harfbuzz"))]
pub struct FaceShapingSet {
    pub faces: Vec<core::ptr::NonNull<hb::ffi::hb_font_t>>,
}
#[cfg(all(feature = "freetype", feature = "harfbuzz"))]
impl Drop for FaceShapingSet {
    fn drop(&mut self) {
        for x in self.faces.drain(..) {
            unsafe {
                hb::ffi::hb_font_destroy(x.as_ptr());
            }
        }
    }
}

pub struct PerWindowFontSet<'d> {
    #[cfg(windows)]
    dw_factory: &'d windows::Win32::Graphics::DirectWrite::IDWriteFactory,
    #[cfg(windows)]
    ui_default: &'d windows::Win32::Graphics::DirectWrite::IDWriteTextFormat,
    #[cfg(windows)]
    ui_title_project_name: &'d windows::Win32::Graphics::DirectWrite::IDWriteTextFormat,
    #[cfg(feature = "freetype")]
    ui_default: FaceSet,
    #[cfg(feature = "freetype")]
    ui_title_project_name: FaceSet,
    #[cfg(feature = "harfbuzz")]
    ui_default_shaping: FaceShapingSet,
    #[cfg(feature = "harfbuzz")]
    ui_title_project_name_shaping: FaceShapingSet,
    #[cfg(feature = "freetype")]
    _marker: core::marker::PhantomData<&'d [ft::raw::FT_Byte]>,
    #[cfg(target_os = "macos")]
    ui_default: &'d apple_sdk_port::text::Font,
    #[cfg(target_os = "macos")]
    ui_title_project_name: &'d apple_sdk_port::text::Font,
}
impl<'d> PerWindowFontSet<'d> {
    pub fn new(root_set: &'d RootFontSet) -> Self {
        #[cfg(feature = "freetype")]
        let ui_default = root_set
            .ui_common_font_data
            .iter()
            .map(|&(ref f, ix)| unsafe {
                ft::new_memory_face(root_set.ft_lib.0, f, ix as _)
                    .expect("FreeType.new_face.ui_default")
            })
            .collect::<Vec<_>>();
        #[cfg(feature = "freetype")]
        let ui_title_project_name = root_set
            .ui_common_font_data
            .iter()
            .map(|&(ref f, ix)| unsafe {
                ft::new_memory_face(root_set.ft_lib.0, f, ix as _)
                    .expect("FreeType.new_face.ui_title_project_name")
            })
            .collect::<Vec<_>>();

        #[cfg(feature = "harfbuzz")]
        let ui_default_shaping = ui_default
            .iter()
            .map(|&f| {
                core::ptr::NonNull::new(unsafe {
                    peridot_tp_harfbuzz::ffi::hb_ft_font_create_referenced(f)
                })
                .expect("hb_ft_font_create_referenced.ui_default")
            })
            .collect::<Vec<_>>();
        #[cfg(feature = "harfbuzz")]
        let ui_title_project_name_shaping = ui_title_project_name
            .iter()
            .map(|&f| {
                core::ptr::NonNull::new(unsafe {
                    peridot_tp_harfbuzz::ffi::hb_ft_font_create_referenced(f)
                })
                .expect("hb_ft_font_create_referenced.ui_title_project_name")
            })
            .collect::<Vec<_>>();

        Self {
            #[cfg(windows)]
            dw_factory: &root_set.dw_factory,
            #[cfg(windows)]
            ui_default: &root_set.ui_default,
            #[cfg(windows)]
            ui_title_project_name: &root_set.ui_title_project_name,
            #[cfg(feature = "freetype")]
            ui_default: FaceSet { faces: ui_default },
            #[cfg(feature = "freetype")]
            ui_title_project_name: FaceSet {
                faces: ui_title_project_name,
            },
            #[cfg(feature = "harfbuzz")]
            ui_default_shaping: FaceShapingSet {
                faces: ui_default_shaping,
            },
            #[cfg(feature = "harfbuzz")]
            ui_title_project_name_shaping: FaceShapingSet {
                faces: ui_title_project_name_shaping,
            },
            #[cfg(feature = "freetype")]
            _marker: core::marker::PhantomData,
            #[cfg(target_os = "macos")]
            ui_default: &root_set.ui_default,
            #[cfg(target_os = "macos")]
            ui_title_project_name: &root_set.ui_title_project_name,
        }
    }

    #[cfg(feature = "freetype")]
    #[tracing::instrument(skip(self))]
    pub fn rescale(&mut self, dpi: u32) {
        use ft::FractionalExt;

        for &x in &self.ui_default.faces {
            if let Err(e) = unsafe { ft::set_char_size(x, 0, 12.0f32.to_f26dot6_lossy(), 0, dpi) } {
                tracing::error!(reason = %e, "FreeType.set_char_size.ui_default");
            }
        }
        for &x in &self.ui_title_project_name.faces {
            if let Err(e) = unsafe { ft::set_char_size(x, 0, 10.0f32.to_f26dot6_lossy(), 0, dpi) } {
                tracing::error!(reason = %e, "FreeType.set_char_size.ui_title_project_name");
            }
        }

        #[cfg(feature = "harfbuzz")]
        unsafe {
            for &x in &self.ui_default_shaping.faces {
                hb::ffi::hb_ft_font_changed(x.as_ptr());
            }
            for &x in &self.ui_title_project_name_shaping.faces {
                hb::ffi::hb_ft_font_changed(x.as_ptr());
            }
        }
    }

    #[cfg(target_os = "macos")]
    #[inline]
    pub fn select(&self, category: FontID) -> &apple_sdk_port::text::Font {
        match category {
            FontID::UIDefault => &self.ui_default,
            FontID::UITitleProjectName => &self.ui_title_project_name,
        }
    }

    #[cfg(feature = "freetype")]
    #[inline]
    pub fn select(&self, category: FontID) -> &FaceSet {
        match category {
            FontID::UIDefault => &self.ui_default,
            FontID::UITitleProjectName => &self.ui_title_project_name,
        }
    }

    #[cfg(feature = "harfbuzz")]
    #[inline]
    pub fn select_shaping(&self, category: FontID) -> &FaceShapingSet {
        match category {
            FontID::UIDefault => &self.ui_default_shaping,
            FontID::UITitleProjectName => &self.ui_title_project_name_shaping,
        }
    }

    #[cfg(windows)]
    #[inline(always)]
    pub const fn native_factory(
        &self,
    ) -> &'d windows::Win32::Graphics::DirectWrite::IDWriteFactory {
        self.dw_factory
    }

    #[cfg(windows)]
    #[inline]
    pub fn select(
        &self,
        category: FontID,
    ) -> &windows::Win32::Graphics::DirectWrite::IDWriteTextFormat {
        match category {
            FontID::UIDefault => &self.ui_default,
            FontID::UITitleProjectName => &self.ui_title_project_name,
        }
    }
}

pub struct RootFontSet {
    #[cfg(target_os = "macos")]
    ui_default: apple_sdk_port::Owned<apple_sdk_port::text::Font>,
    #[cfg(target_os = "macos")]
    ui_title_project_name: apple_sdk_port::Owned<apple_sdk_port::text::Font>,
    #[cfg(feature = "freetype")]
    ft_lib: FreeType,
    #[cfg(feature = "freetype")]
    ui_common_font_data: Vec<(Vec<ft::raw::FT_Byte>, core::ffi::c_int)>,
    #[cfg(windows)]
    dw_factory: windows::Win32::Graphics::DirectWrite::IDWriteFactory,
    #[cfg(windows)]
    ui_default: windows::Win32::Graphics::DirectWrite::IDWriteTextFormat,
    #[cfg(windows)]
    ui_title_project_name: windows::Win32::Graphics::DirectWrite::IDWriteTextFormat,
}
impl RootFontSet {
    #[cfg(windows)]
    pub fn new() -> Self {
        use windows::Win32::{
            Globalization::GetUserDefaultLocaleName, Graphics::DirectWrite::IDWriteFactory,
        };

        let dw: IDWriteFactory = unsafe {
            use windows::Win32::Graphics::DirectWrite::{
                DWRITE_FACTORY_TYPE_SHARED, DWriteCreateFactory,
            };

            DWriteCreateFactory(DWRITE_FACTORY_TYPE_SHARED).expect("dwrite.factory.create")
        };

        let mut locale_name = [const { core::mem::MaybeUninit::uninit() }; 32];
        let len = unsafe {
            GetUserDefaultLocaleName(core::mem::transmute::<
                &mut [core::mem::MaybeUninit<u16>; 32],
                &mut [u16; 32],
            >(&mut locale_name))
        };
        let locale_name = if len == 0 {
            // fallback to en_US
            let e = std::io::Error::last_os_error();
            tracing::warn!(reason = ?e, "GetUserDefaultLocaleName.fallback");

            &[b'e' as u16, b'n' as _, b'_' as _, b'U' as _, b'S' as _, 0]
        } else {
            unsafe {
                core::mem::transmute::<&[core::mem::MaybeUninit<u16>], &[u16]>(
                    &locale_name[..len as usize],
                )
            }
        };

        let ui_default = unsafe {
            dw.CreateTextFormat(
                windows_core::w!("Inter Display"),
                None,
                windows::Win32::Graphics::DirectWrite::DWRITE_FONT_WEIGHT_NORMAL,
                windows::Win32::Graphics::DirectWrite::DWRITE_FONT_STYLE_NORMAL,
                windows::Win32::Graphics::DirectWrite::DWRITE_FONT_STRETCH_NORMAL,
                12.0,
                windows_core::PCWSTR(locale_name.as_ptr()),
            )
            .expect("dwrite.textformat.create.ui_default")
        };
        let ui_title_project_name = unsafe {
            dw.CreateTextFormat(
                windows_core::w!("Inter Display"),
                None,
                windows::Win32::Graphics::DirectWrite::DWRITE_FONT_WEIGHT_NORMAL,
                windows::Win32::Graphics::DirectWrite::DWRITE_FONT_STYLE_NORMAL,
                windows::Win32::Graphics::DirectWrite::DWRITE_FONT_STRETCH_NORMAL,
                10.0,
                windows_core::PCWSTR(locale_name.as_ptr()),
            )
            .expect("dwrite.textformat.create.ui_title_project_name")
        };

        Self {
            dw_factory: dw,
            ui_default,
            ui_title_project_name,
        }
    }

    #[cfg(feature = "freetype")]
    pub fn new() -> Self {
        #[cfg(feature = "fontconfig")]
        let ui_common_fonts = unsafe {
            use std::collections::HashSet;

            fc::init().expect("FontConfig.init");
            let mut pat = fc::Pattern::new().expect("FcPattern.create");
            pat.as_mut()
                .add(fc::Pattern::KEY_FAMILY, c"Inter Display")
                .expect("FcPattern.add.family");
            pat.as_mut()
                .add(fc::Pattern::KEY_WEIGHT, &fc::raw::FC_WEIGHT_REGULAR)
                .expect("FcPattern.add.weight");
            pat.as_mut()
                .add(fc::Pattern::KEY_SIZE, &(12.0 as core::ffi::c_double))
                .expect("FcPattern.add.size");
            fc::Config::current()
                .unwrap_unchecked()
                .as_mut()
                .substitute(pat.as_mut(), fc::MatchKind::Pattern)
                .expect("FcConfig.substitute");
            pat.as_mut().default_substitute();
            let fonts = fc::sort(
                fc::Config::current().unwrap_unchecked().as_mut(),
                pat.as_mut(),
                false,
                None,
            )
            .expect("FontConfig.sort");

            let mut selected_font_paths = HashSet::new();
            let mut fonts_ordered = Vec::new();
            for n in 0..fonts.as_ref().nfont {
                let f = *fonts.as_ref().fonts.add(n as usize);
                let file: &core::ffi::CStr = (*f)
                    .get(fc::Pattern::KEY_FILE)
                    .expect("FcPattern.get.file")
                    .expect("FcPattern.get.not_exist.file");
                let file = file.to_owned();
                let index: core::ffi::c_int = (*f)
                    .get(fc::Pattern::KEY_INDEX)
                    .expect("FcPattern.get.index")
                    .expect("FcPattern.get.not_exist.index");

                if !selected_font_paths.insert((file.clone(), index)) {
                    // すでにロードしたフォント
                    continue;
                }

                fonts_ordered.push((file, index));
            }

            fonts_ordered
        };

        let ui_common_font_data = ui_common_fonts
            .into_iter()
            .map(|(f, ix)| {
                (
                    std::fs::read(f.to_str().expect("cstr.to_str")).expect("font.readfile"),
                    ix,
                )
            })
            .collect::<Vec<_>>();

        Self {
            ft_lib: FreeType::init().expect("freetype.init"),
            ui_common_font_data,
        }
    }

    #[cfg(target_os = "macos")]
    pub fn new() -> Self {
        let ui_default = apple_sdk_port::text::Font::new_ui(
            apple_sdk_port::text::UIFontType::System,
            12.0,
            None,
        );
        let ui_title_project_name = apple_sdk_port::text::Font::new_ui(
            apple_sdk_port::text::UIFontType::System,
            10.0,
            None,
        );

        Self {
            ui_default,
            ui_title_project_name,
        }
    }
}
