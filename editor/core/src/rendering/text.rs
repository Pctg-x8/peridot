use std::{cell::UnsafeCell, collections::HashMap};

#[cfg(target_os = "linux")]
use peridot_tp_budoux as budoux;
#[cfg(feature = "fontconfig")]
use peridot_tp_fontconfig as fc;
#[cfg(feature = "freetype")]
use peridot_tp_freetype as ft;
#[cfg(feature = "harfbuzz")]
use peridot_tp_harfbuzz as hb;
#[cfg(target_os = "linux")]
use peridot_tp_icu as icu;

use crate::{
    rendering::{
        MaskTextureAtlasManager, composite::CompositeRectTextHorizontalAlignment,
        vg::VectorRasterizationState,
    },
    utils::{LogicalUnit, Point, Rect, Size},
};

#[cfg(target_os = "macos")]
mod darwin_coretext;
#[cfg(windows)]
mod dwrite;

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
    UIFormLiftedLabel,
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

pub struct FontSet {
    #[cfg(target_os = "macos")]
    ui_default: apple_sdk_port::Owned<apple_sdk_port::text::Font>,
    #[cfg(target_os = "macos")]
    ui_title_project_name: apple_sdk_port::Owned<apple_sdk_port::text::Font>,
    #[cfg(target_os = "macos")]
    ui_form_lifted_label: apple_sdk_port::Owned<apple_sdk_port::text::Font>,
    #[cfg(feature = "freetype")]
    ui_default: FaceSet,
    #[cfg(feature = "freetype")]
    ui_title_project_name: FaceSet,
    #[cfg(feature = "freetype")]
    ui_form_lifted_label: FaceSet,
    #[cfg(feature = "harfbuzz")]
    ui_default_shaping: FaceShapingSet,
    #[cfg(feature = "harfbuzz")]
    ui_title_project_name_shaping: FaceShapingSet,
    #[cfg(feature = "harfbuzz")]
    ui_form_lifted_label_shaping: FaceShapingSet,
    #[cfg(feature = "freetype")]
    _ft_lib: FreeType,
    #[cfg(windows)]
    dw_factory: windows::Win32::Graphics::DirectWrite::IDWriteFactory,
    #[cfg(windows)]
    ui_default: windows::Win32::Graphics::DirectWrite::IDWriteTextFormat,
    #[cfg(windows)]
    ui_title_project_name: windows::Win32::Graphics::DirectWrite::IDWriteTextFormat,
    #[cfg(windows)]
    ui_form_lifted_label: windows::Win32::Graphics::DirectWrite::IDWriteTextFormat,
}
#[cfg(any(target_os = "macos", feature = "freetype"))]
unsafe impl Sync for FontSet {}
#[cfg(any(target_os = "macos", feature = "freetype"))]
unsafe impl Send for FontSet {}
impl FontSet {
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
        let ui_form_lifted_label = unsafe {
            dw.CreateTextFormat(
                windows_core::w!("Inter Display"),
                None,
                windows::Win32::Graphics::DirectWrite::DWRITE_FONT_WEIGHT_NORMAL,
                windows::Win32::Graphics::DirectWrite::DWRITE_FONT_STYLE_NORMAL,
                windows::Win32::Graphics::DirectWrite::DWRITE_FONT_STRETCH_NORMAL,
                8.0,
                windows_core::PCWSTR(locale_name.as_ptr()),
            )
            .expect("dwrite.textformat.create.ui_form_lifted_label")
        };

        Self {
            dw_factory: dw,
            ui_default,
            ui_title_project_name,
            ui_form_lifted_label,
        }
    }

    #[cfg(feature = "freetype")]
    pub fn new() -> Self {
        let ft_lib = FreeType::init().expect("freetype.init");

        let mut font_binary_paths = Vec::new();
        #[cfg(feature = "fontconfig")]
        let ui_common_font_data = unsafe {
            use std::collections::{HashMap, HashSet};

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

            let mut selected_fonts = HashSet::new();
            let mut loaded_fonts = HashMap::new();
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

                let font_binary_index = match loaded_fonts.entry(file) {
                    std::collections::hash_map::Entry::Occupied(x) => *x.get(),
                    std::collections::hash_map::Entry::Vacant(x) => {
                        font_binary_paths.push(x.key().clone());
                        *x.insert(font_binary_paths.len() - 1)
                    }
                };

                if selected_fonts.insert((font_binary_index, index)) {
                    // 未知のフォント
                    fonts_ordered.push((font_binary_index, index));
                }
            }

            fonts_ordered
        };

        use ft::FractionalExt;

        let ui_default = ui_common_font_data
            .iter()
            .map(|&(f, ix)| {
                let face = unsafe {
                    ft::new_face(ft_lib.0, &font_binary_paths[f], ix as _)
                        .expect("FreeType.new_face.ui_default")
                };
                if let Err(e) =
                    unsafe { ft::set_char_size(face, 0, 12.0f32.to_f26dot6_lossy(), 0, 72) }
                {
                    tracing::error!(reason = %e, "FreeType.set_char_size.ui_default");
                }

                face
            })
            .collect::<Vec<_>>();
        let ui_title_project_name = ui_common_font_data
            .iter()
            .map(|&(f, ix)| {
                let face = unsafe {
                    ft::new_face(ft_lib.0, &font_binary_paths[f], ix as _)
                        .expect("FreeType.new_face.ui_title_project_name")
                };
                if let Err(e) =
                    unsafe { ft::set_char_size(face, 0, 10.0f32.to_f26dot6_lossy(), 0, 72) }
                {
                    tracing::error!(reason = %e, "FreeType.set_char_size.ui_title_project_name");
                }

                face
            })
            .collect::<Vec<_>>();
        let ui_form_lifted_label = ui_common_font_data
            .iter()
            .map(|&(f, ix)| {
                let face = unsafe {
                    ft::new_face(ft_lib.0, &font_binary_paths[f], ix as _)
                        .expect("FreeType.new_face.ui_form_lifted_label")
                };
                if let Err(e) =
                    unsafe { ft::set_char_size(face, 0, 8.0f32.to_f26dot6_lossy(), 0, 72) }
                {
                    tracing::error!(reason = %e, "FreeType.set_char_size.ui_form_lifted_label");
                }

                face
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
        #[cfg(feature = "harfbuzz")]
        let ui_form_lifted_label_shaping = ui_form_lifted_label
            .iter()
            .map(|&f| {
                core::ptr::NonNull::new(unsafe {
                    peridot_tp_harfbuzz::ffi::hb_ft_font_create_referenced(f)
                })
                .expect("hb_ft_font_create_referenced.ui_form_lifted_label")
            })
            .collect::<Vec<_>>();

        Self {
            _ft_lib: ft_lib,
            ui_default: FaceSet { faces: ui_default },
            ui_title_project_name: FaceSet {
                faces: ui_title_project_name,
            },
            ui_form_lifted_label: FaceSet {
                faces: ui_form_lifted_label,
            },
            ui_default_shaping: FaceShapingSet {
                faces: ui_default_shaping,
            },
            ui_title_project_name_shaping: FaceShapingSet {
                faces: ui_title_project_name_shaping,
            },
            ui_form_lifted_label_shaping: FaceShapingSet {
                faces: ui_form_lifted_label_shaping,
            },
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
        let ui_form_lifted_label =
            apple_sdk_port::text::Font::new_ui(apple_sdk_port::text::UIFontType::System, 8.0, None);

        Self {
            ui_default,
            ui_title_project_name,
            ui_form_lifted_label,
        }
    }

    #[cfg(feature = "freetype")]
    #[inline]
    pub fn select(&self, category: FontID) -> &FaceSet {
        match category {
            FontID::UIDefault => &self.ui_default,
            FontID::UITitleProjectName => &self.ui_title_project_name,
            FontID::UIFormLiftedLabel => &self.ui_form_lifted_label,
        }
    }

    #[cfg(feature = "harfbuzz")]
    #[inline]
    pub fn select_shaping(&self, category: FontID) -> &FaceShapingSet {
        match category {
            FontID::UIDefault => &self.ui_default_shaping,
            FontID::UITitleProjectName => &self.ui_title_project_name_shaping,
            FontID::UIFormLiftedLabel => &self.ui_form_lifted_label_shaping,
        }
    }

    #[cfg(target_os = "macos")]
    #[inline]
    pub fn select(&self, category: FontID) -> &apple_sdk_port::text::Font {
        match category {
            FontID::UIDefault => &self.ui_default,
            FontID::UITitleProjectName => &self.ui_title_project_name,
            FontID::UIFormLiftedLabel => &self.ui_form_lifted_label,
        }
    }

    #[cfg(windows)]
    #[inline(always)]
    pub const fn native_factory(&self) -> &windows::Win32::Graphics::DirectWrite::IDWriteFactory {
        &self.dw_factory
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
            FontID::UIFormLiftedLabel => &self.ui_form_lifted_label,
        }
    }
}

pub struct TextRun<'s> {
    pub content: &'s str,
    pub font: FontID,
    pub spacing_inline_start: f32,
}

#[cfg(feature = "harfbuzz")]
pub struct NativeTextRun {
    buffer: *mut hb::ffi::hb_buffer_t,
    byte_range: core::ops::Range<usize>,
    left_offset: f32,
    font_id: FontID,
    face_index: usize,
}
#[cfg(feature = "harfbuzz")]
impl Drop for NativeTextRun {
    #[inline(always)]
    fn drop(&mut self) {
        unsafe {
            hb::ffi::hb_buffer_destroy(self.buffer);
        }
    }
}
#[cfg(feature = "harfbuzz")]
impl NativeTextRun {
    pub fn visual_width(&self, font_set: &FontSet) -> f32 {
        let face = font_set.select(self.font_id).faces[self.face_index];

        let mut glyph_infos_len = core::mem::MaybeUninit::uninit();
        let glyph_infos = unsafe {
            hb::ffi::hb_buffer_get_glyph_infos(self.buffer, glyph_infos_len.as_mut_ptr())
        };
        let mut glyph_positions_len = core::mem::MaybeUninit::uninit();
        let glyph_positions = unsafe {
            hb::ffi::hb_buffer_get_glyph_positions(self.buffer, glyph_positions_len.as_mut_ptr())
        };
        let glyph_infos =
            unsafe { core::slice::from_raw_parts(glyph_infos, glyph_infos_len.assume_init() as _) };
        let glyph_positions = unsafe {
            core::slice::from_raw_parts(glyph_positions, glyph_positions_len.assume_init() as _)
        };
        assert_eq!(glyph_infos.len(), glyph_positions.len());

        match (glyph_positions, glyph_infos) {
            (&[ref advances @ .., _], &[.., ref last_glyph]) => {
                unsafe {
                    ft::load_glyph(face, last_glyph.codepoint, ft::LoadFlags::DEFAULT)
                        .expect("face.load_glyph");
                }

                advances
                    .iter()
                    .map(|p| p.x_advance as f32 / 64.0)
                    .sum::<f32>()
                    + unsafe { &(*(*face).glyph).metrics }.width as f32 / 64.0
            }
            (&[], &[]) => 0.0,
            _ => unreachable!(),
        }
    }
}

#[cfg(feature = "harfbuzz")]
pub struct HarfbuzzLineLayout {
    buffers: Vec<NativeTextRun>,
    width_with_trailing_whitespace: f32,
    height: f32,
    line_top_offset: f32,
    baseline_y_offset: f32,
}
#[cfg(feature = "harfbuzz")]
impl HarfbuzzLineLayout {
    pub fn visual_width(&self, font_set: &FontSet) -> f32 {
        self.buffers
            .iter()
            // compute visual right of each buffers
            .map(|tr| tr.left_offset + tr.visual_width(font_set))
            .fold(0.0, f32::max)
    }
}

crate::perf_section!(CREATE_TEXT_LAYOUT = "TextLayout.New");
crate::perf_section!(RASTERIZE_AND_PLACE_GLYPHS = "TextLayout.RasterizeAndPlaceGlyphs");

pub struct TextLayout {
    #[cfg(feature = "harfbuzz")]
    lines: Vec<HarfbuzzLineLayout>,
    #[cfg(feature = "harfbuzz")]
    height: f32,
    #[cfg(target_os = "macos")]
    internal: darwin_coretext::CoreTextLayout,
    #[cfg(windows)]
    internal: dwrite::TextLayout,
}
impl TextLayout {
    #[inline(always)]
    pub fn new_single<'s>(
        text: &'s str,
        font: FontID,
        font_set: &FontSet,
        alignment: CompositeRectTextHorizontalAlignment,
        max_width: Option<f32>,
    ) -> Self {
        Self::new(
            core::iter::once(TextRun {
                content: text,
                font,
                spacing_inline_start: 0.0,
            }),
            font_set,
            alignment,
            max_width,
        )
    }

    pub fn new<'s>(
        text_runs: impl Iterator<Item = TextRun<'s>>,
        font_set: &FontSet,
        alignment: CompositeRectTextHorizontalAlignment,
        max_width: Option<f32>,
    ) -> Self {
        crate::perf_scope!(CREATE_TEXT_LAYOUT);

        #[cfg(feature = "harfbuzz")]
        let (lb, ub) = text_runs.size_hint();
        #[cfg(feature = "harfbuzz")]
        let mut lines = vec![HarfbuzzLineLayout {
            buffers: Vec::new(),
            width_with_trailing_whitespace: 0.0,
            height: 0.0,
            line_top_offset: 0.0,
            baseline_y_offset: 0.0,
        }];
        #[cfg(feature = "harfbuzz")]
        let mut line_y_offset = 0.0f32;
        #[cfg(feature = "harfbuzz")]
        let mut line_height = 0.0f32;
        #[cfg(feature = "freetype")]
        let mut left_offset = 0.0f32;
        #[cfg(feature = "freetype")]
        let mut final_line_height = 0.0f32;
        #[cfg(feature = "freetype")]
        if let Some(max_width) = max_width {
            for x in text_runs {
                left_offset += x.spacing_inline_start;

                #[cfg(feature = "freetype")]
                let font = font_set.select(x.font);
                #[cfg(feature = "harfbuzz")]
                let shaping_set = font_set.select_shaping(x.font);

                let mut line_boundary_clusters = vec![Vec::new()];
                let mut starting_bytes = 0;
                let mut in_budou_cluster = false;
                loop {
                    let mut ending_bytes = starting_bytes;
                    let mut break_by_newline = false;
                    for c in x.content[starting_bytes..].chars() {
                        if c == '\n' {
                            break_by_newline = true;
                            break;
                        }

                        if in_budou_cluster != crate::utils::is_budou_cluster_char(c) {
                            // breaking method boundary
                            break;
                        }

                        ending_bytes += c.len_utf8();
                    }

                    if starting_bytes != ending_bytes {
                        if in_budou_cluster {
                            line_boundary_clusters
                                .last_mut()
                                .expect("empty lines")
                                .extend(
                                    budoux::BreakIterator::new(
                                        &budoux::embedded::ja_knbc::MODEL,
                                        &x.content[starting_bytes..ending_bytes],
                                    )
                                    .scan(0, |last_boundary, next_boundary| {
                                        let res = if *last_boundary == next_boundary {
                                            // ignore this chunk
                                            None
                                        } else {
                                            Some(
                                                &x.content[*last_boundary + starting_bytes
                                                    ..next_boundary + starting_bytes],
                                            )
                                        };
                                        *last_boundary = next_boundary;
                                        Some(res)
                                    })
                                    .filter_map(crate::utils::identity),
                                );
                        } else {
                            let u16s = x.content[starting_bytes..ending_bytes]
                                .encode_utf16()
                                .collect::<Vec<_>>();
                            line_boundary_clusters
                                .last_mut()
                                .expect("empty lines")
                                .extend(
                                    icu::BreakIterator::new(
                                        icu::BreakIteratorType::Line,
                                        None,
                                        &u16s,
                                    )
                                    .expect("icu.break_iterator.new")
                                    .into_iter()
                                    .scan(0, |last_boundary, next_boundary| {
                                        let next_boundary = next_boundary as usize;
                                        let res = if *last_boundary == next_boundary {
                                            // ignore this chunk
                                            None
                                        } else {
                                            // TODO: ascii以外も来るか？
                                            Some(
                                                &x.content[starting_bytes + *last_boundary
                                                    ..starting_bytes + next_boundary],
                                            )
                                        };

                                        *last_boundary = next_boundary;
                                        Some(res)
                                    })
                                    .filter_map(crate::utils::identity),
                                );
                        }
                    }

                    starting_bytes = ending_bytes;
                    if break_by_newline {
                        line_boundary_clusters.push(Vec::new());
                        starting_bytes += 1;
                    }

                    if starting_bytes >= x.content.len() {
                        break;
                    }

                    in_budou_cluster = !in_budou_cluster;
                }
                tracing::debug!(?line_boundary_clusters);

                let line_count = line_boundary_clusters.len();
                for (n, line) in line_boundary_clusters.into_iter().enumerate() {
                    let mut baseline_y_offset = 0.0f32;
                    let mut line_left_offset = left_offset;
                    for b in line {
                        let mut section_buffers = Vec::new();
                        let mut section_left_offset = 0.0f32;
                        let mut section_visual_right = 0.0f32;
                        let mut section_line_height = 0.0f32;
                        let mut section_height = 0.0f32;

                        let mut face_index = 0;
                        let mut shaped_bytes = 0usize;
                        while shaped_bytes < b.len() {
                            let starting_bytes = shaped_bytes;
                            for c in b[starting_bytes..].chars() {
                                if unsafe { ft::get_char_index(font.faces[face_index], c as _) }
                                    == 0
                                {
                                    // no char in font, needs fallback
                                    break;
                                }

                                shaped_bytes += c.len_utf8();
                            }

                            if starting_bytes != shaped_bytes {
                                // needs shaping
                                let face = font.faces[face_index];
                                let shaping_face = shaping_set.faces[face_index];
                                let byte_range = starting_bytes..shaped_bytes;

                                #[cfg(feature = "harfbuzz")]
                                let buf = unsafe { hb::ffi::hb_buffer_create() };
                                #[cfg(feature = "harfbuzz")]
                                unsafe {
                                    hb::ffi::hb_buffer_add_utf8(
                                        buf,
                                        b.as_ptr().add(byte_range.start).cast(),
                                        byte_range.len() as _,
                                        0,
                                        -1,
                                    );
                                    hb::ffi::hb_buffer_guess_segment_properties(buf);
                                    hb::ffi::hb_shape(
                                        shaping_face.as_ptr(),
                                        buf,
                                        core::ptr::null(),
                                        0,
                                    );
                                }

                                let mut glyph_infos_len = core::mem::MaybeUninit::uninit();
                                let glyph_infos = unsafe {
                                    hb::ffi::hb_buffer_get_glyph_infos(
                                        buf,
                                        glyph_infos_len.as_mut_ptr(),
                                    )
                                };
                                let mut glyph_positions_len = core::mem::MaybeUninit::uninit();
                                let glyph_positions = unsafe {
                                    hb::ffi::hb_buffer_get_glyph_positions(
                                        buf,
                                        glyph_positions_len.as_mut_ptr(),
                                    )
                                };
                                assert_eq!(unsafe { glyph_infos_len.assume_init() }, unsafe {
                                    glyph_positions_len.assume_init()
                                });
                                let glyph_count = unsafe { glyph_infos_len.assume_init() };
                                let glyph_positions = unsafe {
                                    core::slice::from_raw_parts(glyph_positions, glyph_count as _)
                                };
                                let glyph_infos = unsafe {
                                    core::slice::from_raw_parts(glyph_infos, glyph_count as _)
                                };

                                let buf_total_advances = glyph_positions
                                    .iter()
                                    .map(|p| p.x_advance as f32 / 64.0)
                                    .sum::<f32>();
                                section_visual_right = section_visual_right.max(
                                    section_left_offset
                                        + match (glyph_positions, glyph_infos) {
                                            (&[], &[]) => 0.0,
                                            (&[ref pos @ .., _], &[.., ref last_glyph]) => {
                                                unsafe {
                                                    ft::load_glyph(
                                                        face,
                                                        last_glyph.codepoint,
                                                        ft::LoadFlags::DEFAULT,
                                                    )
                                                    .expect("ft.load_glyph");
                                                }
                                                let metrics = unsafe { &(*(*face).glyph).metrics };
                                                pos.iter()
                                                    .map(|x| x.x_advance as f32 / 64.0)
                                                    .sum::<f32>()
                                                    + metrics.width as f32 / 64.0
                                            }
                                            _ => unreachable!(),
                                        },
                                );

                                #[cfg(feature = "harfbuzz")]
                                section_buffers.push(NativeTextRun {
                                    buffer: buf,
                                    byte_range,
                                    left_offset: line_left_offset + section_left_offset,
                                    font_id: x.font,
                                    face_index,
                                });
                                #[cfg(feature = "freetype")]
                                {
                                    // update metrics
                                    let face_metrics = unsafe { &(*(*face).size).metrics };

                                    baseline_y_offset =
                                        baseline_y_offset.max(face_metrics.ascender as f32 / 64.0);
                                    section_line_height =
                                        section_line_height.max(face_metrics.height as f32 / 64.0);

                                    // freetype2のdescenderは符号が逆になってるのでこれで正解
                                    section_height = section_height.max(
                                        (face_metrics.ascender - face_metrics.descender) as f32
                                            / 64.0,
                                    );

                                    section_left_offset += buf_total_advances;
                                }

                                // reset for next chunk
                                face_index = 0;
                            } else {
                                // no chars available for this font, fallback
                                face_index += 1;
                            }
                        }

                        if b.is_empty() {
                            // no chars emitted
                            section_line_height =
                                unsafe { (*(*font.faces[0]).size).metrics.height as f32 / 64.0 };
                        }

                        tracing::debug!(
                            line_right = line_left_offset + section_visual_right,
                            max_width,
                            content = b
                        );
                        if line_left_offset + section_visual_right > max_width {
                            // overflow: should line feed
                            lines.last_mut().expect("empty lines").baseline_y_offset +=
                                line_y_offset;
                            lines.last_mut().expect("empty_lines").height = final_line_height;
                            lines
                                .last_mut()
                                .expect("empty lines")
                                .width_with_trailing_whitespace = line_left_offset;

                            line_y_offset += core::mem::replace(&mut line_height, 0.0);
                            lines.push(HarfbuzzLineLayout {
                                buffers: Vec::new(),
                                width_with_trailing_whitespace: 0.0,
                                height: 0.0,
                                line_top_offset: line_y_offset,
                                baseline_y_offset: 0.0,
                            });

                            // move to line head
                            for x in section_buffers.iter_mut() {
                                x.left_offset -= line_left_offset;
                            }
                            final_line_height = 0.0;
                            line_left_offset = 0.0;
                        }

                        // TODO: そもそも1セクションも長すぎる場合に強制折り返しをさせる

                        #[cfg(feature = "harfbuzz")]
                        let last_line = lines.last_mut().expect("empty lines");
                        last_line.buffers.extend(section_buffers);
                        last_line.baseline_y_offset =
                            last_line.baseline_y_offset.max(baseline_y_offset);
                        line_left_offset += section_left_offset;
                        line_height = line_height.max(section_line_height);
                        final_line_height = final_line_height.max(section_height);
                    }

                    if n < line_count - 1 {
                        // newline
                        lines.last_mut().expect("empty lines").baseline_y_offset += line_y_offset;
                        lines.last_mut().expect("empty lines").height = final_line_height;
                        lines
                            .last_mut()
                            .expect("empty lines")
                            .width_with_trailing_whitespace = line_left_offset;

                        line_y_offset += line_height;
                        lines.push(HarfbuzzLineLayout {
                            buffers: Vec::new(),
                            width_with_trailing_whitespace: 0.0,
                            height: 0.0,
                            line_top_offset: line_y_offset,
                            baseline_y_offset: 0.0,
                        });

                        line_height = 0.0;
                        final_line_height = 0.0;
                        line_left_offset = 0.0;
                    }

                    left_offset = line_left_offset;
                }
            }

            #[cfg(feature = "harfbuzz")]
            {
                lines.last_mut().expect("empty lines").baseline_y_offset += line_y_offset;
                lines
                    .last_mut()
                    .expect("empty lines")
                    .width_with_trailing_whitespace = left_offset;
                lines.last_mut().expect("empty lines").height = final_line_height;
            }
        } else {
            // no max width(no autowrapping): optimal path
            for x in text_runs {
                left_offset += x.spacing_inline_start;

                #[cfg(feature = "freetype")]
                let font = font_set.select(x.font);
                #[cfg(feature = "harfbuzz")]
                let shaping_set = font_set.select_shaping(x.font);

                let mut face_index = 0;
                let mut shaped_bytes = 0usize;
                let mut any_shaped_on_line = false;
                while shaped_bytes < x.content.len() {
                    let starting_bytes = shaped_bytes;
                    let mut newline_break = false;
                    for c in x.content[starting_bytes..].chars() {
                        if c == '\n' {
                            // line break
                            newline_break = true;
                            break;
                        }

                        if unsafe { ft::get_char_index(font.faces[face_index], c as _) } == 0 {
                            // no char in font, needs fallback
                            break;
                        }

                        shaped_bytes += c.len_utf8();
                    }

                    if starting_bytes != shaped_bytes {
                        // needs shaping
                        any_shaped_on_line = true;
                        let face = font.faces[face_index];
                        let shaping_face = shaping_set.faces[face_index];
                        let byte_range = starting_bytes..shaped_bytes;

                        #[cfg(feature = "harfbuzz")]
                        let buf = unsafe { hb::ffi::hb_buffer_create() };
                        #[cfg(feature = "harfbuzz")]
                        unsafe {
                            hb::ffi::hb_buffer_add_utf8(
                                buf,
                                x.content.as_ptr().add(byte_range.start).cast(),
                                byte_range.len() as _,
                                0,
                                -1,
                            );
                            hb::ffi::hb_buffer_guess_segment_properties(buf);
                            hb::ffi::hb_shape(shaping_face.as_ptr(), buf, core::ptr::null(), 0);
                        }

                        let mut glyph_infos_len = core::mem::MaybeUninit::uninit();
                        let _glyph_infos = unsafe {
                            hb::ffi::hb_buffer_get_glyph_infos(buf, glyph_infos_len.as_mut_ptr())
                        };
                        let mut glyph_positions_len = core::mem::MaybeUninit::uninit();
                        let glyph_positions = unsafe {
                            hb::ffi::hb_buffer_get_glyph_positions(
                                buf,
                                glyph_positions_len.as_mut_ptr(),
                            )
                        };
                        assert_eq!(unsafe { glyph_infos_len.assume_init() }, unsafe {
                            glyph_positions_len.assume_init()
                        });
                        let glyph_count = unsafe { glyph_infos_len.assume_init() };
                        let buf_total_advances = unsafe {
                            core::slice::from_raw_parts(glyph_positions, glyph_count as _)
                        }
                        .iter()
                        .map(|p| p.x_advance as f32 / 64.0)
                        .sum::<f32>();

                        #[cfg(feature = "harfbuzz")]
                        let last_line = lines.last_mut().expect("empty lines");

                        #[cfg(feature = "harfbuzz")]
                        last_line.buffers.push(NativeTextRun {
                            buffer: buf,
                            byte_range,
                            left_offset,
                            font_id: x.font,
                            face_index,
                        });
                        #[cfg(feature = "freetype")]
                        {
                            // update metrics
                            let face_metrics = unsafe { &(*(*face).size).metrics };

                            last_line.baseline_y_offset = last_line
                                .baseline_y_offset
                                .max(face_metrics.ascender as f32 / 64.0);
                            line_height = line_height.max(face_metrics.height as f32 / 64.0);

                            // freetype2のdescenderは符号が逆になってるのでこれで正解
                            final_line_height = final_line_height.max(
                                (face_metrics.ascender - face_metrics.descender) as f32 / 64.0,
                            );

                            left_offset += buf_total_advances;
                        }

                        // reset for next chunk
                        face_index = 0;
                    } else {
                        if !newline_break {
                            // no chars available for this font, fallback
                            face_index += 1;
                        }
                    }

                    if newline_break {
                        // broke actual run by newline
                        let last_line = lines.last_mut().expect("empty lines");
                        if !any_shaped_on_line {
                            // no chars processed on the line
                            let face_metrics = unsafe { &(*(*font.faces[0]).size).metrics };

                            last_line.baseline_y_offset = face_metrics.ascender as f32 / 64.0;
                            line_height = face_metrics.height as f32 / 64.0;
                        }

                        last_line.baseline_y_offset += line_y_offset;
                        last_line.width_with_trailing_whitespace = left_offset;
                        last_line.height = final_line_height;

                        line_y_offset += line_height;
                        lines.push(HarfbuzzLineLayout {
                            buffers: Vec::new(),
                            width_with_trailing_whitespace: 0.0,
                            height: 0.0,
                            line_top_offset: line_y_offset,
                            baseline_y_offset: 0.0,
                        });

                        final_line_height = 0.0;
                        line_height = 0.0;
                        left_offset = 0.0;
                        any_shaped_on_line = false;
                        shaped_bytes += 1;
                    }
                }
            }

            #[cfg(feature = "harfbuzz")]
            {
                let last_line = lines.last_mut().expect("empty lines");

                last_line.baseline_y_offset += line_y_offset;
                last_line.width_with_trailing_whitespace = left_offset;
                last_line.height = final_line_height;
            }
        }

        // apply per-line alignment
        #[cfg(feature = "harfbuzz")]
        match alignment {
            CompositeRectTextHorizontalAlignment::Start => (),
            CompositeRectTextHorizontalAlignment::Middle => {
                let line_widths = lines
                    .iter()
                    .map(|x| x.visual_width(font_set))
                    .collect::<Vec<_>>();
                let max_width = line_widths.iter().copied().fold(0.0, f32::max);
                for (x, w) in lines.iter_mut().zip(line_widths.iter()) {
                    let offset = (max_width - w) * 0.5;
                    for x in x.buffers.iter_mut() {
                        x.left_offset += offset;
                    }
                }
            }
            CompositeRectTextHorizontalAlignment::End => {
                let line_widths = lines
                    .iter()
                    .map(|x| x.visual_width(font_set))
                    .collect::<Vec<_>>();
                let max_width = line_widths.iter().copied().fold(0.0, f32::max);
                for (x, w) in lines.iter_mut().zip(line_widths.iter()) {
                    let offset = max_width - w;
                    for x in x.buffers.iter_mut() {
                        x.left_offset += offset;
                    }
                }
            }
        }

        Self {
            #[cfg(feature = "harfbuzz")]
            lines,
            #[cfg(feature = "harfbuzz")]
            height: line_y_offset + final_line_height,
            #[cfg(target_os = "macos")]
            internal: darwin_coretext::CoreTextLayout::new(
                text_runs, font_set, alignment, max_width,
            ),
            #[cfg(windows)]
            internal: dwrite::TextLayout::new(text_runs, font_set, alignment, max_width),
        }
    }

    pub fn rasterize_and_place_glyphs(
        &self,
        #[allow(unused_variables)] font_set: &FontSet,
        vector_rasterization_state: &mut VectorRasterizationState,
        atlas: &mut MaskTextureAtlasManager,
        render_scale: f32,
    ) -> Vec<GlyphPlacementBox> {
        crate::perf_scope!(RASTERIZE_AND_PLACE_GLYPHS);

        #[cfg(not(any(target_os = "macos", windows)))]
        let mut boxes = Vec::new();

        #[cfg(feature = "harfbuzz")]
        for l in self.lines.iter() {
            for x in l.buffers.iter() {
                let font = font_set.select(x.font_id).faces[x.face_index];

                let mut glyph_infos_len = core::mem::MaybeUninit::uninit();
                let glyph_infos = unsafe {
                    hb::ffi::hb_buffer_get_glyph_infos(x.buffer, glyph_infos_len.as_mut_ptr())
                };
                let mut glyph_positions_len = core::mem::MaybeUninit::uninit();
                let glyph_positions = unsafe {
                    hb::ffi::hb_buffer_get_glyph_positions(
                        x.buffer,
                        glyph_positions_len.as_mut_ptr(),
                    )
                };
                let glyph_infos = unsafe {
                    core::slice::from_raw_parts(glyph_infos, glyph_infos_len.assume_init() as _)
                };
                let glyph_positions = unsafe {
                    core::slice::from_raw_parts(
                        glyph_positions,
                        glyph_positions_len.assume_init() as _,
                    )
                };
                assert_eq!(glyph_infos.len(), glyph_positions.len());

                let mut left_cursor = x.left_offset;
                for (glyph_info, glyph_position) in
                    glyph_infos.into_iter().zip(glyph_positions.into_iter())
                {
                    unsafe {
                        ft::load_glyph(font, glyph_info.codepoint, ft::LoadFlags::DEFAULT)
                            .expect("face.load_glyph")
                    };
                    let metrics = unsafe { &(*(*font).glyph).metrics };
                    let glyph_width = metrics.width as f32 / 64.0 * render_scale;
                    let glyph_height = metrics.height as f32 / 64.0 * render_scale;

                    let (r, is_new) = atlas.acquire_for_glyph(
                        (font as _, glyph_info.codepoint as _),
                        glyph_width.ceil() as _,
                        glyph_height.ceil() as _,
                    );
                    boxes.push(GlyphPlacementBox {
                        left: (left_cursor
                            + (glyph_position.x_offset as f32 + metrics.horiBearingX as f32)
                                / 64.0)
                            * render_scale,
                        top: (l.baseline_y_offset - metrics.horiBearingY as f32 / 64.0)
                            * render_scale,
                        tex_left: r.left,
                        tex_top: r.top,
                        width: r.width(),
                        height: r.height(),
                    });

                    if is_new {
                        vector_rasterization_state.updated_rects.push(r.vk_rect());

                        struct OutlineReceiver<'r> {
                            vrender: VectorVertexRenderer<'r>,
                            render_scale: f32,
                            offset_x: f32,
                            offset_y: f32,
                        }
                        impl OutlineReceiver<'_> {
                            #[inline(always)]
                            const fn make_point(&self, v: &ft::Vector) -> Point<VectorTextureUnit> {
                                Point::new_vector_texture(
                                    v.x as f32 / 64.0 * self.render_scale + self.offset_x,
                                    v.y as f32 / 64.0 * self.render_scale + self.offset_y,
                                )
                            }
                        }
                        impl ft::OutlineFuncs for OutlineReceiver<'_> {
                            #[inline(always)]
                            fn move_to(&mut self, to: &ft::Vector) {
                                self.vrender.move_to(self.make_point(to));
                            }

                            #[inline(always)]
                            fn line_to(&mut self, to: &ft::Vector) {
                                self.vrender.line_to(self.make_point(to));
                            }

                            #[inline(always)]
                            fn conic_to(&mut self, control: &ft::Vector, to: &ft::Vector) {
                                self.vrender
                                    .quadratic_to(self.make_point(control), self.make_point(to));
                            }

                            #[inline(always)]
                            fn cubic_to(
                                &mut self,
                                control1: &ft::Vector,
                                control2: &ft::Vector,
                                to: &ft::Vector,
                            ) {
                                self.vrender.cubic_to(
                                    self.make_point(control1),
                                    self.make_point(control2),
                                    self.make_point(to),
                                );
                            }
                        }

                        unsafe {
                            ft::outline_decompose(
                                &mut (*(*font).glyph).outline,
                                &mut OutlineReceiver {
                                    vrender: VectorVertexRenderer::new(vector_rasterization_state),
                                    render_scale,
                                    offset_x: r.left as f32
                                        - metrics.horiBearingX as f32 / 64.0 * render_scale,
                                    offset_y: -(r.top as f32)
                                        - metrics.horiBearingY as f32 / 64.0 * render_scale,
                                },
                                0,
                                0,
                            )
                            .expect("glyph.outline.decompose");
                        }
                    }

                    left_cursor += glyph_position.x_advance as f32 / 64.0;
                }
            }
        }

        #[cfg(not(any(target_os = "macos", windows)))]
        return boxes;

        #[cfg(target_os = "macos")]
        return self.internal.rasterize_and_place_glyphs(
            vector_rasterization_state,
            atlas,
            render_scale,
        );

        #[cfg(windows)]
        return self.internal.rasterize_and_place_glyphs(
            vector_rasterization_state,
            atlas,
            render_scale,
        );
    }

    pub fn size(&self) -> Size<LogicalUnit> {
        #[cfg(feature = "harfbuzz")]
        return Size::new_logical(
            self.lines
                .iter()
                .map(|x| x.width_with_trailing_whitespace)
                .fold(0.0, f32::max),
            self.height,
        );

        #[cfg(windows)]
        return self.internal.size();

        #[cfg(target_os = "macos")]
        return self.internal.size();
    }

    pub fn visual_width(&self, #[allow(unused_variables)] font_set: &FontSet) -> f32 {
        #[cfg(windows)]
        return self.size().width;

        #[cfg(feature = "harfbuzz")]
        return self
            .lines
            .iter()
            .map(|l| l.visual_width(font_set))
            .fold(0.0, f32::max);

        #[cfg(target_os = "macos")]
        return self.internal.measure_visual_width();
    }

    pub fn height(&self) -> f32 {
        #[cfg(feature = "harfbuzz")]
        return self.height;

        #[cfg(windows)]
        return self.internal.size().height;

        #[cfg(target_os = "macos")]
        return self.internal.height();
    }

    #[tracing::instrument(skip(font_set))]
    pub fn measure_height(text: &str, font: FontID, font_set: &FontSet) -> f32 {
        #[cfg(windows)]
        return dwrite::TextLayout::measure_single_height(text, font, font_set);

        #[cfg(not(windows))]
        // TODO: 最適化はあとで
        Self::new(
            core::iter::once(TextRun {
                content: text,
                font,
                spacing_inline_start: 0.0,
            }),
            font_set,
            CompositeRectTextHorizontalAlignment::Start,
            None,
        )
        .height()
    }

    #[tracing::instrument(skip(font_set))]
    pub fn measure_cursor_rect(text: &str, font: FontID, font_set: &FontSet) -> Rect<LogicalUnit> {
        // TODO: 最適化はあとで
        let layout = Self::new_single(
            text,
            font,
            font_set,
            CompositeRectTextHorizontalAlignment::Start,
            None,
        );

        #[cfg(feature = "harfbuzz")]
        let Some(last_line) = layout.lines.last() else {
            #[cfg(feature = "freetype")]
            let face_line_height = unsafe {
                let metrics = &(*(*font_set.select(font).faces[0]).size).metrics;

                (metrics.ascender + metrics.descender) as f32 / 64.0
            };

            return Rect::from_lt_size(
                Point::new_logical(0.0, 0.0),
                Size::new_logical(0.0, face_line_height),
            );
        };

        #[cfg(feature = "harfbuzz")]
        let last_line_height = if last_line.height == 0.0 {
            #[cfg(feature = "freetype")]
            unsafe {
                let metrics = &(*(*font_set.select(font).faces[0]).size).metrics;

                (metrics.ascender + metrics.descender) as f32 / 64.0
            }
        } else {
            last_line.height
        };

        #[cfg(feature = "harfbuzz")]
        return Rect::from_lt_size(
            Point::new_logical(
                last_line.width_with_trailing_whitespace,
                last_line.line_top_offset,
            ),
            Size::new_logical(0.0, last_line_height),
        );

        #[cfg(windows)]
        return layout.internal.measure_cursor_rect_at_end();

        #[cfg(target_os = "macos")]
        {
            // からの行が生成されないようにnbspをくっつけておく
            let mut text = text.to_owned();
            text += "\u{a0}";
            return Self::new_single(
                &text,
                font,
                font_set,
                CompositeRectTextHorizontalAlignment::Start,
                None,
            )
            .internal
            .measure_cursor_rect_at_end(text.chars().count() - 1);
        }
    }

    #[tracing::instrument(skip(font_set))]
    pub fn measure_line_rects(
        text: &str,
        range: core::ops::Range<usize>,
        font: FontID,
        font_set: &FontSet,
    ) -> Vec<Rect<LogicalUnit>> {
        // TODO: 最適化はあとで
        let layout = Self::new_single(
            text,
            font,
            font_set,
            CompositeRectTextHorizontalAlignment::Start,
            None,
        );

        // TODO: RTLサポート
        #[cfg(feature = "harfbuzz")]
        let mut rects = Vec::new();
        #[cfg(feature = "harfbuzz")]
        for l in layout.lines.iter() {
            let mut line_min_x = f32::MAX;
            let mut line_max_x = 0.0f32;

            for tr in l.buffers.iter() {
                let overlapping_range =
                    tr.byte_range.start.max(range.start)..tr.byte_range.end.min(range.end);
                if overlapping_range.is_empty() {
                    // not overlapping
                    continue;
                }

                let mut glyph_infos_len = core::mem::MaybeUninit::uninit();
                let glyph_infos = unsafe {
                    hb::ffi::hb_buffer_get_glyph_infos(tr.buffer, glyph_infos_len.as_mut_ptr())
                };
                let mut glyph_positions_len = core::mem::MaybeUninit::uninit();
                let glyph_positions = unsafe {
                    hb::ffi::hb_buffer_get_glyph_positions(
                        tr.buffer,
                        glyph_positions_len.as_mut_ptr(),
                    )
                };
                let glyph_infos = unsafe {
                    core::slice::from_raw_parts(glyph_infos, glyph_infos_len.assume_init() as _)
                };
                let glyph_positions = unsafe {
                    core::slice::from_raw_parts(
                        glyph_positions,
                        glyph_positions_len.assume_init() as _,
                    )
                };
                assert_eq!(glyph_infos.len(), glyph_positions.len());

                let mut bytes = tr.byte_range.start;
                let mut left_cursor = tr.left_offset;
                for (glyph_position, glyph_info) in glyph_positions.iter().zip(glyph_infos.iter()) {
                    if range.contains(&bytes) {
                        line_min_x = line_min_x.min(left_cursor);
                        line_max_x =
                            line_max_x.max(left_cursor + glyph_position.x_advance as f32 / 64.0);
                    }

                    left_cursor += glyph_position.x_advance as f32 / 64.0;
                    bytes += text[bytes..]
                        .chars()
                        .next()
                        .expect("out of range")
                        .len_utf8();
                }
            }

            if line_min_x < line_max_x {
                rects.push(Rect::from_lt_size(
                    Point::new_logical(line_min_x, l.line_top_offset),
                    Size::new_logical(line_max_x - line_min_x, l.height),
                ));
            }
        }

        #[cfg(feature = "harfbuzz")]
        return rects;

        #[cfg(windows)]
        return layout.internal.measure_line_rects(
            (text[..range.start].encode_utf16().count() as u32
                ..text[..range.end].encode_utf16().count() as u32)
                .into(),
        );

        #[cfg(target_os = "macos")]
        return layout.internal.compute_line_rects_for_range(
            (text[..range.start].chars().count()..text[..range.end].chars().count()).into(),
        );
    }

    #[tracing::instrument(skip(font_set))]
    pub fn measure_visual_width(text: &str, font: FontID, font_set: &FontSet) -> f32 {
        // TODO: 最適化はあとで
        return Self::new(
            core::iter::once(TextRun {
                content: text,
                font,
                spacing_inline_start: 0.0,
            }),
            font_set,
            CompositeRectTextHorizontalAlignment::Start,
            None,
        )
        .visual_width(font_set);
    }

    #[tracing::instrument(skip(font_set))]
    pub fn measure_total_advances(text: &str, font: FontID, font_set: &FontSet) -> f32 {
        // TODO: 最適化はあとで
        let layout = Self::new(
            core::iter::once(TextRun {
                content: text,
                font,
                spacing_inline_start: 0.0,
            }),
            font_set,
            CompositeRectTextHorizontalAlignment::Start,
            None,
        );

        #[cfg(feature = "harfbuzz")]
        return layout
            .lines
            .iter()
            .map(|l| l.width_with_trailing_whitespace)
            .fold(0.0, f32::max);

        #[cfg(target_os = "macos")]
        return layout.internal.measure_total_advances();

        #[cfg(windows)]
        return layout.internal.width_with_trailing_whitespace();
    }

    #[tracing::instrument(skip(font_set))]
    pub fn find_nearest_bytes(
        x: f32,
        y: f32,
        text: &str,
        font: FontID,
        font_set: &FontSet,
    ) -> usize {
        if text.is_empty() {
            // empty content
            return 0;
        }

        // TODO: 最適化はあとで
        let layout = Self::new_single(
            text,
            font,
            font_set,
            CompositeRectTextHorizontalAlignment::Start,
            None,
        );

        // TODO: RTLサポート
        #[cfg(feature = "harfbuzz")]
        let mut bytes = 0;
        #[cfg(feature = "harfbuzz")]
        for (n, l) in layout.lines.iter().enumerate() {
            if n == 0 {
                // first line check
                if let Some(next_line) = layout.lines.get(n + 1)
                    && next_line.line_top_offset < y
                {
                    // never across with this line
                    bytes = l
                        .buffers
                        .iter()
                        .map(|x| x.byte_range.end)
                        .max()
                        .unwrap_or(bytes)
                        + 1;
                    continue;
                }
            } else if n == layout.lines.len() - 1 {
                // last line check
                if y < l.line_top_offset {
                    // never across with this line
                    bytes = l
                        .buffers
                        .iter()
                        .map(|x| x.byte_range.end)
                        .max()
                        .unwrap_or(bytes)
                        + 1;
                    continue;
                }
            } else {
                let next_line_top = layout
                    .lines
                    .get(n + 1)
                    .map_or(l.line_top_offset + l.height, |l| l.line_top_offset);
                if y < l.line_top_offset || next_line_top < y {
                    // never across with this line
                    bytes = l
                        .buffers
                        .iter()
                        .map(|x| x.byte_range.end)
                        .max()
                        .unwrap_or(bytes)
                        + 1;
                    continue;
                }
            }

            for tr in l.buffers.iter() {
                let mut glyph_positions_len = core::mem::MaybeUninit::uninit();
                let glyph_positions = unsafe {
                    hb::ffi::hb_buffer_get_glyph_positions(
                        tr.buffer,
                        glyph_positions_len.as_mut_ptr(),
                    )
                };
                let mut left_cursor = tr.left_offset;
                bytes = tr.byte_range.start;
                for n in 0..unsafe { glyph_positions_len.assume_init() } {
                    let glyph_position = unsafe { &*glyph_positions.add(n as usize) };

                    let left = left_cursor;
                    let right = left + glyph_position.x_advance as f32 / 64.0;
                    let mid = (left + right) / 2.0;

                    if x < left {
                        // overshoot
                        return bytes;
                    }

                    if x <= mid {
                        // left
                        return bytes;
                    }

                    if x <= right {
                        // right
                        let mut next_boundary_bytes = bytes.saturating_add(1);
                        while next_boundary_bytes < text.len()
                            && !text.is_char_boundary(next_boundary_bytes)
                        {
                            next_boundary_bytes += 1;
                        }

                        return next_boundary_bytes;
                    }

                    left_cursor += glyph_position.x_advance as f32 / 64.0;
                    bytes += text[bytes..]
                        .chars()
                        .next()
                        .expect("out of range")
                        .len_utf8();
                }
            }

            // beyond
            return bytes;
        }

        #[cfg(feature = "harfbuzz")]
        unreachable!();

        #[cfg(windows)]
        return text
            .chars()
            .take(layout.internal.find_nearest_text_pos(x, y) as _)
            .map(char::len_utf8)
            .sum::<usize>();

        #[cfg(target_os = "macos")]
        {
            let Some((c, line_prefix_offset)) = layout
                .internal
                .find_nearest_string_index_with_line_offset(x, y)
            else {
                // no hits on the layout
                return text.len();
            };

            // hit on the line
            let line_prefix_bytes = text
                .chars()
                .take(line_prefix_offset)
                .map(char::len_utf8)
                .sum::<usize>();
            // CoreTextのCTLineは末尾の改行文字を自身に含むらしいのでそれを除いたバイト数を返す
            let mut total_byte_count = 0;
            let mut trailing_control_byte_count = 0;
            for c in text[line_prefix_bytes..]
                .chars()
                .take((c - line_prefix_offset) as _)
            {
                trailing_control_byte_count = if c.is_control() {
                    trailing_control_byte_count + c.len_utf8()
                } else {
                    0
                };
                total_byte_count += c.len_utf8();
            }

            return line_prefix_bytes + total_byte_count - trailing_control_byte_count;
        }
    }
}

#[derive(Debug)]
pub struct GlyphPlacementBox {
    pub left: f32,
    pub top: f32,
    pub tex_left: u32,
    pub tex_top: u32,
    pub width: u32,
    pub height: u32,
}
impl GlyphPlacementBox {
    #[inline(always)]
    pub const fn right(&self) -> f32 {
        self.left + self.width as f32
    }

    #[inline(always)]
    pub const fn bottom(&self) -> f32 {
        self.top + self.height as f32
    }
}
