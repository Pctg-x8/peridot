#[cfg(feature = "fontconfig")]
use peridot_tp_fontconfig as fc;
#[cfg(feature = "freetype")]
use peridot_tp_freetype as ft;
#[cfg(feature = "harfbuzz")]
use peridot_tp_harfbuzz as hb;

use crate::{
    rendering::{
        MaskTextureAtlasManager, composite::CompositeRectTextHorizontalAlignment,
        vg::VectorRasterizationState,
    },
    utils::{LogicalUnit, Rect, Size},
};

#[cfg(target_os = "macos")]
mod darwin_coretext;
#[cfg(windows)]
mod dwrite;
#[cfg(all(feature = "freetype", feature = "harfbuzz"))]
mod ft_hb;

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

profiler::section!(CREATE_TEXT_LAYOUT = "TextLayout.New");
profiler::section!(RASTERIZE_AND_PLACE_GLYPHS = "TextLayout.RasterizeAndPlaceGlyphs");

pub struct TextLayout {
    #[cfg(feature = "harfbuzz")]
    internal: ft_hb::TextLayout,
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
        max_lines: Option<usize>,
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
            max_lines,
        )
    }

    pub fn new<'s>(
        text_runs: impl Iterator<Item = TextRun<'s>>,
        font_set: &FontSet,
        alignment: CompositeRectTextHorizontalAlignment,
        max_width: Option<f32>,
        max_lines: Option<usize>,
    ) -> Self {
        profiler::scope!(CREATE_TEXT_LAYOUT);

        Self {
            #[cfg(all(feature = "freetype", feature = "harfbuzz"))]
            internal: ft_hb::TextLayout::new(text_runs, font_set, alignment, max_width),
            #[cfg(target_os = "macos")]
            internal: darwin_coretext::CoreTextLayout::new(
                text_runs, font_set, alignment, max_width,
            ),
            #[cfg(windows)]
            internal: dwrite::TextLayout::new(text_runs, font_set, alignment, max_width, max_lines),
        }
    }

    pub fn rasterize_and_place_glyphs(
        &self,
        #[allow(unused_variables)] font_set: &FontSet,
        vector_rasterization_state: &mut VectorRasterizationState,
        atlas: &mut MaskTextureAtlasManager,
        render_scale: f32,
    ) -> Vec<GlyphPlacementBox> {
        profiler::scope!(RASTERIZE_AND_PLACE_GLYPHS);

        #[cfg(all(feature = "freetype", feature = "harfbuzz"))]
        return self.internal.rasterize_and_place_glyphs(
            font_set,
            vector_rasterization_state,
            atlas,
            render_scale,
        );

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
        #[cfg(all(feature = "freetype", feature = "harfbuzz"))]
        return Size::new_logical(
            self.internal.measure_width_with_trailing_whitespace(),
            self.height(),
        );

        #[cfg(windows)]
        return self.internal.size();

        #[cfg(target_os = "macos")]
        return self.internal.size();
    }

    pub fn visual_width(&self, #[allow(unused_variables)] font_set: &FontSet) -> f32 {
        #[cfg(windows)]
        return self.size().width;

        #[cfg(all(feature = "freetype", feature = "harfbuzz"))]
        return self.internal.measure_visual_width(font_set);

        #[cfg(target_os = "macos")]
        return self.internal.measure_visual_width();
    }

    pub fn height(&self) -> f32 {
        #[cfg(all(feature = "freetype", feature = "harfbuzz"))]
        return self.internal.height();

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
            None,
        );

        #[cfg(all(feature = "freetype", feature = "harfbuzz"))]
        return layout.internal.measure_cursor_rect_at_end(font_set);

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
            None,
        );

        #[cfg(all(feature = "freetype", feature = "harfbuzz"))]
        return layout.internal.measure_line_rects(range.into());

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
            None,
        );

        #[cfg(all(feature = "freetype", feature = "harfbuzz"))]
        return layout.internal.measure_width_with_trailing_whitespace();

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
            None,
        );

        #[cfg(all(feature = "freetype", feature = "harfbuzz"))]
        return layout.internal.find_nearest_byte_pos(x, y);

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
