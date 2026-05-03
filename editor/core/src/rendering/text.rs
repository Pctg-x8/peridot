use std::{cell::UnsafeCell, collections::HashMap};

#[cfg(feature = "fontconfig")]
use peridot_tp_fontconfig as fc;
#[cfg(feature = "freetype")]
use peridot_tp_freetype as ft;
#[cfg(feature = "harfbuzz")]
use peridot_tp_harfbuzz as hb;
#[cfg(windows)]
use windows::Win32::Graphics::{Direct2D::Common::*, DirectWrite::*};
#[cfg(windows)]
use windows_core::*;

use crate::rendering::{
    MaskTextureAtlasManager,
    vg::{VectorRasterizationState, VectorVertexRenderer},
};

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

pub struct ThreadLocalTypingContext {
    #[cfg(feature = "freetype")]
    pub ft_lib: FreeType,
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
    pub fn new(
        root_set: &'d RootFontSet,
        #[allow(unused_variables)] ctx: &ThreadLocalTypingContext,
    ) -> Self {
        #[cfg(feature = "freetype")]
        let ui_default = root_set
            .ui_common_font_data
            .iter()
            .map(|&(f, ix)| unsafe {
                ft::new_memory_face(ctx.ft_lib.0, &root_set.font_binaries[f], ix as _)
                    .expect("FreeType.new_face.ui_default")
            })
            .collect::<Vec<_>>();
        #[cfg(feature = "freetype")]
        let ui_title_project_name = root_set
            .ui_common_font_data
            .iter()
            .map(|&(f, ix)| unsafe {
                ft::new_memory_face(ctx.ft_lib.0, &root_set.font_binaries[f], ix as _)
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

    pub const unsafe fn lifetime_unbound(self) -> PerWindowFontSet<'static> {
        unsafe { core::mem::transmute(self) }
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
    font_binaries: Vec<Vec<ft::raw::FT_Byte>>,
    #[cfg(feature = "freetype")]
    ui_common_font_data: Vec<(usize, core::ffi::c_int)>,
    #[cfg(windows)]
    dw_factory: windows::Win32::Graphics::DirectWrite::IDWriteFactory,
    #[cfg(windows)]
    ui_default: windows::Win32::Graphics::DirectWrite::IDWriteTextFormat,
    #[cfg(windows)]
    ui_title_project_name: windows::Win32::Graphics::DirectWrite::IDWriteTextFormat,
}
#[cfg(target_os = "macos")]
unsafe impl Sync for RootFontSet {}
#[cfg(target_os = "macos")]
unsafe impl Send for RootFontSet {}
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
        let mut font_binaries = Vec::new();
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
                        font_binaries.push(
                            std::fs::read(x.key().to_str().expect("cstr.to_str"))
                                .expect("font.readfile"),
                        );
                        *x.insert(font_binaries.len() - 1)
                    }
                };

                if selected_fonts.insert((font_binary_index, index)) {
                    // 未知のフォント
                    fonts_ordered.push((font_binary_index, index));
                }
            }

            fonts_ordered
        };

        tracing::info!(
            total_data_bytes = %crate::utils::ByteLengthFormatter(
                font_binaries
                    .iter()
                    .map(|b| b.len())
                    .sum::<usize>()
            ),
            "RootFontSet stats"
        );

        Self {
            font_binaries,
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

pub struct TextRun<'s> {
    pub content: &'s str,
    pub font: FontID,
    pub spacing_inline_start: f32,
}

struct FontUniquifyStorage {
    key_to_id: UnsafeCell<HashMap<String, usize>>,
    last_id: UnsafeCell<usize>,
}
impl FontUniquifyStorage {
    fn new() -> Self {
        Self {
            key_to_id: UnsafeCell::new(HashMap::new()),
            last_id: UnsafeCell::new(0),
        }
    }

    fn query(&self, key: String) -> usize {
        unsafe {
            *(*self.key_to_id.get()).entry(key).or_insert_with(|| {
                let r = *self.last_id.get();
                *self.last_id.get() += 1;
                r
            })
        }
    }
}

thread_local! {
    static FONT_UNIQUIFY_STORAGE: FontUniquifyStorage = FontUniquifyStorage::new();
}

pub struct TextLayout {
    #[cfg(feature = "harfbuzz")]
    buffers: Vec<(*mut hb::ffi::hb_buffer_t, f32, FontID, usize)>,
    #[cfg(feature = "harfbuzz")]
    height: f32,
    #[cfg(feature = "harfbuzz")]
    baseline_y_offset: f32,
    #[cfg(target_os = "macos")]
    frame: apple_sdk_port::Owned<apple_sdk_port::text::Frame>,
    #[cfg(windows)]
    layout: windows::Win32::Graphics::DirectWrite::IDWriteTextLayout,
    render_scale: f32,
}
impl Drop for TextLayout {
    fn drop(&mut self) {
        #[cfg(feature = "harfbuzz")]
        for (buf, _, _, _) in self.buffers.drain(..) {
            unsafe {
                hb::ffi::hb_buffer_destroy(buf);
            }
        }
    }
}
impl TextLayout {
    pub fn new<'s>(
        text_runs: impl Iterator<Item = TextRun<'s>>,
        font_set: &PerWindowFontSet,
        render_scale: f32,
    ) -> Self {
        let (lb, ub) = text_runs.size_hint();
        #[cfg(feature = "harfbuzz")]
        let mut buffers = Vec::with_capacity(ub.unwrap_or(lb));
        #[cfg(feature = "freetype")]
        let mut baseline_y_offset = 0.0f32;
        #[cfg(feature = "freetype")]
        let mut left_offset = 0.0f32;
        #[cfg(feature = "freetype")]
        let mut height = 0.0f32;
        #[cfg(feature = "freetype")]
        for x in text_runs {
            left_offset += x.spacing_inline_start * render_scale;

            #[cfg(feature = "freetype")]
            let font = font_set.select(x.font);
            #[cfg(feature = "harfbuzz")]
            let shaping_set = font_set.select_shaping(x.font);

            let mut font_index = 0;
            let mut shaped_bytes = 0usize;
            while shaped_bytes < x.content.len() {
                let starting_bytes = shaped_bytes;
                for c in x.content[starting_bytes..].chars() {
                    if unsafe { ft::get_char_index(font.faces[font_index], c as _) } == 0 {
                        // no char in font, needs fallback
                        break;
                    }

                    shaped_bytes += c.len_utf8();
                }

                if starting_bytes == shaped_bytes {
                    // no chars available for this font, fallback
                    font_index += 1;
                    continue;
                }

                #[cfg(feature = "harfbuzz")]
                let buf = unsafe { hb::ffi::hb_buffer_create() };
                #[cfg(feature = "harfbuzz")]
                unsafe {
                    hb::ffi::hb_buffer_add_utf8(
                        buf,
                        x.content.as_ptr().add(starting_bytes).cast(),
                        (shaped_bytes - starting_bytes) as _,
                        0,
                        -1,
                    );
                    hb::ffi::hb_buffer_guess_segment_properties(buf);
                    hb::ffi::hb_shape(
                        shaping_set.faces[font_index].as_ptr(),
                        buf,
                        core::ptr::null(),
                        0,
                    );
                }

                let mut glyph_infos_len = core::mem::MaybeUninit::uninit();
                let _glyph_infos = unsafe {
                    hb::ffi::hb_buffer_get_glyph_infos(buf, glyph_infos_len.as_mut_ptr())
                };
                let mut glyph_positions_len = core::mem::MaybeUninit::uninit();
                let glyph_positions = unsafe {
                    hb::ffi::hb_buffer_get_glyph_positions(buf, glyph_positions_len.as_mut_ptr())
                };
                assert_eq!(unsafe { glyph_infos_len.assume_init() }, unsafe {
                    glyph_positions_len.assume_init()
                });
                let glyph_count = unsafe { glyph_infos_len.assume_init() };
                let buf_width =
                    unsafe { core::slice::from_raw_parts(glyph_positions, glyph_count as _) }
                        .iter()
                        .map(|p| p.x_advance as f32 / 64.0)
                        .sum::<f32>();

                #[cfg(feature = "harfbuzz")]
                buffers.push((buf, left_offset, x.font, font_index));
                #[cfg(feature = "freetype")]
                {
                    // update metrics

                    baseline_y_offset = baseline_y_offset.max(unsafe {
                        (*(*font.faces[font_index]).size).metrics.ascender as f32 / 64.0
                    });

                    // freetype2のdescenderは符号が逆になってるのでこれで正解
                    // TODO: 複数行になる場合はleadingを行間に足す
                    height = height.max(unsafe {
                        ((*(*font.faces[font_index]).size).metrics.ascender
                            - (*(*font.faces[font_index]).size).metrics.descender)
                            as f32
                            / 64.0
                    });

                    left_offset += buf_width;
                }

                // reset for next chunk
                font_index = 0;
            }
        }

        #[cfg(target_os = "macos")]
        let mut attributed_string_runs = Vec::with_capacity(ub.unwrap_or(lb));
        #[cfg(target_os = "macos")]
        let mut total_bytes = 0;
        #[cfg(target_os = "macos")]
        for r in text_runs {
            use apple_sdk_port::Object;

            let font = font_set.select(r.font);
            let range = apple_sdk_port::foundation::Range {
                location: total_bytes as _,
                length: 0, // replace_attributed_stringでAppendする場合はここを0にする必要があるらしい
            };

            let mut str_attr = apple_sdk_port::foundation::MutableDictionary::<
                apple_sdk_port::foundation::String,
                apple_sdk_port::AnyObject,
            >::new_copying_key_generic_value(None, 3)
            .expect("str_attr.create");
            str_attr.set(
                apple_sdk_port::foundation::AttributedStringKey::font(),
                font.as_any(),
            );
            str_attr.set(
                crate::platform::mac::ak_spacing_inline_start(),
                apple_sdk_port::foundation::Number::new_f32(None, r.spacing_inline_start)
                    .expect("Number.create")
                    .as_any(),
            );
            str_attr.set(
                crate::platform::mac::ak_font_id(),
                apple_sdk_port::foundation::Number::new_i64(None, r.font as usize as _)
                    .expect("Number.create")
                    .as_any(),
            );

            attributed_string_runs.push((
                apple_sdk_port::foundation::AttributedString::new(
                    None,
                    &*unsafe {
                        apple_sdk_port::foundation::String::from_str_no_copy(None, r.content)
                    },
                    Some(&str_attr),
                )
                .expect("str.crate"),
                range,
            ));
            total_bytes += r.content.len();
        }
        #[cfg(target_os = "macos")]
        let mut str =
            apple_sdk_port::foundation::MutableAttributedString::new(None, total_bytes as _)
                .expect("str.create");
        #[cfg(target_os = "macos")]
        str.begin_editing();
        #[cfg(target_os = "macos")]
        for (s, r) in attributed_string_runs {
            str.replace_attributed_string(r, &s);
        }
        #[cfg(target_os = "macos")]
        str.end_editing();
        #[cfg(target_os = "macos")]
        let framesetter = apple_sdk_port::text::Framesetter::from_attributed_string(&str)
            .expect("Framesetter.create");
        #[cfg(target_os = "macos")]
        let frame = framesetter
            .create_frame(
                apple_sdk_port::foundation::Range {
                    location: 0,
                    length: 0,
                },
                &apple_sdk_port::graphics::Path::new_rect(
                    apple_sdk_port::raw::CGRect {
                        origin: apple_sdk_port::raw::CGPoint { x: 0.0, y: 0.0 },
                        size: apple_sdk_port::raw::CGSize {
                            width: f64::MAX,
                            height: f64::MAX,
                        },
                    },
                    None,
                ),
                None,
            )
            .expect("Frame.create");

        #[cfg(windows)]
        let mut run_str_utf16s = Vec::new();
        #[cfg(windows)]
        let mut run_parts = Vec::with_capacity(ub.unwrap_or(lb));
        #[cfg(windows)]
        for r in text_runs {
            let start_pos = run_str_utf16s.len();
            run_str_utf16s.extend(r.content.encode_utf16());
            run_parts.push((
                r.font,
                r.spacing_inline_start,
                windows::Win32::Graphics::DirectWrite::DWRITE_TEXT_RANGE {
                    startPosition: start_pos as _,
                    length: (run_str_utf16s.len() - start_pos) as _,
                },
            ));
        }
        #[cfg(windows)]
        let layout = unsafe {
            font_set
                .native_factory()
                .CreateTextLayout(
                    &run_str_utf16s,
                    font_set.select(FontID::UIDefault),
                    f32::MAX,
                    f32::MAX,
                )
                .expect("dwrite.layout.create")
        };
        #[cfg(windows)]
        let mut inline_spacing_sum = 0.0;
        #[cfg(windows)]
        for (font_id, spacing_inline_start, range) in run_parts.into_iter() {
            let font = font_set.select(font_id);
            inline_spacing_sum += spacing_inline_start;

            unsafe {
                let mut family_name =
                    Vec::with_capacity(font.GetFontFamilyNameLength() as usize + 1);
                font.GetFontFamilyName(core::mem::transmute::<
                    &mut [core::mem::MaybeUninit<_>],
                    &mut [_],
                >(family_name.spare_capacity_mut()))
                    .expect("dwrite.format.get_font_family_name");
                family_name.set_len(family_name.capacity());

                layout
                    .SetFontFamilyName(windows::core::PCWSTR(family_name.as_ptr()), range)
                    .expect("dwrite.layout.set_font_family_name");
                layout
                    .SetFontSize(font.GetFontSize(), range)
                    .expect("dwrite.layout.set_font_size");
                layout
                    .SetFontStretch(font.GetFontStretch(), range)
                    .expect("dwrite.layout.set_font_stretch");
                layout
                    .SetFontStyle(font.GetFontStyle(), range)
                    .expect("dwrite.layout.set_font_style");
                layout
                    .SetFontWeight(font.GetFontWeight(), range)
                    .expect("dwrite.layout.set_font_weight");
                layout
                    .SetDrawingEffect(
                        &IUnknown::from(DrawingEffect {
                            font_id,
                            offset_x: inline_spacing_sum,
                        }),
                        range,
                    )
                    .expect("dwrite.layout.set_drawing_effect");
            }
        }

        Self {
            #[cfg(feature = "harfbuzz")]
            buffers,
            #[cfg(feature = "harfbuzz")]
            height,
            #[cfg(feature = "harfbuzz")]
            baseline_y_offset,
            #[cfg(target_os = "macos")]
            frame,
            #[cfg(windows)]
            layout,
            render_scale,
        }
    }

    pub fn rasterize_and_place_glyphs(
        &self,
        font_set: &PerWindowFontSet,
        vector_rasterization_state: &mut VectorRasterizationState,
        atlas: &mut MaskTextureAtlasManager,
        render_scale: f32,
    ) -> Vec<GlyphPlacementBox> {
        let mut boxes = Vec::new();

        #[cfg(feature = "harfbuzz")]
        for &(buf, left_base, font, fallback_index) in self.buffers.iter() {
            let font = font_set.select(font).faces[fallback_index];

            let mut glyph_infos_len = core::mem::MaybeUninit::uninit();
            let glyph_infos =
                unsafe { hb::ffi::hb_buffer_get_glyph_infos(buf, glyph_infos_len.as_mut_ptr()) };
            let mut glyph_positions_len = core::mem::MaybeUninit::uninit();
            let glyph_positions = unsafe {
                hb::ffi::hb_buffer_get_glyph_positions(buf, glyph_positions_len.as_mut_ptr())
            };
            assert_eq!(unsafe { glyph_infos_len.assume_init() }, unsafe {
                glyph_positions_len.assume_init()
            });
            let baseline_y = self.baseline_y_offset;
            let mut left_cursor = left_base;
            for n in 0..unsafe { glyph_positions_len.assume_init() } {
                let glyph_info = unsafe { &*glyph_infos.add(n as usize) };
                let glyph_position = unsafe { &*glyph_positions.add(n as usize) };

                unsafe {
                    ft::load_glyph(font, glyph_info.codepoint, ft::LoadFlags::DEFAULT)
                        .expect("face.load_glyph")
                };
                let metrics = unsafe { &(*(*font).glyph).metrics };
                let glyph_width = metrics.width as f32 / 64.0;
                let glyph_height = metrics.height as f32 / 64.0;

                let (r, is_new) = atlas.acquire_for_glyph(
                    (font as _, glyph_info.codepoint as _),
                    glyph_width.ceil() as _,
                    glyph_height.ceil() as _,
                );
                let placement_box = GlyphPlacementBox {
                    left: left_cursor
                        + (glyph_position.x_offset as f32 + metrics.horiBearingX as f32) / 64.0,
                    top: baseline_y - metrics.horiBearingY as f32 / 64.0,
                    tex_left: r.left,
                    tex_top: r.top,
                    width: r.width(),
                    height: r.height(),
                };
                boxes.push(placement_box);

                if is_new {
                    vector_rasterization_state.updated_rects.push(r.vk_rect());

                    struct OutlineReceiver<'r> {
                        current_figure: Option<(ft::Vector, usize)>,
                        pen_pos: ft::Vector,
                        sink: &'r mut VectorRasterizationState,
                        offset_x: f32,
                        offset_y: f32,
                    }
                    impl ft::OutlineFuncs for OutlineReceiver<'_> {
                        fn move_to(&mut self, to: &ft::Vector) {
                            self.current_figure =
                                Some((to.clone(), self.sink.fill_tri_points.len()));
                            self.pen_pos = to.clone();
                            self.sink.fill_tri_points.push([
                                to.x as f32 / 64.0 + self.offset_x,
                                to.y as f32 / 64.0 + self.offset_y,
                            ]);
                        }

                        fn line_to(&mut self, to: &ft::Vector) {
                            let Some((_, filltri_index0)) = self.current_figure else {
                                panic!("no figure started?");
                            };

                            let filltri_index1 = self.sink.fill_tri_points.len() - 1;
                            let filltri_index2 = self.sink.fill_tri_points.len();
                            self.sink.fill_tri_points.push([
                                to.x as f32 / 64.0 + self.offset_x,
                                to.y as f32 / 64.0 + self.offset_y,
                            ]);
                            self.sink.fill_tri_indices.extend([
                                filltri_index0 as u16,
                                filltri_index1 as u16,
                                filltri_index2 as u16,
                            ]);
                            self.pen_pos = to.clone();
                        }

                        fn conic_to(&mut self, control: &ft::Vector, to: &ft::Vector) {
                            let Some((_, filltri_index0)) = self.current_figure else {
                                panic!("no figure started?");
                            };

                            let filltri_index1 = self.sink.fill_tri_points.len() - 1;
                            let filltri_index2 = self.sink.fill_tri_points.len();
                            self.sink.fill_tri_points.push([
                                to.x as f32 / 64.0 + self.offset_x,
                                to.y as f32 / 64.0 + self.offset_y,
                            ]);
                            self.sink.fill_tri_indices.extend([
                                filltri_index0 as u16,
                                filltri_index1 as u16,
                                filltri_index2 as u16,
                            ]);
                            self.sink.curve_tris.extend([
                                [
                                    self.pen_pos.x as f32 / 64.0 + self.offset_x,
                                    self.pen_pos.y as f32 / 64.0 + self.offset_y,
                                    0.0,
                                    0.0,
                                ],
                                [
                                    control.x as f32 / 64.0 + self.offset_x,
                                    control.y as f32 / 64.0 + self.offset_y,
                                    0.5,
                                    0.0,
                                ],
                                [
                                    to.x as f32 / 64.0 + self.offset_x,
                                    to.y as f32 / 64.0 + self.offset_y,
                                    1.0,
                                    1.0,
                                ],
                            ]);
                            self.pen_pos = to.clone();
                        }

                        fn cubic_to(
                            &mut self,
                            control1: &ft::Vector,
                            control2: &ft::Vector,
                            to: &ft::Vector,
                        ) {
                            lyon_geom::CubicBezierSegment {
                                from: lyon_geom::point(
                                    self.pen_pos.x as f32 / 64.0 + self.offset_x,
                                    self.pen_pos.y as f32 / 64.0 + self.offset_y,
                                ),
                                ctrl1: lyon_geom::point(
                                    control1.x as f32 / 64.0 + self.offset_x,
                                    control1.y as f32 / 64.0 + self.offset_y,
                                ),
                                ctrl2: lyon_geom::point(
                                    control2.x as f32 / 64.0 + self.offset_x,
                                    control2.y as f32 / 64.0 + self.offset_y,
                                ),
                                to: lyon_geom::point(
                                    to.x as f32 / 64.0 + self.offset_x,
                                    to.y as f32 / 64.0 + self.offset_y,
                                ),
                            }
                            .for_each_quadratic_bezier(0.1, &mut |q| {
                                let Some((_, filltri_index0)) = self.current_figure else {
                                    panic!("no figure started?");
                                };

                                let filltri_index1 = self.sink.fill_tri_points.len() - 1;
                                let filltri_index2 = self.sink.fill_tri_points.len();
                                self.sink.fill_tri_points.push([q.to.x, q.to.y]);
                                self.sink.fill_tri_indices.extend([
                                    filltri_index0 as u16,
                                    filltri_index1 as u16,
                                    filltri_index2 as u16,
                                ]);
                                self.sink.curve_tris.extend([
                                    [q.from.x, q.from.y, 0.0, 0.0],
                                    [q.ctrl.x, q.ctrl.y, 0.5, 0.0],
                                    [q.to.x, q.to.y, 1.0, 1.0],
                                ]);
                            });
                            self.pen_pos = to.clone();
                        }
                    }

                    unsafe {
                        ft::outline_decompose(
                            &mut (*(*font).glyph).outline,
                            &mut OutlineReceiver {
                                current_figure: None,
                                pen_pos: ft::Vector { x: 0, y: 0 },
                                sink: vector_rasterization_state,
                                offset_x: r.left as f32 - metrics.horiBearingX as f32 / 64.0,
                                offset_y: -(r.top as f32) - metrics.horiBearingY as f32 / 64.0,
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

        #[cfg(target_os = "macos")]
        let lines = self.frame.lines();
        #[cfg(target_os = "macos")]
        tracing::debug!(line_count = lines.len(), "frameset lines");
        #[cfg(target_os = "macos")]
        let mut height = 0.0f32;
        #[cfg(target_os = "macos")]
        for l in lines.iter() {
            let runs = l.glyph_runs();
            tracing::debug!(count = runs.len(), "glyph runs");
            let mut baseline_pos: apple_sdk_port::raw::CGFloat = 0.0;
            let mut fonts_per_run = Vec::with_capacity(runs.len() as _);
            let mut accumulated_inline_shifts = Vec::with_capacity(runs.len() as _);
            let mut inline_shifts = 0.0;
            for r in runs.iter() {
                let attributes = r.attributes();
                // r.attributes().apply_untyped_value(|key, value| {
                //     tracing::debug!(?key, ?value, "run attribute");
                // });

                let font = unsafe {
                    apple_sdk_port::text::Font::ref_from_untyped_ptr(
                        attributes
                            .get_untyped_value(
                                apple_sdk_port::foundation::AttributedStringKey::font(),
                            )
                            .expect("font not set?")
                            .as_ptr(),
                    )
                };
                fonts_per_run.push(font);

                baseline_pos = baseline_pos.max(font.ascent());
                // TODO: 複数行になる場合はleadingを行間に足す
                height = height.max((font.ascent() + font.descent()) as f32 * 2.0);

                accumulated_inline_shifts.push(inline_shifts);
                if let Some(x) =
                    attributes.get_untyped_value(crate::platform::mac::ak_spacing_inline_start())
                {
                    inline_shifts += unsafe {
                        apple_sdk_port::foundation::Number::ref_from_untyped_ptr(x.as_ptr())
                            .f32_value()
                            .expect("invalid attr value")
                    }
                };
            }

            for ((r, font), x_shift) in runs
                .iter()
                .zip(fonts_per_run)
                .zip(accumulated_inline_shifts)
            {
                let font_uniq_name = font
                    .copy_name(apple_sdk_port::text::Font::unique_name_key())
                    .or_else(|| font.copy_name(apple_sdk_port::text::Font::full_name_key()))
                    .expect("cannot determine font unique name");
                let font_size = font.size();
                let font_unique_id = FONT_UNIQUIFY_STORAGE
                    .with(|s| s.query(format!("{font_uniq_name:?}.{font_size:.2}")));

                let glyph_count = r.glyph_count();
                tracing::debug!(?font_uniq_name, font_size, count = glyph_count, "run");
                let mut glyph_bounding_rects = Vec::with_capacity(glyph_count as _);
                font.bounding_rects_for_glyphs(
                    apple_sdk_port::text::FontOrientation::Horizontal,
                    unsafe { core::slice::from_raw_parts(r.glyphs_ptr(), glyph_count as _) },
                    glyph_bounding_rects.spare_capacity_mut(),
                );
                unsafe {
                    glyph_bounding_rects.set_len(glyph_count as _);
                }

                for g in 0..glyph_count {
                    let glyph = unsafe { *r.glyphs_ptr().add(g as usize) };
                    let pos = unsafe { &*r.positions().add(g as usize) };
                    let bounding_rect = &glyph_bounding_rects[g as usize];
                    tracing::debug!(glyph, ?pos, ?bounding_rect, "glyph");

                    if bounding_rect.size.width == 0.0 && bounding_rect.size.height == 0.0 {
                        // empty shape(e.g. whitespace)
                        continue;
                    }

                    let (r, is_new) = atlas.acquire_for_glyph(
                        (font_unique_id, glyph),
                        (bounding_rect.size.width as f32 * render_scale).ceil() as _,
                        (bounding_rect.size.height as f32 * render_scale).ceil() as _,
                    );
                    boxes.push(GlyphPlacementBox {
                        left: ((pos.x + bounding_rect.origin.x) as f32 + x_shift) * render_scale,
                        top: (baseline_pos + pos.y
                            - (bounding_rect.size.height + bounding_rect.origin.y))
                            as f32
                            * render_scale,
                        tex_left: r.left,
                        tex_top: r.top,
                        width: r.width(),
                        height: r.height(),
                    });

                    if is_new {
                        vector_rasterization_state.updated_rects.push(r.vk_rect());

                        let offset_x = r.left as f32 - bounding_rect.origin.x as f32 * render_scale;
                        let offset_y = -(r.top as f32)
                            - (bounding_rect.size.height + bounding_rect.origin.y) as f32
                                * render_scale;
                        let mut vrender = VectorVertexRenderer::new(vector_rasterization_state);
                        font.create_path_for_glyph(glyph, None)
                            .expect("font.create_path_for_glyph")
                            .apply(|e| match e.r#type {
                                apple_sdk_port::raw::kCGPathElementMoveToPoint => {
                                    let to = unsafe { &*e.points };

                                    vrender.move_to(
                                        to.x as f32 * render_scale + offset_x,
                                        to.y as f32 * render_scale + offset_y,
                                    );
                                }
                                apple_sdk_port::raw::kCGPathElementAddLineToPoint => {
                                    let to = unsafe { &*e.points };

                                    vrender.line_to(
                                        to.x as f32 * render_scale + offset_x,
                                        to.y as f32 * render_scale + offset_y,
                                    );
                                }
                                apple_sdk_port::raw::kCGPathElementAddQuadCurveToPoint => {
                                    let points =
                                        unsafe { core::slice::from_raw_parts(e.points, 2) };

                                    vrender.quadratic_to(
                                        points[0].x as f32 * render_scale + offset_x,
                                        points[0].y as f32 * render_scale + offset_y,
                                        points[1].x as f32 * render_scale + offset_x,
                                        points[1].y as f32 * render_scale + offset_y,
                                    );
                                }
                                apple_sdk_port::raw::kCGPathElementAddCurveToPoint => {
                                    let points =
                                        unsafe { core::slice::from_raw_parts(e.points, 3) };

                                    vrender.cubic_to(
                                        points[0].x as f32 * render_scale + offset_x,
                                        points[0].y as f32 * render_scale + offset_y,
                                        points[1].x as f32 * render_scale + offset_x,
                                        points[1].y as f32 * render_scale + offset_y,
                                        points[2].x as f32 * render_scale + offset_x,
                                        points[2].y as f32 * render_scale + offset_y,
                                    );
                                }
                                apple_sdk_port::raw::kCGPathElementCloseSubpath => {
                                    vrender.close();
                                }
                                _ => unreachable!(),
                            });
                    }
                }
            }
        }

        #[cfg(windows)]
        unsafe {
            self.layout
                .Draw(
                    None,
                    &IDWriteTextRenderer::from(TextLayoutRenderer {
                        dip_to_pixels_scaling: render_scale,
                        vector_raster_state: vector_rasterization_state,
                        atlas,
                        boxes: &mut boxes,
                    }),
                    0.0,
                    0.0,
                )
                .expect("dwrite.layout.draw");
        }

        #[cfg(windows)]
        #[implement(IDWriteTextRenderer)]
        pub struct TextLayoutRenderer<'d> {
            dip_to_pixels_scaling: f32,
            vector_raster_state: *mut VectorRasterizationState,
            atlas: *mut MaskTextureAtlasManager<'d>,
            boxes: *mut Vec<GlyphPlacementBox>,
        }
        #[cfg(windows)]
        impl IDWritePixelSnapping_Impl for TextLayoutRenderer_Impl<'_> {
            fn GetCurrentTransform(
                &self,
                _clientdrawingcontext: *const core::ffi::c_void,
                transform: *mut windows::Win32::Graphics::DirectWrite::DWRITE_MATRIX,
            ) -> windows_core::Result<()> {
                unsafe {
                    *transform = windows::Win32::Graphics::DirectWrite::DWRITE_MATRIX {
                        m11: 1.0,
                        m12: 0.0,
                        m21: 0.0,
                        m22: 1.0,
                        dx: 0.0,
                        dy: 0.0,
                    };
                }

                Ok(())
            }

            fn GetPixelsPerDip(
                &self,
                _clientdrawingcontext: *const core::ffi::c_void,
            ) -> windows_core::Result<f32> {
                Ok(self.dip_to_pixels_scaling)
            }

            fn IsPixelSnappingDisabled(
                &self,
                _clientdrawingcontext: *const core::ffi::c_void,
            ) -> windows_core::Result<windows_core::BOOL> {
                Ok(BOOL(0))
            }
        }
        #[cfg(windows)]
        impl IDWriteTextRenderer_Impl for TextLayoutRenderer_Impl<'_> {
            fn DrawGlyphRun(
                &self,
                _clientdrawingcontext: *const core::ffi::c_void,
                mut baselineoriginx: f32,
                baselineoriginy: f32,
                _measuringmode: windows::Win32::Graphics::DirectWrite::DWRITE_MEASURING_MODE,
                glyphrun: *const windows::Win32::Graphics::DirectWrite::DWRITE_GLYPH_RUN,
                _glyphrundescription: *const windows::Win32::Graphics::DirectWrite::DWRITE_GLYPH_RUN_DESCRIPTION,
                clientdrawingeffect: windows_core::Ref<windows_core::IUnknown>,
            ) -> windows_core::Result<()> {
                use windows::Win32::Graphics::DirectWrite::DWRITE_GLYPH_METRICS;

                let var = clientdrawingeffect
                    .unwrap()
                    .cast::<IAppDrawingEffect>()
                    .expect("clientdrawingeffect.cast.appDrawingEffect");
                // tracing::debug!(?var, fid = ?unsafe { var.font_id() }, baselineoriginx);

                let glyphrun = unsafe { &*glyphrun };
                let font_face = glyphrun.fontFace.as_ref().expect("no font face");
                let mut font_metrics = core::mem::MaybeUninit::uninit();
                unsafe { font_face.GetMetrics(font_metrics.as_mut_ptr()) };
                let font_metrics = unsafe { font_metrics.assume_init_ref() };
                let design_unit = font_metrics.designUnitsPerEm;
                let mut glyph_metrics: Vec<DWRITE_GLYPH_METRICS> =
                    Vec::with_capacity(glyphrun.glyphCount as _);
                // tracing::debug!(count = glyphrun.glyphCount, "glyphrun");
                unsafe {
                    font_face
                        .GetDesignGlyphMetrics(
                            glyphrun.glyphIndices,
                            glyphrun.glyphCount,
                            glyph_metrics.spare_capacity_mut().as_mut_ptr() as _,
                            glyphrun.isSideways.as_bool(),
                        )
                        .expect("GetDesignGlyphMetrics");
                    glyph_metrics.set_len(glyphrun.glyphCount as _);
                }
                for n in 0..glyphrun.glyphCount as usize {
                    let glyph_width = (glyph_metrics[n].advanceWidth as i32
                        - glyph_metrics[n].leftSideBearing
                        - glyph_metrics[n].rightSideBearing)
                        as f32
                        * glyphrun.fontEmSize
                        * self.dip_to_pixels_scaling
                        / design_unit as f32;
                    let glyph_height = (glyph_metrics[n].advanceHeight as i32
                        - glyph_metrics[n].topSideBearing
                        - glyph_metrics[n].bottomSideBearing)
                        as f32
                        * glyphrun.fontEmSize
                        * self.dip_to_pixels_scaling
                        / design_unit as f32;

                    let (r, is_new) = unsafe {
                        (*self.atlas).acquire_for_glyph(
                            (var.font_id() as _, *glyphrun.glyphIndices.add(n)),
                            glyph_width.ceil() as _,
                            glyph_height.ceil() as _,
                        )
                    };

                    let glyph_placement_box = GlyphPlacementBox {
                        left: ((baselineoriginx
                            + glyph_metrics[n].leftSideBearing as f32 * glyphrun.fontEmSize
                                / design_unit as f32)
                            + unsafe { var.offset_x() })
                            * self.dip_to_pixels_scaling,
                        top: (baselineoriginy
                            - (glyph_metrics[n].verticalOriginY as f32
                                - glyph_metrics[n].topSideBearing as f32)
                                * glyphrun.fontEmSize
                                / design_unit as f32)
                            * self.dip_to_pixels_scaling,
                        tex_left: r.left,
                        tex_top: r.top,
                        width: r.width(),
                        height: r.height(),
                    };
                    /*tracing::debug!(
                        met = ?glyph_metrics[n],
                        font_em_size = glyphrun.fontEmSize,
                        design_unit,
                        scaling = self.dip_to_pixels_scaling,
                        ?glyph_placement_box,
                        is_new
                    );*/

                    unsafe {
                        (*self.boxes).push(glyph_placement_box);
                    }
                    if is_new {
                        unsafe {
                            (*self.vector_raster_state).updated_rects.push(r.vk_rect());
                        }

                        use windows::Win32::Graphics::Direct2D::Common::ID2D1SimplifiedGeometrySink;
                        let mut current_figure_state = None;
                        let sink = ID2D1SimplifiedGeometrySink::from(GlyphOutlineSink {
                            translate: windows_numerics::Vector2 {
                                X: r.left as f32
                                    - glyph_metrics[n].leftSideBearing as f32
                                        * glyphrun.fontEmSize
                                        * self.dip_to_pixels_scaling
                                        / design_unit as f32,
                                Y: -(r.top as f32)
                                    - (glyph_metrics[n].verticalOriginY as f32
                                        - glyph_metrics[n].topSideBearing as f32)
                                        * glyphrun.fontEmSize
                                        * self.dip_to_pixels_scaling
                                        / design_unit as f32,
                            },
                            dip_to_pixels_scale: self.dip_to_pixels_scaling,
                            current_figure_state: &mut current_figure_state,
                            vector_raster_state: self.vector_raster_state,
                        });
                        unsafe {
                            font_face
                                .GetGlyphRunOutline(
                                    glyphrun.fontEmSize,
                                    glyphrun.glyphIndices.add(n),
                                    None,
                                    None,
                                    1,
                                    glyphrun.isSideways.as_bool(),
                                    false,
                                    &sink,
                                )
                                .expect("GetGlyphRunOutline");
                        }
                        assert!(current_figure_state.is_none());
                    }

                    baselineoriginx += unsafe { *glyphrun.glyphAdvances.add(n) };
                }

                Ok(())
            }

            fn DrawInlineObject(
                &self,
                _clientdrawingcontext: *const core::ffi::c_void,
                _originx: f32,
                _originy: f32,
                _inlineobject: windows_core::Ref<
                    windows::Win32::Graphics::DirectWrite::IDWriteInlineObject,
                >,
                _issideways: windows_core::BOOL,
                _isrighttoleft: windows_core::BOOL,
                _clientdrawingeffect: windows_core::Ref<windows_core::IUnknown>,
            ) -> windows_core::Result<()> {
                unimplemented!();
            }

            fn DrawStrikethrough(
                &self,
                _clientdrawingcontext: *const core::ffi::c_void,
                _baselineoriginx: f32,
                _baselineoriginy: f32,
                _strikethrough: *const windows::Win32::Graphics::DirectWrite::DWRITE_STRIKETHROUGH,
                _clientdrawingeffect: windows_core::Ref<windows_core::IUnknown>,
            ) -> windows_core::Result<()> {
                unimplemented!();
            }

            fn DrawUnderline(
                &self,
                _clientdrawingcontext: *const core::ffi::c_void,
                _baselineoriginx: f32,
                _baselineoriginy: f32,
                _underline: *const windows::Win32::Graphics::DirectWrite::DWRITE_UNDERLINE,
                _clientdrawingeffect: windows_core::Ref<windows_core::IUnknown>,
            ) -> windows_core::Result<()> {
                unimplemented!();
            }
        }
        #[cfg(windows)]
        #[implement(ID2D1SimplifiedGeometrySink)]
        struct GlyphOutlineSink {
            translate: windows_numerics::Vector2,
            dip_to_pixels_scale: f32,
            current_figure_state: *mut Option<(windows_numerics::Vector2, u16)>,
            vector_raster_state: *mut VectorRasterizationState,
        }
        #[cfg(windows)]
        impl ID2D1SimplifiedGeometrySink_Impl for GlyphOutlineSink_Impl {
            fn BeginFigure(
                &self,
                startpoint: &windows_numerics::Vector2,
                figurebegin: windows::Win32::Graphics::Direct2D::Common::D2D1_FIGURE_BEGIN,
            ) {
                use windows::Win32::Graphics::Direct2D::Common::D2D1_FIGURE_BEGIN_FILLED;

                assert_eq!(figurebegin, D2D1_FIGURE_BEGIN_FILLED, "not filled figure");

                unsafe {
                    *self.current_figure_state = Some((
                        *startpoint,
                        (*self.vector_raster_state).fill_tri_points.len() as _,
                    ));
                    (*self.vector_raster_state).fill_tri_points.push([
                        startpoint.X * self.dip_to_pixels_scale + self.translate.X,
                        -startpoint.Y * self.dip_to_pixels_scale + self.translate.Y,
                    ]);
                }
            }

            fn EndFigure(
                &self,
                figureend: windows::Win32::Graphics::Direct2D::Common::D2D1_FIGURE_END,
            ) {
                use windows::Win32::Graphics::Direct2D::Common::D2D1_FIGURE_END_CLOSED;

                let (start_point, filltri_index0) = unsafe {
                    (*self.current_figure_state)
                        .take()
                        .expect("no figure started?")
                };

                if figureend == D2D1_FIGURE_END_CLOSED {
                    // line to start
                    unsafe {
                        let filltri_point1 = (*self.vector_raster_state).fill_tri_points.len() - 1;
                        (*self.vector_raster_state).fill_tri_points.push([
                            start_point.X * self.dip_to_pixels_scale + self.translate.X,
                            -start_point.Y * self.dip_to_pixels_scale + self.translate.Y,
                        ]);
                        (*self.vector_raster_state).fill_tri_indices.extend([
                            filltri_index0,
                            filltri_point1 as u16,
                            (*self.vector_raster_state).fill_tri_points.len() as u16 - 1,
                        ]);
                    }
                }
            }

            fn AddLines(&self, points: *const windows_numerics::Vector2, pointscount: u32) {
                let &(_, filltri_index0) = unsafe {
                    (*self.current_figure_state)
                        .as_ref()
                        .expect("no figure started?")
                };

                for p in unsafe { core::slice::from_raw_parts(points, pointscount as _) } {
                    unsafe {
                        let filltri_point1 = (*self.vector_raster_state).fill_tri_points.len() - 1;
                        (*self.vector_raster_state).fill_tri_points.push([
                            p.X * self.dip_to_pixels_scale + self.translate.X,
                            -p.Y * self.dip_to_pixels_scale + self.translate.Y,
                        ]);
                        (*self.vector_raster_state).fill_tri_indices.extend([
                            filltri_index0,
                            filltri_point1 as u16,
                            (*self.vector_raster_state).fill_tri_points.len() as u16 - 1,
                        ]);
                    }
                }
            }

            fn AddBeziers(
                &self,
                beziers: *const windows::Win32::Graphics::Direct2D::Common::D2D1_BEZIER_SEGMENT,
                bezierscount: u32,
            ) {
                let &(_, filltri_index0) = unsafe {
                    (*self.current_figure_state)
                        .as_ref()
                        .expect("no figure started?")
                };

                for p in unsafe { core::slice::from_raw_parts(beziers, bezierscount as _) } {
                    let from_p = unsafe {
                        (*self.vector_raster_state)
                            .fill_tri_points
                            .last()
                            .expect("no points emitted")
                    };
                    let bez = lyon_geom::CubicBezierSegment {
                        from: lyon_geom::point(from_p[0], from_p[1]),
                        ctrl1: lyon_geom::point(
                            p.point1.X * self.dip_to_pixels_scale + self.translate.X,
                            -p.point1.Y * self.dip_to_pixels_scale + self.translate.Y,
                        ),
                        ctrl2: lyon_geom::point(
                            p.point2.X * self.dip_to_pixels_scale + self.translate.X,
                            -p.point2.Y * self.dip_to_pixels_scale + self.translate.Y,
                        ),
                        to: lyon_geom::point(
                            p.point3.X * self.dip_to_pixels_scale + self.translate.X,
                            -p.point3.Y * self.dip_to_pixels_scale + self.translate.Y,
                        ),
                    };

                    bez.for_each_quadratic_bezier(0.1, &mut |q| unsafe {
                        let filltri_point1 = (*self.vector_raster_state).fill_tri_points.len() - 1;
                        (*self.vector_raster_state)
                            .fill_tri_points
                            .push([q.to.x, q.to.y]);
                        (*self.vector_raster_state).fill_tri_indices.extend([
                            filltri_index0,
                            filltri_point1 as u16,
                            (*self.vector_raster_state).fill_tri_points.len() as u16 - 1,
                        ]);

                        (*self.vector_raster_state).curve_tris.extend([
                            [q.from.x, q.from.y, 0.0, 0.0],
                            [q.ctrl.x, q.ctrl.y, 0.5, 0.0],
                            [q.to.x, q.to.y, 1.0, 1.0],
                        ]);
                    });
                }
            }

            fn Close(&self) -> windows_core::Result<()> {
                let &(ref start_point, filltri_index0) = unsafe {
                    (*self.current_figure_state)
                        .as_ref()
                        .expect("no figure started?")
                };

                // line to start
                unsafe {
                    let filltri_point1 = (*self.vector_raster_state).fill_tri_points.len() - 1;
                    (*self.vector_raster_state).fill_tri_points.push([
                        start_point.X * self.dip_to_pixels_scale + self.translate.X,
                        start_point.Y * self.dip_to_pixels_scale + self.translate.Y,
                    ]);
                    (*self.vector_raster_state).fill_tri_indices.extend([
                        filltri_index0,
                        filltri_point1 as u16,
                        (*self.vector_raster_state).fill_tri_points.len() as u16 - 1,
                    ]);
                }

                Ok(())
            }

            fn SetFillMode(
                &self,
                fillmode: windows::Win32::Graphics::Direct2D::Common::D2D1_FILL_MODE,
            ) {
                if fillmode != windows::Win32::Graphics::Direct2D::Common::D2D1_FILL_MODE_WINDING {
                    tracing::warn!("not winding fill mode specified");
                }
            }

            fn SetSegmentFlags(
                &self,
                vertexflags: windows::Win32::Graphics::Direct2D::Common::D2D1_PATH_SEGMENT,
            ) {
                unimplemented!("SetSegmentFlags {vertexflags:?}")
            }
        }

        boxes
    }

    #[cfg(not(windows))]
    #[inline(always)]
    #[cfg(feature = "harfbuzz")]
    pub fn height(&self) -> f32 {
        self.height
    }

    #[cfg(target_os = "macos")]
    #[inline(always)]
    pub fn height(&self) -> f32 {
        if self.frame.lines().len() == 0 {
            // no lines(empty string)
            return 0.0;
        }

        // TODO: multi-line consideration
        let mut ascender = core::mem::MaybeUninit::uninit();
        let mut descender = core::mem::MaybeUninit::uninit();
        let l = &self.frame.lines()[0];
        l.typographic_bounds(Some(&mut ascender), Some(&mut descender), None);

        unsafe { (ascender.assume_init() + descender.assume_init()) as f32 * self.render_scale }
    }

    #[cfg(windows)]
    pub fn height(&self) -> f32 {
        let mut metrics = core::mem::MaybeUninit::uninit();
        unsafe {
            self.layout
                .GetMetrics(metrics.as_mut_ptr())
                .expect("layout.GetMetrics")
        };
        unsafe { metrics.assume_init().height * self.render_scale }
    }

    pub fn measure_visual_width(
        text: &str,
        font: FontID,
        font_set: &PerWindowFontSet,
        render_scale: f32,
    ) -> f32 {
        // TODO: 最適化はあとで
        let layout = Self::new(
            core::iter::once(TextRun {
                content: text,
                font,
                spacing_inline_start: 0.0,
            }),
            font_set,
            render_scale,
        );

        #[cfg(feature = "harfbuzz")]
        let Some(&(last_buf, left_base, font, fallback_index)) = layout.buffers.last() else {
            return 0.0;
        };

        #[cfg(feature = "harfbuzz")]
        let mut glyph_infos_len = core::mem::MaybeUninit::uninit();
        #[cfg(feature = "harfbuzz")]
        let glyph_infos =
            unsafe { hb::ffi::hb_buffer_get_glyph_infos(last_buf, glyph_infos_len.as_mut_ptr()) };
        #[cfg(feature = "harfbuzz")]
        let mut glyph_positions_len = core::mem::MaybeUninit::uninit();
        #[cfg(feature = "harfbuzz")]
        let glyph_positions = unsafe {
            hb::ffi::hb_buffer_get_glyph_positions(last_buf, glyph_positions_len.as_mut_ptr())
        };
        #[cfg(feature = "harfbuzz")]
        assert_eq!(unsafe { glyph_infos_len.assume_init() }, unsafe {
            glyph_positions_len.assume_init()
        });

        #[cfg(feature = "harfbuzz")]
        let font = font_set.select(font).faces[fallback_index];
        #[cfg(feature = "harfbuzz")]
        let mut left_cursor = left_base;
        #[cfg(feature = "harfbuzz")]
        let mut width = 0.0f32;
        #[cfg(feature = "harfbuzz")]
        for n in 0..unsafe { glyph_positions_len.assume_init() } {
            let glyph_info = unsafe { &*glyph_infos.add(n as usize) };
            let glyph_position = unsafe { &*glyph_positions.add(n as usize) };

            unsafe {
                ft::load_glyph(font, glyph_info.codepoint, ft::LoadFlags::DEFAULT)
                    .expect("face.load_glyph")
            };
            let metrics = unsafe { &(*(*font).glyph).metrics };
            let glyph_width = metrics.width as f32 / 64.0;

            width = width.max(
                left_cursor
                    + (glyph_position.x_offset as f32 + metrics.horiBearingX as f32) / 64.0
                    + glyph_width.ceil(),
            );

            left_cursor += glyph_position.x_advance as f32 / 64.0;
        }

        #[cfg(target_os = "macos")]
        let mut width = 0.0f32;
        #[cfg(target_os = "macos")]
        for l in layout.frame.lines().iter() {
            // Note: inline spacingは常に0なので計算しない
            for r in l.glyph_runs().iter() {
                // r.attributes().apply_untyped_value(|key, value| {
                //     tracing::debug!(?key, ?value, "run attribute");
                // });
                let font = unsafe {
                    apple_sdk_port::text::Font::ref_from_untyped_ptr(
                        r.attributes()
                            .get_untyped_value(
                                apple_sdk_port::foundation::AttributedStringKey::font(),
                            )
                            .expect("font not set?")
                            .as_ptr(),
                    )
                };

                let glyph_count = r.glyph_count();
                let positions =
                    unsafe { core::slice::from_raw_parts(r.positions(), glyph_count as _) };
                let mut glyph_bounding_rects = Vec::with_capacity(glyph_count as _);
                font.bounding_rects_for_glyphs(
                    apple_sdk_port::text::FontOrientation::Horizontal,
                    unsafe { core::slice::from_raw_parts(r.glyphs_ptr(), glyph_count as _) },
                    glyph_bounding_rects.spare_capacity_mut(),
                );
                unsafe {
                    glyph_bounding_rects.set_len(glyph_count as _);
                }

                width = positions
                    .into_iter()
                    .zip(glyph_bounding_rects)
                    .map(|(p, r)| {
                        (p.x + r.origin.x) as f32 * render_scale
                            + (r.size.width as f32 * render_scale).ceil()
                    })
                    .fold(width, |a, b| a.max(b));
            }
        }

        #[cfg(windows)]
        let mut metrics = core::mem::MaybeUninit::uninit();
        #[cfg(windows)]
        unsafe {
            layout
                .layout
                .GetMetrics(metrics.as_mut_ptr())
                .expect("layout.GetMetrics")
        };
        #[cfg(windows)]
        let width = unsafe { metrics.assume_init().width * render_scale };

        width
    }

    pub fn measure_total_advances(
        text: &str,
        font: FontID,
        font_set: &PerWindowFontSet,
        render_scale: f32,
    ) -> f32 {
        // TODO: 最適化はあとで
        let layout = Self::new(
            core::iter::once(TextRun {
                content: text,
                font,
                spacing_inline_start: 0.0,
            }),
            font_set,
            render_scale,
        );

        #[cfg(feature = "harfbuzz")]
        let Some(&(last_buf, left_base, _, _)) = layout.buffers.last() else {
            return 0.0;
        };

        #[cfg(feature = "harfbuzz")]
        let mut glyph_positions_len = core::mem::MaybeUninit::uninit();
        #[cfg(feature = "harfbuzz")]
        let glyph_positions = unsafe {
            hb::ffi::hb_buffer_get_glyph_positions(last_buf, glyph_positions_len.as_mut_ptr())
        };

        #[cfg(feature = "harfbuzz")]
        let mut left_cursor = left_base;
        #[cfg(feature = "harfbuzz")]
        for n in 0..unsafe { glyph_positions_len.assume_init() } {
            let glyph_position = unsafe { &*glyph_positions.add(n as usize) };

            left_cursor += glyph_position.x_advance as f32 / 64.0;
        }

        #[cfg(target_os = "macos")]
        let mut left_cursor = 0.0 as f32;
        #[cfg(target_os = "macos")]
        for l in layout.frame.lines().iter() {
            // Note: inline shiftは常に0になるので計算しない
            let mut line_left_cursor = 0.0 as f32;
            for r in l.glyph_runs().iter() {
                // r.attributes().apply_untyped_value(|key, value| {
                //     tracing::debug!(?key, ?value, "run attribute");
                // });
                let font = unsafe {
                    apple_sdk_port::text::Font::ref_from_untyped_ptr(
                        r.attributes()
                            .get_untyped_value(
                                apple_sdk_port::foundation::AttributedStringKey::font(),
                            )
                            .expect("font not set?")
                            .as_ptr(),
                    )
                };

                let glyph_count = r.glyph_count();
                let positions =
                    unsafe { core::slice::from_raw_parts(r.positions(), glyph_count as _) };
                let mut glyph_bounding_rects = Vec::with_capacity(glyph_count as _);
                font.bounding_rects_for_glyphs(
                    apple_sdk_port::text::FontOrientation::Horizontal,
                    unsafe { core::slice::from_raw_parts(r.glyphs_ptr(), glyph_count as _) },
                    glyph_bounding_rects.spare_capacity_mut(),
                );
                unsafe {
                    glyph_bounding_rects.set_len(glyph_count as _);
                }

                line_left_cursor = positions
                    .into_iter()
                    .zip(glyph_bounding_rects)
                    .map(|(p, b)| {
                        (p.x + b.origin.x) as f32 * render_scale
                            + (b.size.width as f32 * render_scale).ceil()
                    })
                    .fold(line_left_cursor, |a, b| a.max(b));
            }

            left_cursor = left_cursor.max(line_left_cursor + l.trailing_whitespace_width() as f32);
        }

        #[cfg(windows)]
        let mut metrics = core::mem::MaybeUninit::uninit();
        #[cfg(windows)]
        unsafe {
            layout
                .layout
                .GetMetrics(metrics.as_mut_ptr())
                .expect("layout.GetMetrics")
        };
        #[cfg(windows)]
        let left_cursor =
            unsafe { metrics.assume_init().widthIncludingTrailingWhitespace * render_scale };

        left_cursor
    }

    pub fn find_nearest_position_with_bytes(
        x: f32,
        text: &str,
        font: FontID,
        font_set: &PerWindowFontSet,
        render_scale: f32,
    ) -> (f32, usize) {
        // TODO: 最適化はあとで
        let layout = Self::new(
            core::iter::once(TextRun {
                content: text,
                font,
                spacing_inline_start: 0.0,
            }),
            font_set,
            1.0,
        );

        // TODO: LTR前提
        #[cfg(feature = "harfbuzz")]
        let mut left_cursor = 0.0;
        #[cfg(feature = "harfbuzz")]
        let mut bytes = 0;
        #[cfg(feature = "harfbuzz")]
        for &(buf, left_base, _, _) in layout.buffers.iter() {
            let mut glyph_positions_len = core::mem::MaybeUninit::uninit();
            let glyph_positions = unsafe {
                hb::ffi::hb_buffer_get_glyph_positions(buf, glyph_positions_len.as_mut_ptr())
            };
            left_cursor = left_base;
            for n in 0..unsafe { glyph_positions_len.assume_init() } {
                let glyph_position = unsafe { &*glyph_positions.add(n as usize) };

                let left = left_cursor;
                let right = left + glyph_position.x_advance as f32 / 64.0;
                let mid = (left + right) / 2.0;

                if x < left {
                    // overshoot
                    return (left, bytes);
                }

                if x <= mid {
                    // left
                    return (left, bytes);
                }

                if x <= right {
                    // right
                    let mut next_boundary_bytes = bytes.saturating_add(1);
                    while next_boundary_bytes < text.len()
                        && !text.is_char_boundary(next_boundary_bytes)
                    {
                        next_boundary_bytes += 1;
                    }

                    return (right, next_boundary_bytes);
                }

                left_cursor += glyph_position.x_advance as f32 / 64.0;
                bytes += text[bytes..]
                    .chars()
                    .next()
                    .expect("out of range")
                    .len_utf8();
            }
        }

        #[cfg(feature = "harfbuzz")]
        // beyond
        return (left_cursor, bytes);

        #[cfg(windows)]
        let mut is_trailing_hit = core::mem::MaybeUninit::uninit();
        #[cfg(windows)]
        let mut is_inside = core::mem::MaybeUninit::uninit();
        #[cfg(windows)]
        let mut metrics = core::mem::MaybeUninit::uninit();
        #[cfg(windows)]
        unsafe {
            layout
                .layout
                .HitTestPoint(
                    x / render_scale,
                    1.0,
                    is_trailing_hit.as_mut_ptr(),
                    is_inside.as_mut_ptr(),
                    metrics.as_mut_ptr(),
                )
                .expect("layout.hittestpoint")
        }
        #[cfg(windows)]
        let is_trailing_hit = unsafe { is_trailing_hit.assume_init().as_bool() };
        #[cfg(windows)]
        let metrics = unsafe { metrics.assume_init() };
        #[cfg(windows)]
        if is_trailing_hit {
            // trailing hitの場合は次の文字を返す（そっちのが近い）
            (
                metrics.left + metrics.width,
                text.chars()
                    .take(metrics.textPosition as usize + 1)
                    .fold(0, |a, c| a + c.len_utf8()),
            )
        } else {
            (
                metrics.left,
                text.chars()
                    .take(metrics.textPosition as _)
                    .fold(0, |a, c| a + c.len_utf8()),
            )
        }

        #[cfg(target_os = "macos")]
        if layout.frame.lines().len() == 0 {
            // no lines(empty string)
            return (0.0, 0);
        }
        #[cfg(target_os = "macos")]
        match layout.frame.lines()[0].string_index_for_position(apple_sdk_port::raw::CGPoint {
            x: (x / render_scale) as _,
            y: 0.0,
        }) {
            Some(x) => (0.0, text.chars().take(x as _).map(|x| x.len_utf8()).sum()),
            None => (0.0, text.len() - 1),
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

// Note: windows_core::interfaceがcfg(windows)をうまく処理してくれないらしいのでこうややこしい感じになってる
#[cfg_attr(not(windows), cfg(windows))]
#[cfg_attr(windows, interface("317f101a-1c78-488b-b1d5-39fedc987e05"))]
unsafe trait IAppDrawingEffect: IUnknown {
    fn font_id(&self) -> FontID;
    fn offset_x(&self) -> f32;
}
#[cfg(windows)]
#[implement(IAppDrawingEffect)]
pub struct DrawingEffect {
    font_id: FontID,
    offset_x: f32,
}
#[cfg(windows)]
impl IAppDrawingEffect_Impl for DrawingEffect_Impl {
    #[inline(always)]
    unsafe fn font_id(&self) -> FontID {
        self.font_id
    }

    #[inline(always)]
    unsafe fn offset_x(&self) -> f32 {
        self.offset_x
    }
}
