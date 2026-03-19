#[cfg(feature = "fontconfig")]
use peridot_tp_fontconfig as fc;
#[cfg(feature = "freetype")]
use peridot_tp_freetype as ft;
#[cfg(feature = "harfbuzz")]
use peridot_tp_harfbuzz as hb;

use crate::rendering::{MaskTextureAtlasManager, composite::VectorRasterizationState};

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
    pub fn new(root_set: &'d RootFontSet, ctx: &ThreadLocalTypingContext) -> Self {
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

pub struct TextLayout {
    #[cfg(feature = "harfbuzz")]
    buffers: Vec<(*mut hb::ffi::hb_buffer_t, f32, FontID, usize)>,
    #[cfg(feature = "harfbuzz")]
    height: f32,
    #[cfg(feature = "harfbuzz")]
    baseline_y_offset: f32,
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

        Self {
            buffers,
            height,
            baseline_y_offset,
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

        boxes
    }

    #[inline(always)]
    pub fn height(&self) -> f32 {
        self.height
    }

    pub fn measure_visual_width(text: &str, font: FontID, font_set: &PerWindowFontSet) -> f32 {
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

        let Some(&(last_buf, left_base, font, fallback_index)) = layout.buffers.last() else {
            return 0.0;
        };

        let mut glyph_infos_len = core::mem::MaybeUninit::uninit();
        let glyph_infos =
            unsafe { hb::ffi::hb_buffer_get_glyph_infos(last_buf, glyph_infos_len.as_mut_ptr()) };
        let mut glyph_positions_len = core::mem::MaybeUninit::uninit();
        let glyph_positions = unsafe {
            hb::ffi::hb_buffer_get_glyph_positions(last_buf, glyph_positions_len.as_mut_ptr())
        };
        assert_eq!(unsafe { glyph_infos_len.assume_init() }, unsafe {
            glyph_positions_len.assume_init()
        });

        let font = font_set.select(font).faces[fallback_index];
        let mut left_cursor = left_base;
        let mut width = 0.0f32;
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

        width
    }

    pub fn measure_total_advances(text: &str, font: FontID, font_set: &PerWindowFontSet) -> f32 {
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

        let Some(&(last_buf, left_base, _, _)) = layout.buffers.last() else {
            return 0.0;
        };

        let mut glyph_positions_len = core::mem::MaybeUninit::uninit();
        let glyph_positions = unsafe {
            hb::ffi::hb_buffer_get_glyph_positions(last_buf, glyph_positions_len.as_mut_ptr())
        };

        let mut left_cursor = left_base;
        for n in 0..unsafe { glyph_positions_len.assume_init() } {
            let glyph_position = unsafe { &*glyph_positions.add(n as usize) };

            left_cursor += glyph_position.x_advance as f32 / 64.0;
        }

        left_cursor
    }

    pub fn find_nearest_position_with_bytes(
        x: f32,
        text: &str,
        font: FontID,
        font_set: &PerWindowFontSet,
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
        let mut left_cursor = 0.0;
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

        // beyond
        (left_cursor, bytes)
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
