use core::mem::MaybeUninit;

use shared::{LogicalUnit, Point, Rect, Size};
use windows::Win32::{
    Foundation::ERROR_INSUFFICIENT_BUFFER,
    Graphics::{
        Direct2D::Common::{
            D2D1_BEZIER_SEGMENT, D2D1_FIGURE_BEGIN, D2D1_FIGURE_BEGIN_FILLED, D2D1_FIGURE_END,
            D2D1_FIGURE_END_CLOSED, D2D1_FILL_MODE, D2D1_FILL_MODE_WINDING, D2D1_PATH_SEGMENT,
            ID2D1SimplifiedGeometrySink, ID2D1SimplifiedGeometrySink_Impl,
        },
        DirectWrite::{
            DWRITE_FACTORY_TYPE_SHARED, DWRITE_FONT_STRETCH_NORMAL, DWRITE_FONT_STYLE_NORMAL,
            DWRITE_FONT_WEIGHT_NORMAL, DWRITE_GLYPH_METRICS, DWRITE_GLYPH_RUN,
            DWRITE_GLYPH_RUN_DESCRIPTION, DWRITE_HIT_TEST_METRICS, DWRITE_LINE_METRICS,
            DWRITE_MATRIX, DWRITE_MEASURING_MODE, DWRITE_STRIKETHROUGH,
            DWRITE_TEXT_ALIGNMENT_CENTER, DWRITE_TEXT_ALIGNMENT_LEADING,
            DWRITE_TEXT_ALIGNMENT_TRAILING, DWRITE_TEXT_RANGE, DWRITE_TRIMMING,
            DWRITE_TRIMMING_GRANULARITY_CHARACTER, DWRITE_UNDERLINE,
            DWRITE_WORD_WRAPPING_EMERGENCY_BREAK, DWriteCreateFactory, IDWriteFactory,
            IDWriteInlineObject, IDWritePixelSnapping_Impl, IDWriteTextFormat, IDWriteTextLayout,
            IDWriteTextRenderer, IDWriteTextRenderer_Impl,
        },
    },
};
use windows_core::{
    BOOL, ComObjectInterface, HRESULT, IUnknown, IUnknown_Vtbl, Interface, PCWSTR, implement,
    interface,
};

use crate::rendering::{
    MaskTextureAtlasManager,
    composite::CompositeRectTextHorizontalAlignment,
    text::{FontID, GlyphPlacementBox, TextRun},
    vg::{
        VectorRasterizationState, VectorTextureUnit, VectorVertexRenderer, point_new_vector_texture,
    },
};

pub struct RootFontSet {
    dw_factory: IDWriteFactory,
    ui_default: IDWriteTextFormat,
    ui_title_project_name: IDWriteTextFormat,
    ui_form_lifted_label: IDWriteTextFormat,
}
impl RootFontSet {
    pub fn new() -> Self {
        let locale_name = crate::utils::platform::windows::user_language();

        let dw: IDWriteFactory = unsafe { DWriteCreateFactory(DWRITE_FACTORY_TYPE_SHARED) }
            .expect("dwrite.factory.create");

        let ui_default = unsafe {
            dw.CreateTextFormat(
                windows_core::w!("Inter Display"),
                None,
                DWRITE_FONT_WEIGHT_NORMAL,
                DWRITE_FONT_STYLE_NORMAL,
                DWRITE_FONT_STRETCH_NORMAL,
                12.0,
                PCWSTR(locale_name.as_ptr()),
            )
            .expect("dwrite.textformat.create.ui_default")
        };
        let ui_title_project_name = unsafe {
            dw.CreateTextFormat(
                windows_core::w!("Inter Display"),
                None,
                DWRITE_FONT_WEIGHT_NORMAL,
                DWRITE_FONT_STYLE_NORMAL,
                DWRITE_FONT_STRETCH_NORMAL,
                10.0,
                PCWSTR(locale_name.as_ptr()),
            )
            .expect("dwrite.textformat.create.ui_title_project_name")
        };
        let ui_form_lifted_label = unsafe {
            dw.CreateTextFormat(
                windows_core::w!("Inter Display"),
                None,
                DWRITE_FONT_WEIGHT_NORMAL,
                DWRITE_FONT_STYLE_NORMAL,
                DWRITE_FONT_STRETCH_NORMAL,
                8.0,
                PCWSTR(locale_name.as_ptr()),
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
}

pub struct FontSet {
    dw_factory: IDWriteFactory,
    ui_default: IDWriteTextFormat,
    ui_title_project_name: IDWriteTextFormat,
    ui_form_lifted_label: IDWriteTextFormat,
}
impl FontSet {
    #[inline(always)]
    pub fn new(root_font_set: &RootFontSet) -> Self {
        Self {
            dw_factory: root_font_set.dw_factory.clone(),
            ui_default: root_font_set.ui_default.clone(),
            ui_title_project_name: root_font_set.ui_title_project_name.clone(),
            ui_form_lifted_label: root_font_set.ui_form_lifted_label.clone(),
        }
    }

    #[inline]
    const fn select(&self, category: FontID) -> &IDWriteTextFormat {
        match category {
            FontID::UIDefault => &self.ui_default,
            FontID::UITitleProjectName => &self.ui_title_project_name,
            FontID::UIFormLiftedLabel => &self.ui_form_lifted_label,
        }
    }
}

pub struct TextLayout {
    layout: IDWriteTextLayout,
}
impl TextLayout {
    pub fn new<'s>(
        runs: impl Iterator<Item = TextRun<'s>>,
        font_set: &FontSet,
        alignment: CompositeRectTextHorizontalAlignment,
        max_width: Option<f32>,
        max_lines: Option<usize>,
    ) -> Self {
        let (lb, ub) = runs.size_hint();
        let mut run_str_utf16s = Vec::new();
        let mut run_parts = Vec::with_capacity(ub.unwrap_or(lb));
        for r in runs {
            let start_pos = run_str_utf16s.len();

            run_str_utf16s.extend(r.content.encode_utf16());
            run_parts.push((
                r.font,
                r.spacing_inline_start,
                DWRITE_TEXT_RANGE {
                    startPosition: start_pos as _,
                    length: (run_str_utf16s.len() - start_pos) as _,
                },
            ));
        }

        let layout = unsafe {
            font_set
                .dw_factory
                .CreateTextLayout(
                    &run_str_utf16s,
                    font_set.select(FontID::UIDefault),
                    max_width.unwrap_or(f32::MAX),
                    f32::MAX,
                )
                .expect("dwrite.layout.create")
        };
        let mut inline_spacing_sum = 0.0;
        for (font_id, spacing_inline_start, range) in run_parts {
            let font = font_set.select(font_id);
            inline_spacing_sum += spacing_inline_start;

            let family_name_len = unsafe { font.GetFontFamilyNameLength() };
            let mut family_name = Vec::with_capacity(family_name_len as usize + 1);
            unsafe {
                font.GetFontFamilyName(core::mem::transmute(family_name.spare_capacity_mut()))
                    .expect("dwrite.font.get_family_name");
            }
            unsafe {
                family_name.set_len(family_name_len as usize + 1);
            }

            unsafe {
                layout
                    .SetFontFamilyName(PCWSTR(family_name.as_ptr()), range)
                    .expect("dwrite.layout.set_font_family_name");
            }
            unsafe {
                layout
                    .SetFontSize(font.GetFontSize(), range)
                    .expect("dwrite.layout.set_font_size");
            }
            unsafe {
                layout
                    .SetFontStretch(font.GetFontStretch(), range)
                    .expect("dwrite.layout.set_font_stretch");
            }
            unsafe {
                layout
                    .SetFontStyle(font.GetFontStyle(), range)
                    .expect("dwrite.layout.set_font_style");
            }
            unsafe {
                layout
                    .SetFontWeight(font.GetFontWeight(), range)
                    .expect("dwrite.layout.set_font_weight");
            }
            unsafe {
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
        unsafe {
            layout
                .SetWordWrapping(DWRITE_WORD_WRAPPING_EMERGENCY_BREAK)
                .expect("dwrite.layout.set_word_wrapping");
        }
        if alignment != CompositeRectTextHorizontalAlignment::Start {
            // 頭揃えじゃないときはサイズ指定が必要なので一回測ってそれを適用する
            let mut metrics = MaybeUninit::uninit();
            unsafe {
                layout
                    .GetMetrics(metrics.as_mut_ptr())
                    .expect("dwrite.layout.get_metrics")
            }
            let metrics = unsafe { metrics.assume_init_ref() };
            unsafe {
                layout
                    .SetMaxWidth(metrics.width)
                    .expect("dwrite.layout.set_max_width");
            }
        }
        unsafe {
            layout
                .SetTextAlignment(match alignment {
                    CompositeRectTextHorizontalAlignment::Start => DWRITE_TEXT_ALIGNMENT_LEADING,
                    CompositeRectTextHorizontalAlignment::Middle => DWRITE_TEXT_ALIGNMENT_CENTER,
                    CompositeRectTextHorizontalAlignment::End => DWRITE_TEXT_ALIGNMENT_TRAILING,
                })
                .expect("dwrite.layout.set_text_alignment");
        }

        if let Some(max_lines) = max_lines {
            // lint count limiting
            let mut line_metrics = Vec::<DWRITE_LINE_METRICS>::with_capacity(4);
            let mut actual_line_count = core::mem::MaybeUninit::uninit();
            match unsafe {
                layout.GetLineMetrics(
                    Some(core::mem::transmute(line_metrics.spare_capacity_mut())),
                    actual_line_count.as_mut_ptr(),
                )
            } {
                Err(e) if e.code() == ERROR_INSUFFICIENT_BUFFER.to_hresult() => {
                    // extend and retry
                    line_metrics.reserve(unsafe { actual_line_count.assume_init() } as _);
                    unsafe {
                        layout
                            .GetLineMetrics(
                                Some(core::mem::transmute(line_metrics.spare_capacity_mut())),
                                actual_line_count.as_mut_ptr(),
                            )
                            .expect("dwrite.layout.get_line_metrics");
                    }
                }
                e => e.expect("dwrite.layout.get_line_metrics"),
            }
            unsafe {
                line_metrics.set_len(actual_line_count.assume_init() as _);
            }
            unsafe {
                layout
                    .SetMaxHeight(
                        line_metrics
                            .iter()
                            .take(max_lines)
                            .map(|x| x.height)
                            .sum::<f32>(),
                    )
                    .expect("dwrite.layout.set_max_height");
            }

            // TODO: Trimming以外も指定できるようにするか？
            let trimming_sign = unsafe {
                font_set
                    .dw_factory
                    .CreateEllipsisTrimmingSign(&layout)
                    .expect("dwrite.create_ellipsis_trimming_sign")
            };
            unsafe {
                layout
                    .SetTrimming(
                        &DWRITE_TRIMMING {
                            granularity: DWRITE_TRIMMING_GRANULARITY_CHARACTER,
                            delimiter: 0,
                            delimiterCount: 0,
                        },
                        &trimming_sign,
                    )
                    .expect("dwrite.layout.set_trimming");
            }
        }

        Self { layout }
    }

    pub fn rasterize_and_place_glyphs(
        &self,
        vr_state: &mut VectorRasterizationState,
        atlas: &mut MaskTextureAtlasManager,
        scale: f32,
    ) -> Vec<GlyphPlacementBox> {
        let mut boxes = Vec::new();

        unsafe {
            self.layout
                .Draw(
                    None,
                    &IDWriteTextRenderer::from(TextLayoutRenderer {
                        dip_to_pixels_scaling: scale,
                        vector_raster_state: vr_state,
                        atlas,
                        boxes: &mut boxes,
                    }),
                    0.0,
                    0.0,
                )
                .expect("dwrite.layout.draw");
        }

        boxes
    }

    /// 文字レイアウトのサイズを取得
    ///
    /// 幅はtrailing whitespaceを含まない（見た目上の幅）
    pub fn size(&self) -> Size<LogicalUnit> {
        let mut metrics = MaybeUninit::uninit();
        unsafe {
            self.layout
                .GetMetrics(metrics.as_mut_ptr())
                .expect("layout.get_metrics");
        }
        let metrics = unsafe { metrics.assume_init_ref() };

        Size::new_logical(metrics.width, metrics.height)
    }

    /// 終端の空白を含んだテキスト幅の計算
    pub fn width_with_trailing_whitespace(&self) -> f32 {
        let mut metrics = MaybeUninit::uninit();
        unsafe {
            self.layout
                .GetMetrics(metrics.as_mut_ptr())
                .expect("layout.get_metrics");
        }
        let metrics = unsafe { metrics.assume_init_ref() };

        metrics.widthIncludingTrailingWhitespace
    }

    /// 座標から文字の位置を計算
    pub fn find_nearest_text_pos(&self, x: f32, y: f32) -> u32 {
        let mut is_trailing_hit = MaybeUninit::uninit();
        let mut is_inside = MaybeUninit::uninit();
        let mut metrics = MaybeUninit::uninit();
        unsafe {
            self.layout
                .HitTestPoint(
                    x,
                    y,
                    is_trailing_hit.as_mut_ptr(),
                    is_inside.as_mut_ptr(),
                    metrics.as_mut_ptr(),
                )
                .expect("dwrite.layout.hit_test_point");
        }
        let is_trailing_hit = unsafe { is_trailing_hit.assume_init().as_bool() };
        let metrics = unsafe { metrics.assume_init() };

        if is_trailing_hit {
            // trailing hitの場合は次の文字を返す（そっちのが近い）
            metrics.textPosition + 1
        } else {
            metrics.textPosition
        }
    }

    /// テキスト末尾をカーソル位置として、その座標を計算
    pub fn measure_cursor_rect_at_end(&self) -> Rect<LogicalUnit> {
        let mut x = MaybeUninit::uninit();
        let mut y = MaybeUninit::uninit();
        let mut metrics = MaybeUninit::uninit();
        unsafe {
            self.layout
                .HitTestTextPosition(
                    u32::MAX,
                    true,
                    x.as_mut_ptr(),
                    y.as_mut_ptr(),
                    metrics.as_mut_ptr(),
                )
                .expect("dwrite.layout.hit_test_text_position");
        }
        let x = unsafe { x.assume_init() };
        let y = unsafe { y.assume_init() };
        let metrics = unsafe { metrics.assume_init_ref() };

        Rect::from_lt_size(
            Point::new_logical(x, y),
            Size::new_logical(metrics.width, metrics.height),
        )
    }

    /// 指定範囲のテキスト行をそれぞれ囲う矩形の座標を計算
    pub fn measure_line_rects(
        &self,
        range_utf16s: core::range::Range<u32>,
    ) -> Vec<Rect<LogicalUnit>> {
        // TODO: できれば小さいバッファはスタック割り当てにしたいかも
        let mut metrics = Vec::<DWRITE_HIT_TEST_METRICS>::with_capacity(8);
        'try_measure: {
            let mut actual_metrics_count = MaybeUninit::uninit();
            match unsafe {
                self.layout.HitTestTextRange(
                    range_utf16s.start,
                    range_utf16s.end - range_utf16s.start,
                    0.0,
                    0.0,
                    Some(core::mem::transmute(metrics.spare_capacity_mut())),
                    actual_metrics_count.as_mut_ptr(),
                )
            } {
                Ok(_) => {
                    // 少ないバッファ数でいけた
                    unsafe {
                        metrics.set_len(actual_metrics_count.assume_init() as _);
                    }
                    break 'try_measure;
                }
                // なんかどうやっても型が合わないのでゴリ押しで変換する
                Err(e)
                    if unsafe { core::mem::transmute::<_, i32>(e.code()) }
                        == HRESULT::from_win32(ERROR_INSUFFICIENT_BUFFER.0).0 =>
                {
                    ()
                }
                e => e.expect("dwrite.layout.hit_test_text_range"),
            }

            // もっと必要なので再割当て
            metrics = Vec::<DWRITE_HIT_TEST_METRICS>::with_capacity(unsafe {
                actual_metrics_count.assume_init()
            } as _);
            unsafe {
                self.layout
                    .HitTestTextRange(
                        range_utf16s.start,
                        range_utf16s.end - range_utf16s.start,
                        0.0,
                        0.0,
                        Some(core::mem::transmute(metrics.spare_capacity_mut())),
                        actual_metrics_count.as_mut_ptr(),
                    )
                    .expect("dwrite.layout.hit_test_text_range");
            }
            unsafe {
                metrics.set_len(actual_metrics_count.assume_init() as _);
            }
        }

        metrics
            .into_iter()
            .map(|m| {
                Rect::from_lt_size(
                    Point::new_logical(m.left, m.top),
                    Size::new_logical(m.width, m.height),
                )
            })
            .collect()
    }

    /// 単一文字列の高さを計測
    pub fn measure_single_height(text: &str, font: FontID, font_set: &FontSet) -> f32 {
        let mut metrics = MaybeUninit::uninit();
        unsafe {
            font_set
                .dw_factory
                .CreateTextLayout(
                    &text.encode_utf16().collect::<Vec<_>>(),
                    font_set.select(font),
                    f32::MAX,
                    f32::MAX,
                )
                .expect("dwrite.layout.create")
                .GetMetrics(metrics.as_mut_ptr())
                .expect("dwrite.layout.get_metrics");
        }

        unsafe { metrics.assume_init_ref() }.height
    }
}

#[interface("317f101a-1c78-488b-b1d5-39fedc987e05")]
unsafe trait IAppDrawingEffect: IUnknown {
    fn font_id(&self) -> FontID;
    fn offset_x(&self) -> f32;
}

#[implement(IAppDrawingEffect)]
struct DrawingEffect {
    font_id: FontID,
    offset_x: f32,
}
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

#[implement(IDWriteTextRenderer)]
struct TextLayoutRenderer<'d> {
    dip_to_pixels_scaling: f32,
    vector_raster_state: *mut VectorRasterizationState,
    atlas: *mut MaskTextureAtlasManager<'d>,
    boxes: *mut Vec<GlyphPlacementBox>,
}
impl IDWritePixelSnapping_Impl for TextLayoutRenderer_Impl<'_> {
    fn GetCurrentTransform(
        &self,
        _clientdrawingcontext: *const core::ffi::c_void,
        transform: *mut DWRITE_MATRIX,
    ) -> windows_core::Result<()> {
        unsafe {
            *transform = DWRITE_MATRIX {
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
    ) -> windows_core::Result<BOOL> {
        Ok(BOOL(0))
    }
}
impl IDWriteTextRenderer_Impl for TextLayoutRenderer_Impl<'_> {
    fn DrawGlyphRun(
        &self,
        _clientdrawingcontext: *const core::ffi::c_void,
        mut baselineoriginx: f32,
        baselineoriginy: f32,
        _measuringmode: DWRITE_MEASURING_MODE,
        glyphrun: *const DWRITE_GLYPH_RUN,
        _glyphrundescription: *const DWRITE_GLYPH_RUN_DESCRIPTION,
        clientdrawingeffect: windows_core::Ref<IUnknown>,
    ) -> windows_core::Result<()> {
        let var = clientdrawingeffect.as_ref().map(|x| {
            x.cast::<IAppDrawingEffect>()
                .expect("clientdrawingeffect.cast.appDrawingEffect")
        });
        let font_id = var
            .as_ref()
            .map_or(FontID::UIDefault, |x| unsafe { x.font_id() });
        let offset_x = var.as_ref().map_or(0.0, |x| unsafe { x.offset_x() });

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
                    glyph_metrics.spare_capacity_mut().as_mut_ptr().cast(),
                    glyphrun.isSideways.as_bool(),
                )
                .expect("GetDesignGlyphMetrics");
            glyph_metrics.set_len(glyphrun.glyphCount as _);
        }
        for n in 0..glyphrun.glyphCount as usize {
            let glyph_width = (glyph_metrics[n].advanceWidth as i32
                - glyph_metrics[n].leftSideBearing
                - glyph_metrics[n].rightSideBearing) as f32
                * glyphrun.fontEmSize
                * self.dip_to_pixels_scaling
                / design_unit as f32;
            let glyph_height = (glyph_metrics[n].advanceHeight as i32
                - glyph_metrics[n].topSideBearing
                - glyph_metrics[n].bottomSideBearing) as f32
                * glyphrun.fontEmSize
                * self.dip_to_pixels_scaling
                / design_unit as f32;

            let (r, is_new) = unsafe {
                (*self.atlas).acquire_for_glyph(
                    (font_id as _, *glyphrun.glyphIndices.add(n)),
                    glyph_width.ceil() as _,
                    glyph_height.ceil() as _,
                )
            };

            let glyph_placement_box = GlyphPlacementBox {
                left: ((baselineoriginx
                    + glyph_metrics[n].leftSideBearing as f32 * glyphrun.fontEmSize
                        / design_unit as f32)
                    + offset_x)
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

                let mut vrender =
                    VectorVertexRenderer::new(unsafe { &mut *self.vector_raster_state });
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
                    vrender: &mut vrender,
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
            }

            baselineoriginx += unsafe { *glyphrun.glyphAdvances.add(n) };
        }

        Ok(())
    }

    fn DrawInlineObject(
        &self,
        clientdrawingcontext: *const core::ffi::c_void,
        originx: f32,
        originy: f32,
        inlineobject: windows_core::Ref<IDWriteInlineObject>,
        issideways: BOOL,
        isrighttoleft: BOOL,
        clientdrawingeffect: windows_core::Ref<IUnknown>,
    ) -> windows_core::Result<()> {
        unsafe {
            inlineobject.as_ref().expect("null inline object").Draw(
                Some(clientdrawingcontext),
                self.as_interface_ref(),
                originx,
                originy,
                issideways.as_bool(),
                isrighttoleft.as_bool(),
                clientdrawingeffect.as_ref(),
            )
        }
    }

    fn DrawStrikethrough(
        &self,
        _clientdrawingcontext: *const core::ffi::c_void,
        _baselineoriginx: f32,
        _baselineoriginy: f32,
        _strikethrough: *const DWRITE_STRIKETHROUGH,
        _clientdrawingeffect: windows_core::Ref<IUnknown>,
    ) -> windows_core::Result<()> {
        unimplemented!();
    }

    fn DrawUnderline(
        &self,
        _clientdrawingcontext: *const core::ffi::c_void,
        _baselineoriginx: f32,
        _baselineoriginy: f32,
        _underline: *const DWRITE_UNDERLINE,
        _clientdrawingeffect: windows_core::Ref<IUnknown>,
    ) -> windows_core::Result<()> {
        unimplemented!();
    }
}

#[implement(ID2D1SimplifiedGeometrySink)]
struct GlyphOutlineSink<'a> {
    translate: windows_numerics::Vector2,
    dip_to_pixels_scale: f32,
    vrender: *mut VectorVertexRenderer<'a>,
}
impl GlyphOutlineSink<'_> {
    #[inline(always)]
    const fn make_vg_point(&self, p: &windows_numerics::Vector2) -> Point<VectorTextureUnit> {
        point_new_vector_texture(
            p.X * self.dip_to_pixels_scale + self.translate.X,
            -p.Y * self.dip_to_pixels_scale + self.translate.Y,
        )
    }
}
impl ID2D1SimplifiedGeometrySink_Impl for GlyphOutlineSink_Impl<'_> {
    fn BeginFigure(&self, startpoint: &windows_numerics::Vector2, figurebegin: D2D1_FIGURE_BEGIN) {
        assert_eq!(figurebegin, D2D1_FIGURE_BEGIN_FILLED, "not filled figure");

        unsafe { &mut *self.vrender }.move_to(self.make_vg_point(startpoint));
    }

    fn EndFigure(&self, figureend: D2D1_FIGURE_END) {
        if figureend != D2D1_FIGURE_END_CLOSED {
            tracing::warn!("figure end without D2D1_FIGURE_END_CLOSED?");
        }

        unsafe { &mut *self.vrender }.close();
    }

    fn AddLines(&self, points: *const windows_numerics::Vector2, pointscount: u32) {
        for p in unsafe { core::slice::from_raw_parts(points, pointscount as _) } {
            unsafe { &mut *self.vrender }.line_to(self.make_vg_point(p));
        }
    }

    fn AddBeziers(&self, beziers: *const D2D1_BEZIER_SEGMENT, bezierscount: u32) {
        for p in unsafe { core::slice::from_raw_parts(beziers, bezierscount as _) } {
            unsafe { &mut *self.vrender }.cubic_to(
                self.make_vg_point(&p.point1),
                self.make_vg_point(&p.point2),
                self.make_vg_point(&p.point3),
            );
        }
    }

    fn Close(&self) -> windows_core::Result<()> {
        #[cfg(debug_assertions)]
        if !unsafe { &mut *self.vrender }.is_figure_opening() {
            return Err(windows::Win32::Foundation::E_ILLEGAL_STATE_CHANGE.into());
        }

        Ok(())
    }

    fn SetFillMode(&self, fillmode: D2D1_FILL_MODE) {
        if fillmode != D2D1_FILL_MODE_WINDING {
            tracing::warn!("not winding fill mode specified");
        }
    }

    fn SetSegmentFlags(&self, vertexflags: D2D1_PATH_SEGMENT) {
        unimplemented!("SetSegmentFlags {vertexflags:?}")
    }
}
