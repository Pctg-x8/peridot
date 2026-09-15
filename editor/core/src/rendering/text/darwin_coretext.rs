//! Core Text(macOS) implementation of text visual services

use core::cell::UnsafeCell;
use std::collections::HashMap;

use apple_sdk_port::{
    AnyObject, Object, Owned,
    foundation::{
        AttributedString, AttributedStringKey, MutableAttributedString, MutableDictionary, Number,
        Range as NSRange, String as NSString,
    },
    graphics::Path,
    raw::{
        CGPoint, CGRect, CGSize, CTParagraphStyleSetting, CTTextAlignment,
        kCTParagraphStyleSpecifierAlignment, kCTTextAlignmentCenter, kCTTextAlignmentLeft,
        kCTTextAlignmentRight,
    },
    text::{Font as NativeFont, FontOrientation, Frame, Framesetter, ParagraphStyle},
};

use crate::{
    platform::mac::ak_spacing_inline_start,
    rendering::{
        MaskTextureAtlasManager,
        composite::CompositeRectTextHorizontalAlignment,
        text::{FontSet, GlyphPlacementBox},
        vg::{VectorRasterizationState, VectorVertexRenderer},
    },
    utils::{LogicalUnit, Point, Rect, Size},
};

pub struct CoreTextLayout {
    frame: Owned<Frame>,
    frame_size: CGSize,
}
impl CoreTextLayout {
    pub fn new<'s>(
        text_runs: impl Iterator<Item = super::TextRun<'s>>,
        font_set: &FontSet,
        alignment: CompositeRectTextHorizontalAlignment,
        max_width: Option<f32>,
    ) -> Self {
        let (lb, ub) = text_runs.size_hint();
        let mut as_runs = Vec::with_capacity(ub.unwrap_or(lb));
        let mut total_bytes = 0;
        for r in text_runs {
            let mut str_attr =
                MutableDictionary::<NSString, AnyObject>::new_copying_key_generic_value(None, 2)
                    .expect("str_attr.create");
            str_attr.set(
                AttributedStringKey::font(),
                font_set.select(r.font).as_any(),
            );
            str_attr.set(
                ak_spacing_inline_start(),
                Number::new_f32(None, r.spacing_inline_start)
                    .expect("spacing_inline_start.boxing")
                    .as_any(),
            );

            as_runs.push((
                AttributedString::new(
                    None,
                    &*unsafe { NSString::from_str_no_copy(None, r.content) },
                    Some(&str_attr),
                )
                .expect("str.create"),
                NSRange {
                    location: total_bytes as _,
                    // Note: replace_attributed_stringでAppend操作をするときはlengthを0にする必要がある
                    length: 0,
                },
            ));
            total_bytes += r.content.len();
        }

        let mut str = MutableAttributedString::new(None, total_bytes as _).expect("str.create");
        str.begin_editing();
        for (s, r) in as_runs {
            str.replace_attributed_string(r, &s);
        }
        // overwrite entire attributes
        let mut entire_attrs =
            MutableDictionary::<NSString, AnyObject>::new_copying_key_generic_value(None, 1)
                .expect("entire_attrs.create");
        let alignment_value = match alignment {
            CompositeRectTextHorizontalAlignment::Start => kCTTextAlignmentLeft,
            CompositeRectTextHorizontalAlignment::Middle => kCTTextAlignmentCenter,
            CompositeRectTextHorizontalAlignment::End => kCTTextAlignmentRight,
        };
        entire_attrs.set(
            AttributedStringKey::paragraph_style(),
            ParagraphStyle::new(&[CTParagraphStyleSetting {
                spec: kCTParagraphStyleSpecifierAlignment,
                value_size: size_of::<CTTextAlignment>(),
                value: core::ptr::from_ref(&alignment_value).cast(),
            }])
            .expect("paragraph_style.create")
            .as_any(),
        );
        let range = NSRange {
            location: 0,
            length: str.len(),
        };
        str.set_attributes(range, &entire_attrs, false);
        str.end_editing();

        let framesetter = Framesetter::from_attributed_string(&str).expect("framesetter.create");
        let framing_range = NSRange {
            location: 0,
            length: 0,
        };
        let frame_size = framesetter.suggest_frame_size_with_constraints(
            framing_range.clone(),
            None,
            CGSize {
                width: max_width.map_or(f64::MAX, |x| x as f64),
                height: f64::MAX,
            },
            None,
        );
        let frame = framesetter
            .create_frame(
                framing_range,
                &Path::new_rect(
                    CGRect {
                        origin: CGPoint { x: 0.0, y: 0.0 },
                        size: frame_size.clone(),
                    },
                    None,
                ),
                None,
            )
            .expect("frame.create");

        Self { frame, frame_size }
    }

    pub fn rasterize_and_place_glyphs(
        &self,
        vr_state: &mut VectorRasterizationState,
        atlas: &mut MaskTextureAtlasManager,
        scale: f32,
    ) -> Vec<GlyphPlacementBox> {
        let mut boxes = Vec::new();

        let lines = self.frame.lines();
        tracing::debug!(line_count = lines.len(), "frameset lines");
        let line_origins = {
            let mut sink = Vec::with_capacity(lines.len() as _);
            self.frame.line_origins(0, sink.spare_capacity_mut());
            unsafe {
                sink.set_len(lines.len() as _);
            }
            sink
        };
        let mut height = 0.0f32;
        for (l, lo) in lines.iter().zip(line_origins.iter()) {
            // y座標が逆（+yが上）なので補正する
            let mut ascender = core::mem::MaybeUninit::uninit();
            l.typographic_bounds(Some(&mut ascender), None, None);
            let render_y_offset =
                (self.frame_size.height - (lo.y + unsafe { ascender.assume_init() })) as f32;

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
                    NativeFont::ref_from_untyped_ptr(
                        attributes
                            .get_untyped_value(AttributedStringKey::font())
                            .expect("font not set?")
                            .as_ptr(),
                    )
                };
                fonts_per_run.push(font);

                let mut ascent = core::mem::MaybeUninit::uninit();
                let mut descent = core::mem::MaybeUninit::uninit();
                r.typographic_bounds(
                    apple_sdk_port::raw::CFRange {
                        location: 0,
                        length: 0,
                    },
                    Some(&mut ascent),
                    Some(&mut descent),
                    None,
                );
                let ascent = unsafe { ascent.assume_init() };
                let descent = unsafe { descent.assume_init() };

                baseline_pos = baseline_pos.max(ascent);
                // TODO: 複数行になる場合はleadingを行間に足す
                height = height.max((ascent + descent) as f32 * 2.0);

                if let Some(x) =
                    attributes.get_untyped_value(crate::platform::mac::ak_spacing_inline_start())
                {
                    inline_shifts += unsafe {
                        apple_sdk_port::foundation::Number::ref_from_untyped_ptr(x.as_ptr())
                            .f32_value()
                            .expect("invalid attr value")
                    }
                };
                accumulated_inline_shifts.push(inline_shifts);
            }

            for ((r, font), x_shift) in runs
                .iter()
                .zip(fonts_per_run)
                .zip(accumulated_inline_shifts)
            {
                let font_uniq_name = font
                    .copy_name(NativeFont::unique_name_key())
                    .or_else(|| font.copy_name(NativeFont::full_name_key()))
                    .expect("cannot determine font unique name");
                let font_size = font.size();
                let font_unique_id = FONT_UNIQUIFY_STORAGE
                    .with(|s| s.query(format!("{font_uniq_name:?}.{font_size:.2}")));

                let glyph_count = r.glyph_count();
                tracing::debug!(?font_uniq_name, font_size, count = glyph_count, "run");
                let mut glyph_bounding_rects = Vec::with_capacity(glyph_count as _);
                font.bounding_rects_for_glyphs(
                    FontOrientation::Horizontal,
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
                        (bounding_rect.size.width as f32 * scale).ceil() as _,
                        (bounding_rect.size.height as f32 * scale).ceil() as _,
                    );
                    boxes.push(GlyphPlacementBox {
                        left: ((lo.x + pos.x + bounding_rect.origin.x) as f32 + x_shift) * scale,
                        top: (render_y_offset
                            + (baseline_pos + pos.y
                                - (bounding_rect.size.height + bounding_rect.origin.y))
                                as f32)
                            * scale,
                        tex_left: r.left,
                        tex_top: r.top,
                        width: r.width(),
                        height: r.height(),
                    });

                    if is_new {
                        vr_state.updated_rects.push(r.vk_rect());

                        let offset_x = r.left as f32 - bounding_rect.origin.x as f32 * scale;
                        let offset_y = -(r.top as f32)
                            - (bounding_rect.size.height + bounding_rect.origin.y) as f32 * scale;
                        let mut vrender = VectorVertexRenderer::new(vr_state);
                        font.create_path_for_glyph(glyph, None)
                            .expect("font.create_path_for_glyph")
                            .apply(|e| match e.r#type {
                                apple_sdk_port::raw::kCGPathElementMoveToPoint => {
                                    let to = unsafe { &*e.points };

                                    vrender.move_to(Point::new_vector_texture(
                                        to.x as f32 * scale + offset_x,
                                        to.y as f32 * scale + offset_y,
                                    ));
                                }
                                apple_sdk_port::raw::kCGPathElementAddLineToPoint => {
                                    let to = unsafe { &*e.points };

                                    vrender.line_to(Point::new_vector_texture(
                                        to.x as f32 * scale + offset_x,
                                        to.y as f32 * scale + offset_y,
                                    ));
                                }
                                apple_sdk_port::raw::kCGPathElementAddQuadCurveToPoint => {
                                    let points =
                                        unsafe { core::slice::from_raw_parts(e.points, 2) };

                                    vrender.quadratic_to(
                                        Point::new_vector_texture(
                                            points[0].x as f32 * scale + offset_x,
                                            points[0].y as f32 * scale + offset_y,
                                        ),
                                        Point::new_vector_texture(
                                            points[1].x as f32 * scale + offset_x,
                                            points[1].y as f32 * scale + offset_y,
                                        ),
                                    );
                                }
                                apple_sdk_port::raw::kCGPathElementAddCurveToPoint => {
                                    let points =
                                        unsafe { core::slice::from_raw_parts(e.points, 3) };

                                    vrender.cubic_to(
                                        Point::new_vector_texture(
                                            points[0].x as f32 * scale + offset_x,
                                            points[0].y as f32 * scale + offset_y,
                                        ),
                                        Point::new_vector_texture(
                                            points[1].x as f32 * scale + offset_x,
                                            points[1].y as f32 * scale + offset_y,
                                        ),
                                        Point::new_vector_texture(
                                            points[2].x as f32 * scale + offset_x,
                                            points[2].y as f32 * scale + offset_y,
                                        ),
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

        boxes
    }

    pub const fn size(&self) -> Size<LogicalUnit> {
        Size::new_logical(self.frame_size.width as _, self.frame_size.height as _)
    }

    pub const fn height(&self) -> f32 {
        self.frame_size.height as _
    }

    pub fn measure_visual_width(&self) -> f32 {
        let mut width = 0.0f32;
        for l in self.frame.lines().iter() {
            // Note: inline spacingは常に0なので計算しない
            for r in l.glyph_runs().iter() {
                // r.attributes().apply_untyped_value(|key, value| {
                //     tracing::debug!(?key, ?value, "run attribute");
                // });
                let font = unsafe {
                    NativeFont::ref_from_untyped_ptr(
                        r.attributes()
                            .get_untyped_value(AttributedStringKey::font())
                            .expect("font not set?")
                            .as_ptr(),
                    )
                };

                let glyph_count = r.glyph_count();
                let positions =
                    unsafe { core::slice::from_raw_parts(r.positions(), glyph_count as _) };
                let mut glyph_bounding_rects = Vec::with_capacity(glyph_count as _);
                font.bounding_rects_for_glyphs(
                    FontOrientation::Horizontal,
                    unsafe { core::slice::from_raw_parts(r.glyphs_ptr(), glyph_count as _) },
                    glyph_bounding_rects.spare_capacity_mut(),
                );
                unsafe {
                    glyph_bounding_rects.set_len(glyph_count as _);
                }

                width = positions
                    .into_iter()
                    .zip(glyph_bounding_rects)
                    .map(|(p, r)| (p.x + r.origin.x) as f32 + (r.size.width as f32).ceil())
                    .fold(width, f32::max);
            }
        }

        width
    }

    pub fn measure_total_advances(&self) -> f32 {
        let mut left_cursor = 0.0 as f32;
        for l in self.frame.lines().iter() {
            // Note: inline shiftは常に0になるので計算しない
            let mut line_left_cursor = 0.0 as f32;
            for r in l.glyph_runs().iter() {
                // r.attributes().apply_untyped_value(|key, value| {
                //     tracing::debug!(?key, ?value, "run attribute");
                // });
                let font = unsafe {
                    NativeFont::ref_from_untyped_ptr(
                        r.attributes()
                            .get_untyped_value(AttributedStringKey::font())
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
                    .map(|(p, b)| (p.x + b.origin.x) as f32 + (b.size.width as f32).ceil())
                    .fold(line_left_cursor, f32::max);
            }

            left_cursor = left_cursor.max(line_left_cursor + l.trailing_whitespace_width() as f32);
        }

        left_cursor
    }

    pub fn measure_cursor_rect_at_end(&self, final_char_index: usize) -> Rect<LogicalUnit> {
        let lines = self.frame.lines();
        let mut line_origins = Vec::with_capacity(1);
        self.frame
            .line_origins(lines.len() - 1, line_origins.spare_capacity_mut());
        unsafe {
            line_origins.set_len(1);
        }

        tracing::debug!(line_count = lines.len());
        assert!(lines.len() >= 1, "measuring empty input?");

        let last_line = &lines[lines.len() - 1];
        let last_line_origin = &line_origins[0];
        let mut ascent = core::mem::MaybeUninit::uninit();
        let mut descent = core::mem::MaybeUninit::uninit();
        last_line.typographic_bounds(Some(&mut ascent), Some(&mut descent), None);
        let ascent = unsafe { ascent.assume_init() };
        let line_height = ascent + unsafe { descent.assume_init() };
        let last_offset = last_line.offset_for_string_index(final_char_index as _, None);
        return Rect::from_lt_size(
            Point::new_logical(
                (last_line_origin.x + last_offset) as _,
                (self.frame_size.height - (last_line_origin.y + ascent)) as _,
            ),
            Size::new_logical(0.0, line_height as _),
        );
    }

    pub fn compute_line_rects_for_range(
        &self,
        char_index_range: core::range::Range<usize>,
    ) -> Vec<Rect<LogicalUnit>> {
        let lines = self.frame.lines();
        let mut line_origins = Vec::with_capacity(lines.len() as _);
        self.frame
            .line_origins(0, line_origins.spare_capacity_mut());
        unsafe {
            line_origins.set_len(lines.len() as _);
        }

        let mut rects = Vec::new();
        for (l, origin) in lines.iter().zip(line_origins.iter()) {
            let sr = l.string_range();
            let overlapping_range = sr.location.max(char_index_range.start as i64)
                ..(sr.location + sr.length).min(char_index_range.end as i64);
            if overlapping_range.is_empty() {
                // not overlapping
                continue;
            }

            let mut ascent = core::mem::MaybeUninit::uninit();
            let mut descent = core::mem::MaybeUninit::uninit();
            l.typographic_bounds(Some(&mut ascent), Some(&mut descent), None);
            let ascent = unsafe { ascent.assume_init() };
            let descent = unsafe { descent.assume_init() };
            let line_height = ascent + descent;
            let o1 = l.offset_for_string_index(char_index_range.start as _, None);
            let o2 = l.offset_for_string_index(char_index_range.end as _, None);
            rects.push(Rect::from_lt_size(
                Point::new_logical(
                    (origin.x + o1) as _,
                    (self.frame_size.height - (origin.y + ascent)) as _,
                ),
                Size::new_logical((o2 - o1) as _, line_height as _),
            ));
        }

        rects
    }

    pub fn find_nearest_string_index_with_line_offset(
        &self,
        x: f32,
        y: f32,
    ) -> Option<(usize, usize)> {
        // CoreGraphicsの座標系が+Y上なので補正
        let p = apple_sdk_port::raw::CGPoint {
            x: x as _,
            y: (self.frame_size.height - y as f64).clamp(0.0, self.frame_size.height),
        };

        let lines = self.frame.lines();
        let mut line_origins = Vec::with_capacity(lines.len() as _);
        self.frame
            .line_origins(0, line_origins.spare_capacity_mut());
        unsafe {
            line_origins.set_len(lines.len() as _);
        }

        for (l, origin) in lines.iter().zip(line_origins.iter()) {
            let mut ascender = core::mem::MaybeUninit::uninit();
            let mut descender = core::mem::MaybeUninit::uninit();
            l.typographic_bounds(Some(&mut ascender), Some(&mut descender), None);
            let ascender = unsafe { ascender.assume_init() };
            let descender = unsafe { descender.assume_init() };

            let line_top = origin.y + ascender;
            let line_bottom = origin.y - descender;
            if line_top < p.y || p.y < line_bottom {
                // never hits on this line
                continue;
            }

            tracing::debug!(?p, ?origin, "hittest: test");
            if let Some(c) = l.string_index_for_position(apple_sdk_port::raw::CGPoint {
                x: p.x - origin.x,
                y: p.y - origin.y,
            }) {
                tracing::debug!(?p, c, "hittest: hit");
                return Some((c as _, l.string_range().location as _));
            }
        }

        // no hit found
        return None;
    }
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
