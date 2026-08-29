//! Text Rendering backend using FreeType2 + Harfbuzz(Generic OSS fallback)

use core::convert::identity;

use peridot_tp_budoux as budoux;
use peridot_tp_freetype::{
    LoadFlags, OutlineFuncs, Vector, get_char_index, load_glyph, outline_decompose,
};
use peridot_tp_harfbuzz::ffi::{
    hb_buffer_add_utf8, hb_buffer_create, hb_buffer_destroy, hb_buffer_get_glyph_infos,
    hb_buffer_get_glyph_positions, hb_buffer_guess_segment_properties, hb_buffer_t, hb_font_t,
    hb_glyph_info_t, hb_glyph_position_t, hb_shape,
};
use peridot_tp_icu as icu;

use crate::{
    rendering::{
        MaskTextureAtlasManager,
        composite::CompositeRectTextHorizontalAlignment,
        text::{FontID, FontSet, GlyphPlacementBox, TextRun},
        vg::{VectorRasterizationState, VectorTextureUnit, VectorVertexRenderer},
    },
    utils::{LogicalUnit, Point, Rect, Size, text::prev_char_byte},
};

pub struct TextLayout {
    lines: Vec<LineLayout>,
    height: f32,
}
impl TextLayout {
    pub fn new<'s>(
        text_runs: impl Iterator<Item = TextRun<'s>>,
        font_set: &FontSet,
        alignment: CompositeRectTextHorizontalAlignment,
        max_width: Option<f32>,
        max_lines: Option<usize>,
    ) -> Self {
        let mut lines = vec![LineLayout {
            buffers: Vec::new(),
            width_with_trailing_whitespace: 0.0,
            height: 0.0,
            line_top_offset: 0.0,
            baseline_y_offset: 0.0,
        }];
        let mut line_y_offset = 0.0f32;
        let mut line_height = 0.0f32;
        let mut left_offset = 0.0f32;
        let mut final_line_height = 0.0f32;
        if let Some(max_width) = max_width {
            // autowrapを適用しながら行を分割していく
            for x in text_runs {
                left_offset += x.spacing_inline_start;

                let font = font_set.select(x.font);
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
                                    .filter_map(identity),
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
                                    .filter_map(identity),
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
                    let mut local_baseline_y_offset = 0.0f32;
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
                                if unsafe { get_char_index(font.faces[face_index], c as _) } == 0 {
                                    // no char in font, needs fallback
                                    break;
                                }

                                shaped_bytes += c.len_utf8();
                            }

                            let byte_range = core::range::Range::from(starting_bytes..shaped_bytes);
                            if !byte_range.is_empty() {
                                // needs shaping
                                let face = font.faces[face_index];
                                let shaping_face = shaping_set.faces[face_index];

                                let buf =
                                    build_shaped_buffer(&b[byte_range], shaping_face.as_ptr());
                                let glyph_infos = buf.get_glyph_infos();
                                let glyph_positions = buf.get_glyph_positions();
                                assert_eq!(glyph_infos.len(), glyph_positions.len());

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
                                                    load_glyph(
                                                        face,
                                                        last_glyph.codepoint,
                                                        LoadFlags::DEFAULT,
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

                                section_buffers.push(ShapedTextRun {
                                    buffer: buf,
                                    char_offset_bytes: b[byte_range]
                                        .char_indices()
                                        .map(|(i, _)| byte_range.start + i)
                                        .chain(core::iter::once(byte_range.end))
                                        .collect(),
                                    left_offset: line_left_offset + section_left_offset,
                                    font_id: x.font,
                                    face_index,
                                });

                                // update metrics
                                let face_metrics = unsafe { &(*(*face).size).metrics };
                                local_baseline_y_offset = local_baseline_y_offset
                                    .max(face_metrics.ascender as f32 / 64.0);
                                section_line_height =
                                    section_line_height.max(face_metrics.height as f32 / 64.0);
                                // freetype2のdescenderは符号が逆になってるのでこれで正解
                                section_height = section_height.max(
                                    (face_metrics.ascender - face_metrics.descender) as f32 / 64.0,
                                );

                                section_left_offset += buf_total_advances;

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
                        if !lines.last_mut().expect("empty lines").buffers.is_empty()
                            && line_left_offset + section_visual_right > max_width
                        {
                            // overflow: this section should be placed on the next line.
                            lines.last_mut().expect("empty lines").baseline_y_offset +=
                                line_y_offset;
                            lines.last_mut().expect("empty_lines").height = final_line_height;
                            lines
                                .last_mut()
                                .expect("empty lines")
                                .width_with_trailing_whitespace = line_left_offset;

                            line_y_offset += core::mem::replace(&mut line_height, 0.0);
                            lines.push(LineLayout {
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

                        while section_visual_right > max_width {
                            // force line break/truncate
                            // tracing::warn!(
                            //     section_visual_right,
                            //     max_width,
                            //     content = b,
                            //     "todo: force line break"
                            // );

                            if let Some(max_lines) = max_lines
                                && lines.len() >= max_lines
                            {
                                // cannot wrap exceeding max_lines: truncate
                                let trimming_sign_face_index = 'find_trimming_sign_font: {
                                    for n in 0.. {
                                        if unsafe {
                                            get_char_index(
                                                font_set.select(FontID::UIDefault).faces[n],
                                                '\u{2026}' as _,
                                            )
                                        } != 0
                                        {
                                            // this font has the character!
                                            break 'find_trimming_sign_font n;
                                        }
                                    }

                                    todo!("fallback: no font have a horizontal ellipsis character");
                                };
                                let mut trimming_sign_buffer = Buffer::new();
                                trimming_sign_buffer.add("\u{2026}", 0);
                                trimming_sign_buffer.guess_segment_properties();
                                unsafe {
                                    hb_shape(
                                        font_set.select_shaping(FontID::UIDefault).faces
                                            [trimming_sign_face_index]
                                            .as_ptr(),
                                        trimming_sign_buffer.0.as_ptr(),
                                        core::ptr::null(),
                                        0,
                                    );
                                }
                                let trimming_sign_width = trimming_sign_buffer
                                    .get_glyph_positions()
                                    .iter()
                                    .fold(0.0, |a, p| a + p.x_advance as f32 / 64.0);

                                let last_line = lines.last_mut().expect("empty lines");
                                let pre_post_limit_width = (max_width - trimming_sign_width) / 2.0;
                                let pre_break_pos = 'brk: {
                                    while let Some(x) = section_buffers.first() {
                                        let pos = x.buffer.get_glyph_positions();
                                        let left_base = x.left_offset;
                                        let mut cursor = 0.0;
                                        for (p, &char_byte_offs) in
                                            pos.into_iter().zip(x.char_offset_bytes.iter())
                                        {
                                            if left_base + cursor >= pre_post_limit_width {
                                                // pre strip here
                                                break 'brk prev_char_byte(b, char_byte_offs);
                                            }

                                            cursor += p.x_advance as f32 / 64.0;
                                        }

                                        // このSectionBufferはbreakしなくていいので今の行に追加
                                        let unbreak_buffer = section_buffers.remove(0);
                                        last_line.buffers.push(unbreak_buffer);
                                    }

                                    unreachable!("not need to be truncated?")
                                };
                                let post_break_pos = 'brk: {
                                    // TODO: いったんleft_offsetは考えない(ちょっとややこしい)
                                    let mut cursor = 0.0;
                                    while let Some(x) = section_buffers.last() {
                                        let pos = x.buffer.get_glyph_positions();
                                        for (p, &char_byte_offs) in pos
                                            .into_iter()
                                            .rev()
                                            .zip(x.char_offset_bytes.iter().rev())
                                        {
                                            if cursor >= pre_post_limit_width {
                                                break 'brk char_byte_offs;
                                            }

                                            cursor += p.x_advance as f32 / 64.0;
                                        }

                                        // このSectionBufferはbreakしなくていいので今の行に追加(位置を調整する)
                                        let mut unbreak_buffer =
                                            section_buffers.pop().expect("empty section buffers");
                                        unbreak_buffer.left_offset = max_width - cursor;
                                        last_line.buffers.push(unbreak_buffer);
                                    }

                                    unreachable!("not need to be truncated?")
                                };
                                let pre_byte_range = core::range::Range::from(
                                    section_buffers[0].char_offset_bytes[0]..pre_break_pos,
                                );
                                let post_byte_range = core::range::Range::from(
                                    post_break_pos
                                        ..*section_buffers
                                            .last()
                                            .unwrap()
                                            .char_offset_bytes
                                            .last()
                                            .unwrap(),
                                );
                                let mut pre_buffer = Buffer::new();
                                let mut post_buffer = Buffer::new();
                                pre_buffer.add(&b[pre_byte_range], 0);
                                post_buffer.add(&b[post_byte_range], 0);
                                pre_buffer.guess_segment_properties();
                                post_buffer.guess_segment_properties();
                                unsafe {
                                    hb_shape(
                                        font_set.select_shaping(section_buffers[0].font_id).faces
                                            [section_buffers[0].face_index]
                                            .as_ptr(),
                                        pre_buffer.0.as_ptr(),
                                        core::ptr::null(),
                                        0,
                                    );
                                    hb_shape(
                                        font_set
                                            .select_shaping(section_buffers.last().unwrap().font_id)
                                            .faces[section_buffers.last().unwrap().face_index]
                                            .as_ptr(),
                                        post_buffer.0.as_ptr(),
                                        core::ptr::null(),
                                        0,
                                    );
                                }

                                let truncated_section_left = section_buffers[0].left_offset;
                                let pre_font_id = section_buffers[0].font_id;
                                let pre_face_index = section_buffers[0].face_index;
                                let post_font_id = section_buffers.last().unwrap().font_id;
                                let pos_face_index = section_buffers.last().unwrap().face_index;
                                let pre_width = pre_buffer
                                    .get_glyph_positions()
                                    .into_iter()
                                    .fold(0.0, |a, p| a + p.x_advance as f32 / 64.0);
                                section_buffers.clear();
                                section_buffers.extend([
                                    ShapedTextRun {
                                        buffer: pre_buffer,
                                        char_offset_bytes: b[pre_byte_range]
                                            .char_indices()
                                            .map(|(i, _)| pre_byte_range.start + i)
                                            .chain(core::iter::once(pre_byte_range.end))
                                            .collect(),
                                        left_offset: truncated_section_left,
                                        font_id: pre_font_id,
                                        face_index: pre_face_index,
                                    },
                                    ShapedTextRun {
                                        buffer: trimming_sign_buffer,
                                        char_offset_bytes: vec![
                                            pre_byte_range.end,
                                            post_byte_range.start,
                                        ],
                                        left_offset: truncated_section_left + pre_width,
                                        font_id: FontID::UIDefault,
                                        face_index: trimming_sign_face_index,
                                    },
                                    ShapedTextRun {
                                        buffer: post_buffer,
                                        char_offset_bytes: b[post_byte_range]
                                            .char_indices()
                                            .map(|(i, _)| post_byte_range.start + i)
                                            .chain(core::iter::once(post_byte_range.end))
                                            .collect(),
                                        left_offset: truncated_section_left
                                            + pre_width
                                            + trimming_sign_width,
                                        font_id: post_font_id,
                                        face_index: pos_face_index,
                                    },
                                ]);
                                // recompute metrics
                                for x in section_buffers.iter_mut() {
                                    let face = font_set.select(x.font_id).faces[x.face_index];

                                    let glyph_infos = x.buffer.get_glyph_infos();
                                    let glyph_positions = x.buffer.get_glyph_positions();
                                    assert_eq!(glyph_infos.len(), glyph_positions.len());
                                    let run_right = glyph_positions
                                        .iter()
                                        .map(|p| p.x_advance as f32 / 64.0)
                                        .sum::<f32>();

                                    let face_metrics = unsafe {
                                        &(*(*font_set.select(x.font_id).faces[x.face_index]).size)
                                            .metrics
                                    };
                                    local_baseline_y_offset = local_baseline_y_offset
                                        .max(face_metrics.ascender as f32 / 64.0);
                                    section_line_height =
                                        section_line_height.max(face_metrics.height as f32 / 64.0);
                                    section_height = section_height.max(
                                        (face_metrics.ascender - face_metrics.descender) as f32
                                            / 64.0,
                                    );
                                    section_visual_right = section_visual_right.max(
                                        section_left_offset
                                            + match (glyph_positions, glyph_infos) {
                                                (&[], &[]) => 0.0,
                                                (&[ref pos @ .., _], &[.., ref last_glyph]) => {
                                                    unsafe {
                                                        load_glyph(
                                                            face,
                                                            last_glyph.codepoint,
                                                            LoadFlags::DEFAULT,
                                                        )
                                                        .expect("ft.load_glyph");
                                                    }
                                                    let metrics =
                                                        unsafe { &(*(*face).glyph).metrics };
                                                    pos.iter()
                                                        .map(|x| x.x_advance as f32 / 64.0)
                                                        .sum::<f32>()
                                                        + metrics.width as f32 / 64.0
                                                }
                                                _ => unreachable!(),
                                            },
                                    );
                                    section_left_offset = run_right;
                                }

                                break;
                            }

                            let last_line = lines.last_mut().expect("empty lines");
                            // TODO: RTL support
                            let mut break_pos = 0;
                            'brk: while let Some(x) = section_buffers.get(0) {
                                let pos = x.buffer.get_glyph_positions();
                                let left_base = x.left_offset;
                                let mut cursor = 0.0;
                                for (p, &char_byte_offs) in
                                    pos.into_iter().zip(x.char_offset_bytes.iter())
                                {
                                    let char_left = left_base + cursor;
                                    if char_left > max_width {
                                        // tracing::debug!(
                                        //     char_left,
                                        //     pre = &b[..char_byte_offs],
                                        //     post = &b[char_byte_offs..],
                                        //     "force line break candidate here"
                                        // );
                                        // これの一つ前で切る
                                        break_pos = prev_char_byte(b, char_byte_offs);
                                        break 'brk;
                                    }
                                    cursor += p.x_advance as f32 / 64.0;
                                }

                                // このSectionBufferはbreakしなくていいので今の行に追加
                                let unbreak_buffer = section_buffers.remove(0);
                                last_line.buffers.push(unbreak_buffer);
                            }
                            // tracing::debug!(break_pos, "force line break");

                            // 前後に分ける
                            let current_head_buffer_start_byte =
                                section_buffers[0].char_offset_bytes[0];
                            let current_head_buffer_end_byte = section_buffers[0]
                                .char_offset_bytes
                                .last()
                                .copied()
                                .expect("empty char offset bytes");
                            let face = font_set.select(section_buffers[0].font_id).faces
                                [section_buffers[0].face_index];
                            let shaping_font = font_set
                                .select_shaping(section_buffers[0].font_id)
                                .faces[section_buffers[0].face_index];
                            let mut pre_buffer = Buffer::new();
                            let mut post_buffer = Buffer::new();
                            pre_buffer.add(&b[current_head_buffer_start_byte..break_pos], 0);
                            post_buffer.add(&b[break_pos..current_head_buffer_end_byte], 0);
                            pre_buffer.guess_segment_properties();
                            post_buffer.guess_segment_properties();
                            unsafe {
                                hb_shape(
                                    shaping_font.as_ptr(),
                                    pre_buffer.0.as_ptr(),
                                    core::ptr::null(),
                                    0,
                                );
                                hb_shape(
                                    shaping_font.as_ptr(),
                                    post_buffer.0.as_ptr(),
                                    core::ptr::null(),
                                    0,
                                );
                            }

                            // pre_bufferはそのまま現在の行に入れる
                            let glyph_infos = pre_buffer.get_glyph_infos();
                            let glyph_positions = pre_buffer.get_glyph_positions();
                            assert_eq!(glyph_infos.len(), glyph_positions.len());
                            let run_right = section_buffers[0].left_offset
                                + glyph_positions
                                    .iter()
                                    .map(|p| p.x_advance as f32 / 64.0)
                                    .sum::<f32>();
                            last_line.buffers.push(ShapedTextRun {
                                buffer: pre_buffer,
                                char_offset_bytes: b[current_head_buffer_start_byte..break_pos]
                                    .char_indices()
                                    .map(|(i, _)| current_head_buffer_start_byte + i)
                                    .chain(core::iter::once(break_pos))
                                    .collect(),
                                left_offset: section_buffers[0].left_offset,
                                font_id: section_buffers[0].font_id,
                                face_index: section_buffers[0].face_index,
                            });

                            // update metrics
                            let face_metrics = unsafe { &(*(*face).size).metrics };
                            local_baseline_y_offset =
                                local_baseline_y_offset.max(face_metrics.ascender as f32 / 64.0);
                            section_line_height =
                                section_line_height.max(face_metrics.height as f32 / 64.0);
                            // freetype2のdescenderは符号が逆になってるのでこれで正解
                            section_height = section_height.max(
                                (face_metrics.ascender - face_metrics.descender) as f32 / 64.0,
                            );
                            line_left_offset = run_right;

                            // 新規行発行
                            last_line.baseline_y_offset = last_line.line_top_offset
                                + last_line
                                    .baseline_y_offset
                                    .max(core::mem::replace(&mut local_baseline_y_offset, 0.0));
                            last_line.height = last_line
                                .height
                                .max(core::mem::replace(&mut section_height, 0.0));
                            last_line.width_with_trailing_whitespace = last_line
                                .width_with_trailing_whitespace
                                .max(core::mem::replace(&mut line_left_offset, 0.0));
                            line_y_offset += core::mem::replace(&mut section_line_height, 0.0);
                            lines.push(LineLayout {
                                buffers: Vec::new(),
                                width_with_trailing_whitespace: 0.0,
                                height: 0.0,
                                line_top_offset: line_y_offset,
                                baseline_y_offset: 0.0,
                            });

                            let glyph_infos = post_buffer.get_glyph_infos();
                            let glyph_positions = post_buffer.get_glyph_positions();
                            assert_eq!(glyph_infos.len(), glyph_positions.len());
                            let run_right = glyph_positions
                                .iter()
                                .map(|p| p.x_advance as f32 / 64.0)
                                .sum::<f32>();

                            // post_bufferはsection_buffersの0を置き替える
                            section_buffers[0].buffer = post_buffer;
                            section_buffers[0].char_offset_bytes = b
                                [break_pos..current_head_buffer_end_byte]
                                .char_indices()
                                .map(|(i, _)| break_pos + i)
                                .chain(core::iter::once(current_head_buffer_end_byte))
                                .collect();
                            let right_shifts = section_buffers[0].left_offset;
                            section_buffers[0].left_offset = 0.0;

                            // update metrics
                            let face_metrics = unsafe { &(*(*face).size).metrics };
                            local_baseline_y_offset = face_metrics.ascender as f32 / 64.0;
                            section_line_height = face_metrics.height as f32 / 64.0;
                            // freetype2のdescenderは符号が逆になってるのでこれで正解
                            section_height =
                                (face_metrics.ascender - face_metrics.descender) as f32 / 64.0;
                            section_visual_right = section_buffers[0].left_offset
                                + section_buffers[0].visual_width(font_set);
                            section_left_offset = run_right;

                            // 後ろにいるsection_bufferの位置を調整
                            for x in section_buffers[1..].iter_mut() {
                                x.left_offset -= right_shifts;

                                // update metrics
                                let glyph_infos = x.buffer.get_glyph_infos();
                                let glyph_positions = x.buffer.get_glyph_positions();
                                assert_eq!(glyph_infos.len(), glyph_positions.len());
                                let run_right = glyph_positions
                                    .iter()
                                    .map(|p| p.x_advance as f32 / 64.0)
                                    .sum::<f32>();

                                let face_metrics = unsafe {
                                    &(*(*font_set.select(x.font_id).faces[x.face_index]).size)
                                        .metrics
                                };
                                local_baseline_y_offset = local_baseline_y_offset
                                    .max(face_metrics.ascender as f32 / 64.0);
                                section_line_height =
                                    section_line_height.max(face_metrics.height as f32 / 64.0);
                                section_height = section_height.max(
                                    (face_metrics.ascender - face_metrics.descender) as f32 / 64.0,
                                );
                                section_visual_right = section_visual_right
                                    .max(section_left_offset + x.visual_width(font_set));
                                section_left_offset = run_right;
                            }
                        }

                        let last_line = lines.last_mut().expect("empty lines");
                        last_line.buffers.extend(section_buffers);
                        last_line.baseline_y_offset =
                            last_line.baseline_y_offset.max(local_baseline_y_offset);
                        line_left_offset += section_left_offset;
                        line_height = line_height.max(section_line_height);
                        final_line_height = final_line_height.max(section_height);
                    }

                    if n < line_count - 1 {
                        // newline

                        // confirm current line
                        let cl = lines.last_mut().expect("empty lines");
                        cl.baseline_y_offset += line_y_offset;
                        cl.height = core::mem::replace(&mut final_line_height, 0.0);
                        cl.width_with_trailing_whitespace =
                            core::mem::replace(&mut line_left_offset, 0.0);

                        line_y_offset += core::mem::replace(&mut line_height, 0.0);
                        lines.push(LineLayout {
                            buffers: Vec::new(),
                            width_with_trailing_whitespace: 0.0,
                            height: 0.0,
                            line_top_offset: line_y_offset,
                            baseline_y_offset: 0.0,
                        });
                    }

                    left_offset = line_left_offset;
                }
            }

            lines.last_mut().expect("empty lines").baseline_y_offset += line_y_offset;
            lines
                .last_mut()
                .expect("empty lines")
                .width_with_trailing_whitespace = left_offset;
            lines.last_mut().expect("empty lines").height = final_line_height;
        } else {
            // no max width(no autowrapping): optimal path
            let mut local_baseline_y_offset = 0.0f32;
            for x in text_runs {
                left_offset += x.spacing_inline_start;

                let font = font_set.select(x.font);
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

                        if unsafe { get_char_index(font.faces[face_index], c as _) } == 0 {
                            // no char in font, needs fallback
                            break;
                        }

                        shaped_bytes += c.len_utf8();
                    }

                    let shaped_range = core::range::Range::from(starting_bytes..shaped_bytes);
                    if !shaped_range.is_empty() {
                        // needs shaping
                        any_shaped_on_line = true;
                        let face = font.faces[face_index];

                        let buf = build_shaped_buffer(
                            &x.content[shaped_range],
                            shaping_set.faces[face_index].as_ptr(),
                        );
                        let glyph_infos = buf.get_glyph_infos();
                        let glyph_positions = buf.get_glyph_positions();
                        assert_eq!(glyph_infos.len(), glyph_positions.len());
                        let buf_total_advances = glyph_positions
                            .iter()
                            .map(|p| p.x_advance as f32 / 64.0)
                            .sum::<f32>();

                        let last_line = lines.last_mut().expect("empty lines");
                        last_line.buffers.push(ShapedTextRun {
                            buffer: buf,
                            char_offset_bytes: x.content[shaped_range]
                                .char_indices()
                                .map(|(i, _)| shaped_range.start + i)
                                .chain(core::iter::once(shaped_range.end))
                                .collect(),
                            left_offset,
                            font_id: x.font,
                            face_index,
                        });

                        // update metrics
                        let face_metrics = unsafe { &(*(*face).size).metrics };
                        local_baseline_y_offset =
                            local_baseline_y_offset.max(face_metrics.ascender as f32 / 64.0);
                        line_height = line_height.max(face_metrics.height as f32 / 64.0);
                        // freetype2のdescenderは符号が逆になってるのでこれで正解
                        final_line_height = final_line_height
                            .max((face_metrics.ascender - face_metrics.descender) as f32 / 64.0);
                        left_offset += buf_total_advances;

                        // reset for next chunk
                        face_index = 0;
                    } else {
                        // no chars available for this font, fallback
                        face_index += 1;
                    }

                    if newline_break {
                        // broke actual run by newline
                        if !core::mem::replace(&mut any_shaped_on_line, false) {
                            // no chars processed on the line
                            // もろもろのmetricsが0なのでいいかんじに計算する
                            let face_metrics = unsafe { &(*(*font.faces[0]).size).metrics };

                            local_baseline_y_offset = face_metrics.ascender as f32 / 64.0;
                            line_height = face_metrics.height as f32 / 64.0;
                            final_line_height =
                                (face_metrics.ascender - face_metrics.descender) as f32 / 64.0;
                        }

                        // confirm last line
                        let last_line = lines.last_mut().expect("empty lines");
                        last_line.baseline_y_offset = last_line.line_top_offset
                            + core::mem::replace(&mut local_baseline_y_offset, 0.0);
                        last_line.width_with_trailing_whitespace =
                            core::mem::replace(&mut left_offset, 0.0);
                        last_line.height = core::mem::replace(&mut final_line_height, 0.0);
                        line_y_offset += core::mem::replace(&mut line_height, 0.0);

                        lines.push(LineLayout {
                            buffers: Vec::new(),
                            width_with_trailing_whitespace: 0.0,
                            height: 0.0,
                            line_top_offset: line_y_offset,
                            baseline_y_offset: 0.0,
                        });

                        shaped_bytes += 1;
                        // reset for next chunk(next chunk may use the face index prior to current)
                        face_index = 0;
                    }
                }
            }

            let last_line = lines.last_mut().expect("empty lines");
            last_line.baseline_y_offset = last_line.line_top_offset + local_baseline_y_offset;
            last_line.width_with_trailing_whitespace = left_offset;
            last_line.height = final_line_height;
        }

        // apply per-line alignment
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
            lines,
            height: line_y_offset + final_line_height,
        }
    }

    pub fn rasterize_and_place_glyphs(
        &self,
        font_set: &FontSet,
        vr_state: &mut VectorRasterizationState,
        atlas: &mut MaskTextureAtlasManager,
        scale: f32,
    ) -> Vec<GlyphPlacementBox> {
        let mut boxes = Vec::new();

        for l in self.lines.iter() {
            for x in l.buffers.iter() {
                let font = font_set.select(x.font_id).faces[x.face_index];
                let glyph_infos = x.buffer.get_glyph_infos();
                let glyph_positions = x.buffer.get_glyph_positions();
                assert_eq!(glyph_infos.len(), glyph_positions.len());

                let mut left_cursor = x.left_offset;
                for (glyph_info, glyph_position) in
                    glyph_infos.into_iter().zip(glyph_positions.into_iter())
                {
                    unsafe {
                        load_glyph(font, glyph_info.codepoint, LoadFlags::DEFAULT)
                            .expect("face.load_glyph")
                    };
                    let metrics = unsafe { &(*(*font).glyph).metrics };
                    let glyph_width = scale * metrics.width as f32 / 64.0;
                    let glyph_height = scale * metrics.height as f32 / 64.0;

                    let (r, is_new) = atlas.acquire_for_glyph(
                        (font as _, glyph_info.codepoint as _),
                        glyph_width.ceil() as _,
                        glyph_height.ceil() as _,
                    );
                    boxes.push(GlyphPlacementBox {
                        left: (left_cursor
                            + (glyph_position.x_offset as f32 + metrics.horiBearingX as f32)
                                / 64.0)
                            * scale,
                        top: (l.baseline_y_offset - metrics.horiBearingY as f32 / 64.0) * scale,
                        tex_left: r.left,
                        tex_top: r.top,
                        width: r.width(),
                        height: r.height(),
                    });

                    if is_new {
                        vr_state.updated_rects.push(r.vk_rect());

                        unsafe {
                            outline_decompose(
                                &mut (*(*font).glyph).outline,
                                &mut OutlineReceiver {
                                    vrender: VectorVertexRenderer::new(vr_state),
                                    scale,
                                    offset_x: r.left as f32
                                        - scale * metrics.horiBearingX as f32 / 64.0,
                                    offset_y: -(r.top as f32)
                                        - scale * metrics.horiBearingY as f32 / 64.0,
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

        boxes
    }

    pub fn measure_visual_width(&self, font_set: &FontSet) -> f32 {
        self.lines
            .iter()
            .map(|x| x.visual_width(font_set))
            .fold(0.0, f32::max)
    }

    pub fn measure_width_with_trailing_whitespace(&self) -> f32 {
        self.lines
            .iter()
            .map(|x| x.width_with_trailing_whitespace)
            .fold(0.0, f32::max)
    }

    pub const fn height(&self) -> f32 {
        self.height
    }

    pub fn find_nearest_byte_pos(&self, x: f32, y: f32) -> usize {
        // TODO: RTLサポート
        let mut bytes = 0;
        for (n, l) in self.lines.iter().enumerate() {
            if n == 0 {
                // first line check
                if let Some(next_line) = self.lines.get(n + 1)
                    && next_line.line_top_offset < y
                {
                    // never across with this line
                    bytes = l
                        .buffers
                        .iter()
                        .map(|x| *x.char_offset_bytes.last().expect("never empty"))
                        .max()
                        .unwrap_or(bytes)
                        + 1;
                    continue;
                }
            } else if n == self.lines.len() - 1 {
                // last line check
                if y < l.line_top_offset {
                    // never across with this line
                    bytes = l
                        .buffers
                        .iter()
                        .map(|x| *x.char_offset_bytes.last().expect("never empty"))
                        .max()
                        .unwrap_or(bytes)
                        + 1;
                    continue;
                }
            } else {
                let next_line_top = self
                    .lines
                    .get(n + 1)
                    .map_or(l.line_top_offset + l.height, |l| l.line_top_offset);
                if y < l.line_top_offset || next_line_top < y {
                    // never across with this line
                    bytes = l
                        .buffers
                        .iter()
                        .map(|x| *x.char_offset_bytes.last().expect("never empty"))
                        .max()
                        .unwrap_or(bytes)
                        + 1;
                    continue;
                }
            }

            for tr in l.buffers.iter() {
                let glyph_positions = tr.buffer.get_glyph_positions();
                let mut left_cursor = tr.left_offset;
                let mut bytes_iter = tr.char_offset_bytes.iter().copied();
                bytes = bytes_iter.next().expect("never empty");
                for glyph_pos in glyph_positions {
                    let left = left_cursor;
                    let right = left + glyph_pos.x_advance as f32 / 64.0;
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
                        return bytes_iter.next().expect("reached to the end");
                    }

                    left_cursor += glyph_pos.x_advance as f32 / 64.0;
                    bytes = bytes_iter.next().expect("reached to the end");
                }
            }

            // beyond
            return bytes;
        }

        unreachable!();
    }

    pub fn measure_cursor_rect_at_end(&self, font_set: &FontSet) -> Rect<LogicalUnit> {
        let Some(last_line) = self.lines.last() else {
            // Default font fallback
            let face_line_height = unsafe {
                let metrics = &(*(*font_set.select(FontID::UIDefault).faces[0]).size).metrics;

                (metrics.ascender + metrics.descender) as f32 / 64.0
            };

            return Rect::from_lt_size(
                Point::new_logical(0.0, 0.0),
                Size::new_logical(0.0, face_line_height),
            );
        };

        let last_line_height = if last_line.height == 0.0 {
            // Default font fallback
            unsafe {
                let metrics = &(*(*font_set.select(FontID::UIDefault).faces[0]).size).metrics;

                (metrics.ascender + metrics.descender) as f32 / 64.0
            }
        } else {
            last_line.height
        };

        Rect::from_lt_size(
            Point::new_logical(
                last_line.width_with_trailing_whitespace,
                last_line.line_top_offset,
            ),
            Size::new_logical(0.0, last_line_height),
        )
    }

    pub fn measure_line_rects(&self, range: core::range::Range<usize>) -> Vec<Rect<LogicalUnit>> {
        // TODO: RTLサポート
        let mut rects = Vec::new();
        for l in self.lines.iter() {
            let mut line_min_x = f32::MAX;
            let mut line_max_x = 0.0f32;

            for tr in l.buffers.iter() {
                let overlapping_range = tr
                    .char_offset_bytes
                    .first()
                    .copied()
                    .expect("never empty")
                    .max(range.start)
                    ..tr.char_offset_bytes
                        .last()
                        .copied()
                        .expect("never empty")
                        .min(range.end);
                if overlapping_range.is_empty() {
                    // not overlapping
                    continue;
                }

                let glyph_positions = tr.buffer.get_glyph_positions();
                let mut left_cursor = tr.left_offset;
                for (glyph_position, &bytepos) in
                    glyph_positions.iter().zip(tr.char_offset_bytes.iter())
                {
                    if range.contains(&bytepos) {
                        line_min_x = line_min_x.min(left_cursor);
                        line_max_x =
                            line_max_x.max(left_cursor + glyph_position.x_advance as f32 / 64.0);
                    }

                    left_cursor += glyph_position.x_advance as f32 / 64.0;
                }
            }

            if line_min_x < line_max_x {
                rects.push(Rect::from_lt_size(
                    Point::new_logical(line_min_x, l.line_top_offset),
                    Size::new_logical(line_max_x - line_min_x, l.height),
                ));
            }
        }

        rects
    }
}

struct OutlineReceiver<'r> {
    vrender: VectorVertexRenderer<'r>,
    scale: f32,
    offset_x: f32,
    offset_y: f32,
}
impl OutlineReceiver<'_> {
    #[inline(always)]
    const fn make_point(&self, v: &Vector) -> Point<VectorTextureUnit> {
        Point::new_vector_texture(
            self.scale * v.x as f32 / 64.0 + self.offset_x,
            self.scale * v.y as f32 / 64.0 + self.offset_y,
        )
    }
}
impl OutlineFuncs for OutlineReceiver<'_> {
    #[inline(always)]
    fn move_to(&mut self, to: &Vector) {
        self.vrender.move_to(self.make_point(to));
    }

    #[inline(always)]
    fn line_to(&mut self, to: &Vector) {
        self.vrender.line_to(self.make_point(to));
    }

    #[inline(always)]
    fn conic_to(&mut self, control: &Vector, to: &Vector) {
        self.vrender
            .quadratic_to(self.make_point(control), self.make_point(to));
    }

    #[inline(always)]
    fn cubic_to(&mut self, control1: &Vector, control2: &Vector, to: &Vector) {
        self.vrender.cubic_to(
            self.make_point(control1),
            self.make_point(control2),
            self.make_point(to),
        );
    }
}

pub struct ShapedTextRun {
    buffer: Buffer,
    /// final element = end of byte range
    char_offset_bytes: Vec<usize>,
    left_offset: f32,
    font_id: FontID,
    face_index: usize,
}
impl ShapedTextRun {
    pub fn visual_width(&self, font_set: &FontSet) -> f32 {
        let glyph_infos = self.buffer.get_glyph_infos();
        let glyph_positions = self.buffer.get_glyph_positions();
        assert_eq!(glyph_infos.len(), glyph_positions.len());

        match (glyph_positions, glyph_infos) {
            (&[ref advances @ .., _], &[.., ref last_glyph]) => {
                let face = font_set.select(self.font_id).faces[self.face_index];

                unsafe {
                    load_glyph(face, last_glyph.codepoint, LoadFlags::DEFAULT)
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

pub struct LineLayout {
    buffers: Vec<ShapedTextRun>,
    width_with_trailing_whitespace: f32,
    height: f32,
    line_top_offset: f32,
    baseline_y_offset: f32,
}
impl LineLayout {
    pub fn visual_width(&self, font_set: &FontSet) -> f32 {
        self.buffers
            .iter()
            // compute visual right of each buffers
            .map(|tr| tr.left_offset + tr.visual_width(font_set))
            .fold(0.0, f32::max)
    }
}

fn build_shaped_buffer(text: &str, font: *mut hb_font_t) -> Buffer {
    let mut b = Buffer::new();
    b.add(text, 0);
    b.guess_segment_properties();
    unsafe {
        hb_shape(font, b.0.as_ptr(), core::ptr::null(), 0);
    }

    b
}

#[repr(transparent)]
pub struct Buffer(core::ptr::NonNull<hb_buffer_t>);
impl Drop for Buffer {
    #[inline(always)]
    fn drop(&mut self) {
        unsafe {
            hb_buffer_destroy(self.0.as_ptr());
        }
    }
}
impl Buffer {
    #[inline(always)]
    pub fn new() -> Self {
        Self(unsafe { core::ptr::NonNull::new_unchecked(hb_buffer_create()) })
    }

    #[inline(always)]
    pub fn add(&mut self, content: &str, offset: core::ffi::c_uint) {
        unsafe {
            hb_buffer_add_utf8(
                self.0.as_ptr(),
                content.as_ptr().cast(),
                content.len() as _,
                offset,
                -1,
            );
        }
    }

    #[inline(always)]
    pub fn guess_segment_properties(&mut self) {
        unsafe { hb_buffer_guess_segment_properties(self.0.as_ptr()) }
    }

    #[inline(always)]
    pub fn get_glyph_infos<'a>(&'a self) -> &'a [hb_glyph_info_t] {
        let mut len = core::mem::MaybeUninit::uninit();
        let ptr = unsafe { hb_buffer_get_glyph_infos(self.0.as_ptr(), len.as_mut_ptr()) };
        unsafe { core::slice::from_raw_parts(ptr, len.assume_init() as _) }
    }

    #[inline(always)]
    pub fn get_glyph_positions<'a>(&'a self) -> &'a [hb_glyph_position_t] {
        let mut len = core::mem::MaybeUninit::uninit();
        let ptr = unsafe { hb_buffer_get_glyph_positions(self.0.as_ptr(), len.as_mut_ptr()) };
        unsafe { core::slice::from_raw_parts(ptr, len.assume_init() as _) }
    }
}
