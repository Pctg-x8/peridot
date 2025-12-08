use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
};

use bedrock::{
    self as br, AnyRenderPassCreateInfo, CommandBufferMut, CommandPoolMut, DescriptorPoolMut,
    Device, Image, ImageChild, RenderPass, ShaderModule, SubmissionBatch, TypedVulkanStructure,
    VkHandle, VulkanStructure,
};
use peridot::math::Zero;
use peridot_command_object::{
    BeginRenderPass, Blending, BufferUsage, ColorAttachmentBlending, EndRenderPass,
    GraphicsCommand, GraphicsCommandCombiner, GraphicsCommandSubmission, NextSubpass,
    PipelineBarrier, PushConstant, RangedBuffer, RangedImage, SetViewportScissors,
    SimpleDrawIndexed, StandardIndexedMesh, StandardMesh,
};
use peridot_vertex_processing_pack::{PvpContainer, PvpShaderModules};
use peridot_vg::{Font, FontProvider, FontProviderConstruct};

#[repr(C)]
pub struct Vertex {
    pub pos: peridot::math::Vector2<f32>,
}
#[repr(C)]
#[derive(Clone)]
pub struct BoxInstance {
    pub pos_st: peridot::math::Vector4<f32>,
    pub uv_st: peridot::math::Vector4<f32>,
    pub col: peridot::math::Vector4<f32>,
}

#[repr(C)]
pub struct CameraParameterUniformBlockData {
    pub view_projection_matrix: peridot::math::Matrix4F32,
    pub target_pixel_size: peridot::math::Vector2F32,
}

#[repr(C)]
pub struct ObjectParameterUniformBlockData {
    pub transform_matrix: peridot::math::Matrix4F32,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UIElementSize {
    Fixed(f32),
    Percent(f32),
    Fill,
    FitContent,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LayoutSize {
    Unscaled,
    Scaled,
    Fixed(f32),
}

#[derive(Clone, Copy, Debug)]
pub enum Overflow {
    Wrap,
    Hidden,
    Overflow,
}

#[derive(Clone, Copy, Debug)]
pub enum LayoutDirection {
    Normal,
    Reverse,
}

#[derive(Clone, Copy, Debug)]
pub enum LayoutJustify {
    Start,
    End,
    Center,
    SpaceBetween,
    SpaceAround,
}

#[derive(Clone, Copy, Debug)]
pub enum LayoutAlignment {
    Start,
    End,
    Center,
    Baseline,
}

pub enum GridCellSize {
    Fixed(f32),
    Flexible(f32),
    FitContent,
}

pub struct RectEdge<T> {
    pub left: T,
    pub right: T,
    pub top: T,
    pub bottom: T,
}
impl<T> RectEdge<T> {
    #[inline]
    pub const fn all(value: T) -> Self
    where
        T: Copy,
    {
        Self {
            left: value,
            right: value,
            top: value,
            bottom: value,
        }
    }

    #[inline]
    pub const fn lt(&self) -> peridot::math::Vector2<T>
    where
        T: Copy,
    {
        peridot::math::Vector2(self.left, self.top)
    }

    #[inline]
    pub const fn rb(&self) -> peridot::math::Vector2<T>
    where
        T: Copy,
    {
        peridot::math::Vector2(self.right, self.bottom)
    }
}

pub enum ChildrenLayoutMode {
    Free,
    Vertical {
        direction: LayoutDirection,
        justify: LayoutJustify,
        alignment: LayoutAlignment,
        overflow: Overflow,
        gap: f32,
    },
    Horizontal {
        direction: LayoutDirection,
        justify: LayoutJustify,
        alignment: LayoutAlignment,
        overflow: Overflow,
        gap: f32,
    },
    Grid {
        columns: Vec<GridCellSize>,
        rows: Vec<GridCellSize>,
        column_alignment: LayoutAlignment,
        row_alignment: LayoutAlignment,
        gap: f32,
    },
}

pub struct TextFontData {
    internal: peridot_vg::DefaultFont,
    ref_font_size: f32,
    sdf_max_distance_pixels: u32,
    glyph_atlas: RefCell<GlyphAtlas>,
}
impl TextFontData {
    pub fn new(
        internal: peridot_vg::DefaultFont,
        mm: &mut peridot_memory_manager::MemoryManager,
        g: &mut peridot::Graphics,
        glyph_atlas_init_size: u32,
        sdf_max_distance_pixels: u32,
    ) -> Self {
        Self {
            ref_font_size: internal.size(),
            internal,
            sdf_max_distance_pixels,
            glyph_atlas: RefCell::new(GlyphAtlas::new(
                mm,
                g,
                glyph_atlas_init_size,
                glyph_atlas_init_size,
            )),
        }
    }

    pub fn request_char(&self, c: char, size: f32) -> CharacterData {
        let glyph_id = self.internal.glyph_id(c).expect("font.glyph_id failed");
        let bounds = self.internal.bounds(&glyph_id).expect("font.bounds failed");

        CharacterData {
            width: bounds.size.width * size / self.ref_font_size,
            height: bounds.size.height * size / self.ref_font_size,
            left_offset: bounds.min_x() * size / self.ref_font_size,
            top_offset: 0.0,
            advance_x: self
                .internal
                .advance_h(&glyph_id)
                .expect("font.advance_h failed")
                * size
                / self.ref_font_size,
            ascend: self.internal.ascent() * size / self.ref_font_size,
        }
    }

    pub fn ensure_char_bitmap(&self, c: char) -> GlyphRect {
        let glyph_id = self.internal.glyph_id(c).expect("font.glyph_id failed");
        let bounds = self.internal.bounds(&glyph_id).expect("font.bounds failed");

        let gr = self
            .glyph_atlas
            .borrow_mut()
            .alloc(
                glyph_id,
                bounds.size.width.ceil() as u32 + self.sdf_max_distance_pixels * 2,
                bounds.size.height.ceil() as u32 + self.sdf_max_distance_pixels * 2,
            )
            .expect("glyph_atlas.alloc failed");
        println!("glyph atlas rect: {c} {gr:?}");

        gr
    }

    pub fn rasterize_dirty_glyphs(
        &self,
        e: &mut peridot::Engine<impl peridot::NativeLinker>,
        mm: &mut peridot_memory_manager::MemoryManager,
    ) {
        let mut locked_atlas = self.glyph_atlas.borrow_mut();
        let &mut GlyphAtlas {
            ref mut dirty_glyphs,
            ref glyphs,
            width,
            height,
            ref tex_view,
            ..
        } = &mut *locked_atlas;
        let mut renderer = TwoPassStencilSDFRenderer::new(
            e,
            br::vk::VK_FORMAT_R8_UNORM,
            br::ImageLayout::ShaderReadOnlyOpt,
            br::PipelineStageFlags::FRAGMENT_SHADER,
            br::Extent2D { width, height },
            self.sdf_max_distance_pixels as _,
        );
        let stencil_buffer = mm
            .allocate_device_local_image(
                e.graphics(),
                br::ImageCreateInfo::new(br::Extent2D { width, height }, br::vk::VK_FORMAT_S8_UINT)
                    .set_usage(
                        br::ImageUsageFlags::DEPTH_STENCIL_ATTACHMENT
                            | br::ImageUsageFlags::TRANSIENT_ATTACHMENT,
                    ),
            )
            .expect("alloc stencil buffer");
        let stencil_buffer_view = LocalImageView {
            handle: unsafe {
                br::vkfn_wrapper::create_image_view(
                    e.graphics().device().native_ptr(),
                    &br::ImageViewCreateInfo::new(
                        &stencil_buffer,
                        br::ImageSubresourceRange::new(br::AspectMask::STENCIL, 0..1, 0..1),
                        br::vk::VK_IMAGE_VIEW_TYPE_2D,
                        br::vk::VK_FORMAT_S8_UINT,
                    ),
                    None,
                )
                .expect("create stencil buffer view")
            },
            device: e.graphics().device().clone(),
        };
        let rt = TwoPassStencilSDFRenderTarget::new(
            e.graphics().device(),
            &renderer,
            tex_view.as_transparent_ref(),
            stencil_buffer_view.as_transparent_ref(),
            br::Extent2D { width, height },
        );

        for x in dirty_glyphs.drain() {
            use peridot_vg::FlatPathBuilder;

            let gr = &glyphs[&x];
            println!("rasterize glyph: {x:?} {gr:?}");

            let metric = self.internal.bounds(&x).expect("no glyph contained");
            println!("metric: {metric:?}");
            let mut gen = peridot_vg::SDFGenerator::new(1.0, self.sdf_max_distance_pixels as _);
            self.internal
                .outline(
                    &x,
                    &peridot_vg::sdf_generator::Transform2D::create_translation(
                        -metric.origin.x + self.sdf_max_distance_pixels as f32,
                        -metric.origin.y - self.sdf_max_distance_pixels as f32,
                    ),
                    &mut gen,
                )
                .expect("font.outline failed");
            let figure_vertices = gen.build();
            if figure_vertices.is_empty() {
                // nothing rasterization needed
                continue;
            }

            renderer.update_render_area(br::Rect2D {
                offset: br::Offset2D {
                    x: gr.left as _,
                    y: gr.top as _,
                },
                extent: br::Extent2D {
                    width: gr.width,
                    height: gr.height,
                },
            });

            let (
                figure_fill_triangle_points_count,
                figure_fill_triangle_indices_count,
                figure_curve_triangles_count,
                outline_rects_count,
            ) = figure_vertices
                .iter()
                .fold((0, 0, 0, 0), |(t, t2, t3, t4), f| {
                    (
                        t + f.fill_triangle_points.len(),
                        t2 + f.fill_triangle_indices.len(),
                        t3 + f.curve_triangles.len(),
                        t4 + f.parabola_rects.len(),
                    )
                });

            let mut bp = peridot::BufferPrealloc::new(e.graphics());
            let flip_fill_rect = bp.add(peridot::BufferContent::vertex::<
                [peridot::math::Vector2<f32>; 4],
            >());
            let figures_fill_triangle_points_offset =
                bp.add(peridot::BufferContent::vertices::<
                    peridot::math::Vector2<f32>,
                >(figure_fill_triangle_points_count));
            let figures_fill_triangle_indices_offset = bp.add(
                peridot::BufferContent::indices::<u16>(figure_fill_triangle_indices_count),
            );
            let figure_curve_triangles_offset = bp.add(peridot::BufferContent::vertices::<
                peridot::VertexUV2D,
            >(figure_curve_triangles_count));
            let outline_rects_offset = bp.add(peridot::BufferContent::vertices::<
                peridot_vg::sdf_generator::ParabolaRectVertex,
            >(outline_rects_count * 6));

            let buffer = peridot::mthelper::SharedRef::new(
                mm.allocate_device_local_buffer(
                    e.graphics(),
                    bp.build_desc().with_usage(br::BufferUsage::TRANSFER_DEST),
                )
                .expect("alloc buffer"),
            );
            let mut init_buffer: RangedBuffer<_> = mm
                .allocate_upload_buffer(
                    e.graphics(),
                    bp.build_desc_custom_usage(br::BufferUsage::TRANSFER_SRC),
                )
                .expect("alloc init buffer")
                .into();
            init_buffer
                .0
                .guard_map(peridot_memory_manager::BufferMapMode::Write, |m| unsafe {
                    m.clone_slice_to(
                        flip_fill_rect as _,
                        &[
                            peridot::math::Vector2(0.0f32, 0.0),
                            peridot::math::Vector2(1.0, 0.0),
                            peridot::math::Vector2(0.0, -1.0),
                            peridot::math::Vector2(1.0, -1.0),
                        ],
                    );

                    let s = m.slice_mut(
                        figures_fill_triangle_points_offset as _,
                        figure_fill_triangle_points_count,
                    );
                    let si = m.slice_mut(
                        figures_fill_triangle_indices_offset as _,
                        figure_fill_triangle_indices_count,
                    );
                    let c = m.slice_mut(
                        figure_curve_triangles_offset as _,
                        figure_curve_triangles_count,
                    );
                    let o = m.slice_mut(outline_rects_offset as _, outline_rects_count * 6);
                    let (mut s_offset, mut si_offset, mut c_offset, mut o_offset) = (0, 0, 0, 0);
                    for f in figure_vertices.iter() {
                        s[s_offset..s_offset + f.fill_triangle_points.len()]
                            .clone_from_slice(&f.fill_triangle_points);
                        si[si_offset..si_offset + f.fill_triangle_indices.len()]
                            .copy_from_slice(&f.fill_triangle_indices);
                        c[c_offset..c_offset + f.curve_triangles.len()]
                            .clone_from_slice(&f.curve_triangles);
                        for pr in f.parabola_rects.iter() {
                            o[o_offset..o_offset + 6].clone_from_slice(&pr.make_vertices());
                            o_offset += 6;
                        }
                        s_offset += f.fill_triangle_points.len();
                        si_offset += f.fill_triangle_indices.len();
                        c_offset += f.curve_triangles.len();
                    }
                })
                .expect("write init buffer");

            {
                let all_buffer = RangedBuffer::from(&*buffer);
                let stencil_buffer = RangedImage::single_stencil_plane(&stencil_buffer);

                let copy = all_buffer.byref_mirror_from(&init_buffer);

                let [all_buffer_in_barrier, all_buffer_out_barrier] =
                    all_buffer.clone().usage_barrier3(
                        BufferUsage::UNUSED,
                        BufferUsage::TRANSFER_DST,
                        BufferUsage::VERTEX_BUFFER | BufferUsage::INDEX_BUFFER,
                    );
                let in_barriers = [
                    init_buffer
                        .make_ref()
                        .usage_barrier(BufferUsage::HOST_RW, BufferUsage::TRANSFER_SRC),
                    all_buffer_in_barrier,
                ];
                let out_barriers = PipelineBarrier::new()
                    .with_barrier(all_buffer_out_barrier)
                    .with_barrier(
                        stencil_buffer
                            .barrier(br::ImageLayout::DepthStencilReadOnlyOpt.from_undefined()),
                    )
                    .by_region();

                copy.between(in_barriers, out_barriers)
                    .submit(e)
                    .expect("Failed to initialize resources");
            }

            let figures_fill_triangle_points_buffer = RangedBuffer::from_offset_length(
                buffer.clone(),
                figures_fill_triangle_points_offset,
                core::mem::size_of::<peridot::math::Vector2<f32>>()
                    * figure_fill_triangle_points_count,
            );
            let figures_fill_triangle_indices_buffer = RangedBuffer::from_offset_length(
                buffer.clone(),
                figures_fill_triangle_indices_offset,
                core::mem::size_of::<u16>() * figure_fill_triangle_indices_count,
            );
            let figures_curve_triangles_buffer = RangedBuffer::from_offset_length(
                buffer.clone(),
                figure_curve_triangles_offset,
                core::mem::size_of::<peridot::VertexUV2D>() * figure_curve_triangles_count,
            );
            let outline_rects_buffer = RangedBuffer::from_offset_length(
                buffer.clone(),
                outline_rects_offset,
                core::mem::size_of::<peridot_vg::sdf_generator::ParabolaRectVertex>()
                    * outline_rects_count,
            );
            let flip_fill_rect_buffer = RangedBuffer::for_type::<[peridot::math::Vector2<f32>; 4]>(
                buffer,
                flip_fill_rect as _,
            );

            let fill_triangle_groups: Vec<_> = figure_vertices
                .iter()
                .map(|f| {
                    (
                        f.fill_triangle_points.len() as u32,
                        f.fill_triangle_indices.len() as u32,
                    )
                })
                .collect();
            let buffers = TwoPassStencilSDFRendererBuffers {
                fill_triangle_mesh: StandardIndexedMesh {
                    vertex_buffers: vec![figures_fill_triangle_points_buffer],
                    index_buffer: figures_fill_triangle_indices_buffer,
                    index_type: br::IndexType::U16,
                    vertex_count: 0, // ignored value
                },
                fill_triangle_groups,
                curve_triangles_mesh: StandardMesh {
                    vertex_buffers: vec![figures_curve_triangles_buffer],
                    vertex_count: figure_curve_triangles_count as _,
                },
                outline_rects_mesh: StandardMesh {
                    vertex_buffers: vec![outline_rects_buffer],
                    vertex_count: (outline_rects_count * 6) as _,
                },
                invert_fill_rect_mesh: StandardMesh {
                    vertex_buffers: vec![flip_fill_rect_buffer],
                    vertex_count: 4,
                },
            };
            e.submit_commands(|rec| renderer.commands(&rt, &buffers).execute(rec))
                .expect("submit commands");
        }
    }
}

struct CharacterData {
    pub width: f32,
    pub height: f32,
    pub left_offset: f32,
    pub top_offset: f32,
    pub advance_x: f32,
    pub ascend: f32,
}

pub struct UIElement<'s> {
    pub size: peridot::math::Vector2<UIElementSize>,
    pub scale: peridot::math::Vector2<f32>,
    pub offset: peridot::math::Vector2<f32>,
    pub margin: RectEdge<f32>,
    pub padding: RectEdge<f32>,
    pub layout_width: LayoutSize,
    pub layout_height: LayoutSize,
    pub layout_alignment_override: Option<LayoutAlignment>,
    pub column_alignment_override: Option<LayoutAlignment>,
    pub row_alignment_override: Option<LayoutAlignment>,
    pub children_layout: ChildrenLayoutMode,
    pub debug_color: peridot::math::Vector4<f32>,
    pub font: Option<&'s TextFontData>,
    pub text: &'s str,
    pub children: Vec<UIElement<'s>>,
}
impl Default for UIElement<'_> {
    fn default() -> Self {
        Self {
            size: peridot::math::Vector2(UIElementSize::FitContent, UIElementSize::FitContent),
            scale: peridot::math::Vector2(1.0, 1.0),
            offset: peridot::math::Vector2(0.0, 0.0),
            margin: RectEdge::all(0.0),
            padding: RectEdge::all(0.0),
            layout_width: LayoutSize::Unscaled,
            layout_height: LayoutSize::Unscaled,
            layout_alignment_override: None,
            column_alignment_override: None,
            row_alignment_override: None,
            children_layout: ChildrenLayoutMode::Free,
            debug_color: peridot::math::Vector4(0.0, 0.0, 0.0, 0.0),
            font: None,
            text: "",
            children: Vec::new(),
        }
    }
}

pub enum LayoutStatePlacementBase {
    LeftTop,
    RightTop,
    LeftBottom,
    RightBottom,
}

pub struct LayoutState {
    global_content_offset: peridot::math::Vector2<f32>,
    placement_base: LayoutStatePlacementBase,
    available_content_size: peridot::math::Vector2<f32>,
}

pub struct LayoutResult {
    layout_size: peridot::math::Vector2<f32>,
}

pub enum InstantiatedGridCellSize {
    Fixed(f32),
    Flexible(f32),
}

#[derive(Clone, PartialEq)]
pub struct LayoutRect {
    pos: peridot::math::Vector2<f32>,
    size: peridot::math::Vector2<f32>,
}
impl LayoutRect {
    #[inline]
    pub fn r#move(mut self, offset: peridot::math::Vector2<f32>) -> Self {
        self.pos += offset;
        self
    }

    #[inline(always)]
    pub const fn width(&self) -> f32 {
        self.size.0
    }

    #[inline(always)]
    pub const fn height(&self) -> f32 {
        self.size.1
    }

    #[inline(always)]
    pub fn right(&self) -> f32 {
        self.pos.0 + self.size.0
    }

    #[inline(always)]
    pub fn bottom(&self) -> f32 {
        self.pos.1 + self.size.1
    }

    #[inline(always)]
    pub fn max_point(&self) -> peridot::math::Vector2<f32> {
        peridot::math::Vector2(self.right(), self.bottom())
    }
}

fn compute_layout_rect(
    target: &UIElement,
    available_size: Option<peridot::math::Vector2<f32>>,
) -> LayoutRect {
    let pos = target.offset;
    let inner_content_size = if target.size.0 == UIElementSize::FitContent
        || target.size.1 == UIElementSize::FitContent
    {
        // どっちかがFitContentなら参照されるので計算しておく
        match target.children_layout {
            ChildrenLayoutMode::Free => target
                .children
                .iter()
                .map(|c| compute_layout_rect(c, None).max_point())
                .fold(peridot::math::Vector2::ZERO, peridot::math::Vector2::max),
            ChildrenLayoutMode::Vertical { overflow, gap, .. } => {
                let mut max_right = 0.0f32;
                let mut max_bottom = 0.0f32;
                for c in target.children.iter() {
                    let child_layout = compute_layout_rect(c, None);

                    max_right = max_right.max(child_layout.right());
                    max_bottom += child_layout.bottom() + gap;
                }

                peridot::math::Vector2(max_right, max_bottom - gap)
            }
            ChildrenLayoutMode::Horizontal { overflow, gap, .. } => {
                match target.size.0 {
                    UIElementSize::FitContent => {
                        // Note: 配置方向がFitContentの場合はoverflowしないはずなので(中身のサイズに外側が合うようになるから)処理しない
                        let mut max_right = 0.0f32;
                        let mut max_bottom = 0.0f32;
                        for c in target.children.iter() {
                            let child_layout = compute_layout_rect(c, None);

                            max_right += child_layout.width() + gap;
                            max_bottom = max_bottom.max(child_layout.height());
                        }

                        peridot::math::Vector2(max_right - gap, max_bottom)
                    }
                    UIElementSize::Fill => {
                        let mut max_right = 0.0f32;
                        let mut max_bottom = 0.0f32;
                        let mut row_height = 0.0f32;
                        let mut wrapped = false;
                        for c in target.children.iter() {
                            let child_layout = compute_layout_rect(c, None);
                            if available_size
                                .is_some_and(|s| max_right + child_layout.width() > s.0)
                            {
                                // overflowしそう

                                match overflow {
                                    Overflow::Wrap => {
                                        // 改行
                                        max_right = 0.0;
                                        max_bottom += row_height + gap;
                                        row_height = 0.0;
                                        wrapped = true;
                                    }
                                    Overflow::Hidden => (),
                                    Overflow::Overflow => (),
                                }
                            }

                            max_right += child_layout.width() + gap;
                            row_height = row_height.max(child_layout.height());
                        }

                        max_bottom += row_height;

                        peridot::math::Vector2(
                            if wrapped {
                                available_size.map_or(0.0, |x| x.0)
                            } else {
                                max_right - gap
                            },
                            max_bottom,
                        )
                    }
                    UIElementSize::Percent(r) => {
                        let mut max_right = 0.0f32;
                        let mut max_bottom = 0.0f32;
                        let mut row_height = 0.0f32;
                        let mut wrapped = false;
                        let available_width = available_size.map(|x| x.0 * r / 100.0);
                        for c in target.children.iter() {
                            let child_layout = compute_layout_rect(c, None);
                            if available_width.is_some_and(|s| max_right + child_layout.width() > s)
                            {
                                // overflowしそう

                                match overflow {
                                    Overflow::Wrap => {
                                        // 改行
                                        max_right = 0.0;
                                        max_bottom += row_height + gap;
                                        row_height = 0.0;
                                        wrapped = true;
                                    }
                                    Overflow::Hidden => (),
                                    Overflow::Overflow => (),
                                }
                            }

                            max_right += child_layout.width() + gap;
                            row_height = row_height.max(child_layout.height());
                        }

                        max_bottom += row_height;

                        peridot::math::Vector2(
                            if wrapped {
                                available_size.map_or(0.0, |x| x.0)
                            } else {
                                max_right - gap
                            },
                            max_bottom,
                        )
                    }
                    UIElementSize::Fixed(available_width) => {
                        let mut max_right = 0.0f32;
                        let mut max_bottom = 0.0f32;
                        let mut row_height = 0.0f32;
                        let mut wrapped = false;
                        for c in target.children.iter() {
                            let child_layout = compute_layout_rect(c, None);
                            if max_right + child_layout.width() > available_width {
                                // overflowしそう

                                match overflow {
                                    Overflow::Wrap => {
                                        // 改行
                                        max_right = 0.0;
                                        max_bottom += row_height + gap;
                                        row_height = 0.0;
                                        wrapped = true;
                                    }
                                    Overflow::Hidden => (),
                                    Overflow::Overflow => (),
                                }
                            }

                            max_right += child_layout.width() + gap;
                            row_height = row_height.max(child_layout.height());
                        }

                        max_bottom += row_height;

                        peridot::math::Vector2(
                            if wrapped {
                                available_size.map_or(0.0, |x| x.0)
                            } else {
                                max_right - gap
                            },
                            max_bottom,
                        )
                    }
                }
            }
            ChildrenLayoutMode::Grid {
                ref columns,
                ref rows,
                gap,
                ..
            } => {
                let mut max_right = 0.0f32;
                let mut row_right = 0.0f32;
                let mut accum_bottom = 0.0f32;
                let mut row_bottom = 0.0f32;
                let mut current_column = 0;
                let mut current_row = 0;
                for c in target.children.iter() {
                    if current_column >= columns.len() {
                        current_column = 0;
                        max_right = max_right.max(row_right);
                        accum_bottom += row_bottom + gap;
                        row_bottom = 0.0;
                        row_right = 0.0;
                        current_row += 1;
                    }

                    if current_column > 0 {
                        row_right += gap;
                    }

                    let cell_content_rect = compute_layout_rect(c, None);
                    row_right += match columns[current_column] {
                        GridCellSize::FitContent => cell_content_rect.right(),
                        GridCellSize::Fixed(x) => x,
                        // 一旦計算しない（あとでやる）
                        GridCellSize::Flexible(_) => 0.0,
                    };
                    row_bottom = row_bottom.max(match rows[current_row] {
                        GridCellSize::FitContent => cell_content_rect.bottom(),
                        GridCellSize::Fixed(x) => x,
                        // 一旦計算しない（あとでやる）
                        GridCellSize::Flexible(_) => 0.0,
                    });

                    current_column += 1;
                }

                peridot::math::Vector2(max_right.max(row_right), accum_bottom + row_bottom)
            }
        }
    } else {
        peridot::math::Vector2::ZERO
    };

    let content_size = peridot::math::Vector2(
        match target.size.0 {
            UIElementSize::Fill => available_size.map_or(0.0, |s| s.0),
            UIElementSize::Percent(p) => available_size.map_or(0.0, |s| s.0) * p / 100.0,
            UIElementSize::Fixed(x) => x,
            UIElementSize::FitContent => {
                inner_content_size.0 + target.padding.left + target.padding.right
            }
        },
        match target.size.1 {
            UIElementSize::Fill => available_size.map_or(0.0, |s| s.1),
            UIElementSize::Percent(p) => available_size.map_or(0.0, |s| s.1) * p / 100.0,
            UIElementSize::Fixed(x) => x,
            UIElementSize::FitContent => {
                inner_content_size.1 + target.padding.top + target.padding.bottom
            }
        },
    );

    LayoutRect {
        pos,
        size: peridot::math::Vector2(
            match target.layout_width {
                LayoutSize::Unscaled => content_size.0,
                LayoutSize::Scaled => content_size.0 * target.scale.0,
                LayoutSize::Fixed(x) => x,
            },
            match target.layout_height {
                LayoutSize::Unscaled => content_size.1,
                LayoutSize::Scaled => content_size.1 * target.scale.1,
                LayoutSize::Fixed(x) => x,
            },
        ),
    }
}

fn compute_vertical_alignment_axis_offset(
    available_width: f32,
    element_width: f32,
    alignment: LayoutAlignment,
) -> f32 {
    match alignment {
        LayoutAlignment::Start => 0.0,
        LayoutAlignment::End => available_width - element_width,
        LayoutAlignment::Center => (available_width - element_width) * 0.5,
        // VerticalのAlignmentはBaselineが取れないのでEndと同じ扱いにする
        LayoutAlignment::Baseline => available_width - element_width,
    }
}

fn compute_horizontal_alignment_axis_offset(
    available_height: f32,
    element_height: f32,
    alignment: LayoutAlignment,
) -> f32 {
    match alignment {
        LayoutAlignment::Start => 0.0,
        LayoutAlignment::End => available_height - element_height,
        LayoutAlignment::Center => (available_height - element_height) * 0.5,
        // TODO: Baseline Alignment(フォント指定がない場合はEndとおなじ扱いにする)
        LayoutAlignment::Baseline => available_height - element_height,
    }
}

#[inline]
fn apply_layout_rects<'e, 's>(
    targets: impl Iterator<Item = &'e UIElement<'s>>,
    layout_rects: impl Iterator<Item = LayoutRect>,
    boxes: &mut BoxInstanceEmitter,
) where
    's: 'e,
{
    for (c, r) in targets.zip(layout_rects) {
        layout1(c, boxes, r);
    }
}

pub trait HorizontalJustifyMethod {
    fn horizontal_justify(
        &self,
        row_layout_rects: impl ExactSizeIterator<Item = (LayoutRect, LayoutAlignment)>,
        content_total_width: f32,
        available_width: f32,
    ) -> impl Iterator<Item = (LayoutRect, LayoutAlignment)>;
}

pub struct HorizontalJustifyEnd {
    pub gap: f32,
}
impl HorizontalJustifyMethod for HorizontalJustifyEnd {
    #[inline]
    fn horizontal_justify(
        &self,
        row_layout_rects: impl ExactSizeIterator<Item = (LayoutRect, LayoutAlignment)>,
        content_total_width: f32,
        available_width: f32,
    ) -> impl Iterator<Item = (LayoutRect, LayoutAlignment)> {
        let space = available_width
            - (content_total_width + self.gap * (row_layout_rects.len() - 1) as f32);

        row_layout_rects.scan(space, move |offset, (r, a)| {
            let o = *offset;
            *offset += r.width() + self.gap;

            Some((r.r#move(peridot::math::Vector2(o, 0.0)), a))
        })
    }
}

pub struct HorizontalJustifyCenter {
    pub gap: f32,
}
impl HorizontalJustifyMethod for HorizontalJustifyCenter {
    #[inline]
    fn horizontal_justify(
        &self,
        row_layout_rects: impl ExactSizeIterator<Item = (LayoutRect, LayoutAlignment)>,
        content_total_width: f32,
        available_width: f32,
    ) -> impl Iterator<Item = (LayoutRect, LayoutAlignment)> {
        let space = available_width
            - (content_total_width + self.gap * (row_layout_rects.len() - 1) as f32);

        row_layout_rects.scan(space * 0.5f32, move |offset, (r, a)| {
            let o = *offset;
            *offset += r.width() + self.gap;

            Some((r.r#move(peridot::math::Vector2(o, 0.0)), a))
        })
    }
}

pub struct HorizontalJustifySpaceBetween {
    pub min_gap: f32,
}
impl HorizontalJustifyMethod for HorizontalJustifySpaceBetween {
    fn horizontal_justify(
        &self,
        row_layout_rects: impl ExactSizeIterator<Item = (LayoutRect, LayoutAlignment)>,
        content_total_width: f32,
        available_width: f32,
    ) -> impl Iterator<Item = (LayoutRect, LayoutAlignment)> {
        let space = available_width - content_total_width;
        let new_gap = match row_layout_rects.len() {
            x if x <= 1 => self.min_gap,
            x => (space / (x - 1) as f32).max(self.min_gap),
        };

        row_layout_rects.scan(0.0f32, move |left, (r, a)| {
            let place_left = *left;
            *left += r.width() + new_gap;

            Some((r.r#move(peridot::math::Vector2(place_left, 0.0)), a))
        })
    }
}

pub struct HorizontalJustifySpaceAround {
    pub min_gap: f32,
}
impl HorizontalJustifyMethod for HorizontalJustifySpaceAround {
    fn horizontal_justify(
        &self,
        row_layout_rects: impl ExactSizeIterator<Item = (LayoutRect, LayoutAlignment)>,
        content_total_width: f32,
        available_width: f32,
    ) -> impl Iterator<Item = (LayoutRect, LayoutAlignment)> {
        let space = available_width - content_total_width;
        let new_gap = space / (row_layout_rects.len() + 1) as f32;
        let (new_gap, offset) = if new_gap < self.min_gap {
            // min_gapを最低保証にしたいので、均等割がそれ以下になったら両端のスペースを作らないようにする
            (self.min_gap, 0.0)
        } else {
            (new_gap, new_gap)
        };

        row_layout_rects.scan(offset, move |offset, (r, a)| {
            let offs = *offset;
            *offset += r.width() + new_gap;

            Some((r.r#move(peridot::math::Vector2(offs, 0.0)), a))
        })
    }
}

fn layout_horizontal_justify_per_row<'e, 's>(
    elements_ordered: impl Iterator<Item = &'e UIElement<'s>>,
    alignment: LayoutAlignment,
    overflow: Overflow,
    gap: f32,
    global_rect: &LayoutRect,
    justify: impl HorizontalJustifyMethod,
) -> Vec<LayoutRect>
where
    's: 'e,
{
    let (lb, ub) = elements_ordered.size_hint();
    let mut layout_rects = Vec::with_capacity(ub.unwrap_or(lb));
    let mut row_rects = Vec::<(LayoutRect, LayoutAlignment)>::new();

    let mut content_total_width = 0.0f32;
    let mut row_height = 0.0f32;
    let mut content_y_offset = 0.0f32;
    let mut available_content_size = global_rect.size;
    for e in elements_ordered {
        let child_layout = compute_layout_rect(e, Some(available_content_size));
        let content_width = child_layout.width();

        if available_content_size.0 < content_width {
            // overflowしそう
            match overflow {
                Overflow::Wrap => {
                    layout_rects.extend(
                        justify
                            .horizontal_justify(
                                row_rects.drain(..),
                                content_total_width,
                                global_rect.size.0,
                            )
                            .map(|(r, a)| {
                                let yoffs = compute_horizontal_alignment_axis_offset(
                                    row_height,
                                    r.height(),
                                    a,
                                );

                                r.r#move(global_rect.pos + peridot::math::Vector2(0.0, yoffs))
                            }),
                    );

                    available_content_size.0 = global_rect.size.0;
                    available_content_size.1 -= row_height + gap;
                    content_y_offset += row_height + gap;
                    content_total_width = 0.0;
                    row_height = 0.0;
                }
                Overflow::Hidden | Overflow::Overflow => (),
            }
        }

        row_height = row_height.max(child_layout.height());
        row_rects.push((
            child_layout.r#move(peridot::math::Vector2(0.0, content_y_offset)),
            e.layout_alignment_override.unwrap_or(alignment),
        ));
        content_total_width += content_width;
        available_content_size.0 -= content_width + gap;
    }

    layout_rects.extend(
        justify
            .horizontal_justify(row_rects.drain(..), content_total_width, global_rect.size.0)
            .map(|(r, a)| {
                let yoffs = compute_horizontal_alignment_axis_offset(row_height, r.height(), a);

                r.r#move(global_rect.pos + peridot::math::Vector2(0.0, yoffs))
            }),
    );
    layout_rects
}

fn layout_horizontal<'e, 's>(
    elements_ordered: impl Iterator<Item = &'e UIElement<'s>>,
    alignment: LayoutAlignment,
    overflow: Overflow,
    gap: f32,
    global_rect: &LayoutRect,
) -> Vec<LayoutRect>
where
    's: 'e,
{
    let (lb, ub) = elements_ordered.size_hint();
    let mut layout_rects = Vec::with_capacity(ub.unwrap_or(lb));
    let mut row_rects = Vec::<LayoutRect>::new();

    let mut available_content_size = global_rect.size;
    let mut global_content_offset = global_rect.pos;
    let mut row_height = 0.0f32;

    for c in elements_ordered {
        let child_layout = compute_layout_rect(c, Some(available_content_size));
        let content_width = child_layout.width();

        if available_content_size.0 < content_width {
            // overflowしそう
            match overflow {
                Overflow::Wrap => {
                    // 改行
                    layout_rects.extend(row_rects.drain(..).map(|r| {
                        let yoffs = compute_horizontal_alignment_axis_offset(
                            row_height,
                            r.height(),
                            alignment,
                        );

                        r.r#move(peridot::math::Vector2(0.0, yoffs))
                    }));

                    global_content_offset.0 = global_rect.pos.0;
                    global_content_offset.1 += row_height + gap;
                    available_content_size.0 = global_rect.size.0;
                    available_content_size.1 -= row_height + gap;
                    row_height = 0.0;
                }
                Overflow::Hidden | Overflow::Overflow => (),
            }
        }

        row_height = row_height.max(child_layout.height());
        row_rects.push(child_layout.r#move(global_content_offset));
        global_content_offset.0 += content_width + gap;
        available_content_size.0 -= content_width + gap;
    }

    layout_rects.extend(row_rects.drain(..).map(|r| {
        let yoffs = compute_horizontal_alignment_axis_offset(row_height, r.height(), alignment);

        r.r#move(peridot::math::Vector2(0.0, yoffs))
    }));
    layout_rects
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BoxGroupTexture {
    User(usize),
    GlyphAtlas,
}

struct BoxGroup {
    texture: BoxGroupTexture,
    instances: Vec<BoxInstance>,
}

struct BoxInstanceEmitter {
    groups: Vec<BoxGroup>,
}
impl BoxInstanceEmitter {
    pub fn new() -> Self {
        Self { groups: Vec::new() }
    }

    pub fn emit(&mut self, tex: BoxGroupTexture, r#box: BoxInstance) -> (usize, usize) {
        if self.groups.is_empty() {
            // first emit
            self.groups.push(BoxGroup {
                texture: tex,
                instances: vec![r#box],
            });

            return (0, 0);
        }

        let active_group = self.groups.last_mut().unwrap();
        if active_group.texture == tex {
            // same render state: batching
            active_group.instances.push(r#box);
            let box_index = active_group.instances.len() - 1;
            return (self.groups.len() - 1, box_index);
        }

        self.groups.push(BoxGroup {
            texture: tex,
            instances: vec![r#box],
        });
        (self.groups.len() - 1, 0)
    }
}

fn layout1(target: &UIElement, boxes: &mut BoxInstanceEmitter, layout_rect: LayoutRect) {
    if layout_rect.size.0 > 0.0 && layout_rect.size.1 > 0.0 {
        // TODO: TextureはあとでUIElementから取得/計算できるようにする
        boxes.emit(
            BoxGroupTexture::User(0),
            BoxInstance {
                pos_st: peridot::math::Vector4(
                    layout_rect.size.0 * target.scale.0,
                    layout_rect.size.1 * target.scale.1,
                    layout_rect.pos.0,
                    layout_rect.pos.1,
                ),
                uv_st: peridot::math::Vector4(0.0, 0.0, 0.0, 0.0),
                col: target.debug_color,
            },
        );
    }

    if let Some(ref f) = target.font {
        let mut char_offset_x = 0.0;
        for c in target.text.chars() {
            let cd = f.request_char(c, 12.0);
            let gr = f.ensure_char_bitmap(c);

            boxes.emit(
                BoxGroupTexture::GlyphAtlas,
                BoxInstance {
                    pos_st: peridot::math::Vector4(
                        cd.width * target.scale.0,
                        cd.height * target.scale.1,
                        layout_rect.pos.0 + char_offset_x,
                        layout_rect.pos.1 + cd.ascend - cd.height,
                    ),
                    // TODO: ここでUV化してるけどアトラスのサイズが後で変わる可能性があるかも
                    uv_st: peridot::math::Vector4(
                        (gr.width - 32 - 32) as f32 / f.glyph_atlas.borrow().width as f32,
                        (gr.height - 32 - 32) as f32 / f.glyph_atlas.borrow().height as f32,
                        (gr.left + 32) as f32 / f.glyph_atlas.borrow().width as f32,
                        (gr.top + 32) as f32 / f.glyph_atlas.borrow().height as f32,
                    ),
                    col: peridot::math::Vector4(1.0, 1.0, 1.0, 1.0),
                },
            );

            char_offset_x += cd.advance_x;
        }
    }

    let child_layout_global_offset = layout_rect.pos + target.padding.lt();
    let child_layout_available_size = layout_rect.size - target.padding.lt() - target.padding.rb();
    let child_layout_global_rect = LayoutRect {
        pos: layout_rect.pos + target.padding.lt(),
        size: layout_rect.size - target.padding.lt() - target.padding.rb(),
    };
    match target.children_layout {
        ChildrenLayoutMode::Free => {
            for c in target.children.iter() {
                let layout_rect = compute_layout_rect(c, Some(child_layout_available_size));

                layout1(
                    c,
                    boxes,
                    LayoutRect {
                        pos: layout_rect.pos + child_layout_global_offset,
                        size: layout_rect.size,
                    },
                );
            }
        }
        ChildrenLayoutMode::Vertical {
            overflow,
            gap,
            direction,
            justify,
            alignment,
        } => {
            let mut available_content_size = child_layout_available_size;

            match justify {
                LayoutJustify::Start => {
                    let mut global_content_offset = child_layout_global_offset;
                    let mut layout_rects = Vec::with_capacity(target.children.len());
                    match direction {
                        LayoutDirection::Normal => {
                            for c in target.children.iter() {
                                let child_layout =
                                    compute_layout_rect(c, Some(available_content_size));
                                let left_offset = compute_vertical_alignment_axis_offset(
                                    available_content_size.0,
                                    child_layout.size.0,
                                    c.layout_alignment_override.unwrap_or(alignment),
                                );

                                let child_height = child_layout.size.1;
                                layout_rects.push(child_layout.r#move(
                                    global_content_offset
                                        + peridot::math::Vector2(left_offset, 0.0),
                                ));
                                global_content_offset.1 += child_height + gap;
                                available_content_size.1 -= child_height + gap;
                                // TODO: overflow
                            }
                        }
                        LayoutDirection::Reverse => {
                            for c in target.children.iter().rev() {
                                let child_layout =
                                    compute_layout_rect(c, Some(available_content_size));
                                let left_offset = compute_vertical_alignment_axis_offset(
                                    available_content_size.0,
                                    child_layout.size.0,
                                    c.layout_alignment_override.unwrap_or(alignment),
                                );

                                let child_height = child_layout.size.1;
                                layout_rects.push(child_layout.r#move(
                                    global_content_offset
                                        + peridot::math::Vector2(left_offset, 0.0),
                                ));
                                global_content_offset.1 += child_height + gap;
                                available_content_size.1 -= child_height + gap;
                                // TODO: overflow
                            }

                            layout_rects.reverse();
                        }
                    }

                    apply_layout_rects(target.children.iter(), layout_rects.into_iter(), boxes);
                }
                LayoutJustify::End => {
                    let mut global_content_offset = peridot::math::Vector2(
                        child_layout_global_offset.0,
                        child_layout_global_offset.1 + available_content_size.1,
                    );

                    let mut layout_rects = Vec::with_capacity(target.children.len());
                    // 下から積んでくるので逆向きに動かす
                    match direction {
                        LayoutDirection::Normal => {
                            for c in target.children.iter().rev() {
                                let child_layout =
                                    compute_layout_rect(c, Some(available_content_size));
                                let left_offset = compute_vertical_alignment_axis_offset(
                                    available_content_size.0,
                                    child_layout.size.0,
                                    c.layout_alignment_override.unwrap_or(alignment),
                                );

                                let child_height = child_layout.size.1;
                                layout_rects.push(child_layout.r#move(
                                    global_content_offset
                                        + peridot::math::Vector2(left_offset, -child_height),
                                ));
                                global_content_offset.1 -= child_height + gap;
                                available_content_size.1 -= child_height + gap;
                                // TODO: overflow
                            }

                            layout_rects.reverse();
                        }
                        LayoutDirection::Reverse => {
                            for c in target.children.iter() {
                                let child_layout =
                                    compute_layout_rect(c, Some(available_content_size));
                                let left_offset = compute_vertical_alignment_axis_offset(
                                    available_content_size.0,
                                    child_layout.size.0,
                                    c.layout_alignment_override.unwrap_or(alignment),
                                );

                                let child_height = child_layout.size.1;
                                layout_rects.push(child_layout.r#move(
                                    global_content_offset
                                        + peridot::math::Vector2(left_offset, -child_height),
                                ));
                                global_content_offset.1 -= child_height + gap;
                                available_content_size.1 -= child_height + gap;
                                // TODO: overflow
                            }
                        }
                    }

                    apply_layout_rects(target.children.iter(), layout_rects.into_iter(), boxes);
                }
                LayoutJustify::Center => {
                    let mut layout_rects = Vec::with_capacity(target.children.len());
                    let mut top_offset = 0.0f32;
                    let mut available_size = child_layout_available_size;
                    match direction {
                        LayoutDirection::Normal => {
                            for c in target.children.iter() {
                                let child_layout = compute_layout_rect(c, Some(available_size));
                                let left_offset = compute_vertical_alignment_axis_offset(
                                    available_content_size.0,
                                    child_layout.size.0,
                                    c.layout_alignment_override.unwrap_or(alignment),
                                );

                                let child_height = child_layout.bottom();
                                layout_rects.push(
                                    child_layout
                                        .r#move(peridot::math::Vector2(left_offset, top_offset)),
                                );
                                top_offset += child_height + gap;
                                available_size.1 -= child_height + gap;
                            }
                        }
                        LayoutDirection::Reverse => {
                            for c in target.children.iter().rev() {
                                let child_layout = compute_layout_rect(c, Some(available_size));
                                let left_offset = compute_vertical_alignment_axis_offset(
                                    available_content_size.0,
                                    child_layout.size.0,
                                    c.layout_alignment_override.unwrap_or(alignment),
                                );

                                let child_height = child_layout.bottom();
                                layout_rects.push(
                                    child_layout
                                        .r#move(peridot::math::Vector2(left_offset, top_offset)),
                                );
                                top_offset += child_height + gap;
                                available_size.1 -= child_height + gap;
                            }

                            layout_rects.reverse();
                        }
                    }

                    top_offset -= gap;
                    let space = available_content_size.1 - top_offset;

                    let global_content_offset =
                        child_layout_global_offset + peridot::math::Vector2(0.0, space * 0.5);
                    for (c, r) in target.children.iter().zip(layout_rects.into_iter()) {
                        layout1(
                            c,
                            boxes,
                            LayoutRect {
                                pos: global_content_offset + r.pos,
                                size: r.size,
                            },
                        );
                    }
                }
                LayoutJustify::SpaceBetween => {
                    let content_height = target
                        .children
                        .iter()
                        .map(|c| compute_layout_rect(c, Some(child_layout_available_size)).bottom())
                        .sum::<f32>();
                    let space = available_content_size.1 - content_height;
                    let gap = if target.children.len() == 1 {
                        0.0
                    } else {
                        space / (target.children.len() - 1) as f32
                    };

                    let mut global_content_offset = child_layout_global_offset;
                    match direction {
                        LayoutDirection::Normal => {
                            for c in target.children.iter() {
                                let child_layout =
                                    compute_layout_rect(c, Some(available_content_size));
                                let left_offset = compute_vertical_alignment_axis_offset(
                                    available_content_size.0,
                                    child_layout.size.0,
                                    c.layout_alignment_override.unwrap_or(alignment),
                                );

                                layout1(
                                    c,
                                    boxes,
                                    LayoutRect {
                                        pos: global_content_offset
                                            + child_layout.pos
                                            + peridot::math::Vector2(left_offset, 0.0),
                                        size: child_layout.size,
                                    },
                                );

                                global_content_offset.1 += child_layout.size.1 + gap;
                                available_content_size.1 -= child_layout.size.1 + gap;
                            }
                        }
                        LayoutDirection::Reverse => {
                            for c in target.children.iter().rev() {
                                let child_layout =
                                    compute_layout_rect(c, Some(available_content_size));
                                let left_offset = compute_vertical_alignment_axis_offset(
                                    available_content_size.0,
                                    child_layout.size.0,
                                    c.layout_alignment_override.unwrap_or(alignment),
                                );

                                layout1(
                                    c,
                                    boxes,
                                    LayoutRect {
                                        pos: global_content_offset
                                            + child_layout.pos
                                            + peridot::math::Vector2(left_offset, 0.0),
                                        size: child_layout.size,
                                    },
                                );

                                global_content_offset.1 += child_layout.size.1 + gap;
                                available_content_size.1 -= child_layout.size.1 + gap;
                            }
                        }
                    }
                }
                LayoutJustify::SpaceAround => {
                    let content_height = target
                        .children
                        .iter()
                        .map(|c| compute_layout_rect(c, Some(child_layout_available_size)).bottom())
                        .sum::<f32>();
                    let space = available_content_size.1 - content_height;
                    let gap = space / (target.children.len() + 1) as f32;

                    let mut global_content_offset =
                        child_layout_global_offset + peridot::math::Vector2(0.0, gap);
                    match direction {
                        LayoutDirection::Normal => {
                            for c in target.children.iter() {
                                let child_layout =
                                    compute_layout_rect(c, Some(available_content_size));
                                let left_offset = compute_vertical_alignment_axis_offset(
                                    available_content_size.0,
                                    child_layout.size.0,
                                    c.layout_alignment_override.unwrap_or(alignment),
                                );

                                layout1(
                                    c,
                                    boxes,
                                    LayoutRect {
                                        pos: global_content_offset
                                            + child_layout.pos
                                            + peridot::math::Vector2(left_offset, 0.0),
                                        size: child_layout.size,
                                    },
                                );

                                global_content_offset.1 += child_layout.size.1 + gap;
                                available_content_size.1 -= child_layout.size.1 + gap;
                            }
                        }
                        LayoutDirection::Reverse => {
                            for c in target.children.iter().rev() {
                                let child_layout =
                                    compute_layout_rect(c, Some(available_content_size));
                                let left_offset = compute_vertical_alignment_axis_offset(
                                    available_content_size.0,
                                    child_layout.size.0,
                                    c.layout_alignment_override.unwrap_or(alignment),
                                );

                                layout1(
                                    c,
                                    boxes,
                                    LayoutRect {
                                        pos: global_content_offset
                                            + child_layout.pos
                                            + peridot::math::Vector2(left_offset, 0.0),
                                        size: child_layout.size,
                                    },
                                );

                                global_content_offset.1 += child_layout.size.1 + gap;
                                available_content_size.1 -= child_layout.size.1 + gap;
                            }
                        }
                    }
                }
            }
        }
        ChildrenLayoutMode::Horizontal {
            overflow,
            gap,
            direction,
            justify,
            alignment,
        } => match (justify, direction) {
            (LayoutJustify::Start, LayoutDirection::Normal) => {
                let layout_rects = layout_horizontal(
                    target.children.iter(),
                    alignment,
                    overflow,
                    gap,
                    &child_layout_global_rect,
                );

                apply_layout_rects(target.children.iter(), layout_rects.into_iter(), boxes);
            }
            (LayoutJustify::Start, LayoutDirection::Reverse) => {
                let layout_rects = layout_horizontal(
                    target.children.iter().rev(),
                    alignment,
                    overflow,
                    gap,
                    &child_layout_global_rect,
                );

                apply_layout_rects(
                    target.children.iter().rev(),
                    layout_rects.into_iter(),
                    boxes,
                );
            }
            (LayoutJustify::End, LayoutDirection::Normal) => {
                let layout_rects = layout_horizontal_justify_per_row(
                    target.children.iter(),
                    alignment,
                    overflow,
                    gap,
                    &child_layout_global_rect,
                    HorizontalJustifyEnd { gap },
                );

                apply_layout_rects(target.children.iter(), layout_rects.into_iter(), boxes);
            }
            (LayoutJustify::End, LayoutDirection::Reverse) => {
                let layout_rects = layout_horizontal_justify_per_row(
                    target.children.iter().rev(),
                    alignment,
                    overflow,
                    gap,
                    &child_layout_global_rect,
                    HorizontalJustifyEnd { gap },
                );

                apply_layout_rects(
                    target.children.iter().rev(),
                    layout_rects.into_iter(),
                    boxes,
                );
            }
            (LayoutJustify::Center, LayoutDirection::Normal) => {
                let layout_rects = layout_horizontal_justify_per_row(
                    target.children.iter(),
                    alignment,
                    overflow,
                    gap,
                    &child_layout_global_rect,
                    HorizontalJustifyCenter { gap },
                );

                apply_layout_rects(target.children.iter(), layout_rects.into_iter(), boxes);
            }
            (LayoutJustify::Center, LayoutDirection::Reverse) => {
                let layout_rects = layout_horizontal_justify_per_row(
                    target.children.iter().rev(),
                    alignment,
                    overflow,
                    gap,
                    &child_layout_global_rect,
                    HorizontalJustifyCenter { gap },
                );

                apply_layout_rects(
                    target.children.iter().rev(),
                    layout_rects.into_iter(),
                    boxes,
                );
            }
            (LayoutJustify::SpaceBetween, LayoutDirection::Normal) => {
                let layout_rects = layout_horizontal_justify_per_row(
                    target.children.iter(),
                    alignment,
                    overflow,
                    gap,
                    &child_layout_global_rect,
                    HorizontalJustifySpaceBetween { min_gap: gap },
                );

                apply_layout_rects(target.children.iter(), layout_rects.into_iter(), boxes);
            }
            (LayoutJustify::SpaceBetween, LayoutDirection::Reverse) => {
                let layout_rects = layout_horizontal_justify_per_row(
                    target.children.iter().rev(),
                    alignment,
                    overflow,
                    gap,
                    &child_layout_global_rect,
                    HorizontalJustifySpaceBetween { min_gap: gap },
                );

                apply_layout_rects(
                    target.children.iter().rev(),
                    layout_rects.into_iter(),
                    boxes,
                );
            }
            (LayoutJustify::SpaceAround, LayoutDirection::Normal) => {
                let layout_rects = layout_horizontal_justify_per_row(
                    target.children.iter(),
                    alignment,
                    overflow,
                    gap,
                    &child_layout_global_rect,
                    HorizontalJustifySpaceAround { min_gap: gap },
                );

                apply_layout_rects(target.children.iter(), layout_rects.into_iter(), boxes);
            }
            (LayoutJustify::SpaceAround, LayoutDirection::Reverse) => {
                let layout_rects = layout_horizontal_justify_per_row(
                    target.children.iter().rev(),
                    alignment,
                    overflow,
                    gap,
                    &child_layout_global_rect,
                    HorizontalJustifySpaceAround { min_gap: gap },
                );

                apply_layout_rects(
                    target.children.iter().rev(),
                    layout_rects.into_iter(),
                    boxes,
                );
            }
        },
        ChildrenLayoutMode::Grid {
            ref columns,
            ref rows,
            column_alignment,
            row_alignment,
            gap,
        } => {
            let mut column_fixed_sizes = vec![0.0; columns.len()];
            let mut row_fixed_sizes = vec![0.0; columns.len()];
            let mut current_column = 0;
            let mut current_row = 0;
            for c in target.children.iter() {
                let cell_rect = compute_layout_rect(c, None);
                match columns[current_column] {
                    GridCellSize::Fixed(x) => {
                        column_fixed_sizes[current_column] = x;
                    }
                    GridCellSize::Flexible(_) => (),
                    GridCellSize::FitContent => {
                        column_fixed_sizes[current_column] =
                            column_fixed_sizes[current_column].max(cell_rect.right());
                    }
                };
                match rows[current_row] {
                    GridCellSize::Fixed(x) => {
                        row_fixed_sizes[current_row] = x;
                    }
                    GridCellSize::Flexible(_) => (),
                    GridCellSize::FitContent => {
                        row_fixed_sizes[current_row] =
                            row_fixed_sizes[current_row].max(cell_rect.bottom());
                    }
                }

                current_column += 1;
                if current_column >= columns.len() {
                    // 折り返し
                    current_column = 0;
                    current_row += 1;
                }
            }
            let column_flexible_total = columns
                .iter()
                .filter_map(|x| match x {
                    GridCellSize::Flexible(x) => Some(x),
                    _ => None,
                })
                .sum::<f32>();
            let row_flexible_total = rows
                .iter()
                .filter_map(|x| match x {
                    GridCellSize::Flexible(x) => Some(x),
                    _ => None,
                })
                .sum::<f32>();
            let column_flexible_region = child_layout_available_size.0
                - column_fixed_sizes.iter().copied().sum::<f32>()
                - (gap * (columns.len() - 1) as f32);
            let row_flexible_region = child_layout_available_size.1
                - row_fixed_sizes.iter().copied().sum::<f32>()
                - (gap * (rows.len() - 1) as f32);

            let column_size = columns
                .iter()
                .zip(column_fixed_sizes.into_iter())
                .map(|(x, f)| match x {
                    GridCellSize::FitContent => f,
                    GridCellSize::Fixed(x) => *x,
                    GridCellSize::Flexible(n) => column_flexible_region * n / column_flexible_total,
                })
                .collect::<Vec<_>>();
            let row_size = rows
                .iter()
                .zip(row_fixed_sizes.into_iter())
                .map(|(x, f)| match x {
                    GridCellSize::FitContent => f,
                    GridCellSize::Fixed(x) => *x,
                    GridCellSize::Flexible(n) => row_flexible_region * n / row_flexible_total,
                })
                .collect::<Vec<_>>();
            let column_offsets = column_size
                .iter()
                .scan(0.0f32, |a, v| {
                    let o = *a;
                    *a += v + gap;
                    Some(o)
                })
                .collect::<Vec<_>>();
            let row_offsets = row_size
                .iter()
                .scan(0.0f32, |a, v| {
                    let o = *a;
                    *a += v + gap;
                    Some(o)
                })
                .collect::<Vec<_>>();

            let mut current_column = 0;
            let mut current_row = 0;

            for c in target.children.iter() {
                let child_layout = compute_layout_rect(
                    c,
                    Some(peridot::math::Vector2(
                        column_size[current_column],
                        row_size[current_row],
                    )),
                );
                let left_offset = compute_vertical_alignment_axis_offset(
                    column_size[current_column],
                    child_layout.size.0,
                    c.row_alignment_override.unwrap_or(row_alignment),
                );
                let top_offset = compute_horizontal_alignment_axis_offset(
                    row_size[current_row],
                    child_layout.size.1,
                    c.column_alignment_override.unwrap_or(column_alignment),
                );

                layout1(
                    c,
                    boxes,
                    LayoutRect {
                        pos: child_layout_global_offset
                            + peridot::math::Vector2(
                                column_offsets[current_column],
                                // TODO: 行数が多くなったときの処理（親コンテナの残りを全部割当、でいいはず）
                                row_offsets[current_row],
                            )
                            + child_layout.pos
                            + peridot::math::Vector2(left_offset, top_offset),
                        size: child_layout.size,
                    },
                );

                current_column += 1;
                if current_column >= columns.len() {
                    // 折り返し
                    current_column = 0;
                    current_row += 1;
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
struct GlyphRect {
    pub left: u32,
    pub top: u32,
    pub width: u32,
    pub height: u32,
}

struct Skyline {
    pub y: u32,
    pub width: u32,
}

struct GlyphAtlas {
    tex: peridot_memory_manager::Image,
    tex_view: LocalImageView,
    width: u32,
    height: u32,
    glyphs: HashMap<<peridot_vg::DefaultFont as peridot_vg::Font>::GlyphID, GlyphRect>,
    dirty_glyphs: HashSet<<peridot_vg::DefaultFont as peridot_vg::Font>::GlyphID>,
    skylines: Vec<Skyline>,
}
impl GlyphAtlas {
    pub fn new(
        mm: &mut peridot_memory_manager::MemoryManager,
        g: &mut peridot::Graphics,
        width: u32,
        height: u32,
    ) -> Self {
        let tex = mm
            .allocate_device_local_image(
                g,
                br::ImageCreateInfo::new(
                    br::Extent2D { width, height },
                    br::vk::VK_FORMAT_R8_UNORM,
                )
                .set_usage(br::ImageUsageFlags::SAMPLED | br::ImageUsageFlags::COLOR_ATTACHMENT),
            )
            .expect("alloc glyph atlas tex");
        let tex_view = LocalImageView {
            handle: unsafe {
                br::vkfn_wrapper::create_image_view(
                    g.device().native_ptr(),
                    &br::ImageViewCreateInfo::new(
                        &tex,
                        br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
                        br::vk::VK_IMAGE_VIEW_TYPE_2D,
                        br::vk::VK_FORMAT_R8_UNORM,
                    ),
                    None,
                )
                .expect("create tex view")
            },
            device: g.device().clone(),
        };

        g.submit_commands(|rec| {
            rec.pipeline_barrier(
                br::PipelineStageFlags(0),
                br::PipelineStageFlags::TRANSFER,
                0,
                &[],
                &[],
                &[br::ImageMemoryBarrier::new(
                    &tex,
                    br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
                    br::ImageLayout::TransferDestOpt.from_undefined(),
                )],
            )
            .clear_color_image(
                &tex,
                br::ImageLayout::TransferDestOpt,
                &[br::ClearColorValue::from([0.0f32; 4])],
                &[br::ImageSubresourceRange::new(
                    br::AspectMask::COLOR,
                    0..1,
                    0..1,
                )],
            )
            .pipeline_barrier(
                br::PipelineStageFlags::TRANSFER,
                br::PipelineStageFlags::FRAGMENT_SHADER,
                0,
                &[],
                &[],
                &[br::ImageMemoryBarrier::new(
                    &tex,
                    br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
                    br::ImageLayout::TransferDestOpt.to(br::ImageLayout::ShaderReadOnlyOpt),
                )
                .access_mask_transition(
                    br::AccessFlags::TRANSFER.write,
                    br::AccessFlags::SHADER.read,
                )],
            )
        })
        .expect("initialize glyph tex");

        Self {
            tex,
            tex_view,
            width,
            height,
            glyphs: HashMap::new(),
            dirty_glyphs: HashSet::new(),
            skylines: vec![Skyline { y: 0, width }],
        }
    }

    pub fn alloc(
        &mut self,
        glyph_id: <peridot_vg::DefaultFont as peridot_vg::Font>::GlyphID,
        width: u32,
        height: u32,
    ) -> Option<GlyphRect> {
        if let Some(r) = self.glyphs.get(&glyph_id) {
            return Some(r.clone());
        }

        let mut fit_left_top = None;
        let mut left = 0;
        let mut n = 0;
        while n < self.skylines.len() && left + width <= self.width {
            let skyline = &self.skylines[n];
            let skyline_height = self.height - skyline.y;
            if skyline_height >= height && fit_left_top.is_none_or(|(_, t, _)| skyline.y < t) {
                let mut y = skyline.y;

                // potentially overlapping skylines at right
                let mut l1 = left + skyline.width;
                let mut m = n + 1;
                while m < self.skylines.len() && l1 <= left + width {
                    let skyline2 = &self.skylines[m];

                    y = y.max(skyline2.y);
                    l1 += skyline2.width;
                    m += 1;
                }

                // recompute whether it fits
                let skyline_height = self.height - y;
                if skyline_height >= height && fit_left_top.is_none_or(|(_, t, _)| y < t) {
                    fit_left_top = Some((left, y, n));
                }
            }

            left += skyline.width;
            n += 1;
        }

        let Some((left, top, left_skyline_point)) = fit_left_top else {
            // no available rects
            return None;
        };

        // update skyline
        let mut left_w = width;
        let mut skyline_point_index = left_skyline_point;
        while left_w > 0 {
            let skyline = &self.skylines[skyline_point_index];

            if skyline.width > left_w {
                // needs splitting(and finishes at this step)
                if skyline_point_index > 0
                    && self.skylines[skyline_point_index - 1].y == top + height
                {
                    // fuse with previous
                    self.skylines[skyline_point_index - 1].width += left_w;
                    self.skylines[skyline_point_index].width -= left_w;
                } else {
                    let org_skyline_y = skyline.y;
                    let right_skyline_width = skyline.width - left_w;
                    self.skylines[skyline_point_index] = Skyline {
                        y: top + height,
                        width: left_w,
                    };
                    self.skylines.insert(
                        skyline_point_index + 1,
                        Skyline {
                            y: org_skyline_y,
                            width: right_skyline_width,
                        },
                    );
                }

                break;
            }

            let sw = skyline.width;
            if skyline_point_index > 0 && self.skylines[skyline_point_index - 1].y == top + height {
                // fuse with previous
                self.skylines[skyline_point_index - 1].width += sw;
                self.skylines.remove(skyline_point_index);
                skyline_point_index -= 1;
            } else {
                // just move this skyline
                self.skylines[left_skyline_point].y = top + height;
            }

            left_w -= sw.min(left_w);
            skyline_point_index += 1;
        }

        let gr = GlyphRect {
            left,
            top,
            width,
            height,
        };
        self.glyphs.insert(glyph_id, gr.clone());
        self.dirty_glyphs.insert(glyph_id);
        Some(gr)
    }
}

#[derive(br::SpecializationConstants)]
struct OutlineVertexShaderParameters {
    #[constant_id = 0]
    sdf_max_distance: f32,
}

#[derive(br::SpecializationConstants)]
struct FillFragmentShaderParameters {
    #[constant_id = 0]
    enable_color_output: br::vk::VkBool32,
}

pub struct TwoPassStencilSDFRenderer {
    gfx_device: peridot::VulkanGfx,
    render_pass: br::vk::VkRenderPass,
    target_size: br::Extent2D,
    fill_shader: PvpContainer,
    fill_shader_modules: PvpShaderModules<peridot::VulkanGfx>,
    curve_fill_shader: PvpContainer,
    curve_fill_shader_modules: PvpShaderModules<peridot::VulkanGfx>,
    outline_shader: PvpContainer,
    outline_shader_modules: PvpShaderModules<peridot::VulkanGfx>,
    pipeline_layout: br::vk::VkPipelineLayout,
    triangle_fans_stencil_pipeline: br::vk::VkPipeline,
    curve_triangles_stencil_pipeline: br::vk::VkPipeline,
    outline_distance_pipeline: br::vk::VkPipeline,
    invert_pipeline: br::vk::VkPipeline,
    sdf_max_distance: f32,
    active_render_area: br::Rect2D,
}
impl Drop for TwoPassStencilSDFRenderer {
    fn drop(&mut self) {
        unsafe {
            br::vkfn_wrapper::destroy_pipeline(
                self.gfx_device.native_ptr(),
                self.invert_pipeline,
                None,
            );
            br::vkfn_wrapper::destroy_pipeline(
                self.gfx_device.native_ptr(),
                self.outline_distance_pipeline,
                None,
            );
            br::vkfn_wrapper::destroy_pipeline(
                self.gfx_device.native_ptr(),
                self.curve_triangles_stencil_pipeline,
                None,
            );
            br::vkfn_wrapper::destroy_pipeline(
                self.gfx_device.native_ptr(),
                self.triangle_fans_stencil_pipeline,
                None,
            );
            br::vkfn_wrapper::destroy_pipeline_layout(
                self.gfx_device.native_ptr(),
                self.pipeline_layout,
                None,
            );
            br::vkfn_wrapper::destroy_render_pass(
                self.gfx_device.native_ptr(),
                self.render_pass,
                None,
            );
        }
    }
}
impl TwoPassStencilSDFRenderer {
    const STENCIL_INVERT: br::vk::VkStencilOpState =
        StencilState::with_op_all(br::StencilOp::Invert)
            .write_mask(0x01)
            .into_vk();
    const STENCIL_MATCH: br::vk::VkStencilOpState = StencilState::with_op_all(br::StencilOp::Keep)
        .when(StencilCompare::eq(0x01).mask(0x01))
        .into_vk();
    const STENCIL_NOOP: br::vk::VkStencilOpState = StencilState::NOOP.into_vk();

    pub fn new(
        e: &peridot::Engine<impl peridot::NativeLinker>,
        color_format: br::Format,
        target_final_layout: br::ImageLayout,
        target_layout_transition_stage: br::PipelineStageFlags,
        target_size: br::Extent2D,
        sdf_max_distance: f32,
    ) -> Self {
        let active_render_area = target_size.into_rect(br::Offset2D::ZERO);

        let attachments = [
            br::vk::VkAttachmentDescription::new(
                color_format,
                target_final_layout,
                target_final_layout,
            )
            .color_memory_op(br::LoadOp::Load, br::StoreOp::Store),
            br::vk::VkAttachmentDescription::new(
                br::vk::VK_FORMAT_S8_UINT,
                br::ImageLayout::Undefined,
                br::ImageLayout::DepthStencilReadOnlyOpt,
            )
            .stencil_memory_op(br::LoadOp::Clear, br::StoreOp::DontCare),
        ];
        let depth_stencil_attachment_ref =
            br::vk::VkAttachmentReference::new(1, br::ImageLayout::DepthStencilAttachmentOpt);
        let color_attachments = [br::vk::VkAttachmentReference::new(
            0,
            br::ImageLayout::ColorAttachmentOpt,
        )];
        let subpasses = [
            br::SubpassDescription::new().depth_stencil_attachment(&depth_stencil_attachment_ref),
            br::SubpassDescription::new()
                .color_attachments(&color_attachments, &[])
                .depth_stencil_attachment(&depth_stencil_attachment_ref),
        ];
        let spdep_color = br::vk::VkSubpassDependency {
            srcSubpass: br::vk::VK_SUBPASS_EXTERNAL,
            dstSubpass: 0,
            srcStageMask: target_layout_transition_stage.0,
            dstStageMask: br::vk::VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
                // Note: LoadOpがClearだとLoad時にWriteが走るらしいのでearlyステージで遷移できてないといけない
                | br::vk::VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT,
            srcAccessMask: 0,
            dstAccessMask: br::AccessFlags::COLOR_ATTACHMENT.write
                | br::AccessFlags::DEPTH_STENCIL_ATTACHMENT.write,
            dependencyFlags: br::vk::VK_DEPENDENCY_BY_REGION_BIT,
        };
        let spdep_stencil = br::vk::VkSubpassDependency {
            srcSubpass: 0,
            dstSubpass: 1,
            srcStageMask: br::PipelineStageFlags::LATE_FRAGMENT_TESTS.0,
            dstStageMask: br::PipelineStageFlags::EARLY_FRAGMENT_TESTS.0,
            srcAccessMask: br::AccessFlags::DEPTH_STENCIL_ATTACHMENT.write,
            dstAccessMask: br::AccessFlags::DEPTH_STENCIL_ATTACHMENT.read,
            dependencyFlags: br::vk::VK_DEPENDENCY_BY_REGION_BIT,
        };
        let render_pass =
            br::RenderPassCreateInfo::new(&attachments, &subpasses, &[spdep_color, spdep_stencil])
                .execute(e.graphics().device(), None)
                .expect("Failed to create RenderPass");

        let outline_vsh_parameters = OutlineVertexShaderParameters { sdf_max_distance };
        let fill_fsh_color_output = FillFragmentShaderParameters {
            enable_color_output: true as _,
        };

        let fill_shader: PvpContainer = e
            .load("builtin.vg.sdf.shaders-dynamic-area.triangle_fans")
            .expect("Failed to load triangle_fans shader asset");
        let fill_shader_modules = PvpShaderModules::new(e.graphics().device(), &fill_shader)
            .expect("Failed to create triangle_fans shader modules");
        let fill_vertex_input_state = br::PipelineVertexInputStateCreateInfo::new(
            &fill_shader.vertex_bindings,
            &fill_shader.vertex_attributes,
        );

        let curve_fill_shader: PvpContainer = e
            .load("builtin.vg.sdf.shaders-dynamic-area.curve_triangles")
            .expect("Failed to load curve_triangles shader asset");
        let curve_fill_shader_modules =
            PvpShaderModules::new(e.graphics().device(), &curve_fill_shader)
                .expect("Failed to create curve_triangles shader modules");
        let curve_fill_vertex_input_state = br::PipelineVertexInputStateCreateInfo::new(
            &curve_fill_shader.vertex_bindings,
            &curve_fill_shader.vertex_attributes,
        );

        let outline_shader: PvpContainer = e
            .load("builtin.vg.sdf.shaders-dynamic-area.outline_distance")
            .expect("Failed to load outline_distance shader asset");
        let outline_shader_modules = PvpShaderModules::new(e.graphics().device(), &outline_shader)
            .expect("Failed to create outline_disdtance shader modules");
        let outline_vertex_input_state = br::PipelineVertexInputStateCreateInfo::new(
            &outline_shader.vertex_bindings,
            &outline_shader.vertex_attributes,
        );

        let pipeline_layout = unsafe {
            br::vkfn_wrapper::create_pipeline_layout(
                e.graphics().device().native_ptr(),
                &br::PipelineLayoutCreateInfo::new(
                    &[],
                    &[br::PushConstantRange::for_type::<br::Extent2D>(
                        br::vk::VK_SHADER_STAGE_VERTEX_BIT,
                        0,
                    )],
                ),
                None,
            )
            .expect("Failed to create pipeline layout")
        };

        let fill_color_fsh_parameters = &br::SpecializationInfo::new(&fill_fsh_color_output);
        let outline_vsh_parameters = br::SpecializationInfo::new(&outline_vsh_parameters);

        let color_blends = [ColorAttachmentBlending::Disabled.into_vk()];
        let color_blend_state = br::PipelineColorBlendStateCreateInfo::new(&color_blends);

        let rasterization_state = br::PipelineRasterizationStateCreateInfo::new(
            br::PolygonMode::Fill,
            br::CullModeFlags::NONE,
            br::FrontFace::CounterClockwise,
        );
        let multisample_state = br::PipelineMultisampleStateCreateInfo::new();

        let [triangle_fans_stencil_pipeline, curve_triangles_stencil_pipeline, invert_pipeline, outline_distance_pipeline] = unsafe {
            br::vkfn_wrapper::create_graphics_pipeline_array(
                e.graphics().device().native_ptr(),
                None,
                &[
                    br::GraphicsPipelineCreateInfo::new(
                        &br::VkHandleRef::dangling(pipeline_layout),
                        br::SubpassRef(&br::VkHandleRef::dangling(render_pass), 0),
                        &[
                            fill_shader_modules.pipeline_vertex_shader_stage(),
                            fill_shader_modules
                                .pipeline_fragment_shader_stage()
                                .expect("no fsh?"),
                        ],
                        &fill_vertex_input_state,
                        &br::PipelineInputAssemblyStateCreateInfo::new(
                            br::PrimitiveTopology::TriangleList,
                        ),
                        &br::PipelineViewportStateCreateInfo::new_dynamic(1),
                        &rasterization_state,
                        &color_blend_state,
                    )
                    .set_multisample_state(&multisample_state)
                    .set_depth_stencil_state(
                        &br::PipelineDepthStencilStateCreateInfo::new()
                            .stencil_state_front(Self::STENCIL_INVERT)
                            .stencil_state_back(Self::STENCIL_INVERT)
                            .stencil_test(true),
                    )
                    .set_dynamic_state(
                        &br::PipelineDynamicStateCreateInfo::new(&[
                            br::vk::VK_DYNAMIC_STATE_VIEWPORT,
                            br::vk::VK_DYNAMIC_STATE_SCISSOR,
                        ]),
                    ),
                    br::GraphicsPipelineCreateInfo::new(
                        &br::VkHandleRef::dangling(pipeline_layout),
                        br::SubpassRef(&br::VkHandleRef::dangling(render_pass), 0),
                        &[
                            curve_fill_shader_modules.pipeline_vertex_shader_stage(),
                            curve_fill_shader_modules
                                .pipeline_fragment_shader_stage()
                                .expect("no fsh?"),
                        ],
                        &curve_fill_vertex_input_state,
                        &br::PipelineInputAssemblyStateCreateInfo::new(
                            br::PrimitiveTopology::TriangleList,
                        ),
                        &br::PipelineViewportStateCreateInfo::new_dynamic(1),
                        &rasterization_state,
                        &color_blend_state,
                    )
                    .set_multisample_state(&multisample_state)
                    .set_depth_stencil_state(
                        &br::PipelineDepthStencilStateCreateInfo::new()
                            .stencil_state_front(Self::STENCIL_INVERT)
                            .stencil_state_back(Self::STENCIL_INVERT)
                            .stencil_test(true),
                    )
                    .set_dynamic_state(
                        &br::PipelineDynamicStateCreateInfo::new(&[
                            br::vk::VK_DYNAMIC_STATE_VIEWPORT,
                            br::vk::VK_DYNAMIC_STATE_SCISSOR,
                        ]),
                    ),
                    br::GraphicsPipelineCreateInfo::new(
                        &br::VkHandleRef::dangling(pipeline_layout),
                        br::SubpassRef(&br::VkHandleRef::dangling(render_pass), 1),
                        &[
                            fill_shader_modules.pipeline_vertex_shader_stage(),
                            fill_shader_modules
                                .pipeline_fragment_shader_stage()
                                .expect("no fsh?")
                                .with_specialization_info(&fill_color_fsh_parameters),
                        ],
                        &fill_vertex_input_state,
                        &br::PipelineInputAssemblyStateCreateInfo::new(
                            br::PrimitiveTopology::TriangleStrip,
                        ),
                        &br::PipelineViewportStateCreateInfo::new_dynamic(1),
                        &rasterization_state,
                        &br::PipelineColorBlendStateCreateInfo::new(&[
                            ColorAttachmentBlending::new(
                                Blending::source_only(br::vk::VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR),
                                Blending::source_only(br::vk::VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA),
                            )
                            .into_vk(),
                        ]),
                    )
                    .set_multisample_state(&multisample_state)
                    .set_depth_stencil_state(
                        &br::PipelineDepthStencilStateCreateInfo::new()
                            .stencil_state_front(Self::STENCIL_MATCH)
                            .stencil_state_back(Self::STENCIL_MATCH)
                            .stencil_test(true),
                    )
                    .set_dynamic_state(
                        &br::PipelineDynamicStateCreateInfo::new(&[
                            br::vk::VK_DYNAMIC_STATE_VIEWPORT,
                            br::vk::VK_DYNAMIC_STATE_SCISSOR,
                        ]),
                    ),
                    br::GraphicsPipelineCreateInfo::new(
                        &br::VkHandleRef::dangling(pipeline_layout),
                        br::SubpassRef(&br::VkHandleRef::dangling(render_pass), 1),
                        &[
                            outline_shader_modules
                                .pipeline_vertex_shader_stage()
                                .with_specialization_info(&outline_vsh_parameters),
                            outline_shader_modules
                                .pipeline_fragment_shader_stage()
                                .expect("no fsh?"),
                        ],
                        &outline_vertex_input_state,
                        &br::PipelineInputAssemblyStateCreateInfo::new(
                            br::PrimitiveTopology::TriangleList,
                        ),
                        &br::PipelineViewportStateCreateInfo::new_dynamic(1),
                        &rasterization_state,
                        &br::PipelineColorBlendStateCreateInfo::new(&[
                            ColorAttachmentBlending::MAX.into_vk(),
                        ]),
                    )
                    .set_multisample_state(&multisample_state)
                    .set_depth_stencil_state(
                        &br::PipelineDepthStencilStateCreateInfo::new()
                            .stencil_state_front(Self::STENCIL_NOOP)
                            .stencil_state_back(Self::STENCIL_NOOP)
                            .stencil_test(false),
                    )
                    .set_dynamic_state(
                        &br::PipelineDynamicStateCreateInfo::new(&[
                            br::vk::VK_DYNAMIC_STATE_VIEWPORT,
                            br::vk::VK_DYNAMIC_STATE_SCISSOR,
                        ]),
                    ),
                ],
                None,
            )
            .expect("Failed to create graphics pipelines")
        };

        Self {
            gfx_device: e.graphics().device().clone(),
            render_pass,
            target_size,
            fill_shader,
            fill_shader_modules,
            curve_fill_shader,
            curve_fill_shader_modules,
            outline_shader,
            outline_shader_modules,
            pipeline_layout,
            triangle_fans_stencil_pipeline,
            curve_triangles_stencil_pipeline,
            invert_pipeline,
            outline_distance_pipeline,
            sdf_max_distance,
            active_render_area,
        }
    }

    pub const fn render_pass<'s>(&'s self) -> br::VkHandleRef<'s, br::vk::VkRenderPass> {
        unsafe { br::VkHandleRef::dangling(self.render_pass) }
    }

    pub const fn pipeline_layout<'s>(&'s self) -> br::VkHandleRef<'s, br::vk::VkPipelineLayout> {
        unsafe { br::VkHandleRef::dangling(self.pipeline_layout) }
    }

    pub const fn triangle_fans_stencil_pipeline<'s>(
        &'s self,
    ) -> br::VkHandleRef<'s, br::vk::VkPipeline> {
        unsafe { br::VkHandleRef::dangling(self.triangle_fans_stencil_pipeline) }
    }

    pub const fn curve_triangles_stencil_pipeline<'s>(
        &'s self,
    ) -> br::VkHandleRef<'s, br::vk::VkPipeline> {
        unsafe { br::VkHandleRef::dangling(self.curve_triangles_stencil_pipeline) }
    }

    pub const fn invert_pipeline<'s>(&'s self) -> br::VkHandleRef<'s, br::vk::VkPipeline> {
        unsafe { br::VkHandleRef::dangling(self.invert_pipeline) }
    }

    pub const fn outline_distance_pipeline<'s>(
        &'s self,
    ) -> br::VkHandleRef<'s, br::vk::VkPipeline> {
        unsafe { br::VkHandleRef::dangling(self.outline_distance_pipeline) }
    }

    pub const fn render_area(&self) -> br::vk::VkRect2D {
        self.target_size.into_rect(br::Offset2D::ZERO)
    }

    pub const CLEAR_VALUES: &'static [br::ClearValue] = &[
        br::ClearValue::color_f32([0.0; 4]), // ignored
        br::ClearValue::depth_stencil(0.0, 0),
    ];

    pub fn update_render_area(&mut self, area: br::Rect2D) {
        self.active_render_area = area;
    }
}

struct TwoPassStencilSDFRenderTarget {
    gfx_device: peridot::VulkanGfx,
    framebuffer: br::vk::VkFramebuffer,
}
impl Drop for TwoPassStencilSDFRenderTarget {
    fn drop(&mut self) {
        unsafe {
            br::vkfn_wrapper::destroy_framebuffer(
                self.gfx_device.native_ptr(),
                self.framebuffer,
                None,
            );
        }
    }
}
impl br::VkHandle for TwoPassStencilSDFRenderTarget {
    type Handle = br::vk::VkFramebuffer;

    #[inline(always)]
    fn native_ptr(&self) -> Self::Handle {
        self.framebuffer
    }
}
impl TwoPassStencilSDFRenderTarget {
    pub fn new(
        g: &peridot::VulkanGfx,
        renderer: &TwoPassStencilSDFRenderer,
        color_buffer_view: br::VkHandleRef<br::vk::VkImageView>,
        stencil_buffer_view: br::VkHandleRef<br::vk::VkImageView>,
        size: br::Extent2D,
    ) -> Self {
        let framebuffer = unsafe {
            br::vkfn_wrapper::create_framebuffer(
                g.native_ptr(),
                &br::FramebufferCreateInfo::new(
                    &renderer.render_pass(),
                    &[color_buffer_view, stencil_buffer_view],
                    size.width,
                    size.height,
                ),
                None,
            )
            .expect("Failed to create framebuffer")
        };

        Self {
            gfx_device: g.clone(),
            framebuffer,
        }
    }
}

pub struct TwoPassStencilSDFRendererBuffers {
    fill_triangle_mesh: StandardIndexedMesh<
        peridot::mthelper::SharedRef<peridot_memory_manager::Buffer>,
        peridot::mthelper::SharedRef<peridot_memory_manager::Buffer>,
    >,
    fill_triangle_groups: Vec<(u32, u32)>,
    curve_triangles_mesh:
        StandardMesh<peridot::mthelper::SharedRef<peridot_memory_manager::Buffer>>,
    outline_rects_mesh: StandardMesh<peridot::mthelper::SharedRef<peridot_memory_manager::Buffer>>,
    invert_fill_rect_mesh:
        StandardMesh<peridot::mthelper::SharedRef<peridot_memory_manager::Buffer>>,
}
impl TwoPassStencilSDFRenderer {
    pub fn commands<'s>(
        &'s self,
        framebuffer: &'s (impl br::VkHandle<Handle = br::vk::VkFramebuffer> + ?Sized),
        buffers: &'s TwoPassStencilSDFRendererBuffers,
    ) -> impl GraphicsCommand + 's {
        let rp = BeginRenderPass::new(
            self.render_pass(),
            framebuffer,
            self.render_area(),
            br::SubpassContents::Inline,
        )
        .with_clear_values(Self::CLEAR_VALUES.into());

        let stencil_fill_triangles_render = buffers
            .fill_triangle_groups
            .iter()
            .fold((vec![], 0), |(mut commands, vo), &(vertices, indices)| {
                commands.push(SimpleDrawIndexed::new(indices, 1).with_vertex_offset(vo as _));
                (commands, vo + vertices)
            })
            .0;
        let stencil_pass = (
            stencil_fill_triangles_render
                .after_of(buffers.fill_triangle_mesh.ref_pre_configure_for_draw())
                .after_of(PushConstant::for_vertex(
                    self.pipeline_layout(),
                    0,
                    [
                        self.active_render_area.extent.width as f32,
                        self.active_render_area.extent.height as f32,
                    ],
                ))
                .after_of(peridot_command_object::BindGraphicsPipeline(
                    self.triangle_fans_stencil_pipeline(),
                )),
            buffers
                .curve_triangles_mesh
                .ref_draw(1)
                .after_of(PushConstant::for_vertex(
                    self.pipeline_layout(),
                    0,
                    [
                        self.active_render_area.extent.width as f32,
                        self.active_render_area.extent.height as f32,
                    ],
                ))
                .after_of(peridot_command_object::BindGraphicsPipeline(
                    self.curve_triangles_stencil_pipeline(),
                )),
        );
        let outline_distance_pass = (
            buffers
                .outline_rects_mesh
                .ref_draw(1)
                .after_of(PushConstant::for_vertex(
                    self.pipeline_layout(),
                    0,
                    [
                        self.active_render_area.extent.width as f32,
                        self.active_render_area.extent.height as f32,
                    ],
                ))
                .after_of(peridot_command_object::BindGraphicsPipeline(
                    self.outline_distance_pipeline(),
                )),
            buffers
                .invert_fill_rect_mesh
                .ref_draw(1)
                .after_of(PushConstant::for_vertex(
                    self.pipeline_layout(),
                    0,
                    [0.5f32, 0.5f32],
                ))
                .after_of(peridot_command_object::BindGraphicsPipeline(
                    self.invert_pipeline(),
                )),
        );

        (
            SetViewportScissors {
                viewports: vec![self.active_render_area.make_viewport(0.0..1.0)],
                scissors: vec![self.active_render_area],
            },
            stencil_pass,
            NextSubpass::WITH_INLINE_COMMANDS,
            outline_distance_pass,
        )
            .between(rp, EndRenderPass)
    }
}

pub async fn game_main(e: &mut peridot::Engine<'_, impl peridot::NativeLinker>) {
    let screen_size = e.back_buffer_size();
    let mut scissor_rect = br::Extent2D::from(screen_size).into_rect(br::vk::VkOffset2D::ZERO);
    let mut viewport = scissor_rect.make_viewport(0.0..1.0);
    let mut pmm = peridot_memory_manager::MemoryManager::new(e.graphics());

    let main_renderpass = br::RenderPassObject::new(
        e.graphics().device().clone(),
        &br::RenderPassCreateInfo::new(
            &[e.back_buffer_attachment_desc()
                .color_memory_op(br::LoadOp::Clear, br::StoreOp::Store)],
            &[br::SubpassDescription::new().color_attachments(
                &[br::vk::VkAttachmentReference::new(
                    0,
                    br::ImageLayout::ColorAttachmentOpt,
                )],
                &[],
            )],
            &[peridot::SubpassDependencyTemplates::to_color_attachment_in(
                None, 0, true,
            )],
        ),
    )
    .expect("Failed to create main renderpass");
    let mut backbuffer_resources = e
        .iter_back_buffers()
        .map(|x| LocalImageView {
            handle: unsafe {
                br::vkfn_wrapper::create_image_view(
                    e.graphics_device().native_ptr(),
                    &br::ImageViewCreateInfo::new(
                        &x,
                        br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
                        br::vk::VK_IMAGE_VIEW_TYPE_2D,
                        e.back_buffer_format(),
                    ),
                    None,
                )
                .expect("create_image_view failed")
            },
            device: e.graphics_device().clone(),
        })
        .collect::<Vec<_>>();
    let mut main_framebuffers = backbuffer_resources
        .iter()
        .map(|bb| {
            br::FramebufferObject::new(
                e.graphics().device().clone(),
                &br::FramebufferCreateInfo::new(
                    &main_renderpass,
                    &[bb.as_transparent_ref()],
                    screen_size.0,
                    screen_size.1,
                ),
            )
            .expect("Failed to create main framebuffer")
        })
        .collect::<Vec<_>>();

    let dsl_ub1 = br::DescriptorSetLayoutObject::new(
        e.graphics().device().clone(),
        &br::DescriptorSetLayoutCreateInfo::new(&[
            br::DescriptorType::UniformBuffer.make_binding(0, 1)
        ]),
    )
    .expect("create descriptor set layout for ub1");
    let dsl_cis1 = br::DescriptorSetLayoutObject::new(
        e.graphics().device().clone(),
        &br::DescriptorSetLayoutCreateInfo::new(&[
            br::DescriptorType::CombinedImageSampler.make_binding(0, 1)
        ]),
    )
    .expect("create descriptor set layout for cis1");

    let shader = e
        .load::<peridot_rendering_configuration::CompiledRenderingConfigurationVk>("shaders.ui")
        .expect("loading shader");
    let unlit_fill_pipeline_layout = br::PipelineLayoutObject::new(
        e.graphics().device().clone(),
        &br::PipelineLayoutCreateInfo::new(
            &[
                dsl_ub1.as_transparent_ref(),
                dsl_ub1.as_transparent_ref(),
                dsl_cis1.as_transparent_ref(),
            ],
            &if shader.push_constant_buffer_size_bytes > 0 {
                vec![br::PushConstantRange::new(
                    br::vk::VK_SHADER_STAGE_ALL,
                    0..shader.push_constant_buffer_size_bytes as _,
                )]
            } else {
                vec![]
            },
        ),
    )
    .expect("Failed to create pipeline layout");
    let [unlit_fill_pipeline] = match shader.passes["Unlit"] {
        peridot_rendering_configuration::ShadingPassVk::SimpleDeriveBuiltinPass { ref name } => {
            unreachable!("using builtin pass: {name}");
        }
        peridot_rendering_configuration::ShadingPassVk::Custom {
            ref option_overrides,
            ref variants,
        } => {
            let peridot_rendering_configuration::Code {
                ref vertex_semantic_to_location,
                ref vertex_entry_point_name,
                ref fragment_entry_point_name,
                ref words,
            } = variants[&peridot_rendering_configuration::VariantKey { instancing: false }];

            let shader = br::ShaderModuleObject::new(
                e.graphics().device().clone(),
                &br::ShaderModuleCreateInfo::new(words),
            )
            .expect("ShaderModuleObject::new");
            let mut shader_stage_with_ep_names = Vec::with_capacity(2);
            if let Some(e) = vertex_entry_point_name {
                shader_stage_with_ep_names.push((
                    br::ShaderStage::Vertex,
                    std::ffi::CString::new(e as &str).expect("invalid entry point name"),
                ));
            }
            if let Some(e) = fragment_entry_point_name {
                shader_stage_with_ep_names.push((
                    br::ShaderStage::Fragment,
                    std::ffi::CString::new(e as &str).expect("invalid entry point name"),
                ));
            }

            e.graphics()
                .device()
                .new_graphics_pipeline_array(
                    &[br::GraphicsPipelineCreateInfo::new(
                        &unlit_fill_pipeline_layout,
                        main_renderpass.subpass(0),
                        &shader_stage_with_ep_names
                            .iter()
                            .map(|&(s, ref e)| shader.on_stage(s, e))
                            .collect::<Vec<_>>(),
                        &br::PipelineVertexInputStateCreateInfo::new(
                            &[
                                br::VertexInputBindingDescription::per_vertex_typed::<Vertex>(0),
                                br::VertexInputBindingDescription::per_instance_typed::<BoxInstance>(1)
                            ], &[
                                br::VertexInputAttributeDescription {
                                    binding: 0,
                                    location: vertex_semantic_to_location[&peridot_rendering_configuration::VertexInputSemantic::Position(0)],
                                    format: br::vk::VK_FORMAT_R32G32_SFLOAT,
                                    offset: core::mem::offset_of!(Vertex, pos) as _
                                },
                                br::VertexInputAttributeDescription {
                                    binding: 1,
                                    location: vertex_semantic_to_location[&peridot_rendering_configuration::VertexInputSemantic::Position(1)],
                                    format: br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
                                    offset: core::mem::offset_of!(BoxInstance, pos_st) as _
                                },
                                br::VertexInputAttributeDescription {
                                    binding: 1,
                                    location: vertex_semantic_to_location[&peridot_rendering_configuration::VertexInputSemantic::Texcoord(0)],
                                    format: br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
                                    offset: core::mem::offset_of!(BoxInstance, uv_st) as _
                                },
                                br::VertexInputAttributeDescription {
                                    binding: 1,
                                    location: vertex_semantic_to_location[&peridot_rendering_configuration::VertexInputSemantic::Color(0)],
                                    format: br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
                                    offset: core::mem::offset_of!(BoxInstance, col) as _
                                }
                            ]
                        ),
                        &br::PipelineInputAssemblyStateCreateInfo::new(
                            br::PrimitiveTopology::TriangleStrip,
                        ),
                        &br::PipelineViewportStateCreateInfo::new_array(
                            &[viewport],
                            &[scissor_rect],
                        ),
                        &br::PipelineRasterizationStateCreateInfo::new(
                            br::PolygonMode::Fill,
                            br::CullModeFlags::NONE,
                            br::FrontFace::CounterClockwise,
                        ),
                        &br::PipelineColorBlendStateCreateInfo::new(&[
                            br::vk::VkPipelineColorBlendAttachmentState::PREMULTIPLIED,
                        ]),
                    )
                    .set_multisample_state(&br::PipelineMultisampleStateCreateInfo::new())],
                    None::<&br::PipelineCacheObject<peridot::DeviceObject>>,
                )
                .expect("new_graphics_pipeline_array")
        }
    };
    let mut unlit_fill_pipeline = unlit_fill_pipeline.clone_parent();

    let main_font = TextFontData::new(
        peridot_vg::DefaultFontProvider::new()
            .expect("DefaultFontProvider::new failed")
            .best_match("system-ui", &peridot_vg::FontProperties::default(), 96.0)
            .expect("DefaultFontProvider::best_match"),
        &mut pmm,
        e.graphics_mut(),
        2048,
        32,
    );

    let tex_sampler =
        br::SamplerObject::new(e.graphics().device().clone(), &br::SamplerCreateInfo::new())
            .expect("tex sampler new");

    // builtin tex
    let white_2d_image = pmm
        .allocate_device_local_image(
            e.graphics(),
            br::ImageCreateInfo::new(
                br::Extent2D {
                    width: 1,
                    height: 1,
                },
                br::vk::VK_FORMAT_R8G8B8A8_UNORM,
            )
            .set_usage(br::ImageUsageFlags::SAMPLED),
        )
        .expect("white 2d image create");
    let white_2d_image_view = LocalImageView {
        handle: unsafe {
            br::vkfn_wrapper::create_image_view(
                e.graphics().device().native_ptr(),
                &br::ImageViewCreateInfo::new(
                    &white_2d_image,
                    br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
                    br::vk::VK_IMAGE_VIEW_TYPE_2D,
                    br::vk::VK_FORMAT_R8G8B8A8_UNORM,
                ),
                None,
            )
            .expect("white 2d image view create")
        },
        device: e.graphics().device().clone(),
    };
    e.submit_commands(|rec| {
        rec.pipeline_barrier(
            br::PipelineStageFlags::HOST,
            br::PipelineStageFlags::TRANSFER,
            0,
            &[],
            &[],
            &[br::ImageMemoryBarrier::new(
                &white_2d_image,
                br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
                br::ImageLayout::TransferDestOpt.from_undefined(),
            )],
        )
        .clear_color_image(
            &white_2d_image,
            br::ImageLayout::TransferDestOpt,
            &[br::ClearColorValue::from([1.0f32; 4])],
            &[br::ImageSubresourceRange::new(
                br::AspectMask::COLOR,
                0..1,
                0..1,
            )],
        )
        .pipeline_barrier(
            br::PipelineStageFlags::TRANSFER,
            br::PipelineStageFlags::FRAGMENT_SHADER,
            0,
            &[],
            &[],
            &[br::ImageMemoryBarrier::new(
                &white_2d_image,
                br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
                br::ImageLayout::TransferDestOpt.to(br::ImageLayout::ShaderReadOnlyOpt),
            )
            .access_mask_transition(
                br::AccessFlags::TRANSFER.write,
                br::AccessFlags::SHADER.read,
            )],
        )
    })
    .expect("init white 2d image");

    // rendering configuration default resources
    let (prc_uniform_block_data, [prc_camera_parameter_offset, prc_object_parameter_offset]) = pmm
        .allocate_device_local_buffer_with_content_array(
            e.graphics(),
            &[
                peridot::BufferContent::uniform::<CameraParameterUniformBlockData>(),
                peridot::BufferContent::uniform::<ObjectParameterUniformBlockData>(),
            ],
            br::BufferUsage::TRANSFER_DEST,
        )
        .expect("alloc prc_uniform_block_data");
    let mut prc_descriptor_pool = br::DescriptorPoolObject::new(
        e.graphics().device().clone(),
        &br::DescriptorPoolCreateInfo::new(2, &[br::DescriptorType::UniformBuffer.make_size(2)]),
    )
    .expect("DescriptorPoolObject::new");
    let [prc_camera_parameter_descriptor_set, prc_object_parameter_descriptor_set] =
        prc_descriptor_pool
            .alloc_array(&[dsl_ub1.as_transparent_ref(), dsl_ub1.as_transparent_ref()])
            .expect("prc_descriptor_pool.alloc_array");
    let mut ui_descriptor_pool = br::DescriptorPoolObject::new(
        e.graphics().device().clone(),
        &br::DescriptorPoolCreateInfo::new(
            2,
            &[br::DescriptorType::CombinedImageSampler.make_size(2)],
        ),
    )
    .expect("ui descriptor pool new");
    let [ui_descriptor_pool_glyph_tex, ui_descriptor_pool_tex1] = ui_descriptor_pool
        .alloc_array(&[dsl_cis1.as_transparent_ref(), dsl_cis1.as_transparent_ref()])
        .expect("ui descriptor alloc");
    e.graphics().device().update_descriptor_sets(
        &[
            prc_camera_parameter_descriptor_set.binding_at(0).write(
                br::DescriptorContents::uniform_buffer(
                    &prc_uniform_block_data,
                    prc_camera_parameter_offset
                        ..prc_camera_parameter_offset
                            + core::mem::size_of::<CameraParameterUniformBlockData>() as u64,
                ),
            ),
            prc_object_parameter_descriptor_set.binding_at(0).write(
                br::DescriptorContents::uniform_buffer(
                    &prc_uniform_block_data,
                    prc_object_parameter_offset
                        ..prc_object_parameter_offset
                            + core::mem::size_of::<ObjectParameterUniformBlockData>() as u64,
                ),
            ),
            ui_descriptor_pool_glyph_tex.binding_at(0).write(
                br::DescriptorContents::CombinedImageSampler(vec![br::DescriptorImageInfo::new(
                    &main_font.glyph_atlas.borrow().tex_view,
                    br::ImageLayout::ShaderReadOnlyOpt,
                )
                .with_sampler(&tex_sampler)]),
            ),
            ui_descriptor_pool_tex1.binding_at(0).write(
                br::DescriptorContents::CombinedImageSampler(vec![br::DescriptorImageInfo::new(
                    &white_2d_image_view,
                    br::ImageLayout::ShaderReadOnlyOpt,
                )
                .with_sampler(&tex_sampler)]),
            ),
        ],
        &[],
    );

    const UI_BASE_WIDTH: f32 = 640.0;
    const UI_BASE_HEIGHT: f32 = 480.0;

    // プレイヤーカード風UI試作
    let user_card_cell_ui = UIElement {
        size: peridot::math::Vector2(UIElementSize::Fill, UIElementSize::FitContent),
        padding: RectEdge::all(8.0),
        debug_color: peridot::math::Vector4(1.0, 1.0, 1.0, 0.5),
        children_layout: ChildrenLayoutMode::Horizontal {
            direction: LayoutDirection::Normal,
            justify: LayoutJustify::Start,
            alignment: LayoutAlignment::Start,
            overflow: Overflow::Hidden,
            gap: 8.0,
        },
        children: vec![
            // user_icon
            UIElement {
                size: peridot::math::Vector2(
                    UIElementSize::Fixed(64.0),
                    UIElementSize::Fixed(64.0),
                ),
                debug_color: peridot::math::Vector4(1.0, 0.0, 1.0, 0.5),
                ..Default::default()
            },
            // detail_rows
            UIElement {
                size: peridot::math::Vector2(UIElementSize::Fill, UIElementSize::FitContent),
                // debug_color: peridot::math::Vector4(1.0, 1.0, 1.0, 0.5),
                children_layout: ChildrenLayoutMode::Vertical {
                    direction: LayoutDirection::Normal,
                    justify: LayoutJustify::Start,
                    alignment: LayoutAlignment::Start,
                    overflow: Overflow::Hidden,
                    gap: 4.0,
                },
                children: vec![
                    // name_container
                    UIElement {
                        size: peridot::math::Vector2(
                            UIElementSize::Fill,
                            UIElementSize::FitContent,
                        ),
                        children_layout: ChildrenLayoutMode::Grid {
                            columns: vec![GridCellSize::Flexible(1.0), GridCellSize::FitContent],
                            rows: vec![GridCellSize::FitContent],
                            column_alignment: LayoutAlignment::Start,
                            row_alignment: LayoutAlignment::Start,
                            gap: 4.0,
                        },
                        children: vec![
                            // name
                            UIElement {
                                size: peridot::math::Vector2(
                                    UIElementSize::Fill,
                                    UIElementSize::Fixed(24.0),
                                ),
                                debug_color: peridot::math::Vector4(0.5, 0.0, 0.0, 1.0),
                                font: Some(&main_font),
                                text: "player #111",
                                ..Default::default()
                            },
                            // level
                            UIElement {
                                size: peridot::math::Vector2(
                                    UIElementSize::Fixed(64.0),
                                    UIElementSize::Fixed(20.0),
                                ),
                                debug_color: peridot::math::Vector4(0.5, 0.0, 0.0, 1.0),
                                font: Some(&main_font),
                                text: "Lv.10",
                                ..Default::default()
                            },
                        ],
                        ..Default::default()
                    },
                    // separator
                    UIElement {
                        size: peridot::math::Vector2(
                            UIElementSize::Fill,
                            UIElementSize::Fixed(4.0),
                        ),
                        debug_color: peridot::math::Vector4(0.5, 0.0, 0.0, 0.5),
                        ..Default::default()
                    },
                    // details_block
                    UIElement {
                        size: peridot::math::Vector2(
                            UIElementSize::Fill,
                            UIElementSize::FitContent,
                        ),
                        children_layout: ChildrenLayoutMode::Grid {
                            columns: vec![GridCellSize::Flexible(1.0), GridCellSize::FitContent],
                            rows: vec![GridCellSize::FitContent],
                            column_alignment: LayoutAlignment::Start,
                            row_alignment: LayoutAlignment::Start,
                            gap: 4.0,
                        },
                        children: vec![
                            // comment_area
                            UIElement {
                                size: peridot::math::Vector2(
                                    UIElementSize::Fill,
                                    UIElementSize::FitContent,
                                ),
                                debug_color: peridot::math::Vector4(1.0, 1.0, 0.5, 0.125),
                                padding: RectEdge {
                                    left: 16.0,
                                    right: 16.0,
                                    top: 24.0,
                                    bottom: 24.0,
                                },
                                children: vec![
                                    // comment
                                    UIElement {
                                        size: peridot::math::Vector2(
                                            UIElementSize::Fill,
                                            UIElementSize::Fixed(20.0),
                                        ),
                                        debug_color: peridot::math::Vector4(0.5, 0.0, 0.0, 0.5),
                                        ..Default::default()
                                    },
                                ],
                                ..Default::default()
                            },
                            // buttons_container
                            UIElement {
                                size: peridot::math::Vector2(
                                    UIElementSize::FitContent,
                                    UIElementSize::FitContent,
                                ),
                                column_alignment_override: Some(LayoutAlignment::End),
                                children: vec![
                                    // follow_button
                                    UIElement {
                                        size: peridot::math::Vector2(
                                            UIElementSize::FitContent,
                                            UIElementSize::FitContent,
                                        ),
                                        debug_color: peridot::math::Vector4(0.5, 1.0, 0.0, 1.0),
                                        padding: RectEdge {
                                            left: 4.0,
                                            right: 4.0,
                                            top: 4.0,
                                            bottom: 4.0,
                                        },
                                        children: vec![
                                            // button_label
                                            UIElement {
                                                size: peridot::math::Vector2(
                                                    UIElementSize::Fixed(64.0),
                                                    UIElementSize::Fixed(20.0),
                                                ),
                                                debug_color: peridot::math::Vector4(
                                                    0.5, 0.0, 0.0, 1.0,
                                                ),
                                                font: Some(&main_font),
                                                text: "follow",
                                                ..Default::default()
                                            },
                                        ],
                                        ..Default::default()
                                    },
                                ],
                                ..Default::default()
                            },
                        ],
                        ..Default::default()
                    },
                ],
                ..Default::default()
            },
        ],
        ..Default::default()
    };
    let ui_tree = UIElement {
        size: peridot::math::Vector2(UIElementSize::Fill, UIElementSize::Fill),
        padding: RectEdge::all(8.0),
        debug_color: peridot::math::Vector4(1.0, 1.0, 1.0, 0.0),
        children_layout: ChildrenLayoutMode::Vertical {
            direction: LayoutDirection::Normal,
            justify: LayoutJustify::Start,
            alignment: LayoutAlignment::Start,
            overflow: Overflow::Overflow,
            gap: 8.0,
        },
        children: vec![
            UIElement {
                size: peridot::math::Vector2(
                    UIElementSize::Percent(50.0),
                    UIElementSize::FitContent,
                ),
                debug_color: peridot::math::Vector4(1.0, 0.0, 1.0, 0.5),
                children_layout: ChildrenLayoutMode::Horizontal {
                    direction: LayoutDirection::Normal,
                    justify: LayoutJustify::Center,
                    alignment: LayoutAlignment::Center,
                    overflow: Overflow::Wrap,
                    gap: 8.0,
                },
                children: vec![
                    UIElement {
                        size: peridot::math::Vector2(
                            UIElementSize::Fixed(100.0),
                            UIElementSize::Fixed(32.0),
                        ),
                        debug_color: peridot::math::Vector4(1.0, 1.0, 0.0, 0.8),
                        ..Default::default()
                    },
                    UIElement {
                        size: peridot::math::Vector2(
                            UIElementSize::Fixed(100.0),
                            UIElementSize::Fixed(16.0),
                        ),
                        debug_color: peridot::math::Vector4(1.0, 0.5, 0.5, 0.8),
                        ..Default::default()
                    },
                    UIElement {
                        size: peridot::math::Vector2(
                            UIElementSize::Fixed(100.0),
                            UIElementSize::Fixed(24.0),
                        ),
                        debug_color: peridot::math::Vector4(1.0, 0.5, 0.5, 0.8),
                        ..Default::default()
                    },
                    UIElement {
                        size: peridot::math::Vector2(
                            UIElementSize::Fixed(32.0),
                            UIElementSize::Fixed(32.0),
                        ),
                        debug_color: peridot::math::Vector4(1.0, 0.5, 0.5, 0.8),
                        ..Default::default()
                    },
                    UIElement {
                        size: peridot::math::Vector2(
                            UIElementSize::Fixed(100.0),
                            UIElementSize::Fixed(40.0),
                        ),
                        debug_color: peridot::math::Vector4(1.0, 0.5, 0.5, 0.8),
                        ..Default::default()
                    },
                    UIElement {
                        size: peridot::math::Vector2(
                            UIElementSize::Fixed(100.0),
                            UIElementSize::Fixed(32.0),
                        ),
                        debug_color: peridot::math::Vector4(1.0, 0.5, 0.5, 0.8),
                        ..Default::default()
                    },
                    UIElement {
                        size: peridot::math::Vector2(
                            UIElementSize::Fixed(100.0),
                            UIElementSize::Fixed(32.0),
                        ),
                        debug_color: peridot::math::Vector4(1.0, 0.5, 0.5, 0.8),
                        ..Default::default()
                    },
                    UIElement {
                        size: peridot::math::Vector2(
                            UIElementSize::Fixed(100.0),
                            UIElementSize::Fixed(24.0),
                        ),
                        debug_color: peridot::math::Vector4(1.0, 0.5, 0.5, 0.8),
                        ..Default::default()
                    },
                ],
                ..Default::default()
            },
            UIElement {
                size: peridot::math::Vector2(
                    UIElementSize::Fixed(192.0),
                    UIElementSize::Fixed(32.0),
                ),
                debug_color: peridot::math::Vector4(0.0, 1.0, 1.0, 0.8),
                ..Default::default()
            },
            UIElement {
                size: peridot::math::Vector2(UIElementSize::Fill, UIElementSize::FitContent),
                debug_color: peridot::math::Vector4(1.0, 1.0, 1.0, 0.2),
                padding: RectEdge::all(4.0),
                children_layout: ChildrenLayoutMode::Grid {
                    columns: vec![
                        GridCellSize::FitContent,
                        GridCellSize::Flexible(1.0),
                        GridCellSize::Flexible(1.0),
                    ],
                    rows: vec![GridCellSize::FitContent, GridCellSize::FitContent],
                    column_alignment: LayoutAlignment::Start,
                    row_alignment: LayoutAlignment::Start,
                    gap: 4.0,
                },
                children: vec![
                    UIElement {
                        size: peridot::math::Vector2(
                            UIElementSize::Fixed(32.0),
                            UIElementSize::Fixed(32.0),
                        ),
                        debug_color: peridot::math::Vector4(0.0, 1.0, 1.0, 0.8),
                        ..Default::default()
                    },
                    UIElement {
                        size: peridot::math::Vector2(
                            UIElementSize::Fill,
                            UIElementSize::Fixed(64.0),
                        ),
                        debug_color: peridot::math::Vector4(0.0, 1.0, 1.0, 0.8),
                        ..Default::default()
                    },
                    UIElement {
                        size: peridot::math::Vector2(UIElementSize::Fill, UIElementSize::Fill),
                        debug_color: peridot::math::Vector4(0.0, 1.0, 1.0, 0.8),
                        ..Default::default()
                    },
                    UIElement {
                        size: peridot::math::Vector2(
                            UIElementSize::Fixed(32.0),
                            UIElementSize::Fixed(32.0),
                        ),
                        debug_color: peridot::math::Vector4(0.0, 1.0, 1.0, 0.8),
                        ..Default::default()
                    },
                    UIElement {
                        size: peridot::math::Vector2(
                            UIElementSize::Fixed(32.0),
                            UIElementSize::Fixed(32.0),
                        ),
                        debug_color: peridot::math::Vector4(0.0, 1.0, 1.0, 0.8),
                        ..Default::default()
                    },
                ],
                ..Default::default()
            },
            user_card_cell_ui,
        ],
        ..Default::default()
    };
    let mut boxes = BoxInstanceEmitter::new();
    layout1(
        &ui_tree,
        &mut boxes,
        LayoutRect {
            pos: peridot::math::Vector2(0.0, 0.0),
            size: peridot::math::Vector2(UI_BASE_WIDTH, UI_BASE_HEIGHT),
        },
    );

    main_font.rasterize_dirty_glyphs(e, &mut pmm);

    let total_box_count = boxes
        .groups
        .iter()
        .map(|x| x.instances.len())
        .sum::<usize>();
    println!("layout boxes: {total_box_count}");
    // TODO: レイアウトボックスが1024を超えたときの対応（どうしよ）
    assert!(total_box_count < 1024, "too many layout boxes!!");
    let [vertex_buffer, instance_buffer] = pmm
        .allocate_device_local_buffer_array(
            e.graphics(),
            [
                br::BufferCreateInfo::new_for_type::<[Vertex; 4]>(
                    br::BufferUsage::VERTEX_BUFFER.transfer_dest(),
                ),
                br::BufferCreateInfo::new_for_type::<[BoxInstance; 1024]>(
                    br::BufferUsage::VERTEX_BUFFER.transfer_dest(),
                ),
            ],
        )
        .expect("Failed to create device local buffers");
    #[repr(C)]
    struct BufferInitContent {
        vertex: [Vertex; 4],
        target_pixel_size: peridot::math::Vector2F32,
    }
    let mut init_buffer = pmm
        .allocate_upload_buffer(
            e.graphics(),
            br::BufferCreateInfo::new_for_type::<BufferInitContent>(br::BufferUsage::TRANSFER_SRC),
        )
        .expect("Failed to create init buffer");
    init_buffer
        .write_content(BufferInitContent {
            vertex: [
                Vertex {
                    pos: peridot::math::Vector2(0.0, 0.0),
                },
                Vertex {
                    pos: peridot::math::Vector2(1.0, 0.0),
                },
                Vertex {
                    pos: peridot::math::Vector2(0.0, 1.0),
                },
                Vertex {
                    pos: peridot::math::Vector2(1.0, 1.0),
                },
            ],
            target_pixel_size: peridot::math::Vector2(UI_BASE_WIDTH, UI_BASE_HEIGHT),
        })
        .expect("Failed to write init buffer content");
    let mut instance_init_buffer = pmm
        .allocate_upload_buffer(
            e.graphics(),
            br::BufferCreateInfo::new(
                core::mem::size_of::<BoxInstance>() * total_box_count,
                br::BufferUsage::TRANSFER_SRC,
            ),
        )
        .expect("Failed to create instance init buffer");
    struct DrawGroup {
        tex: BoxGroupTexture,
        instance_start: usize,
        instance_count: usize,
    }
    let mut draw_group = Vec::new();
    instance_init_buffer
        .guard_map(peridot_memory_manager::BufferMapMode::Write, |p| unsafe {
            let mut instance_start = 0;
            for g in boxes.groups.iter() {
                p.clone_slice_to(
                    instance_start * core::mem::size_of::<BoxInstance>(),
                    &g.instances,
                );
                draw_group.push(DrawGroup {
                    tex: g.texture,
                    instance_start,
                    instance_count: g.instances.len(),
                });
                instance_start += g.instances.len();
            }
        })
        .expect("init box instances");
    let content_init = e
        .submit_commands_async(|r| {
            r.copy_buffer(
                &init_buffer,
                &vertex_buffer,
                &[br::BufferCopy::copy_data::<[Vertex; 4]>(
                    core::mem::offset_of!(BufferInitContent, vertex) as _,
                    0,
                )],
            )
            .copy_buffer(
                &instance_init_buffer,
                &instance_buffer,
                &[br::BufferCopy {
                    srcOffset: 0,
                    dstOffset: 0,
                    size: (core::mem::size_of::<BoxInstance>() * total_box_count) as _,
                }],
            )
            .copy_buffer(
                &init_buffer,
                &prc_uniform_block_data,
                &[br::BufferCopy::copy_data::<peridot::math::Vector2F32>(
                    core::mem::offset_of!(BufferInitContent, target_pixel_size) as _,
                    prc_camera_parameter_offset
                        + core::mem::offset_of!(CameraParameterUniformBlockData, target_pixel_size)
                            as u64,
                )],
            )
            .pipeline_barrier(
                br::PipelineStageFlags::TRANSFER,
                br::PipelineStageFlags::VERTEX_INPUT | br::PipelineStageFlags::VERTEX_SHADER,
                0,
                &[br::vk::VkMemoryBarrier {
                    sType: br::vk::VkMemoryBarrier::TYPE,
                    pNext: core::ptr::null(),
                    srcAccessMask: br::AccessFlags::TRANSFER.write,
                    dstAccessMask: br::AccessFlags::VERTEX_ATTRIBUTE_READ
                        | br::AccessFlags::UNIFORM_READ,
                }],
                &[],
                &[],
            )
        })
        .expect("Failed to send init commands");

    let mut ui_render_cp = br::CommandPoolObject::new(
        e.graphics().device().clone(),
        &br::CommandPoolCreateInfo::new(e.graphics_queue_family_index()),
    )
    .expect("Failed to create ui render command pool");
    let [ui_render_cb] = unsafe {
        e.graphics()
            .device()
            .allocate_command_buffer_array(&br::CommandBufferFixedCountAllocateInfo::new(
                &mut ui_render_cp,
                br::CommandBufferLevel::Secondary,
            ))
            .expect("Failed to allocate ui render command buffer")
    };
    let mut ui_render_cb = ui_render_cb.clone_parent();
    unsafe {
        let inherit_info = br::CommandBufferInheritanceInfo::of_rendering(
            main_renderpass.subpass(0),
            None::<&br::FramebufferObject<peridot::DeviceObject>>,
        );
        let begin_info = br::CommandBufferBeginInfo::new()
            .with_inheritance_info(&inherit_info)
            .renderpass_continue()
            .simultaneous_use();

        ui_render_cb
            .begin(&begin_info)
            .expect("Failed to begin ui render command recording")
    }
    .bind_pipeline(br::PipelineBindPoint::Graphics, &unlit_fill_pipeline)
    .bind_descriptor_sets(
        br::PipelineBindPoint::Graphics,
        &unlit_fill_pipeline_layout,
        0,
        &[
            prc_camera_parameter_descriptor_set,
            prc_object_parameter_descriptor_set,
        ],
        &[],
    )
    .bind_vertex_buffers(
        0,
        &[
            vertex_buffer.as_transparent_ref(),
            instance_buffer.as_transparent_ref(),
        ],
        &[0, 0],
    )
    .inject(|rec| {
        draw_group.iter().fold(rec, |rec, g| {
            rec.bind_descriptor_sets(
                br::PipelineBindPoint::Graphics,
                &unlit_fill_pipeline_layout,
                2,
                &[match g.tex {
                    BoxGroupTexture::GlyphAtlas => ui_descriptor_pool_glyph_tex,
                    BoxGroupTexture::User(_) => ui_descriptor_pool_tex1,
                }],
                &[],
            )
            .draw(4, g.instance_count as _, 0, g.instance_start as _)
        })
    })
    .end()
    .expect("Failed to finish ui render command recording");

    let mut render_cp = br::CommandPoolObject::new(
        e.graphics().device().clone(),
        &br::CommandPoolCreateInfo::new(e.graphics_queue_family_index()),
    )
    .expect("Failed to create render command pool");
    let render_cb = unsafe {
        e.graphics()
            .device()
            .allocate_command_buffers_alloc(&br::CommandBufferAllocateInfo::new(
                &mut render_cp,
                e.back_buffer_count() as _,
                br::CommandBufferLevel::Primary,
            ))
            .expect("Failed to allocate render command buffers")
    };
    let mut render_cb = render_cb
        .into_iter()
        .map(|x| x.clone_parent())
        .collect::<Vec<_>>();
    for (cb, fb) in render_cb.iter_mut().zip(main_framebuffers.iter()) {
        unsafe {
            cb.begin(&br::CommandBufferBeginInfo::new())
                .expect("Failed to begin render command recording")
                .begin_render_pass(
                    &br::RenderPassBeginInfo::new(
                        &main_renderpass,
                        fb,
                        scissor_rect,
                        &[br::ClearValue::color_f32([0.1, 0.2, 0.3, 0.0])],
                    ),
                    br::SubpassContents::SecondaryCommandBuffers,
                )
                .execute_commands(&[ui_render_cb.as_transparent_ref()])
                .end_render_pass()
                .end()
                .expect("Failed to finish render command recording");
        }
    }

    content_init.await.expect("Failed to initialize content");

    let mut new_size: Option<peridot::math::Vector2<u32>> = None;

    loop {
        match e.next_event().await {
            peridot::Event::Shutdown => break,
            peridot::Event::NextFrame => {
                if let Some(ns) = new_size.take() {
                    e.wait_for_last_rendering_completion();

                    unsafe {
                        render_cp
                            .reset(br::CommandPoolResetFlags::RELEASE_RESOURCES)
                            .expect("Failed to reset render command pool");
                        ui_render_cp
                            .reset(br::CommandPoolResetFlags::RELEASE_RESOURCES)
                            .expect("Failed to reset ui render command pool");
                    }
                    drop(main_framebuffers);
                    drop(backbuffer_resources);

                    e.resize_presenter_backbuffers(ns);

                    scissor_rect = br::vk::VkExtent2D::from(ns).into_rect(br::vk::VkOffset2D::ZERO);
                    viewport = scissor_rect.make_viewport(0.0..1.0);

                    backbuffer_resources = e
                        .iter_back_buffers()
                        .map(|x| LocalImageView {
                            handle: unsafe {
                                br::vkfn_wrapper::create_image_view(
                                    e.graphics_device().native_ptr(),
                                    &br::ImageViewCreateInfo::new(
                                        &x,
                                        br::ImageSubresourceRange::new(
                                            br::AspectMask::COLOR,
                                            0..1,
                                            0..1,
                                        ),
                                        br::vk::VK_IMAGE_VIEW_TYPE_2D,
                                        e.back_buffer_format(),
                                    ),
                                    None,
                                )
                                .expect("create_image_view failed")
                            },
                            device: e.graphics_device().clone(),
                        })
                        .collect::<Vec<_>>();
                    main_framebuffers = backbuffer_resources
                        .iter()
                        .map(|bb| {
                            br::FramebufferObject::new(
                                e.graphics().device().clone(),
                                &br::FramebufferCreateInfo::new(
                                    &main_renderpass,
                                    &[bb.as_transparent_ref()],
                                    ns.0,
                                    ns.1,
                                ),
                            )
                            .expect("Failed to create main framebuffer")
                        })
                        .collect::<Vec<_>>();

                    let [pl] = match shader.passes["Unlit"] {
                        peridot_rendering_configuration::ShadingPassVk::SimpleDeriveBuiltinPass { ref name } => {
                            unreachable!("using builtin pass: {name}");
                        }
                        peridot_rendering_configuration::ShadingPassVk::Custom {
                            ref option_overrides,
                            ref variants
                        } => {
                            let peridot_rendering_configuration::Code {
                                ref vertex_semantic_to_location,
                                ref vertex_entry_point_name,
                                ref fragment_entry_point_name,
                                ref words,
                            } = variants[&peridot_rendering_configuration::VariantKey { instancing: false }];

                            let shader = br::ShaderModuleObject::new(
                                e.graphics().device().clone(),
                                &br::ShaderModuleCreateInfo::new(words),
                            )
                            .expect("ShaderModuleObject::new");
                            let mut shader_stage_with_ep_names = Vec::with_capacity(2);
                            if let Some(e) = vertex_entry_point_name {
                                shader_stage_with_ep_names.push((
                                    br::ShaderStage::Vertex,
                                    std::ffi::CString::new(e as &str).expect("invalid entry point name"),
                                ));
                            }
                            if let Some(e) = fragment_entry_point_name {
                                shader_stage_with_ep_names.push((
                                    br::ShaderStage::Fragment,
                                    std::ffi::CString::new(e as &str).expect("invalid entry point name"),
                                ));
                            }

                            e.graphics()
                                .device()
                                .new_graphics_pipeline_array(
                                    &[br::GraphicsPipelineCreateInfo::new(
                                        &unlit_fill_pipeline_layout,
                                        main_renderpass.subpass(0),
                                        &shader_stage_with_ep_names
                                            .iter()
                                            .map(|&(s, ref e)| shader.on_stage(s, e))
                                            .collect::<Vec<_>>(),
                                        &br::PipelineVertexInputStateCreateInfo::new(
                                            &[
                                                br::VertexInputBindingDescription::per_vertex_typed::<Vertex>(0),
                                                br::VertexInputBindingDescription::per_instance_typed::<BoxInstance>(1)
                                            ], &[
                                                br::VertexInputAttributeDescription {
                                                    binding: 0,
                                                    location: vertex_semantic_to_location[&peridot_rendering_configuration::VertexInputSemantic::Position(0)],
                                                    format: br::vk::VK_FORMAT_R32G32_SFLOAT,
                                                    offset: core::mem::offset_of!(Vertex, pos) as _
                                                },
                                                br::VertexInputAttributeDescription {
                                                    binding: 1,
                                                    location: vertex_semantic_to_location[&peridot_rendering_configuration::VertexInputSemantic::Position(1)],
                                                    format: br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
                                                    offset: core::mem::offset_of!(BoxInstance, pos_st) as _
                                                },
                                                br::VertexInputAttributeDescription {
                                                    binding: 1,
                                                    location: vertex_semantic_to_location[&peridot_rendering_configuration::VertexInputSemantic::Texcoord(0)],
                                                    format: br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
                                                    offset: core::mem::offset_of!(BoxInstance, uv_st) as _
                                                },
                                                br::VertexInputAttributeDescription {
                                                    binding: 1,
                                                    location: vertex_semantic_to_location[&peridot_rendering_configuration::VertexInputSemantic::Color(0)],
                                                    format: br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
                                                    offset: core::mem::offset_of!(BoxInstance, col) as _
                                                }
                                            ]
                                        ),
                                        &br::PipelineInputAssemblyStateCreateInfo::new(
                                            br::PrimitiveTopology::TriangleStrip,
                                        ),
                                        &br::PipelineViewportStateCreateInfo::new_array(
                                            &[viewport],
                                            &[scissor_rect],
                                        ),
                                        &br::PipelineRasterizationStateCreateInfo::new(
                                            br::PolygonMode::Fill,
                                            br::CullModeFlags::NONE,
                                            br::FrontFace::CounterClockwise,
                                        ),
                                        &br::PipelineColorBlendStateCreateInfo::new(&[
                                            br::vk::VkPipelineColorBlendAttachmentState::PREMULTIPLIED,
                                        ]),
                                    )
                                    .set_multisample_state(&br::PipelineMultisampleStateCreateInfo::new())],
                                    None::<&br::PipelineCacheObject<peridot::DeviceObject>>,
                                )
                                .expect("new_graphics_pipeline_array")
                        }
                    };
                    unlit_fill_pipeline = pl.clone_parent();

                    struct BufferUpdateContent {
                        target_pixel_size: peridot::math::Vector2F32,
                    }
                    let mut update_buffer = pmm
                        .allocate_upload_buffer(
                            e.graphics(),
                            br::BufferCreateInfo::new_for_type::<BufferUpdateContent>(
                                br::BufferUsage::TRANSFER_SRC,
                            ),
                        )
                        .expect("allocate_upload_buffer");
                    unsafe {
                        update_buffer
                            .write_content_unchecked(BufferUpdateContent {
                                target_pixel_size: peridot::math::Vector2(
                                    UI_BASE_WIDTH,
                                    UI_BASE_WIDTH * ns.1 as f32 / ns.0 as f32,
                                ),
                            })
                            .expect("write_content(update)");
                    }
                    e.submit_commands(|rec| {
                        rec.pipeline_barrier(
                            br::PipelineStageFlags::VERTEX_SHADER,
                            br::PipelineStageFlags::TRANSFER,
                            0,
                            &[br::vk::VkMemoryBarrier {
                                sType: br::vk::VkMemoryBarrier::TYPE,
                                pNext: core::ptr::null(),
                                srcAccessMask: br::AccessFlags::UNIFORM_READ,
                                dstAccessMask: br::AccessFlags::TRANSFER.write,
                            }],
                            &[],
                            &[],
                        )
                        .copy_buffer(
                            &update_buffer,
                            &prc_uniform_block_data,
                            &[br::BufferCopy::copy_data::<peridot::math::Vector2F32>(
                                core::mem::offset_of!(BufferUpdateContent, target_pixel_size) as _,
                                prc_camera_parameter_offset
                                    + core::mem::offset_of!(
                                        CameraParameterUniformBlockData,
                                        target_pixel_size
                                    ) as u64,
                            )],
                        )
                        .pipeline_barrier(
                            br::PipelineStageFlags::TRANSFER,
                            br::PipelineStageFlags::VERTEX_SHADER,
                            0,
                            &[br::vk::VkMemoryBarrier {
                                sType: br::vk::VkMemoryBarrier::TYPE,
                                pNext: core::ptr::null(),
                                srcAccessMask: br::AccessFlags::TRANSFER.write,
                                dstAccessMask: br::AccessFlags::UNIFORM_READ,
                            }],
                            &[],
                            &[],
                        )
                    })
                    .expect("submit_commands(update commands)");

                    unsafe {
                        let inherit_info = br::CommandBufferInheritanceInfo::of_rendering(
                            main_renderpass.subpass(0),
                            None::<&br::FramebufferObject<peridot::DeviceObject>>,
                        );
                        let begin_info = br::CommandBufferBeginInfo::new()
                            .with_inheritance_info(&inherit_info)
                            .renderpass_continue()
                            .simultaneous_use();

                        ui_render_cb
                            .begin(&begin_info)
                            .expect("Failed to begin ui render command recording")
                    }
                    .bind_pipeline(br::PipelineBindPoint::Graphics, &unlit_fill_pipeline)
                    .bind_descriptor_sets(
                        br::PipelineBindPoint::Graphics,
                        &unlit_fill_pipeline_layout,
                        0,
                        &[
                            prc_camera_parameter_descriptor_set,
                            prc_object_parameter_descriptor_set,
                        ],
                        &[],
                    )
                    .bind_vertex_buffers(
                        0,
                        &[
                            vertex_buffer.as_transparent_ref(),
                            instance_buffer.as_transparent_ref(),
                        ],
                        &[0, 0],
                    )
                    .inject(|rec| {
                        draw_group.iter().fold(rec, |rec, g| {
                            rec.bind_descriptor_sets(
                                br::PipelineBindPoint::Graphics,
                                &unlit_fill_pipeline_layout,
                                2,
                                &[match g.tex {
                                    BoxGroupTexture::GlyphAtlas => ui_descriptor_pool_glyph_tex,
                                    BoxGroupTexture::User(_) => ui_descriptor_pool_tex1,
                                }],
                                &[],
                            )
                            .draw(
                                4,
                                g.instance_count as _,
                                0,
                                g.instance_start as _,
                            )
                        })
                    })
                    .end()
                    .expect("Failed to finish ui render command recording");

                    for (cb, fb) in render_cb.iter_mut().zip(main_framebuffers.iter()) {
                        unsafe {
                            cb.begin(&br::CommandBufferBeginInfo::new())
                                .expect("Failed to begin render command recording")
                                .begin_render_pass(
                                    &br::RenderPassBeginInfo::new(
                                        &main_renderpass,
                                        fb,
                                        scissor_rect,
                                        &[br::ClearValue::color_f32([0.1, 0.2, 0.3, 0.0])],
                                    ),
                                    br::SubpassContents::SecondaryCommandBuffers,
                                )
                                .execute_commands(&[ui_render_cb.as_transparent_ref()])
                                .end_render_pass()
                                .end()
                                .expect("Failed to finish render command recording");
                        }
                    }
                }

                let fd = e.prepare_frame().expect("Failed to prepare frame");
                let mut render_submission = peridot::SubmissionBatchBuilder::new();
                render_submission.add_command_buffers(
                    render_cb[fd.backbuffer_index as usize..=fd.backbuffer_index as usize]
                        .iter()
                        .map(|x| x.as_transparent_ref()),
                );
                e.do_render(fd.backbuffer_index, None, render_submission)
                    .expect("Failed to render");
            }
            peridot::Event::Resize(ns) => {
                new_size = Some(ns);
            }
        }
    }

    unsafe {
        e.graphics().device().wait().expect("Failed to wait works");
    }
}

struct LocalImageView {
    handle: br::vk::VkImageView,
    device: peridot::VulkanGfx,
}
impl Drop for LocalImageView {
    fn drop(&mut self) {
        unsafe {
            br::vkfn_wrapper::destroy_image_view(self.device.native_ptr(), self.handle, None);
        }
    }
}
impl br::VkHandle for LocalImageView {
    type Handle = br::vk::VkImageView;

    fn native_ptr(&self) -> Self::Handle {
        self.handle
    }
}

pub struct StencilCompare {
    pub op: br::CompareOp,
    pub r#ref: u32,
    pub mask: u32,
}
impl StencilCompare {
    pub const ALWAYS: Self = Self::new(br::CompareOp::Always, 0);

    #[inline(always)]
    pub const fn eq(r#ref: u32) -> Self {
        Self::new(br::CompareOp::Equal, r#ref)
    }

    #[inline(always)]
    pub const fn new(op: br::CompareOp, r#ref: u32) -> Self {
        Self {
            op,
            r#ref,
            mask: 0xffffffff,
        }
    }

    #[inline(always)]
    pub const fn mask(mut self, mask: u32) -> Self {
        self.mask = mask;
        self
    }
}

pub struct StencilState {
    pub pass_op: br::StencilOp,
    pub fail_op: br::StencilOp,
    pub depth_fail_op: br::StencilOp,
    pub compare: StencilCompare,
    pub write_mask: u32,
}
impl StencilState {
    pub const NOOP: Self = Self::with_op_all(br::StencilOp::Keep);

    #[inline(always)]
    pub const fn with_op_all(op: br::StencilOp) -> Self {
        Self {
            pass_op: op,
            fail_op: op,
            depth_fail_op: op,
            compare: StencilCompare::ALWAYS,
            write_mask: 0xffffffff,
        }
    }

    #[inline(always)]
    pub const fn when(mut self, compare: StencilCompare) -> Self {
        self.compare = compare;
        self
    }

    #[inline(always)]
    pub const fn write_mask(mut self, mask: u32) -> Self {
        self.write_mask = mask;
        self
    }

    #[inline(always)]
    pub const fn into_vk(self) -> br::vk::VkStencilOpState {
        br::vk::VkStencilOpState {
            passOp: self.pass_op as _,
            failOp: self.fail_op as _,
            depthFailOp: self.depth_fail_op as _,
            compareOp: self.compare.op as _,
            reference: self.compare.r#ref,
            compareMask: self.compare.mask,
            writeMask: self.write_mask,
        }
    }
}
