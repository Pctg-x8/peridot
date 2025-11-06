use bedrock::{
    self as br, CommandBufferMut, CommandPoolMut, Device, Image, ImageChild, RenderPass,
    SubmissionBatch, TypedVulkanStructure, VkHandle, VulkanStructure,
};
use peridot::math::Zero;
use peridot_vertex_processing_pack::PvpShaderModules;
use peridot_vg::{Font, FontProvider, FontProviderConstruct};

#[repr(C)]
pub struct Vertex {
    pub pos: peridot::math::Vector2<f32>,
}
#[repr(C)]
#[derive(Clone)]
pub struct BoxInstance {
    pub pos_st: peridot::math::Vector4<f32>,
    pub col: peridot::math::Vector4<f32>,
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
}
impl TextFontData {
    pub fn new(internal: peridot_vg::DefaultFont) -> Self {
        Self { internal }
    }

    pub fn request_char(&self, c: char) -> CharacterData {
        let glyph_id = self.internal.glyph_id(c).expect("font.glyph_id failed");
        let bounds = self.internal.bounds(&glyph_id).expect("font.bounds failed");

        CharacterData {
            width: bounds.size.width,
            height: bounds.size.height,
            left_offset: bounds.min_x(),
            top_offset: 0.0,
            advance_x: self
                .internal
                .advance_h(&glyph_id)
                .expect("font.advance_h failed"),
            ascend: self.internal.ascent(),
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
    boxes: &mut Vec<BoxInstance>,
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

fn layout1(target: &UIElement, boxes: &mut Vec<BoxInstance>, layout_rect: LayoutRect) {
    if layout_rect.size.0 > 0.0 && layout_rect.size.1 > 0.0 {
        boxes.push(BoxInstance {
            pos_st: peridot::math::Vector4(
                layout_rect.size.0 * target.scale.0,
                layout_rect.size.1 * target.scale.1,
                layout_rect.pos.0,
                layout_rect.pos.1,
            ),
            col: target.debug_color,
        });
    }

    if let Some(ref f) = target.font {
        let mut char_offset_x = 0.0;
        for c in target.text.chars() {
            let cd = f.request_char(c);

            boxes.push(BoxInstance {
                pos_st: peridot::math::Vector4(
                    cd.width * target.scale.0,
                    cd.height * target.scale.1,
                    layout_rect.pos.0 + char_offset_x,
                    layout_rect.pos.1 + cd.ascend - cd.height,
                ),
                col: peridot::math::Vector4(1.0, 1.0, 1.0, 0.5),
            });

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

pub async fn game_main(e: &mut peridot::Engine<'_, impl peridot::NativeLinker>) {
    let screen_size = e.back_buffer_size();
    let mut scissor_rect = br::Extent2D::from(screen_size).into_rect(br::vk::VkOffset2D::ZERO);
    let mut viewport = scissor_rect.make_viewport(0.0..1.0);

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

    let unlit_fill_shader = e
        .load("shaders.unlit_fill")
        .expect("Failed to load unlit_fill shader");
    let unlit_fill_shader_modules =
        PvpShaderModules::new(e.graphics().device(), &unlit_fill_shader)
            .expect("Failed to create unlit_fill shader modules");
    let unlit_fill_pipeline_layout = br::PipelineLayoutObject::new(
        e.graphics().device().clone(),
        &br::PipelineLayoutCreateInfo::new(
            &[],
            &[br::vk::VkPushConstantRange::for_type::<
                peridot::math::Vector2<f32>,
            >(br::vk::VK_SHADER_STAGE_VERTEX_BIT, 0)],
        ),
    )
    .expect("Failed to create pipeline layout");
    let [unlit_fill_pipeline] = e
        .graphics()
        .device()
        .new_graphics_pipeline_array(
            &[br::GraphicsPipelineCreateInfo::new(
                &unlit_fill_pipeline_layout,
                main_renderpass.subpass(0),
                &[
                    unlit_fill_shader_modules.pipeline_vertex_shader_stage(),
                    unlit_fill_shader_modules
                        .pipeline_fragment_shader_stage()
                        .expect("no fsh?"),
                ],
                &br::PipelineVertexInputStateCreateInfo::new(
                    &unlit_fill_shader.vertex_bindings,
                    &unlit_fill_shader.vertex_attributes,
                ),
                &br::PipelineInputAssemblyStateCreateInfo::new(
                    br::PrimitiveTopology::TriangleStrip,
                ),
                &br::PipelineViewportStateCreateInfo::new_array(&[viewport], &[scissor_rect]),
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
        .expect("Failed to create unlit fill pipeline");
    let mut unlit_fill_pipeline = unlit_fill_pipeline.clone_parent();

    let main_font = TextFontData::new(
        peridot_vg::DefaultFontProvider::new()
            .expect("DefaultFontProvider::new failed")
            .best_match("system-ui", &peridot_vg::FontProperties::default(), 12.0)
            .expect("DefaultFontProvider::best_match"),
    );

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
                                ..Default::default()
                            },
                            // level
                            UIElement {
                                size: peridot::math::Vector2(
                                    UIElementSize::Fixed(64.0),
                                    UIElementSize::Fixed(20.0),
                                ),
                                debug_color: peridot::math::Vector4(0.5, 0.0, 0.0, 1.0),
                                ..Default::default()
                            },
                        ],
                        font: Some(&main_font),
                        text: "player #111",
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
    let mut boxes = Vec::new();
    layout1(
        &ui_tree,
        &mut boxes,
        LayoutRect {
            pos: peridot::math::Vector2(0.0, 0.0),
            size: peridot::math::Vector2(640.0, 480.0),
        },
    );

    println!("layout boxes: {}", boxes.len());
    // TODO: レイアウトボックスが1024を超えたときの対応（どうしよ）
    assert!(boxes.len() < 1024, "too many layout boxes!!");
    let mut pmm = peridot_memory_manager::MemoryManager::new(e.graphics());
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
        })
        .expect("Failed to write init buffer content");
    let mut instance_init_buffer = pmm
        .allocate_upload_buffer(
            e.graphics(),
            br::BufferCreateInfo::new(
                core::mem::size_of::<BoxInstance>() * boxes.len(),
                br::BufferUsage::TRANSFER_SRC,
            ),
        )
        .expect("Failed to create instance init buffer");
    instance_init_buffer
        .clone_content_from_slice(&boxes)
        .expect("Failed to write instance init content");
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
                    size: (core::mem::size_of::<BoxInstance>() * boxes.len()) as _,
                }],
            )
            .pipeline_barrier(
                br::PipelineStageFlags::TRANSFER,
                br::PipelineStageFlags::VERTEX_INPUT,
                0,
                &[br::vk::VkMemoryBarrier {
                    sType: br::vk::VkMemoryBarrier::TYPE,
                    pNext: core::ptr::null(),
                    srcAccessMask: br::AccessFlags::TRANSFER.write,
                    dstAccessMask: br::AccessFlags::VERTEX_ATTRIBUTE_READ,
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
    .push_constant(
        &unlit_fill_pipeline_layout,
        br::vk::VK_SHADER_STAGE_VERTEX_BIT,
        0,
        &peridot::math::Vector2(640.0f32, 480.0),
    )
    .bind_vertex_buffers(
        0,
        &[
            vertex_buffer.as_transparent_ref(),
            instance_buffer.as_transparent_ref(),
        ],
        &[0, 0],
    )
    .draw(4, boxes.len() as _, 0, 0)
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

                    let [p1] = e
                        .graphics()
                        .device()
                        .new_graphics_pipeline_array(
                            &[br::GraphicsPipelineCreateInfo::new(
                                &unlit_fill_pipeline_layout,
                                main_renderpass.subpass(0),
                                &[
                                    unlit_fill_shader_modules.pipeline_vertex_shader_stage(),
                                    unlit_fill_shader_modules
                                        .pipeline_fragment_shader_stage()
                                        .expect("no fsh?"),
                                ],
                                &br::PipelineVertexInputStateCreateInfo::new(
                                    &unlit_fill_shader.vertex_bindings,
                                    &unlit_fill_shader.vertex_attributes,
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
                        .expect("Failed to create unlit fill pipeline");
                    unlit_fill_pipeline = p1.clone_parent();

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
                    .push_constant(
                        &unlit_fill_pipeline_layout,
                        br::vk::VK_SHADER_STAGE_VERTEX_BIT,
                        0,
                        &peridot::math::Vector2(640.0f32, ns.1 as f32 * 640.0f32 / ns.0 as f32),
                    )
                    .bind_vertex_buffers(
                        0,
                        &[
                            vertex_buffer.as_transparent_ref(),
                            instance_buffer.as_transparent_ref(),
                        ],
                        &[0, 0],
                    )
                    .draw(4, boxes.len() as _, 0, 0)
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
