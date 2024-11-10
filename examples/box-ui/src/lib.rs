use bedrock::{
    self as br, CommandBufferMut, CommandPoolMut, Device, GraphicsPipelineBuilder, Image,
    ImageChild, RenderPass, SubmissionBatch, VkHandle, VkRawHandle, VulkanStructure,
};
use peridot_vertex_processing_pack::PvpShaderModules;

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
    Flexible,
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
        gap: f32,
    },
}

pub struct UIElement {
    pub size: peridot::math::Vector2<UIElementSize>,
    pub scale: peridot::math::Vector2<f32>,
    pub offset: peridot::math::Vector2<f32>,
    pub margin: RectEdge<f32>,
    pub padding: RectEdge<f32>,
    pub layout_width: LayoutSize,
    pub layout_height: LayoutSize,
    pub children_layout: ChildrenLayoutMode,
    pub debug_color: peridot::math::Vector4<f32>,
    pub children: Vec<UIElement>,
}
impl Default for UIElement {
    fn default() -> Self {
        Self {
            size: peridot::math::Vector2(UIElementSize::FitContent, UIElementSize::FitContent),
            scale: peridot::math::Vector2(1.0, 1.0),
            offset: peridot::math::Vector2(0.0, 0.0),
            margin: RectEdge::all(0.0),
            padding: RectEdge::all(0.0),
            layout_width: LayoutSize::Unscaled,
            layout_height: LayoutSize::Unscaled,
            children_layout: ChildrenLayoutMode::Free,
            debug_color: peridot::math::Vector4(1.0, 1.0, 1.0, 1.0),
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
    pub fn right(&self) -> f32 {
        self.pos.0 + self.size.0
    }

    #[inline(always)]
    pub fn bottom(&self) -> f32 {
        self.pos.1 + self.size.1
    }
}

fn compute_layout_rect(
    target: &UIElement,
    available_size: peridot::math::Vector2<f32>,
) -> LayoutRect {
    // TODO: marginの考慮
    let pos = target.offset;
    let content_size = peridot::math::Vector2(
        match target.size.0 {
            UIElementSize::Fill => available_size.0,
            UIElementSize::Percent(p) => available_size.0 * p / 100.0,
            UIElementSize::Fixed(x) => x,
            UIElementSize::FitContent => {
                let base = match target.children_layout {
                    ChildrenLayoutMode::Free => target
                        .children
                        .iter()
                        .map(|c| compute_layout_rect(c, peridot::math::Vector2(0.0, 0.0)).right())
                        .fold(0.0f32, |a, b| a.max(b)),
                    ChildrenLayoutMode::Vertical { overflow, .. } => {
                        // TODO: overflow
                        let mut max_right = 0.0f32;
                        for c in target.children.iter() {
                            max_right = max_right.max(
                                compute_layout_rect(c, peridot::math::Vector2(0.0, 0.0)).right(),
                            );
                        }

                        max_right
                    }
                    ChildrenLayoutMode::Horizontal { overflow, gap, .. } => {
                        // TODO: overflow
                        let mut max_right = 0.0f32;
                        let mut first = true;
                        for c in target.children.iter() {
                            if !first {
                                max_right += gap;
                            }

                            first = false;
                            max_right +=
                                compute_layout_rect(c, peridot::math::Vector2(0.0, 0.0)).right();
                        }

                        max_right
                    }
                    ChildrenLayoutMode::Grid {
                        ref columns, gap, ..
                    } => {
                        let mut max_right = 0.0f32;
                        let mut row_right = 0.0f32;
                        let mut current_column = 0;
                        for c in target.children.iter() {
                            if current_column > 0 {
                                row_right += gap;
                            }

                            let cell_content_rect =
                                compute_layout_rect(c, peridot::math::Vector2(0.0, 0.0));
                            row_right += match columns[current_column] {
                                GridCellSize::FitContent => cell_content_rect.right(),
                                GridCellSize::Fixed(x) => x,
                                // 一旦計算しない（あとでやる）
                                GridCellSize::Flexible(_) => 0.0,
                            };

                            current_column += 1;
                            if current_column >= columns.len() {
                                current_column = 0;
                                max_right = max_right.max(row_right);
                                row_right = 0.0;
                            }
                        }

                        max_right.max(row_right)
                    }
                };

                base + target.padding.left + target.padding.right
            }
        },
        match target.size.1 {
            UIElementSize::Fill => available_size.1,
            UIElementSize::Percent(p) => available_size.1 * p / 100.0,
            UIElementSize::Fixed(x) => x,
            UIElementSize::FitContent => {
                let base = match target.children_layout {
                    ChildrenLayoutMode::Free => target
                        .children
                        .iter()
                        .map(|c| compute_layout_rect(c, peridot::math::Vector2(0.0, 0.0)).bottom())
                        .fold(0.0f32, |a, b| a.max(b)),
                    ChildrenLayoutMode::Vertical { overflow, gap, .. } => {
                        // TODO: overflow
                        let mut max_bottom = 0.0f32;
                        let mut first = true;
                        for c in target.children.iter() {
                            if !first {
                                max_bottom += gap;
                            }

                            first = false;
                            max_bottom +=
                                compute_layout_rect(c, peridot::math::Vector2(0.0, 0.0)).bottom();
                        }

                        max_bottom
                    }
                    ChildrenLayoutMode::Horizontal { overflow, .. } => {
                        // TODO: overflow
                        let mut max_bottom = 0.0f32;
                        for c in target.children.iter() {
                            max_bottom = max_bottom.max(
                                compute_layout_rect(c, peridot::math::Vector2(0.0, 0.0)).bottom(),
                            );
                        }

                        max_bottom
                    }
                    ChildrenLayoutMode::Grid {
                        ref columns,
                        ref rows,
                        gap,
                        ..
                    } => {
                        let mut accum_bottom = 0.0f32;
                        let mut row_bottom = 0.0f32;
                        let mut current_column = 0;
                        let mut current_row = 0;
                        for c in target.children.iter() {
                            let cell_content_rect =
                                compute_layout_rect(c, peridot::math::Vector2(0.0, 0.0));
                            row_bottom = row_bottom.max(match rows[current_row] {
                                GridCellSize::FitContent => cell_content_rect.bottom(),
                                GridCellSize::Fixed(x) => x,
                                // 一旦計算しない（あとでやる）
                                GridCellSize::Flexible(_) => 0.0,
                            });

                            current_column += 1;
                            if current_column >= columns.len() {
                                current_column = 0;
                                accum_bottom += row_bottom + gap;
                                row_bottom = 0.0;
                                current_row += 1;
                            }
                        }

                        accum_bottom + row_bottom
                    }
                };

                base + target.padding.top + target.padding.bottom
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
        // VerticalのAlignmentはBaselineが取れないのでStartと同じ扱いにする
        LayoutAlignment::Baseline => 0.0,
    }
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

    let child_layout_global_offset = layout_rect.pos + target.padding.lt();
    let child_layout_available_size = layout_rect.size - target.padding.lt() - target.padding.rb();
    match target.children_layout {
        ChildrenLayoutMode::Free => {
            for c in target.children.iter() {
                let layout_rect = compute_layout_rect(c, child_layout_available_size);

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
                                let child_layout = compute_layout_rect(c, available_content_size);
                                let left_offset = compute_vertical_alignment_axis_offset(
                                    available_content_size.0,
                                    child_layout.size.0,
                                    alignment,
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
                                let child_layout = compute_layout_rect(c, available_content_size);
                                let left_offset = compute_vertical_alignment_axis_offset(
                                    available_content_size.0,
                                    child_layout.size.0,
                                    alignment,
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

                    for (c, r) in target.children.iter().zip(layout_rects.into_iter()) {
                        layout1(c, boxes, r);
                    }
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
                                let child_layout = compute_layout_rect(c, available_content_size);
                                let left_offset = compute_vertical_alignment_axis_offset(
                                    available_content_size.0,
                                    child_layout.size.0,
                                    alignment,
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
                                let child_layout = compute_layout_rect(c, available_content_size);
                                let left_offset = compute_vertical_alignment_axis_offset(
                                    available_content_size.0,
                                    child_layout.size.0,
                                    alignment,
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

                    for (c, r) in target.children.iter().zip(layout_rects.into_iter()) {
                        layout1(c, boxes, r);
                    }
                }
                LayoutJustify::Center => {
                    let mut layout_rects = Vec::with_capacity(target.children.len());
                    let mut top_offset = 0.0f32;
                    let mut available_size = child_layout_available_size;
                    match direction {
                        LayoutDirection::Normal => {
                            for c in target.children.iter() {
                                let child_layout = compute_layout_rect(c, available_size);
                                let left_offset = compute_vertical_alignment_axis_offset(
                                    available_content_size.0,
                                    child_layout.size.0,
                                    alignment,
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
                                let child_layout = compute_layout_rect(c, available_size);
                                let left_offset = compute_vertical_alignment_axis_offset(
                                    available_content_size.0,
                                    child_layout.size.0,
                                    alignment,
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
                        .map(|c| compute_layout_rect(c, child_layout_available_size).bottom())
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
                                let child_layout = compute_layout_rect(c, available_content_size);
                                let left_offset = compute_vertical_alignment_axis_offset(
                                    available_content_size.0,
                                    child_layout.size.0,
                                    alignment,
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
                                let child_layout = compute_layout_rect(c, available_content_size);
                                let left_offset = compute_vertical_alignment_axis_offset(
                                    available_content_size.0,
                                    child_layout.size.0,
                                    alignment,
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
                        .map(|c| compute_layout_rect(c, child_layout_available_size).bottom())
                        .sum::<f32>();
                    let space = available_content_size.1 - content_height;
                    let gap = space / (target.children.len() + 1) as f32;

                    let mut global_content_offset =
                        child_layout_global_offset + peridot::math::Vector2(0.0, gap);
                    match direction {
                        LayoutDirection::Normal => {
                            for c in target.children.iter() {
                                let child_layout = compute_layout_rect(c, available_content_size);
                                let left_offset = compute_vertical_alignment_axis_offset(
                                    available_content_size.0,
                                    child_layout.size.0,
                                    alignment,
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
                                let child_layout = compute_layout_rect(c, available_content_size);
                                let left_offset = compute_vertical_alignment_axis_offset(
                                    available_content_size.0,
                                    child_layout.size.0,
                                    alignment,
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
        } => {
            let mut available_content_size = child_layout_available_size;

            match (direction, justify) {
                (LayoutDirection::Normal, LayoutJustify::Start) => {
                    let mut global_content_offset = child_layout_global_offset;

                    for c in target.children.iter() {
                        let child_layout = compute_layout_rect(c, available_content_size);

                        layout1(
                            c,
                            boxes,
                            LayoutRect {
                                pos: global_content_offset + child_layout.pos,
                                size: child_layout.size,
                            },
                        );

                        global_content_offset.0 += child_layout.size.0 + gap;
                        available_content_size.0 -= child_layout.size.0 + gap;
                        // TODO: overflow
                    }
                }
                (LayoutDirection::Reverse, LayoutJustify::Start) => {
                    let mut global_content_offset = child_layout_global_offset;

                    for c in target.children.iter().rev() {
                        let child_layout = compute_layout_rect(c, available_content_size);

                        layout1(
                            c,
                            boxes,
                            LayoutRect {
                                pos: global_content_offset + child_layout.pos,
                                size: child_layout.size,
                            },
                        );

                        global_content_offset.0 += child_layout.size.0 + gap;
                        available_content_size.0 -= child_layout.size.0 + gap;
                        // TODO: overflow
                    }
                }
                (LayoutDirection::Normal, LayoutJustify::End) => {
                    let mut global_content_offset = peridot::math::Vector2(
                        child_layout_global_offset.0 + available_content_size.0,
                        child_layout_global_offset.1,
                    );

                    // 右から積んでくるので逆向きに動かす
                    for c in target.children.iter().rev() {
                        let child_layout = compute_layout_rect(c, available_content_size);

                        layout1(
                            c,
                            boxes,
                            LayoutRect {
                                pos: global_content_offset + child_layout.pos
                                    - peridot::math::Vector2(child_layout.size.0, 0.0),
                                size: child_layout.size,
                            },
                        );

                        global_content_offset.0 -= child_layout.size.0 + gap;
                        available_content_size.0 -= child_layout.size.0 + gap;
                        // TODO: overflow
                    }
                }
                (LayoutDirection::Reverse, LayoutJustify::End) => {
                    let mut global_content_offset = peridot::math::Vector2(
                        child_layout_global_offset.0 + available_content_size.0,
                        child_layout_global_offset.1,
                    );

                    // 右から積んでくるので逆向きに動かす
                    for c in target.children.iter() {
                        let child_layout = compute_layout_rect(c, available_content_size);

                        layout1(
                            c,
                            boxes,
                            LayoutRect {
                                pos: global_content_offset + child_layout.pos
                                    - peridot::math::Vector2(child_layout.size.0, 0.0),
                                size: child_layout.size,
                            },
                        );

                        global_content_offset.0 -= child_layout.size.0 + gap;
                        available_content_size.0 -= child_layout.size.0 + gap;
                        // TODO: overflow
                    }
                }
                (LayoutDirection::Normal, LayoutJustify::Center) => {
                    let mut offsets = Vec::with_capacity(target.children.len());
                    let mut left_offset = 0.0f32;
                    let mut available_size = child_layout_available_size;
                    for c in target.children.iter() {
                        let child_layout = compute_layout_rect(c, available_size);

                        offsets.push(left_offset);
                        left_offset += child_layout.right() + gap;
                        available_size.0 -= child_layout.right() + gap;
                    }

                    left_offset -= gap;
                    let left_width = available_content_size.0 - left_offset;

                    let global_content_offset = peridot::math::Vector2(
                        child_layout_global_offset.0 + left_width * 0.5,
                        child_layout_global_offset.1,
                    );

                    for (c, o) in target.children.iter().zip(offsets.into_iter()) {
                        let child_layout = compute_layout_rect(c, available_content_size);

                        layout1(
                            c,
                            boxes,
                            LayoutRect {
                                pos: global_content_offset
                                    + child_layout.pos
                                    + peridot::math::Vector2(o, 0.0),
                                size: child_layout.size,
                            },
                        );

                        available_content_size.0 -= child_layout.size.0 + gap;
                    }
                }
                (LayoutDirection::Reverse, LayoutJustify::Center) => {
                    let mut offsets = Vec::with_capacity(target.children.len());
                    let mut left_offset = 0.0f32;
                    let mut available_size = child_layout_available_size;
                    for c in target.children.iter().rev() {
                        let child_layout = compute_layout_rect(c, available_size);

                        offsets.push(left_offset);
                        left_offset += child_layout.right() + gap;
                        available_size.0 -= child_layout.right() + gap;
                    }

                    left_offset -= gap;
                    let left_width = available_content_size.0 - left_offset;

                    let global_content_offset = peridot::math::Vector2(
                        child_layout_global_offset.0 + left_width * 0.5,
                        child_layout_global_offset.1,
                    );

                    for (c, o) in target.children.iter().rev().zip(offsets.into_iter()) {
                        let child_layout = compute_layout_rect(c, available_content_size);

                        layout1(
                            c,
                            boxes,
                            LayoutRect {
                                pos: global_content_offset
                                    + child_layout.pos
                                    + peridot::math::Vector2(o, 0.0),
                                size: child_layout.size,
                            },
                        );

                        available_content_size.0 -= child_layout.size.0 + gap;
                    }
                }
                (LayoutDirection::Normal, LayoutJustify::SpaceBetween) => {
                    let mut content_width = 0.0f32;
                    let mut available_size = child_layout_available_size;
                    for c in target.children.iter() {
                        let w = compute_layout_rect(c, available_size).right();

                        content_width += w;
                        available_size.0 -= w;
                    }

                    let left_width = available_content_size.0 - content_width;
                    let gap = left_width / (target.children.len() - 1) as f32;

                    let mut global_content_offset = child_layout_global_offset;

                    for c in target.children.iter() {
                        let child_layout = compute_layout_rect(c, available_content_size);

                        layout1(
                            c,
                            boxes,
                            LayoutRect {
                                pos: global_content_offset + child_layout.pos,
                                size: child_layout.size,
                            },
                        );

                        global_content_offset.0 += child_layout.size.0 + gap;
                        available_content_size.0 -= child_layout.size.0 + gap;
                    }
                }
                (LayoutDirection::Reverse, LayoutJustify::SpaceBetween) => {
                    let mut content_width = 0.0f32;
                    let mut available_size = child_layout_available_size;
                    for c in target.children.iter().rev() {
                        let w = compute_layout_rect(c, available_size).right();

                        content_width += w;
                        available_size.0 -= w;
                    }

                    let left_width = available_content_size.0 - content_width;
                    let gap = left_width / (target.children.len() - 1) as f32;

                    let mut global_content_offset = child_layout_global_offset;

                    for c in target.children.iter().rev() {
                        let child_layout = compute_layout_rect(c, available_content_size);

                        layout1(
                            c,
                            boxes,
                            LayoutRect {
                                pos: global_content_offset + child_layout.pos,
                                size: child_layout.size,
                            },
                        );

                        global_content_offset.0 += child_layout.size.0 + gap;
                        available_content_size.0 -= child_layout.size.0 + gap;
                    }
                }
                (LayoutDirection::Normal, LayoutJustify::SpaceAround) => {
                    let mut content_width = 0.0f32;
                    let mut available_size = child_layout_available_size;
                    for c in target.children.iter() {
                        let w = compute_layout_rect(c, available_size).right();

                        content_width += w;
                        available_size.0 -= w;
                    }

                    let left_width = available_content_size.0 - content_width;
                    let gap = left_width / (target.children.len() + 1) as f32;

                    let mut global_content_offset = peridot::math::Vector2(
                        child_layout_global_offset.0 + gap,
                        child_layout_global_offset.1,
                    );

                    for c in target.children.iter() {
                        let child_layout = compute_layout_rect(c, available_content_size);

                        layout1(
                            c,
                            boxes,
                            LayoutRect {
                                pos: global_content_offset + child_layout.pos,
                                size: child_layout.size,
                            },
                        );

                        global_content_offset.0 += child_layout.size.0 + gap;
                        available_content_size.0 -= child_layout.size.0 + gap;
                    }
                }
                (LayoutDirection::Reverse, LayoutJustify::SpaceAround) => {
                    let mut content_width = 0.0f32;
                    let mut available_size = child_layout_available_size;
                    for c in target.children.iter().rev() {
                        let w = compute_layout_rect(c, available_size).right();

                        content_width += w;
                        available_size.0 -= w;
                    }

                    let left_width = available_content_size.0 - content_width;
                    let gap = left_width / (target.children.len() + 1) as f32;

                    let mut global_content_offset = peridot::math::Vector2(
                        child_layout_global_offset.0 + gap,
                        child_layout_global_offset.1,
                    );

                    for c in target.children.iter().rev() {
                        let child_layout = compute_layout_rect(c, available_content_size);

                        layout1(
                            c,
                            boxes,
                            LayoutRect {
                                pos: global_content_offset + child_layout.pos,
                                size: child_layout.size,
                            },
                        );

                        global_content_offset.0 += child_layout.size.0 + gap;
                        available_content_size.0 -= child_layout.size.0 + gap;
                    }
                }
            }
        }
        ChildrenLayoutMode::Grid {
            ref columns,
            ref rows,
            gap,
        } => {
            let mut column_fixed_sizes = vec![0.0; columns.len()];
            let mut row_fixed_sizes = vec![0.0; columns.len()];
            let mut current_column = 0;
            let mut current_row = 0;
            for c in target.children.iter() {
                let cell_rect = compute_layout_rect(c, peridot::math::Vector2(0.0, 0.0));
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
                    peridot::math::Vector2(column_size[current_column], row_size[current_row]),
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
                            + child_layout.pos,
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

pub async fn game_main(e: &mut peridot::Engine<impl peridot::NativeLinker>) {
    let screen_size = e
        .back_buffer(0)
        .expect("no backbuffers?")
        .image()
        .size()
        .as_2d_ref()
        .clone();
    let scissor_rect = screen_size.into_rect(br::vk::VkOffset2D::ZERO);
    let viewport = scissor_rect.make_viewport(0.0..1.0);

    let main_renderpass = br::RenderPassBuilder::new(
        &[e.back_buffer_attachment_desc()
            .color_memory_op(br::LoadOp::Clear, br::StoreOp::Store)],
        &[br::SubpassDescription::new().color_attachments(
            &[br::AttachmentReference::new(
                0,
                br::ImageLayout::ColorAttachmentOpt,
            )],
            &[],
        )],
        &[peridot::SubpassDependencyTemplates::to_color_attachment_in(
            None, 0, true,
        )],
    )
    .create(e.graphics().device().clone())
    .expect("Failed to create main renderpass");
    let backbuffer_resources = e.iter_back_buffers().cloned().collect::<Vec<_>>();
    let main_framebuffers = backbuffer_resources
        .iter()
        .map(|bb| {
            br::FramebufferBuilder::new_with_attachment(&main_renderpass, bb)
                .create()
                .expect("Failed to create main framebuffer")
        })
        .collect::<Vec<_>>();

    let unlit_fill_shader = PvpShaderModules::new(
        e.graphics().device(),
        e.load("shaders.unlit_fill")
            .expect("Failed to load unlit_fill shader"),
    )
    .expect("Failed to create unlit_fill shader modules");
    let unlit_fill_pipeline_layout = br::PipelineLayoutBuilder::new(
        &[],
        &[
            br::PushConstantRange::for_type::<peridot::math::Vector2<f32>>(
                br::ShaderStage::VERTEX,
                0,
            ),
        ],
    )
    .create(e.graphics().device().clone())
    .expect("Failed to create pipeline layout");
    let unlit_fill_pipeline = {
        let mut builder = br::NonDerivedGraphicsPipelineBuilder::new(
            &unlit_fill_pipeline_layout,
            main_renderpass.subpass(0),
            unlit_fill_shader.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP),
        );
        builder
            .viewport_scissors(
                br::DynamicArrayState::Static(&[viewport]),
                br::DynamicArrayState::Static(&[scissor_rect]),
            )
            .add_attachment_blend(br::AttachmentColorBlendState::premultiplied())
            .multisample_state(Some(br::MultisampleState::new()));

        builder
            .create(
                e.graphics().device().clone(),
                None::<&br::PipelineCacheObject<peridot::DeviceObject>>,
            )
            .expect("Failed to create unlit_fill pipeline")
    };

    let ui_tree = UIElement {
        size: peridot::math::Vector2(UIElementSize::Fill, UIElementSize::Fill),
        padding: RectEdge::all(8.0),
        debug_color: peridot::math::Vector4(1.0, 1.0, 1.0, 0.0),
        children_layout: ChildrenLayoutMode::Vertical {
            direction: LayoutDirection::Normal,
            justify: LayoutJustify::Center,
            alignment: LayoutAlignment::Center,
            overflow: Overflow::Overflow,
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

    let mut pmm = peridot_memory_manager::MemoryManager::new(e.graphics());
    let [vertex_buffer, instance_buffer] = pmm
        .allocate_device_local_buffer_array(
            e.graphics(),
            [
                br::BufferDesc::new_for_type::<[Vertex; 4]>(
                    br::BufferUsage::VERTEX_BUFFER.transfer_dest(),
                ),
                br::BufferDesc::new_for_type::<[BoxInstance; 1024]>(
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
            br::BufferDesc::new_for_type::<BufferInitContent>(br::BufferUsage::TRANSFER_SRC),
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
            br::BufferDesc::new(
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
                &[br::BufferCopy(br::vk::VkBufferCopy {
                    srcOffset: 0,
                    dstOffset: 0,
                    size: (core::mem::size_of::<BoxInstance>() * boxes.len()) as _,
                })],
            )
            .pipeline_barrier(
                br::PipelineStageFlags::TRANSFER,
                br::PipelineStageFlags::VERTEX_INPUT,
                false,
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

    let mut ui_render_cp = br::CommandPoolBuilder::new(e.graphics_queue_family_index())
        .create(e.graphics().device().clone())
        .expect("Failed to create ui render command pool");
    let [mut ui_render_cb] = ui_render_cp
        .alloc_array::<1>(false)
        .expect("Failed to allocate ui render command buffer");
    unsafe {
        let inherit_info = br::vk::VkCommandBufferInheritanceInfo {
            sType: br::vk::VkCommandBufferInheritanceInfo::TYPE,
            pNext: core::ptr::null(),
            renderPass: main_renderpass.native_ptr(),
            subpass: 0,
            framebuffer: br::vk::VkFramebuffer::NULL,
            occlusionQueryEnable: false as _,
            queryFlags: 0,
            pipelineStatistics: 0,
        };
        let begin_info = br::vk::VkCommandBufferBeginInfo {
            sType: br::vk::VkCommandBufferBeginInfo::TYPE,
            pNext: core::ptr::null(),
            flags: br::vk::VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT
                | br::vk::VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT,
            pInheritanceInfo: &inherit_info,
        };

        ui_render_cb
            .begin_raw(&begin_info, e.graphics().device())
            .expect("Failed to begin ui render command recording")
    }
    .bind_graphics_pipeline(&unlit_fill_pipeline)
    .push_constant(
        &unlit_fill_pipeline_layout,
        br::ShaderStage::VERTEX,
        0,
        &peridot::math::Vector2(640.0f32, 480.0),
    )
    .bind_vertex_buffers(
        0,
        &[
            br::BufferObjectRef::new(&vertex_buffer),
            br::BufferObjectRef::new(&instance_buffer),
        ],
        &[0, 0],
    )
    .draw(4, boxes.len() as _, 0, 0)
    .end()
    .expect("Failed to finish ui render command recording");

    let mut render_cp = br::CommandPoolBuilder::new(e.graphics_queue_family_index())
        .create(e.graphics().device().clone())
        .expect("Failed to create render command pool");
    let mut render_cb = render_cp
        .alloc(e.back_buffer_count() as _, true)
        .expect("Failed to allocate render command buffers");
    for (cb, fb) in render_cb.iter_mut().zip(main_framebuffers.iter()) {
        unsafe {
            cb.begin(e.graphics().device())
                .expect("Failed to begin render command recording")
                .begin_render_pass(
                    &main_renderpass,
                    fb,
                    scissor_rect,
                    &[br::ClearValue::color_f32([0.1, 0.2, 0.3, 0.0])],
                    false,
                )
                .execute_commands(&[ui_render_cb.native_ptr()])
                .end_render_pass()
                .end()
                .expect("Failed to finish render command recording");
        }
    }

    content_init.await.expect("Failed to initialize content");

    while let Some(ev) = e.event_receivers().wait_for_event().await {
        match ev {
            peridot::Event::Shutdown => break,
            peridot::Event::NextFrame => {
                let fd = e.prepare_frame().expect("Failed to prepare frame");

                e.do_render(
                    fd.backbuffer_index,
                    None::<br::EmptySubmissionBatch>,
                    br::EmptySubmissionBatch.with_command_buffers(
                        &render_cb[fd.backbuffer_index as usize..=fd.backbuffer_index as usize],
                    ),
                )
                .expect("Failed to render");
            }
            peridot::Event::Resize(ns) => {
                println!("not implemented: Resize: {ns:?}");
            }
        }
    }

    unsafe {
        e.graphics().device().wait().expect("Failed to wait works");
    }
}
