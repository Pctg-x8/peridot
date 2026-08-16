//! Layout System

use crate::{
    uikit::{MeasureContext, ViewIdentifier, ViewInstanceStore, ViewTreeRelationStore},
    utils::{LogicalUnit, Point, Rect, Size},
};

#[derive(Debug, Clone, Default)]
pub struct RectEdge {
    pub left: f32,
    pub top: f32,
    pub right: f32,
    pub bottom: f32,
}
impl RectEdge {
    pub const fn set_all(&mut self, v: f32) {
        self.left = v;
        self.top = v;
        self.right = v;
        self.bottom = v;
    }
}

#[derive(Debug, Clone, Default)]
pub struct ViewLayout {
    pub width: ViewSize,
    pub height: ViewSize,
    pub padding: RectEdge,
    pub child: ViewLayoutChild,
    pub flow_basis: ViewLayoutFlowBasis,
    pub flow_self_alignment: Option<ViewLayoutFlowAlignment>,
}

#[derive(Debug, Clone)]
pub enum ViewSize {
    Fixed(f32),
    Percent(f32),
    FitContent,
    FillParent,
}
impl Default for ViewSize {
    #[inline(always)]
    fn default() -> Self {
        Self::FitContent
    }
}

#[derive(Debug, Clone)]
pub enum ViewLayoutFlowBasis {
    Flexible(f32),
    Fixed(f32),
    FixedFitContent,
}
impl Default for ViewLayoutFlowBasis {
    #[inline(always)]
    fn default() -> Self {
        Self::Flexible(1.0)
    }
}

#[derive(Debug, Clone, Default)]
pub enum ViewLayoutChild {
    #[default]
    Free,
    Flow {
        direction: ViewLayoutFlowDirection,
        alignment: ViewLayoutFlowAlignment,
        justify: ViewLayoutFlowJustify,
        overflow: ViewLayoutOverflow,
        gap: f32,
    },
    Grid {
        cols: Vec<ViewLayoutGridCell>,
        rows: Vec<ViewLayoutGridCell>,
        gap_cols: f32,
        gap_rows: f32,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum ViewLayoutFlowDirection {
    Vertical,
    Horizontal,
}

#[derive(Debug, Clone, Copy, Default)]
pub enum ViewLayoutFlowAlignment {
    #[default]
    Start,
    End,
    Center,
    FirstBaseline,
    LastBaseline,
    CenterFirstLine,
    CenterLastLine,
}

#[derive(Debug, Clone, Copy, Default)]
pub enum ViewLayoutFlowJustify {
    #[default]
    Start,
    End,
    Center,
    SpaceBetween,
    SpaceAround,
}

#[derive(Debug, Clone, Copy, Default)]
pub enum ViewLayoutOverflow {
    #[default]
    Overflow,
    Wrap,
}

#[derive(Debug, Clone, Copy)]
pub enum ViewLayoutGridCell {
    Flexible(f32),
    Fixed(f32),
    FixedFitContent,
}
impl Default for ViewLayoutGridCell {
    #[inline(always)]
    fn default() -> Self {
        Self::Flexible(1.0)
    }
}

pub(super) struct ViewLayoutState {
    pub(super) layout_rect: Rect<LogicalUnit>,
}
impl ViewLayoutState {
    pub(super) fn init() -> Self {
        Self {
            layout_rect: Rect::from_lt_size(
                Point::new_logical(0.0, 0.0),
                Size::new_logical(0.0, 0.0),
            ),
        }
    }
}

pub struct ViewLayoutStateStore(Vec<ViewLayoutState>);
impl ViewLayoutStateStore {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn push_empty(&mut self) {
        self.0.push(ViewLayoutState::init());
    }

    pub fn pop(&mut self) {
        self.0.pop();
    }

    pub fn set_empty(&mut self, id: ViewIdentifier) {
        self.0[id.into_array_index()] = ViewLayoutState::init();
    }

    pub(super) fn get(&self, id: ViewIdentifier) -> &ViewLayoutState {
        &self.0[id.into_array_index()]
    }
}

pub fn layout_view_partial_recursive(
    target: ViewIdentifier,
    ctx: &mut MeasureContext,
    instance_store: &ViewInstanceStore,
    tree_relation_store: &ViewTreeRelationStore,
    layout_state_store: &mut ViewLayoutStateStore,
    mut cb_perform_target_relayout: impl FnMut(ViewIdentifier),
) {
    let available_rect = layout_state_store.get(target).layout_rect.clone();

    layout_view_recursive(
        target,
        ctx,
        available_rect,
        instance_store,
        tree_relation_store,
        layout_state_store,
        &mut cb_perform_target_relayout,
    )
}

#[tracing::instrument(skip(
    ctx,
    instance_store,
    tree_relation_store,
    layout_state_store,
    cb_perform_target_relayout
))]
pub fn layout_view_recursive(
    target: ViewIdentifier,
    ctx: &mut MeasureContext,
    available_rect: Rect<LogicalUnit>,
    instance_store: &ViewInstanceStore,
    tree_relation_store: &ViewTreeRelationStore,
    layout_state_store: &mut ViewLayoutStateStore,
    cb_perform_target_relayout: &mut impl FnMut(ViewIdentifier),
) {
    let content_size = compute_actual_content_size(
        target,
        ctx,
        &available_rect,
        instance_store,
        tree_relation_store,
    );
    layout_state_store.0[target.into_array_index()].layout_rect =
        Rect::from_lt_size(available_rect.left_top(), content_size);
    cb_perform_target_relayout(target);

    let target_inst = instance_store.get(target);
    let base_point = if target_inst
        .instance
        .as_ref()
        .expect("no instance")
        .create_new_layout_layer()
    {
        Point::new_logical(0.0, 0.0)
    } else {
        available_rect.left_top()
    };
    let child_available_rect = Rect::from_lt_size(
        base_point.with_offset(Point::new_logical(
            target_inst.layout.padding.left,
            target_inst.layout.padding.top,
        )),
        Size::new_logical(
            content_size.width - target_inst.layout.padding.left - target_inst.layout.padding.right,
            content_size.height
                - target_inst.layout.padding.top
                - target_inst.layout.padding.bottom,
        ),
    );
    match instance_store.get(target).layout.child {
        ViewLayoutChild::Free => {
            for &child in tree_relation_store.relations[target.into_array_index()]
                .children
                .iter()
            {
                layout_view_recursive(
                    child,
                    ctx,
                    child_available_rect.clone(),
                    instance_store,
                    tree_relation_store,
                    layout_state_store,
                    cb_perform_target_relayout,
                );
            }
        }
        ViewLayoutChild::Flow {
            direction: ViewLayoutFlowDirection::Horizontal,
            alignment,
            justify,
            overflow,
            gap,
        } => {
            let child_content_sizes = tree_relation_store.relations[target.into_array_index()]
                .children
                .iter()
                .map(|&child| {
                    compute_actual_content_size(
                        child,
                        ctx,
                        &child_available_rect,
                        instance_store,
                        tree_relation_store,
                    )
                })
                .collect::<Vec<_>>();

            let mut left_placement = 0.0;
            let mut top_placement = 0.0;
            let mut size = Size::new_logical(0.0, 0.0);
            for (&child, child_size) in tree_relation_store.relations[target.into_array_index()]
                .children
                .iter()
                .zip(child_content_sizes)
            {
                if left_placement + child_size.width >= available_rect.width {
                    // overflow
                    match overflow {
                        ViewLayoutOverflow::Overflow => {
                            // do nothing
                        }
                        ViewLayoutOverflow::Wrap => {
                            top_placement = size.height + gap;
                            left_placement = 0.0;
                        }
                    }
                }

                layout_view_recursive(
                    child,
                    ctx,
                    Rect::from_lt_size(
                        child_available_rect
                            .left_top()
                            .with_offset(Point::new_logical(left_placement, top_placement)),
                        child_size.clone(),
                    ),
                    instance_store,
                    tree_relation_store,
                    layout_state_store,
                    cb_perform_target_relayout,
                );

                size.width = size.width.max(left_placement + child_size.width);
                size.height = size.height.max(top_placement + child_size.height);
                left_placement += child_size.width + gap;
            }
        }
        ViewLayoutChild::Flow {
            direction: ViewLayoutFlowDirection::Vertical,
            alignment,
            justify,
            overflow,
            gap,
        } => {
            let child_content_sizes = tree_relation_store.relations[target.into_array_index()]
                .children
                .iter()
                .map(|&child| {
                    compute_actual_content_size(
                        child,
                        ctx,
                        &child_available_rect,
                        instance_store,
                        tree_relation_store,
                    )
                })
                .collect::<Vec<_>>();

            let mut left_placement = 0.0;
            let mut top_placement = 0.0;
            let mut size = Size::new_logical(0.0, 0.0);
            for (&child, child_size) in tree_relation_store.relations[target.into_array_index()]
                .children
                .iter()
                .zip(child_content_sizes)
            {
                if top_placement + child_size.height >= available_rect.height {
                    // overflow
                    match overflow {
                        ViewLayoutOverflow::Overflow => {
                            // do nothing
                        }
                        ViewLayoutOverflow::Wrap => {
                            left_placement = size.width + gap;
                            top_placement = 0.0;
                        }
                    }
                }

                layout_view_recursive(
                    child,
                    ctx,
                    Rect::from_lt_size(
                        child_available_rect
                            .left_top()
                            .with_offset(Point::new_logical(left_placement, top_placement)),
                        child_size.clone(),
                    ),
                    instance_store,
                    tree_relation_store,
                    layout_state_store,
                    cb_perform_target_relayout,
                );

                size.width = size.width.max(left_placement + child_size.width);
                size.height = size.height.max(top_placement + child_size.height);
                top_placement += child_size.height + gap;
            }
        }
        ViewLayoutChild::Grid {
            ref cols,
            ref rows,
            gap_cols,
            gap_rows,
        } => {
            let mut col_widths = vec![0.0; cols.len()];
            let mut row_heights = Vec::<f32>::new();
            let col_flexible_totals = cols
                .iter()
                .map(|x| match x {
                    &ViewLayoutGridCell::Flexible(x) => x,
                    _ => 0.0,
                })
                .sum::<f32>();
            let row_flexible_totals = rows
                .iter()
                .map(|x| match x {
                    &ViewLayoutGridCell::Flexible(x) => x,
                    _ => 0.0,
                })
                .sum::<f32>();
            for (n, &child) in tree_relation_store.relations[target.into_array_index()]
                .children
                .iter()
                .enumerate()
            {
                let col_index = n % cols.len();
                let row_index = n / cols.len();

                let row_cell = rows
                    .get(row_index)
                    .unwrap_or(&ViewLayoutGridCell::FixedFitContent);
                if row_heights.len() <= row_index {
                    row_heights.push(0.0);
                }

                let content_size = if matches!(cols[col_index], ViewLayoutGridCell::FixedFitContent)
                    || matches!(row_cell, ViewLayoutGridCell::FixedFitContent)
                {
                    // require child size
                    compute_actual_content_size(
                        child,
                        ctx,
                        &child_available_rect,
                        instance_store,
                        tree_relation_store,
                    )
                } else {
                    Size::new_logical(0.0, 0.0)
                };

                match cols[col_index] {
                    ViewLayoutGridCell::Flexible(_) => {}
                    ViewLayoutGridCell::Fixed(v) => {
                        col_widths[col_index] = v;
                    }
                    ViewLayoutGridCell::FixedFitContent => {
                        col_widths[col_index] = col_widths[col_index].max(content_size.width);
                    }
                }

                match row_cell {
                    ViewLayoutGridCell::Flexible(_) => {}
                    &ViewLayoutGridCell::Fixed(v) => {
                        row_heights[row_index] = v;
                    }
                    ViewLayoutGridCell::FixedFitContent => {
                        row_heights[row_index] = row_heights[row_index].max(content_size.height);
                    }
                }
            }

            let col_flexible_leftovers = child_available_rect.width
                - (col_widths.iter().copied().sum::<f32>()
                    + gap_cols * col_widths.len().saturating_sub(1) as f32);
            let row_flexible_leftovers = child_available_rect.height
                - (row_heights.iter().copied().sum::<f32>()
                    + gap_rows * row_heights.len().saturating_sub(1) as f32);
            for (c, w) in cols.iter().zip(col_widths.iter_mut()) {
                if let ViewLayoutGridCell::Flexible(fw) = c {
                    *w = col_flexible_leftovers * fw / col_flexible_totals;
                }
            }
            for (r, h) in rows.iter().zip(row_heights.iter_mut()) {
                if let ViewLayoutGridCell::Flexible(fh) = r {
                    *h = row_flexible_leftovers * fh / row_flexible_totals;
                }
            }

            tracing::debug!(
                ?col_widths,
                ?row_heights,
                ?child_available_rect,
                "grid layout in"
            );

            let mut left_placement = 0.0;
            let mut top_placement = 0.0;
            let mut col_index = 0;
            let mut row_index = 0;
            for &child in tree_relation_store.relations[target.into_array_index()]
                .children
                .iter()
            {
                layout_view_recursive(
                    child,
                    ctx,
                    Rect::from_lt_size(
                        child_available_rect
                            .left_top()
                            .with_offset(Point::new_logical(left_placement, top_placement)),
                        Size::new_logical(col_widths[col_index], row_heights[row_index]),
                    ),
                    instance_store,
                    tree_relation_store,
                    layout_state_store,
                    cb_perform_target_relayout,
                );

                left_placement += col_widths[col_index] + gap_cols;

                col_index += 1;
                if col_index >= cols.len() {
                    // new row
                    left_placement = 0.0;
                    top_placement += row_heights[row_index] + gap_rows;
                    col_index = 0;
                    row_index += 1;
                }
            }
        }
    }
}

fn compute_actual_content_size(
    target: ViewIdentifier,
    ctx: &mut MeasureContext,
    available_rect: &Rect<LogicalUnit>,
    instance_store: &ViewInstanceStore,
    tree_relation_store: &ViewTreeRelationStore,
) -> Size<LogicalUnit> {
    let target_inst = instance_store.get(target);
    let inner_size = if matches!(target_inst.layout.width, ViewSize::FitContent)
        || matches!(target_inst.layout.height, ViewSize::FitContent)
    {
        // サイズ計算で参照されるので計算する
        let child_available_rect = Rect::from_lt_size(
            available_rect.left_top().with_offset(Point::new_logical(
                target_inst.layout.padding.left,
                target_inst.layout.padding.top,
            )),
            Size::new_logical(
                available_rect.width
                    - target_inst.layout.padding.left
                    - target_inst.layout.padding.right,
                available_rect.height
                    - target_inst.layout.padding.top
                    - target_inst.layout.padding.bottom,
            ),
        );

        match target_inst.layout.child {
            ViewLayoutChild::Free => {
                let self_content_size = target_inst.instance.as_ref().map_or_else(
                    || Size::new_logical(0.0, 0.0),
                    |x| x.measure_preferred_content_size(ctx),
                );

                tree_relation_store.relations[target.into_array_index()]
                    .children
                    .iter()
                    .map(|&t| {
                        compute_actual_content_size(
                            t,
                            ctx,
                            &child_available_rect,
                            instance_store,
                            tree_relation_store,
                        )
                    })
                    .fold(self_content_size, |a, r| {
                        Size::new_logical(a.width.max(r.width), a.height.max(r.height))
                    })
            }
            ViewLayoutChild::Flow {
                direction: ViewLayoutFlowDirection::Horizontal,
                alignment,
                justify,
                overflow,
                gap,
            } => {
                let children = &tree_relation_store.relations[target.into_array_index()].children;
                if children.is_empty() {
                    Size::new_logical(0.0, 0.0)
                } else {
                    let mut left_placement = 0.0;
                    let mut top_placement = 0.0;
                    let mut size = Size::new_logical(0.0, 0.0);
                    for &child in children.iter() {
                        let child_size = compute_actual_content_size(
                            child,
                            ctx,
                            &child_available_rect,
                            instance_store,
                            tree_relation_store,
                        );

                        if left_placement + child_size.width >= available_rect.width {
                            // overflow
                            match overflow {
                                ViewLayoutOverflow::Overflow => {
                                    // do nothing
                                }
                                ViewLayoutOverflow::Wrap => {
                                    top_placement = size.height + gap;
                                    left_placement = 0.0;
                                }
                            }
                        }

                        size.width = size.width.max(left_placement + child_size.width);
                        size.height = size.height.max(top_placement + child_size.height);
                        left_placement += child_size.width + gap;
                    }

                    size
                }
            }
            ViewLayoutChild::Flow {
                direction: ViewLayoutFlowDirection::Vertical,
                alignment,
                justify,
                overflow,
                gap,
            } => {
                let children = &tree_relation_store.relations[target.into_array_index()].children;
                if children.is_empty() {
                    Size::new_logical(0.0, 0.0)
                } else {
                    let mut left_placement = 0.0;
                    let mut top_placement = 0.0;
                    let mut size = Size::new_logical(0.0, 0.0);
                    for &child in children.iter() {
                        let child_size = compute_actual_content_size(
                            child,
                            ctx,
                            &child_available_rect,
                            instance_store,
                            tree_relation_store,
                        );

                        if top_placement + child_size.height >= available_rect.height {
                            // overflow
                            match overflow {
                                ViewLayoutOverflow::Overflow => {
                                    // do nothing
                                }
                                ViewLayoutOverflow::Wrap => {
                                    left_placement = size.width + gap;
                                    top_placement = 0.0;
                                }
                            }
                        }

                        size.width = size.width.max(left_placement + child_size.width);
                        size.height = size.height.max(top_placement + child_size.height);
                        top_placement += child_size.height + gap;
                    }

                    size
                }
            }
            ViewLayoutChild::Grid {
                ref cols,
                ref rows,
                gap_cols,
                gap_rows,
            } => {
                let children = &tree_relation_store.relations[target.into_array_index()].children;
                if children.is_empty() {
                    Size::new_logical(0.0, 0.0)
                } else {
                    let mut col_widths = vec![0.0; cols.len()];
                    let mut row_heights = Vec::<f32>::new();
                    for (n, &child) in children.iter().enumerate() {
                        let col_index = n % cols.len();
                        let row_index = n / cols.len();

                        let row_cell = rows
                            .get(row_index)
                            .unwrap_or(&ViewLayoutGridCell::FixedFitContent);
                        if row_heights.len() <= row_index {
                            row_heights.push(0.0);
                        }

                        let content_size =
                            if matches!(cols[col_index], ViewLayoutGridCell::FixedFitContent)
                                || matches!(row_cell, ViewLayoutGridCell::FixedFitContent)
                            {
                                // require child size
                                compute_actual_content_size(
                                    child,
                                    ctx,
                                    &child_available_rect,
                                    instance_store,
                                    tree_relation_store,
                                )
                            } else {
                                Size::new_logical(0.0, 0.0)
                            };

                        match cols[col_index] {
                            ViewLayoutGridCell::Flexible(_) => {}
                            ViewLayoutGridCell::Fixed(v) => {
                                col_widths[col_index] = v;
                            }
                            ViewLayoutGridCell::FixedFitContent => {
                                col_widths[col_index] =
                                    col_widths[col_index].max(content_size.width);
                            }
                        }

                        match row_cell {
                            ViewLayoutGridCell::Flexible(_) => {}
                            &ViewLayoutGridCell::Fixed(v) => {
                                row_heights[row_index] = v;
                            }
                            ViewLayoutGridCell::FixedFitContent => {
                                row_heights[row_index] =
                                    row_heights[row_index].max(content_size.height);
                            }
                        }
                    }

                    let col_flexible_leftovers = child_available_rect.width
                        - (col_widths.iter().copied().sum::<f32>()
                            + gap_cols * col_widths.len().saturating_sub(1) as f32);
                    let row_flexible_leftovers = child_available_rect.height
                        - (row_heights.iter().copied().sum::<f32>()
                            + gap_rows * row_heights.len().saturating_sub(1) as f32);
                    let col_flexible_totals = cols
                        .iter()
                        .map(|x| match x {
                            &ViewLayoutGridCell::Flexible(x) => x,
                            _ => 0.0,
                        })
                        .sum::<f32>();
                    let row_flexible_totals = rows
                        .iter()
                        .map(|x| match x {
                            &ViewLayoutGridCell::Flexible(x) => x,
                            _ => 0.0,
                        })
                        .sum::<f32>();
                    for (c, w) in cols.iter().zip(col_widths.iter_mut()) {
                        if let ViewLayoutGridCell::Flexible(fw) = c {
                            *w = col_flexible_leftovers * fw / col_flexible_totals;
                        }
                    }
                    for (r, h) in rows.iter().zip(row_heights.iter_mut()) {
                        if let ViewLayoutGridCell::Flexible(fh) = r {
                            *h = row_flexible_leftovers * fh / row_flexible_totals;
                        }
                    }

                    // tracing::debug!(?col_widths, ?row_heights, gap_cols, gap_rows, ?target_inst.layout, "grid size compute");

                    Size::new_logical(
                        col_widths.iter().copied().sum::<f32>()
                            + gap_cols * col_widths.len().saturating_sub(1) as f32,
                        row_heights.iter().copied().sum::<f32>()
                            + gap_rows * row_heights.len().saturating_sub(1) as f32,
                    )
                }
            }
        }
    } else {
        Size::new_logical(0.0, 0.0)
    };

    Size::new_logical(
        match target_inst.layout.width {
            ViewSize::Fixed(x) => x,
            ViewSize::Percent(x) => available_rect.width * x,
            ViewSize::FitContent => {
                inner_size.width
                    + target_inst.layout.padding.left
                    + target_inst.layout.padding.right
            }
            ViewSize::FillParent => available_rect.width,
        },
        match target_inst.layout.height {
            ViewSize::Fixed(x) => x,
            ViewSize::Percent(x) => available_rect.height * x,
            ViewSize::FitContent => {
                inner_size.height
                    + target_inst.layout.padding.top
                    + target_inst.layout.padding.bottom
            }
            ViewSize::FillParent => available_rect.height,
        },
    )
}
