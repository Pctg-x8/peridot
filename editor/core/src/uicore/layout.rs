//! Layout System

use shared::{LogicalUnit, Point, Rect, Size};

use crate::uicore::{MeasureContext, ViewIdentifier, ViewInstanceStore, ViewTreeRelationStore};

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

#[derive(Debug, Clone, Default)]
pub enum ViewSize {
    Fixed(f32),
    Percent(f32),
    #[default]
    FitContent,
    FillAvailable,
}

#[derive(Debug, Clone, Default)]
pub enum ViewLayoutFlowBasis {
    Flexible(f32),
    Fixed(f32),
    #[default]
    FixedFitContent,
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
    Stretch,
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

pub struct ViewLayoutState {
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

    #[inline(always)]
    pub fn size(&self) -> Size<LogicalUnit> {
        self.layout_rect.size()
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

    #[inline(always)]
    pub fn get(&self, id: ViewIdentifier) -> &ViewLayoutState {
        &self.0[id.into_array_index()]
    }
}

#[profiler::instrument("View.LayoutPartial")]
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

#[profiler::instrument("View.Layout")]
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
    if !target_inst.active {
        // skip children layout
        return;
    }
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
            let mut content_widths = vec![
                0.0;
                tree_relation_store.relations[target.into_array_index()]
                    .children
                    .len()
            ];
            let mut flexible_value_total = 0.0;
            for (i, &child) in tree_relation_store.relations[target.into_array_index()]
                .children
                .iter()
                .enumerate()
            {
                match instance_store.get(child).layout.flow_basis {
                    ViewLayoutFlowBasis::Fixed(v) => {
                        content_widths[i] = v;
                    }
                    ViewLayoutFlowBasis::FixedFitContent => {
                        content_widths[i] = compute_actual_content_size(
                            child,
                            ctx,
                            &child_available_rect,
                            instance_store,
                            tree_relation_store,
                        )
                        .width;
                    }
                    ViewLayoutFlowBasis::Flexible(v) => {
                        flexible_value_total += v;
                    }
                }
            }
            let flexible_leftover = available_rect.width
                - content_widths.iter().sum::<f32>()
                - gap * content_widths.len().saturating_sub(1) as f32;
            for (i, &child) in tree_relation_store.relations[target.into_array_index()]
                .children
                .iter()
                .enumerate()
            {
                if let ViewLayoutFlowBasis::Flexible(v) =
                    instance_store.get(child).layout.flow_basis
                {
                    content_widths[i] = v * flexible_leftover / flexible_value_total;
                }
            }

            let mut left_placement = 0.0;
            let mut top_placement = 0.0;
            let mut size = Size::new_logical(0.0, 0.0);
            for (&child, cw) in tree_relation_store.relations[target.into_array_index()]
                .children
                .iter()
                .zip(content_widths)
            {
                if left_placement + cw >= available_rect.width {
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
                        Size::new_logical(cw, child_available_rect.height),
                    ),
                    instance_store,
                    tree_relation_store,
                    layout_state_store,
                    cb_perform_target_relayout,
                );

                size.width = size.width.max(left_placement + cw);
                size.height = size.height.max(top_placement + child_available_rect.height);
                left_placement += cw + gap;
            }
        }
        ViewLayoutChild::Flow {
            direction: ViewLayoutFlowDirection::Vertical,
            alignment,
            justify,
            overflow,
            gap,
        } => {
            let content_sizes = tree_relation_store.relations[target.into_array_index()]
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

            let mut content_heights = vec![
                0.0;
                tree_relation_store.relations[target.into_array_index()]
                    .children
                    .len()
            ];
            let mut flexible_value_total = 0.0;
            for (i, &child) in tree_relation_store.relations[target.into_array_index()]
                .children
                .iter()
                .enumerate()
            {
                match instance_store.get(child).layout.flow_basis {
                    ViewLayoutFlowBasis::Fixed(v) => {
                        content_heights[i] = v;
                    }
                    ViewLayoutFlowBasis::FixedFitContent => {
                        content_heights[i] = content_sizes[i].height;
                    }
                    ViewLayoutFlowBasis::Flexible(v) => {
                        flexible_value_total += v;
                    }
                }
            }
            let flexible_leftover = available_rect.height
                - content_heights.iter().sum::<f32>()
                - gap * content_heights.len().saturating_sub(1) as f32;
            for (i, &child) in tree_relation_store.relations[target.into_array_index()]
                .children
                .iter()
                .enumerate()
            {
                if let ViewLayoutFlowBasis::Flexible(v) =
                    instance_store.get(child).layout.flow_basis
                {
                    content_heights[i] = v * flexible_leftover / flexible_value_total;
                }
            }

            let mut left_placement = 0.0;
            let mut top_placement = 0.0;
            let mut size = Size::new_logical(0.0, 0.0);
            for ((&child, ch), content_size) in tree_relation_store.relations
                [target.into_array_index()]
            .children
            .iter()
            .zip(content_heights)
            .zip(content_sizes)
            {
                if top_placement + ch >= available_rect.height {
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

                let left_offset = match instance_store
                    .get(child)
                    .layout
                    .flow_self_alignment
                    .unwrap_or(alignment)
                {
                    ViewLayoutFlowAlignment::Start => child_available_rect.left + left_placement,
                    ViewLayoutFlowAlignment::End => {
                        child_available_rect.left + left_placement + child_available_rect.width
                            - content_size.width
                    }
                    ViewLayoutFlowAlignment::Center => {
                        child_available_rect.left
                            + left_placement
                            + (child_available_rect.width - content_size.width) * 0.5
                    }
                    // TODO: baseline for vertical layout
                    ViewLayoutFlowAlignment::FirstBaseline => {
                        child_available_rect.left + left_placement
                    }
                    ViewLayoutFlowAlignment::LastBaseline => {
                        child_available_rect.left + left_placement
                    }
                    ViewLayoutFlowAlignment::CenterFirstLine => {
                        child_available_rect.left + left_placement
                    }
                    ViewLayoutFlowAlignment::CenterLastLine => {
                        child_available_rect.left + left_placement
                    }
                };

                layout_view_recursive(
                    child,
                    ctx,
                    Rect::from_lt_size(
                        Point::new_logical(left_offset, child_available_rect.top + top_placement),
                        Size::new_logical(content_size.width, ch),
                    ),
                    instance_store,
                    tree_relation_store,
                    layout_state_store,
                    cb_perform_target_relayout,
                );

                size.width = size.width.max(left_placement + content_size.width);
                size.height = size.height.max(top_placement + ch);
                top_placement += ch + gap;
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

            // tracing::debug!(
            //     ?col_widths,
            //     ?row_heights,
            //     ?child_available_rect,
            //     "grid layout in"
            // );

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

#[profiler::instrument("View.Layout.ComputeContentSize")]
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
                    let mut content_widths = vec![
                        0.0;
                        tree_relation_store.relations
                            [target.into_array_index()]
                        .children
                        .len()
                    ];
                    let mut content_heights = Vec::with_capacity(
                        tree_relation_store.relations[target.into_array_index()]
                            .children
                            .len(),
                    );
                    let mut flexible_value_total = 0.0;
                    for (i, &child) in tree_relation_store.relations[target.into_array_index()]
                        .children
                        .iter()
                        .enumerate()
                    {
                        let content_size = compute_actual_content_size(
                            child,
                            ctx,
                            &child_available_rect,
                            instance_store,
                            tree_relation_store,
                        );
                        content_heights.push(content_size.height);

                        match instance_store.get(child).layout.flow_basis {
                            ViewLayoutFlowBasis::Fixed(v) => {
                                content_widths[i] = v;
                            }
                            ViewLayoutFlowBasis::FixedFitContent => {
                                content_widths[i] = content_size.width;
                            }
                            ViewLayoutFlowBasis::Flexible(v) => {
                                flexible_value_total += v;
                            }
                        }
                    }
                    let flexible_leftover = available_rect.width
                        - content_widths.iter().sum::<f32>()
                        - gap * content_widths.len().saturating_sub(1) as f32;
                    for (i, &child) in tree_relation_store.relations[target.into_array_index()]
                        .children
                        .iter()
                        .enumerate()
                    {
                        if let ViewLayoutFlowBasis::Flexible(v) =
                            instance_store.get(child).layout.flow_basis
                        {
                            content_widths[i] = v * flexible_leftover / flexible_value_total;
                        }
                    }

                    let mut left_placement = 0.0;
                    let mut top_placement = 0.0;
                    let mut size = Size::new_logical(0.0, 0.0);
                    for (cw, ch) in content_widths.into_iter().zip(content_heights) {
                        if left_placement + cw >= available_rect.width {
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

                        size.width = size.width.max(left_placement + cw);
                        size.height = size.height.max(top_placement + ch);
                        left_placement += cw + gap;
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
                    let mut content_heights = vec![
                        0.0;
                        tree_relation_store.relations
                            [target.into_array_index()]
                        .children
                        .len()
                    ];
                    let mut content_widths = Vec::with_capacity(
                        tree_relation_store.relations[target.into_array_index()]
                            .children
                            .len(),
                    );
                    let mut flexible_value_total = 0.0;
                    for (i, &child) in tree_relation_store.relations[target.into_array_index()]
                        .children
                        .iter()
                        .enumerate()
                    {
                        let content_size = compute_actual_content_size(
                            child,
                            ctx,
                            &child_available_rect,
                            instance_store,
                            tree_relation_store,
                        );
                        content_widths.push(content_size.width);

                        match instance_store.get(child).layout.flow_basis {
                            ViewLayoutFlowBasis::Fixed(v) => {
                                content_heights[i] = v;
                            }
                            ViewLayoutFlowBasis::FixedFitContent => {
                                content_heights[i] = content_size.height;
                            }
                            ViewLayoutFlowBasis::Flexible(v) => {
                                flexible_value_total += v;
                            }
                        }
                    }
                    let flexible_leftover = available_rect.height
                        - content_heights.iter().sum::<f32>()
                        - gap * content_heights.len().saturating_sub(1) as f32;
                    for (i, &child) in tree_relation_store.relations[target.into_array_index()]
                        .children
                        .iter()
                        .enumerate()
                    {
                        if let ViewLayoutFlowBasis::Flexible(v) =
                            instance_store.get(child).layout.flow_basis
                        {
                            content_heights[i] = v * flexible_leftover / flexible_value_total;
                        }
                    }

                    let mut left_placement = 0.0;
                    let mut top_placement = 0.0;
                    let mut size = Size::new_logical(0.0, 0.0);
                    for (ch, cw) in content_heights.into_iter().zip(content_widths) {
                        if top_placement + ch >= available_rect.height {
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

                        size.width = size.width.max(left_placement + cw);
                        size.height = size.height.max(top_placement + ch);
                        top_placement += ch + gap;
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
            ViewSize::FillAvailable => available_rect.width,
        },
        match target_inst.layout.height {
            ViewSize::Fixed(x) => x,
            ViewSize::Percent(x) => available_rect.height * x,
            ViewSize::FitContent => {
                inner_size.height
                    + target_inst.layout.padding.top
                    + target_inst.layout.padding.bottom
            }
            ViewSize::FillAvailable => available_rect.height,
        },
    )
}
