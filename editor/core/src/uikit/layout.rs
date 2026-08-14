//! Layout System

use crate::{
    uikit::{ViewIdentifier, ViewInstanceStore, ViewTreeRelationStore},
    utils::{LogicalUnit, Point, Rect, Size},
};

#[derive(Debug, Clone, Default)]
pub struct RectEdge {
    pub left: f32,
    pub top: f32,
    pub right: f32,
    pub bottom: f32,
}

#[derive(Debug, Clone, Default)]
pub struct ViewLayout {
    pub width: ViewSize,
    pub height: ViewSize,
    pub margin: RectEdge,
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

    pub(self) fn get(&self, id: ViewIdentifier) -> &ViewLayoutState {
        &self.0[id.into_array_index()]
    }
}

pub fn layout_view_recursive(
    target: ViewIdentifier,
    available_rect: Rect<LogicalUnit>,
    instance_store: &ViewInstanceStore,
    tree_relation_store: &ViewTreeRelationStore,
    layout_state_store: &mut ViewLayoutStateStore,
) {
    let content_size =
        compute_actual_content_size(target, &available_rect, instance_store, tree_relation_store);
    // TODO: Viewによって子の座標基準が0になる場合がある（CompositeTreeが親子関係を構築する場合）
    let child_available_rect = Rect::from_lt_size(available_rect.left_top(), content_size);
    layout_state_store.0[target.into_array_index()].layout_rect = child_available_rect.clone();
    match instance_store.get(target).layout.child {
        ViewLayoutChild::Free => {
            for &child in tree_relation_store.relations[target.into_array_index()]
                .children
                .iter()
            {
                layout_view_recursive(
                    child,
                    child_available_rect.clone(),
                    instance_store,
                    tree_relation_store,
                    layout_state_store,
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
                layout_view_recursive(
                    child,
                    available_rect.clone(),
                    instance_store,
                    tree_relation_store,
                    layout_state_store,
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
                layout_view_recursive(
                    child,
                    child_available_rect.clone(),
                    instance_store,
                    tree_relation_store,
                    layout_state_store,
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
        }
        ViewLayoutChild::Grid {
            ref cols,
            ref rows,
            gap_cols,
            gap_rows,
        } => {
            let child_content_sizes = tree_relation_store.relations[target.into_array_index()]
                .children
                .iter()
                .map(|&child| {
                    compute_actual_content_size(
                        child,
                        &child_available_rect,
                        instance_store,
                        tree_relation_store,
                    )
                })
                .collect::<Vec<_>>();

            let mut left_placement = 0.0;
            let mut top_placement = 0.0;
            let mut size = Size::new_logical(0.0, 0.0);
            let mut col_index = 0;
            for (&child, child_size) in tree_relation_store.relations[target.into_array_index()]
                .children
                .iter()
                .zip(child_content_sizes)
            {
                layout_view_recursive(
                    child,
                    child_available_rect.clone(),
                    instance_store,
                    tree_relation_store,
                    layout_state_store,
                );

                size.width = size.width.max(left_placement + child_size.width);
                size.height = size.height.max(top_placement + child_size.height);
                top_placement += child_size.height + gap_cols;

                if col_index >= cols.len() {
                    // new row
                    left_placement = 0.0;
                    top_placement = size.height + gap_rows;
                }
                col_index += 1;
            }
        }
    }
}

fn compute_actual_content_size(
    target: ViewIdentifier,
    available_rect: &Rect<LogicalUnit>,
    instance_store: &ViewInstanceStore,
    tree_relation_store: &ViewTreeRelationStore,
) -> Size<LogicalUnit> {
    let target_inst = instance_store.get(target);
    let inner_size = if matches!(target_inst.layout.width, ViewSize::FitContent)
        || matches!(target_inst.layout.height, ViewSize::FitContent)
    {
        // サイズ計算で参照されるので計算する
        match target_inst.layout.child {
            ViewLayoutChild::Free => tree_relation_store.relations[target.into_array_index()]
                .children
                .iter()
                .map(|&t| {
                    compute_actual_content_size(
                        t,
                        available_rect,
                        instance_store,
                        tree_relation_store,
                    )
                })
                .fold(Size::new_logical(0.0, 0.0), |a, r| {
                    Size::new_logical(a.width.max(r.width), a.height.max(r.height))
                }),
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
                            available_rect,
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
                            available_rect,
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
                    let mut left_placement = 0.0;
                    let mut top_placement = 0.0;
                    let mut size = Size::new_logical(0.0, 0.0);
                    let mut col_index = 0;
                    for &child in children.iter() {
                        let child_size = compute_actual_content_size(
                            child,
                            available_rect,
                            instance_store,
                            tree_relation_store,
                        );

                        size.width = size.width.max(left_placement + child_size.width);
                        size.height = size.height.max(top_placement + child_size.height);
                        top_placement += child_size.height + gap_cols;

                        if col_index >= cols.len() {
                            // new row
                            left_placement = 0.0;
                            top_placement = size.height + gap_rows;
                        }
                        col_index += 1;
                    }

                    size
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
            ViewSize::FitContent => inner_size.width,
            ViewSize::FillParent => available_rect.width,
        },
        match target_inst.layout.height {
            ViewSize::Fixed(x) => x,
            ViewSize::Percent(x) => available_rect.height * x,
            ViewSize::FitContent => inner_size.height,
            ViewSize::FillParent => available_rect.height,
        },
    )
}
