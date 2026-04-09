use std::rc::Rc;

use crate::{
    input::hittest::{
        CursorShape, HitTestTreeActionHandler, HitTestTreeCreate, HitTestTreeData, HitTestTreeRef,
    },
    rendering::{
        MainThreadTextureIDIssuer, RenderMessage,
        composite::{
            AnimatableColor, AnimatableFloat, AnimationCurve, CompositeMode, CompositeRect,
            CompositeRectText, CompositeRectTextHorizontalAlignment, CompositeRectTextRun,
            CompositeRectTextVerticalAlignment, CompositeTree, CompositeTreeRef, Gradient,
            GradientRef,
        },
        text::{FontID, PerWindowFontSet, TextLayout},
    },
    uikit::{MountContext, MountTarget, ViewInitContext},
    utils::{LogicalUnit, SafeF32, Size},
};

pub enum MenuItem {
    Heading { label: String },
    Command { label: String, command_id: u64 },
    SubMenu { label: String, items: Vec<MenuItem> },
    Separator,
}

pub enum MenuItemView {
    Heading(HeadingView),
    Command(CommandView),
    SubMenu(SubMenuView),
    Separator(SeparatorView),
}
impl MenuItemView {
    pub fn mount(&self, ctx: &mut MountContext, target: &(impl MountTarget + ?Sized)) {
        match self {
            MenuItemView::Heading(heading) => heading.mount(ctx, target),
            MenuItemView::Command(command) => command.mount(ctx, target),
            MenuItemView::SubMenu(submenu) => submenu.mount(ctx, target),
            MenuItemView::Separator(separator) => separator.mount(ctx, target),
        }
    }
}

pub struct MenuItemLayout {
    pub item: MenuItem,
    pub placement_y: f32,
    pub height: f32,
    pub required_width: SafeF32,
}
impl MenuItemLayout {
    pub fn build(
        items: impl Iterator<Item = MenuItem>,
        font_set: &PerWindowFontSet,
        render_scale: f32,
    ) -> Vec<Self> {
        items
            .scan(0.0, |top, item| {
                let (pre_spacing, view_height, post_spacing, required_width);
                match item {
                    MenuItem::Heading { ref label } => {
                        pre_spacing = 0.0;
                        view_height = ITEM_HEIGHT;
                        post_spacing = 0.0;
                        required_width = SafeF32::new(
                            TextLayout::measure_visual_width(
                                label,
                                FontID::UIDefault,
                                font_set,
                                render_scale,
                            ) / render_scale
                                + TEXT_INLINE_MARGIN * 2.0,
                        )
                        .expect("invalid width measured");
                    }
                    MenuItem::Command { ref label, .. } => {
                        pre_spacing = 0.0;
                        view_height = ITEM_HEIGHT;
                        post_spacing = 0.0;
                        required_width = SafeF32::new(
                            TextLayout::measure_visual_width(
                                label,
                                FontID::UIDefault,
                                font_set,
                                render_scale,
                            ) / render_scale
                                + TEXT_INLINE_MARGIN * 2.0,
                        )
                        .expect("invalid width measured");
                    }
                    MenuItem::SubMenu { ref label, .. } => {
                        pre_spacing = 0.0;
                        view_height = ITEM_HEIGHT;
                        post_spacing = 0.0;
                        required_width = SafeF32::new(
                            TextLayout::measure_visual_width(
                                label,
                                FontID::UIDefault,
                                font_set,
                                render_scale,
                            ) / render_scale
                                + TEXT_INLINE_MARGIN * 2.0
                                + SubMenuView::ICON_SIZE.width,
                        )
                        .expect("invalid width measured")
                            + LR_TEXT_MINIMUM_MARGIN;
                    }
                    MenuItem::Separator => {
                        pre_spacing = 2.0;
                        view_height = 1.0;
                        post_spacing = 2.0;
                        required_width = MINIMUM_WIDTH;
                    }
                }

                let placement_y = *top + pre_spacing;
                *top += pre_spacing + view_height + post_spacing;
                Some(Self {
                    item,
                    placement_y,
                    height: view_height + post_spacing,
                    required_width,
                })
            })
            .collect()
    }

    pub fn height<'x>(layout: impl Iterator<Item = &'x Self>) -> SafeF32 {
        layout
            .filter_map(|x| SafeF32::new(x.placement_y + x.height))
            .max()
            .unwrap_or(unsafe { SafeF32::new_unchecked(0.0) })
            .max(MINIMUM_HEIGHT)
    }

    pub fn min_width<'x>(layout: impl Iterator<Item = &'x Self>) -> SafeF32 {
        layout
            .map(|x| x.required_width)
            .max()
            .unwrap_or(unsafe { SafeF32::new_unchecked(0.0) })
            .max(MINIMUM_WIDTH)
    }

    pub fn instantiate(
        layout: impl Iterator<Item = Self>,
        ctx: &mut ViewInitContext,
        common_res: &CommonResources,
    ) -> Vec<MenuItemView> {
        layout
            .scan(0.0, |ad_accum, x| match x.item {
                MenuItem::Heading { label } => Some(MenuItemView::Heading(HeadingView::new(
                    ctx,
                    common_res,
                    label,
                    x.placement_y,
                ))),
                MenuItem::Command { label, command_id } => {
                    let ad = *ad_accum;
                    *ad_accum += ANIMATION_DELAY_PER_ELEMENT;

                    Some(MenuItemView::Command(CommandView::new(
                        ctx,
                        common_res,
                        label,
                        command_id,
                        ad,
                        x.placement_y,
                    )))
                }
                MenuItem::SubMenu { label, items } => {
                    let ad = *ad_accum;
                    *ad_accum += ANIMATION_DELAY_PER_ELEMENT;

                    Some(MenuItemView::SubMenu(SubMenuView::new(
                        ctx,
                        common_res,
                        label,
                        items,
                        ad,
                        x.placement_y,
                    )))
                }
                MenuItem::Separator => Some(MenuItemView::Separator(SeparatorView::new(
                    ctx,
                    common_res,
                    x.placement_y,
                ))),
            })
            .collect()
    }
}

pub struct CommonResources {
    light_gradient: GradientRef,
    tid_submenu_arrow: usize,
}
impl CommonResources {
    pub fn new<E>(
        composite_tree: &mut CompositeTree<E>,
        tid_issuer: &mut MainThreadTextureIDIssuer,
        rt_sender: &std::sync::mpsc::Sender<RenderMessage>,
    ) -> Self {
        let light_gradient = composite_tree.create_gradient(Gradient::Radial {
            start_color: [0.75, 1.0, 1.5, 1.0],
            end_color: [0.25, 0.5, 1.0, 0.0],
            center_relative: [0.5, 0.9],
            radius: [0.5, 0.1],
        });
        let tid_submenu_arrow = tid_issuer.issue();
        rt_sender
            .send(RenderMessage::RegisterNormalized2DStaticMeshTexture {
                id: tid_submenu_arrow,
                vertices: SubMenuView::ARROW_VERTICES,
                indices: SubMenuView::ARROW_INDICES,
                width: SubMenuView::ICON_SIZE.width,
                height: SubMenuView::ICON_SIZE.height,
            })
            .expect("rt_sender.send");

        Self {
            light_gradient,
            tid_submenu_arrow,
        }
    }

    pub const fn composite_mode_light<E>(&self) -> CompositeMode<E> {
        CompositeMode::FillRadialGradient(self.light_gradient)
    }
}

const ITEM_HEIGHT: f32 = 20.0;
const TEXT_INLINE_MARGIN: f32 = 8.0;
const MINIMUM_WIDTH: SafeF32 = unsafe { SafeF32::new_unchecked(16.0) };
const MINIMUM_HEIGHT: SafeF32 = unsafe { SafeF32::new_unchecked(8.0) };
const LR_TEXT_MINIMUM_MARGIN: SafeF32 = unsafe { SafeF32::new_unchecked(16.0) };
const ANIMATION_DELAY_PER_ELEMENT: f32 = 0.025;
const ANIMATION_DURATION: f32 = 0.125;

pub struct HeadingView {
    ct_root: CompositeTreeRef,
}
impl HeadingView {
    pub fn new(
        ctx: &mut ViewInitContext,
        common_res: &CommonResources,
        label: String,
        placement_y: f32,
    ) -> Self {
        let base_scale_factor = ctx.ui_scale_factor;
        let ct_root = ctx.composite_tree.create(CompositeRect {
            base_scale_factor,
            relative_size_adjustment: [1.0, 0.0],
            size: [
                AnimatableFloat::Value(0.0),
                AnimatableFloat::Value(ITEM_HEIGHT),
            ],
            offset: [
                AnimatableFloat::Value(0.0),
                AnimatableFloat::Value(placement_y),
            ],
            has_bitmap: true,
            composite_mode: CompositeMode::FillColor(AnimatableColor::Value([0.0, 0.0, 0.0, 0.9])),
            text: Some(CompositeRectText {
                runs: vec![CompositeRectTextRun {
                    content: label,
                    font_id: FontID::UIDefault,
                    color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                    ..Default::default()
                }],
                horizontal_alignment: CompositeRectTextHorizontalAlignment::Middle,
                vertical_alignment: CompositeRectTextVerticalAlignment::Middle,
                ..Default::default()
            }),
            ..Default::default()
        });

        Self { ct_root }
    }

    pub fn mount(&self, ctx: &mut MountContext, parent: &(impl MountTarget + ?Sized)) {
        ctx.composite_tree.add_child(parent.ct_root(), self.ct_root);
    }
}

pub struct CommandView {
    _event_handler: Rc<CommandViewEventHandler>,
    ht_root: HitTestTreeRef,
    ct_root: CompositeTreeRef,
}
impl CommandView {
    pub fn new(
        ctx: &mut ViewInitContext,
        common_res: &CommonResources,
        label: String,
        command_id: u64,
        animation_delay: f32,
        placement_y: f32,
    ) -> Self {
        let base_scale_factor = ctx.ui_scale_factor;
        let ht_root = ctx.ht_manager.create(HitTestTreeData {
            width_adjustment_factor: 1.0,
            height: ITEM_HEIGHT,
            top: placement_y,
            cursor_shape: CursorShape::Pointer,
            ..Default::default()
        });
        let animation_base_time = ctx.current_sec + animation_delay;
        let ct_root = ctx.composite_tree.create(CompositeRect {
            base_scale_factor,
            relative_size_adjustment: [1.0, 0.0],
            size: [
                AnimatableFloat::Value(0.0),
                AnimatableFloat::Value(ITEM_HEIGHT),
            ],
            offset: [
                AnimatableFloat::Animated {
                    from_value: 4.0,
                    to_value: 0.0,
                    start_sec: animation_base_time,
                    end_sec: animation_base_time + ANIMATION_DURATION,
                    curve: AnimationCurve::EASE_OUT,
                    event_on_complete: None,
                },
                AnimatableFloat::Value(placement_y),
            ],
            opacity: AnimatableFloat::Animated {
                from_value: 0.0,
                to_value: 1.0,
                start_sec: animation_base_time,
                end_sec: animation_base_time + ANIMATION_DURATION,
                curve: AnimationCurve::Linear,
                event_on_complete: None,
            },
            ..Default::default()
        });
        let ct_label = ctx.composite_tree.create(CompositeRect {
            base_scale_factor,
            relative_size_adjustment: [1.0, 1.0],
            offset: [
                AnimatableFloat::Value(TEXT_INLINE_MARGIN),
                AnimatableFloat::Value(0.0),
            ],
            size: [
                AnimatableFloat::Value(-TEXT_INLINE_MARGIN * 2.0),
                AnimatableFloat::Value(0.0),
            ],
            text: Some(CompositeRectText {
                runs: vec![CompositeRectTextRun {
                    font_id: FontID::UIDefault,
                    content: label,
                    color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                    ..Default::default()
                }],
                horizontal_alignment: CompositeRectTextHorizontalAlignment::Start,
                vertical_alignment: CompositeRectTextVerticalAlignment::Middle,
                ..Default::default()
            }),
            ..Default::default()
        });
        let ct_light = ctx.composite_tree.create(CompositeRect {
            base_scale_factor,
            relative_size_adjustment: [1.0, 1.0],
            has_bitmap: true,
            composite_mode: common_res.composite_mode_light(),
            opacity: AnimatableFloat::Value(0.0),
            ..Default::default()
        });
        ctx.composite_tree.add_child(ct_root, ct_light);
        ctx.composite_tree.add_child(ct_root, ct_label);
        let eh = std::rc::Rc::new(CommandViewEventHandler {
            ct_light,
            command_id,
        });
        ctx.ht_manager.set_action_handler(ht_root, &eh);

        Self {
            _event_handler: eh,
            ht_root,
            ct_root,
        }
    }

    pub fn mount(&self, ctx: &mut MountContext, parent: &(impl MountTarget + ?Sized)) {
        ctx.composite_tree.add_child(parent.ct_root(), self.ct_root);
        ctx.ht_manager.add_child(parent.ht_root(), self.ht_root);
    }
}

pub struct SubMenuView {
    _event_handler: Rc<SubMenuViewEventHandler>,
    ht_root: HitTestTreeRef,
    ct_root: CompositeTreeRef,
}
impl SubMenuView {
    const ICON_SIZE: Size<LogicalUnit> = Size::new_logical(6.0, 8.0);
    const ARROW_VERTICES: &[[f32; 2]] = &[
        [0.0, 0.0],
        [0.0 + 1.5 / Self::ICON_SIZE.width, 0.0],
        [1.0 - 1.5 / Self::ICON_SIZE.width, 0.5],
        [1.0, 0.5],
        [0.0, 1.0],
        [0.0 + 1.5 / Self::ICON_SIZE.width, 1.0],
    ];
    const ARROW_INDICES: &[u16] = &[0, 1, 2, 2, 1, 3, 2, 4, 5, 5, 3, 2];

    pub fn new(
        ctx: &mut ViewInitContext,
        common_res: &CommonResources,
        label: String,
        items: Vec<MenuItem>,
        animation_delay: f32,
        placement_y: f32,
    ) -> Self {
        let base_scale_factor = ctx.ui_scale_factor;
        let ht_root = ctx.ht_manager.create(HitTestTreeData {
            width_adjustment_factor: 1.0,
            height: ITEM_HEIGHT,
            top: placement_y,
            cursor_shape: CursorShape::Pointer,
            ..Default::default()
        });
        let animation_base_time = ctx.current_sec + animation_delay;
        let ct_root = ctx.composite_tree.create(CompositeRect {
            base_scale_factor,
            relative_size_adjustment: [1.0, 0.0],
            size: [
                AnimatableFloat::Value(0.0),
                AnimatableFloat::Value(ITEM_HEIGHT),
            ],
            offset: [
                AnimatableFloat::Animated {
                    from_value: 4.0,
                    to_value: 0.0,
                    start_sec: animation_base_time,
                    end_sec: animation_base_time + ANIMATION_DURATION,
                    curve: AnimationCurve::EASE_OUT,
                    event_on_complete: None,
                },
                AnimatableFloat::Value(placement_y),
            ],
            opacity: AnimatableFloat::Animated {
                from_value: 0.0,
                to_value: 1.0,
                start_sec: animation_base_time,
                end_sec: animation_base_time + ANIMATION_DURATION,
                curve: AnimationCurve::Linear,
                event_on_complete: None,
            },
            ..Default::default()
        });
        let ct_label = ctx.composite_tree.create(CompositeRect {
            base_scale_factor,
            relative_size_adjustment: [1.0, 1.0],
            offset: [
                AnimatableFloat::Value(TEXT_INLINE_MARGIN),
                AnimatableFloat::Value(0.0),
            ],
            size: [
                AnimatableFloat::Value(-TEXT_INLINE_MARGIN * 2.0),
                AnimatableFloat::Value(0.0),
            ],
            text: Some(CompositeRectText {
                runs: vec![CompositeRectTextRun {
                    font_id: FontID::UIDefault,
                    content: label,
                    color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                    ..Default::default()
                }],
                horizontal_alignment: CompositeRectTextHorizontalAlignment::Start,
                vertical_alignment: CompositeRectTextVerticalAlignment::Middle,
                ..Default::default()
            }),
            ..Default::default()
        });
        let ct_arrow = ctx.composite_tree.create(CompositeRect {
            base_scale_factor,
            relative_offset_adjustment: [1.0, 0.5],
            size: [
                AnimatableFloat::Value(Self::ICON_SIZE.width),
                AnimatableFloat::Value(Self::ICON_SIZE.height),
            ],
            offset: [
                AnimatableFloat::Value(-Self::ICON_SIZE.width - TEXT_INLINE_MARGIN),
                AnimatableFloat::Value(-Self::ICON_SIZE.height * 0.5),
            ],
            has_bitmap: true,
            texatlas_rect_id: Some(common_res.tid_submenu_arrow),
            composite_mode: CompositeMode::ColorTint(AnimatableColor::Value([1.0, 1.0, 1.0, 1.0])),
            ..Default::default()
        });
        let ct_light = ctx.composite_tree.create(CompositeRect {
            base_scale_factor,
            relative_size_adjustment: [1.0, 1.0],
            has_bitmap: true,
            composite_mode: common_res.composite_mode_light(),
            opacity: AnimatableFloat::Value(0.0),
            ..Default::default()
        });
        ctx.composite_tree.add_child(ct_root, ct_light);
        ctx.composite_tree.add_child(ct_root, ct_label);
        ctx.composite_tree.add_child(ct_root, ct_arrow);
        let eh = std::rc::Rc::new(SubMenuViewEventHandler { ct_light });
        ctx.ht_manager.set_action_handler(ht_root, &eh);

        Self {
            _event_handler: eh,
            ht_root,
            ct_root,
        }
    }

    pub fn mount(&self, ctx: &mut MountContext, parent: &(impl MountTarget + ?Sized)) {
        ctx.composite_tree.add_child(parent.ct_root(), self.ct_root);
        ctx.ht_manager.add_child(parent.ht_root(), self.ht_root);
    }
}

pub struct SeparatorView {
    ct_root: CompositeTreeRef,
}
impl SeparatorView {
    pub fn new(ctx: &mut ViewInitContext, common_res: &CommonResources, placement_y: f32) -> Self {
        let base_scale_factor = ctx.ui_scale_factor;
        let ct_root = ctx.composite_tree.create(CompositeRect {
            base_scale_factor,
            relative_size_adjustment: [1.0, 0.0],
            size: [AnimatableFloat::Value(0.0), AnimatableFloat::Value(1.0)],
            offset: [
                AnimatableFloat::Value(0.0),
                AnimatableFloat::Value(placement_y),
            ],
            has_bitmap: true,
            composite_mode: CompositeMode::FillColor(AnimatableColor::Value([1.0, 1.0, 1.0, 0.5])),
            ..Default::default()
        });

        Self { ct_root }
    }

    pub fn mount(&self, ctx: &mut MountContext, parent: &(impl MountTarget + ?Sized)) {
        ctx.composite_tree.add_child(parent.ct_root(), self.ct_root);
    }
}

struct CommandViewEventHandler {
    ct_light: CompositeTreeRef,
    command_id: u64,
}
impl HitTestTreeActionHandler for CommandViewEventHandler {
    fn on_pointer_enter(
        &self,
        sender: HitTestTreeRef,
        context: &mut crate::input::InputEventContext,
        args: &crate::input::hittest::PointerActionArgs,
    ) -> crate::input::EventContinueControl {
        context.composite_tree.get_mut(self.ct_light).opacity = AnimatableFloat::Animated {
            from_value: 0.0,
            to_value: 1.0,
            start_sec: context.current_sec,
            end_sec: context.current_sec + 0.1,
            curve: AnimationCurve::Linear,
            event_on_complete: None,
        };
        context.composite_tree.mark_dirty(self.ct_light);

        crate::input::EventContinueControl::STOP_PROPAGATION
    }

    fn on_pointer_leave(
        &self,
        sender: HitTestTreeRef,
        context: &mut crate::input::InputEventContext,
        args: &crate::input::hittest::PointerActionArgs,
    ) -> crate::input::EventContinueControl {
        context.composite_tree.get_mut(self.ct_light).opacity = AnimatableFloat::Animated {
            from_value: 1.0,
            to_value: 0.0,
            start_sec: context.current_sec,
            end_sec: context.current_sec + 0.1,
            curve: AnimationCurve::Linear,
            event_on_complete: None,
        };
        context.composite_tree.mark_dirty(self.ct_light);

        crate::input::EventContinueControl::STOP_PROPAGATION
    }
}

struct SubMenuViewEventHandler {
    ct_light: CompositeTreeRef,
}
impl HitTestTreeActionHandler for SubMenuViewEventHandler {
    fn on_pointer_enter(
        &self,
        sender: HitTestTreeRef,
        context: &mut crate::input::InputEventContext,
        args: &crate::input::hittest::PointerActionArgs,
    ) -> crate::input::EventContinueControl {
        context.composite_tree.get_mut(self.ct_light).opacity = AnimatableFloat::Animated {
            from_value: 0.0,
            to_value: 1.0,
            start_sec: context.current_sec,
            end_sec: context.current_sec + 0.1,
            curve: AnimationCurve::Linear,
            event_on_complete: None,
        };
        context.composite_tree.mark_dirty(self.ct_light);

        crate::input::EventContinueControl::STOP_PROPAGATION
    }

    fn on_pointer_leave(
        &self,
        sender: HitTestTreeRef,
        context: &mut crate::input::InputEventContext,
        args: &crate::input::hittest::PointerActionArgs,
    ) -> crate::input::EventContinueControl {
        context.composite_tree.get_mut(self.ct_light).opacity = AnimatableFloat::Animated {
            from_value: 1.0,
            to_value: 0.0,
            start_sec: context.current_sec,
            end_sec: context.current_sec + 0.1,
            curve: AnimationCurve::Linear,
            event_on_complete: None,
        };
        context.composite_tree.mark_dirty(self.ct_light);

        crate::input::EventContinueControl::STOP_PROPAGATION
    }
}
