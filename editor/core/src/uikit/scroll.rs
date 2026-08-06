use std::rc::Rc;

use crate::{
    Event,
    input::{
        EventContinueControl, InputEventContext, ModifierKey,
        hittest::{
            HitTestTreeActionHandler, HitTestTreeData, HitTestTreeManager, HitTestTreeRef,
            PointerActionArgs, PointerButtonActionArgs, ScrollWheelActionArgs,
            ScrollWheelActionResponse,
        },
    },
    rendering::composite::{
        AnimatableColor, AnimatableFloat, AnimationCurve, ClipConfig, CompositeMode, CompositeRect,
        CompositeRectScaleFactor, CompositeTree, CompositeTreeRef, CornerRadius,
        FloatAnimationTemplate,
    },
    uikit::{
        MountContext, MountTarget, RawMountTarget, RenderChildScheduler, RenderContext,
        TeardownContext, View, ViewEventHandler, ViewIdentifier, ViewInitContext,
        ViewInstanceModifier, ViewNewRenderElements, ViewUpdateContext,
    },
    utils::{InteriorMutableLogicalUnit, LogicalUnit, Point, Rect, SafeF32, Size},
};

const DEFAULT_SCROLL_BAR_THICKNESS: f32 = 4.0;
const ACTIVE_SCROLL_BAR_THICKNESS: f32 = 8.0;
const SCROLL_THUMB_SPACING: f32 = 1.0;
const SCROLL_FADEOUT_DELAY_SECS: f32 = 0.625;
const SCROLL_FADEOUT_DURATION_SECS: f32 = 0.375;
const SCROLL_AMOUNT_MULTIPLIER: f32 = 24.0;
const ACTIVE_THUMB_COLOR: [f32; 4] = [0.0, 0.0, 0.0, 0.75];
const INACTIVE_THUMB_COLOR: [f32; 4] = [1.0, 1.0, 1.0, 0.5];

const SCROLL_THUMB_DEACTIVATE_OPACITY_ANIM: &FloatAnimationTemplate = &FloatAnimationTemplate {
    from_value: 1.0,
    to_value: 0.0,
    curve: AnimationCurve::Linear,
    duration: SCROLL_FADEOUT_DURATION_SECS,
};
const SCROLL_BAR_ACTIVATE_OPACITY_ANIM: &FloatAnimationTemplate = &FloatAnimationTemplate {
    from_value: 0.0,
    to_value: 1.0,
    curve: AnimationCurve::Linear,
    duration: 0.1,
};
const SCROLL_BAR_DEACTIVATE_OPACITY_ANIM: &FloatAnimationTemplate =
    &SCROLL_BAR_ACTIVATE_OPACITY_ANIM.flip(AnimationCurve::Linear);
const SCROLL_THUMB_ACTIVATE_THICKNESS_ANIM: &FloatAnimationTemplate = &FloatAnimationTemplate {
    from_value: DEFAULT_SCROLL_BAR_THICKNESS,
    to_value: ACTIVE_SCROLL_BAR_THICKNESS - SCROLL_THUMB_SPACING * 2.0,
    curve: AnimationCurve::Linear,
    duration: 0.1,
};
const SCROLL_THUMB_DEACTIVATE_THICKNESS_ANIM: &FloatAnimationTemplate =
    &SCROLL_THUMB_ACTIVATE_THICKNESS_ANIM.flip(AnimationCurve::Linear);
const SCROLL_THUMB_ACTIVATE_OFFSET_ANIM: &FloatAnimationTemplate = &FloatAnimationTemplate {
    from_value: -SCROLL_THUMB_SPACING - DEFAULT_SCROLL_BAR_THICKNESS,
    to_value: -SCROLL_THUMB_SPACING - (ACTIVE_SCROLL_BAR_THICKNESS - SCROLL_THUMB_SPACING * 2.0),
    curve: AnimationCurve::Linear,
    duration: 0.1,
};
const SCROLL_THUMB_DEACTIVATE_OFFSET_ANIM: &FloatAnimationTemplate =
    &SCROLL_THUMB_ACTIVATE_OFFSET_ANIM.flip(AnimationCurve::Linear);

/// Mount-Unmount方式(Dock)からRender-Teradown方式(ScrollContainer)に一度に書き換えるのが厳しそうなので間となるクッション要素を挟む
pub struct ScrollContainerTemp {
    ct_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
}
impl ScrollContainerTemp {
    pub fn new(ctx: &mut ViewInitContext, rect: Rect<LogicalUnit>) -> Self {
        let ct_root = ctx.mount_context.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            offset: [
                AnimatableFloat::Value(rect.left),
                AnimatableFloat::Value(rect.top),
            ],
            relative_size_adjustment: [1.0, 1.0],
            ..Default::default()
        });
        let ht_root = ctx.ht_manager.create(HitTestTreeData {
            left: rect.left,
            top: rect.top,
            width_adjustment_factor: 1.0,
            height_adjustment_factor: 1.0,
            ..Default::default()
        });

        Self { ct_root, ht_root }
    }

    pub fn mount(&self, ctx: &mut MountContext, target: &(impl MountTarget + ?Sized)) {
        ctx.composite_tree.add_child(target.ct_root(), self.ct_root);
        ctx.ht_manager.add_child(target.ht_root(), self.ht_root);
    }

    pub fn unmount(&self, ctx: &mut MountContext) {
        ctx.composite_tree.remove_child(self.ct_root);
        ctx.ht_manager.remove_child(self.ht_root);
    }
}
impl MountTarget for ScrollContainerTemp {
    #[inline(always)]
    fn ct_root(&self) -> CompositeTreeRef {
        self.ct_root
    }

    #[inline(always)]
    fn ht_root(&self) -> HitTestTreeRef {
        self.ht_root
    }
}

pub struct ScrollContainer {
    id: ViewIdentifier,
    eh: Option<Rc<ScrollContainerEventHandler>>,
    offset: Point<LogicalUnit>,
    viewport_size_changes: Option<Size<LogicalUnit>>,
    content_size_changes: Option<Size<LogicalUnit>>,
}
impl ScrollContainer {
    pub fn new(id: ViewIdentifier, rect: Rect<LogicalUnit>) -> Self {
        Self {
            id,
            eh: None,
            offset: rect.left_top(),
            viewport_size_changes: Some(rect.size()),
            content_size_changes: Some(Size::new_logical(0.0, 0.0)),
        }
    }

    pub fn resize(&mut self, size: Size<LogicalUnit>) {
        self.viewport_size_changes = Some(size);
    }

    pub fn set_content_size(&mut self, size: Size<LogicalUnit>) {
        self.content_size_changes = Some(size);
    }
}
impl View for ScrollContainer {
    fn render(
        &mut self,
        self_instance: &mut ViewInstanceModifier,
        ctx: &mut RenderContext,
        sched: &mut RenderChildScheduler,
    ) -> ViewNewRenderElements {
        match self.eh {
            Some(ref eh) => {
                let mut recompute_scroll_bars = false;
                if let Some(viewport_size) = self.viewport_size_changes.take() {
                    eh.viewport_size.width.set(viewport_size.width);
                    eh.viewport_size.height.set(viewport_size.height);

                    ctx.composite_tree
                        .begin_mod_chain(eh.ct_root)
                        .size_imm(viewport_size.width, viewport_size.height)
                        .apply();
                    ctx.ht_manager.get_data_mut(eh.ht_root).width = viewport_size.width;
                    ctx.ht_manager.get_data_mut(eh.ht_root).height = viewport_size.height;

                    recompute_scroll_bars = true;
                }

                if let Some(content_size) = self.content_size_changes.take() {
                    eh.content_size.width.set(content_size.width);
                    eh.content_size.height.set(content_size.height);

                    recompute_scroll_bars = true;
                }

                if recompute_scroll_bars {
                    // mount/dismount scroll bars only if needed
                    eh.recompute_scroll_bars(ctx);
                }

                sched.schedule_render_children(RawMountTarget {
                    ct_root: eh.ct_content_root,
                    ht_root: eh.ht_content_root,
                });
                ViewNewRenderElements::EMPTY
            }
            None => {
                // first render
                let init_viewport_size =
                    self.viewport_size_changes.take().expect("not initialized");
                let init_content_size = self.content_size_changes.take().expect("not initialized");

                let ct_root = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    offset: [
                        AnimatableFloat::Value(self.offset.x),
                        AnimatableFloat::Value(self.offset.y),
                    ],
                    size: [
                        AnimatableFloat::Value(init_viewport_size.width),
                        AnimatableFloat::Value(init_viewport_size.height),
                    ],
                    has_bitmap: false,
                    // composite_mode: CompositeMode::FillColor(AnimatableColor::Value([
                    //     1.0, 1.0, 1.0, 0.0625,
                    // ])),
                    clip_child: Some(ClipConfig {
                        left_softness: SafeF32::ZERO,
                        top_softness: SafeF32::ZERO,
                        right_softness: SafeF32::ZERO,
                        bottom_softness: SafeF32::ZERO,
                    }),
                    ..Default::default()
                });
                let ht_root = ctx.ht_manager.create(HitTestTreeData {
                    left: self.offset.x,
                    top: self.offset.y,
                    width: init_viewport_size.width,
                    height: init_viewport_size.height,
                    clip_children: true,
                    ..Default::default()
                });
                let ct_content_root = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    offset: [AnimatableFloat::Value(0.0), AnimatableFloat::Value(0.0)],
                    size: [AnimatableFloat::Value(0.0), AnimatableFloat::Value(0.0)],
                    ..Default::default()
                });
                let ht_content_root = ctx.ht_manager.create(HitTestTreeData {
                    left: 0.0,
                    top: 0.0,
                    width: init_viewport_size.width,
                    height: init_viewport_size.height,
                    ..Default::default()
                });

                let ct_scroll_bar_vert = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    offset: [
                        AnimatableFloat::Value(-ACTIVE_SCROLL_BAR_THICKNESS),
                        AnimatableFloat::Value(0.0),
                    ],
                    relative_offset_adjustment: [1.0, 0.0],
                    size: [
                        AnimatableFloat::Value(ACTIVE_SCROLL_BAR_THICKNESS),
                        AnimatableFloat::Value(0.0),
                    ],
                    relative_size_adjustment: [0.0, 1.0],
                    has_bitmap: true,
                    composite_mode: CompositeMode::FillColor(AnimatableColor::Value([
                        0.75, 0.75, 0.75, 0.5,
                    ])),
                    opacity: AnimatableFloat::Value(0.0),
                    ..Default::default()
                });
                let ct_scroll_thumb_vert = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    offset: [
                        AnimatableFloat::Value(
                            -SCROLL_THUMB_SPACING - DEFAULT_SCROLL_BAR_THICKNESS,
                        ),
                        AnimatableFloat::Value(SCROLL_THUMB_SPACING),
                    ],
                    relative_offset_adjustment: [1.0, 0.0],
                    size: [
                        AnimatableFloat::Value(DEFAULT_SCROLL_BAR_THICKNESS),
                        AnimatableFloat::Value(
                            init_viewport_size.height - SCROLL_THUMB_SPACING * 2.0,
                        ),
                    ],
                    has_bitmap: true,
                    composite_mode: CompositeMode::FillColor(AnimatableColor::Value(
                        INACTIVE_THUMB_COLOR,
                    )),
                    corner_radius: CornerRadius::all(DEFAULT_SCROLL_BAR_THICKNESS * 0.5),
                    opacity: AnimatableFloat::Value(0.0),
                    ..Default::default()
                });
                let ht_scroll_bar_vert = ctx.ht_manager.create(HitTestTreeData {
                    left: -ACTIVE_SCROLL_BAR_THICKNESS,
                    top: 0.0,
                    left_adjustment_factor: 1.0,
                    width: ACTIVE_SCROLL_BAR_THICKNESS,
                    height_adjustment_factor: 1.0,
                    ..Default::default()
                });
                let ht_scroll_thumb_vert = ctx.ht_manager.create(HitTestTreeData {
                    left: -ACTIVE_SCROLL_BAR_THICKNESS,
                    top: 0.0,
                    left_adjustment_factor: 1.0,
                    width: ACTIVE_SCROLL_BAR_THICKNESS,
                    height: init_viewport_size.height,
                    ..Default::default()
                });

                let ct_scroll_bar_horz = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    offset: [
                        AnimatableFloat::Value(0.0),
                        AnimatableFloat::Value(-ACTIVE_SCROLL_BAR_THICKNESS),
                    ],
                    relative_offset_adjustment: [0.0, 1.0],
                    size: [
                        AnimatableFloat::Value(0.0),
                        AnimatableFloat::Value(ACTIVE_SCROLL_BAR_THICKNESS),
                    ],
                    relative_size_adjustment: [1.0, 0.0],
                    has_bitmap: true,
                    composite_mode: CompositeMode::FillColor(AnimatableColor::Value([
                        0.75, 0.75, 0.75, 0.5,
                    ])),
                    opacity: AnimatableFloat::Value(0.0),
                    ..Default::default()
                });
                let ct_scroll_thumb_horz = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    offset: [
                        AnimatableFloat::Value(SCROLL_THUMB_SPACING),
                        AnimatableFloat::Value(
                            -SCROLL_THUMB_SPACING - DEFAULT_SCROLL_BAR_THICKNESS,
                        ),
                    ],
                    relative_offset_adjustment: [0.0, 1.0],
                    size: [
                        AnimatableFloat::Value(
                            init_viewport_size.width - SCROLL_THUMB_SPACING * 2.0,
                        ),
                        AnimatableFloat::Value(DEFAULT_SCROLL_BAR_THICKNESS),
                    ],
                    has_bitmap: true,
                    composite_mode: CompositeMode::FillColor(AnimatableColor::Value(
                        INACTIVE_THUMB_COLOR,
                    )),
                    corner_radius: CornerRadius::all(DEFAULT_SCROLL_BAR_THICKNESS * 0.5),
                    opacity: AnimatableFloat::Value(0.0),
                    ..Default::default()
                });
                let ht_scroll_bar_horz = ctx.ht_manager.create(HitTestTreeData {
                    left: 0.0,
                    top: -ACTIVE_SCROLL_BAR_THICKNESS,
                    top_adjustment_factor: 1.0,
                    width_adjustment_factor: 1.0,
                    height: ACTIVE_SCROLL_BAR_THICKNESS,
                    ..Default::default()
                });
                let ht_scroll_thumb_horz = ctx.ht_manager.create(HitTestTreeData {
                    left: 0.0,
                    top: -ACTIVE_SCROLL_BAR_THICKNESS,
                    top_adjustment_factor: 1.0,
                    width: init_viewport_size.width,
                    height: ACTIVE_SCROLL_BAR_THICKNESS,
                    ..Default::default()
                });

                ctx.composite_tree.add_child(ct_root, ct_content_root);
                ctx.ht_manager.add_child(ht_root, ht_content_root);

                let eh = Rc::new(ScrollContainerEventHandler {
                    view_id: self.id,
                    ct_root,
                    ht_root,
                    ct_content_root,
                    ht_content_root,
                    ct_scroll_thumb_vert,
                    ht_scroll_thumb_vert,
                    ht_scroll_bar_vert,
                    ct_scroll_bar_vert,
                    ct_scroll_thumb_horz,
                    ht_scroll_thumb_horz,
                    ht_scroll_bar_horz,
                    ct_scroll_bar_horz,
                    viewport_size: Size::new_logical_interior_mutable(
                        init_viewport_size.width,
                        init_viewport_size.height,
                    ),
                    content_size: Size::new_logical_interior_mutable(
                        init_content_size.width,
                        init_content_size.height,
                    ),
                    content_offset: Point::new_logical_interior_mutable(0.0, 0.0),
                    pointer_grab_state: core::cell::Cell::new(
                        ScrollContainerPointerGrabState::None,
                    ),
                    bar_active: core::cell::Cell::new(false),
                    bar_active_horz: core::cell::Cell::new(false),
                    should_scroll_vert: core::cell::Cell::new(false),
                    should_scroll_horz: core::cell::Cell::new(false),
                });
                ctx.ht_manager.set_action_handler(ht_root, &eh);
                ctx.ht_manager.set_action_handler(ht_scroll_bar_vert, &eh);
                ctx.ht_manager.set_action_handler(ht_scroll_thumb_vert, &eh);
                ctx.ht_manager.set_action_handler(ht_scroll_bar_horz, &eh);
                ctx.ht_manager.set_action_handler(ht_scroll_thumb_horz, &eh);
                self_instance.bind_event_handler(&eh);

                // initial setup for scroll bars
                eh.recompute_scroll_bars(ctx);

                self.eh = Some(eh);
                sched.schedule_render_children(RawMountTarget {
                    ct_root: ct_content_root,
                    ht_root: ht_content_root,
                });
                ViewNewRenderElements {
                    composite_tree: Some(ct_root),
                    hit_tree: Some(ht_root),
                    ..ViewNewRenderElements::EMPTY
                }
            }
        }
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        let Some(entity) = self.eh.take() else {
            // not rendered
            return;
        };

        ctx.mount_context.composite_tree.free_all(entity.ct_root);
        ctx.mount_context.ht_manager.free_all(entity.ht_root);
    }
}

struct ScrollContainerEventHandler {
    view_id: ViewIdentifier,
    ct_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    ct_content_root: CompositeTreeRef,
    ht_content_root: HitTestTreeRef,
    ht_scroll_thumb_vert: HitTestTreeRef,
    ct_scroll_thumb_vert: CompositeTreeRef,
    ht_scroll_bar_vert: HitTestTreeRef,
    ct_scroll_bar_vert: CompositeTreeRef,
    ht_scroll_thumb_horz: HitTestTreeRef,
    ct_scroll_thumb_horz: CompositeTreeRef,
    ht_scroll_bar_horz: HitTestTreeRef,
    ct_scroll_bar_horz: CompositeTreeRef,
    viewport_size: Size<InteriorMutableLogicalUnit>,
    content_size: Size<InteriorMutableLogicalUnit>,
    content_offset: Point<InteriorMutableLogicalUnit>,
    pointer_grab_state: core::cell::Cell<ScrollContainerPointerGrabState>,
    bar_active: core::cell::Cell<bool>,
    bar_active_horz: core::cell::Cell<bool>,
    should_scroll_vert: core::cell::Cell<bool>,
    should_scroll_horz: core::cell::Cell<bool>,
}
impl HitTestTreeActionHandler for ScrollContainerEventHandler {
    fn on_pointer_enter(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerActionArgs,
    ) -> EventContinueControl {
        if sender == self.ht_scroll_bar_vert {
            self.activate_bar(context.composite_tree, context.current_sec);
            return EventContinueControl::STOP_PROPAGATION;
        }
        if sender == self.ht_scroll_thumb_vert {
            self.activate_bar(context.composite_tree, context.current_sec);
            // override
            context
                .composite_tree
                .get_mut(self.ct_scroll_thumb_vert)
                .composite_mode =
                CompositeMode::FillColor(AnimatableColor::Value([0.0, 0.0, 0.0, 1.0]));
            context.composite_tree.mark_dirty(self.ct_scroll_thumb_vert);

            return EventContinueControl::STOP_PROPAGATION;
        }

        if sender == self.ht_scroll_bar_horz {
            self.activate_bar_horz(context.composite_tree, context.current_sec);
            return EventContinueControl::STOP_PROPAGATION;
        }
        if sender == self.ht_scroll_thumb_horz {
            self.activate_bar_horz(context.composite_tree, context.current_sec);
            // override
            context
                .composite_tree
                .get_mut(self.ct_scroll_thumb_horz)
                .composite_mode =
                CompositeMode::FillColor(AnimatableColor::Value([0.0, 0.0, 0.0, 1.0]));
            context.composite_tree.mark_dirty(self.ct_scroll_thumb_horz);

            return EventContinueControl::STOP_PROPAGATION;
        }

        EventContinueControl::empty()
    }

    fn on_pointer_leave(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerActionArgs,
    ) -> EventContinueControl {
        if sender == self.ht_scroll_bar_vert {
            self.deactivate_bar(context.composite_tree, context.current_sec);
            return EventContinueControl::STOP_PROPAGATION;
        }
        if sender == self.ht_scroll_thumb_vert {
            self.deactivate_bar(context.composite_tree, context.current_sec);
            return EventContinueControl::STOP_PROPAGATION;
        }

        if sender == self.ht_scroll_bar_horz {
            self.deactivate_bar_horz(context.composite_tree, context.current_sec);
            return EventContinueControl::STOP_PROPAGATION;
        }
        if sender == self.ht_scroll_thumb_horz {
            self.deactivate_bar_horz(context.composite_tree, context.current_sec);
            return EventContinueControl::STOP_PROPAGATION;
        }

        EventContinueControl::empty()
    }

    fn on_pointer_down(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        if sender == self.ht_scroll_thumb_vert {
            let (_, base_offset_y, _, _) = context.ht_manager.translate_client_to_tree_local(
                sender,
                args.client_pos.x,
                args.client_pos.y,
                args.client_size.width,
                args.client_size.height,
            );
            self.pointer_grab_state
                .set(ScrollContainerPointerGrabState::ThumbVert { base_offset_y });

            return EventContinueControl::STOP_PROPAGATION | EventContinueControl::CAPTURE_ELEMENT;
        }
        if sender == self.ht_scroll_bar_vert {
            self.pointer_grab_state
                .set(ScrollContainerPointerGrabState::BarVert);

            return EventContinueControl::STOP_PROPAGATION | EventContinueControl::CAPTURE_ELEMENT;
        }

        if sender == self.ht_scroll_thumb_horz {
            let (base_offset_x, _, _, _) = context.ht_manager.translate_client_to_tree_local(
                sender,
                args.client_pos.x,
                args.client_pos.y,
                args.client_size.width,
                args.client_size.height,
            );
            self.pointer_grab_state
                .set(ScrollContainerPointerGrabState::ThumbHorz { base_offset_x });

            return EventContinueControl::STOP_PROPAGATION | EventContinueControl::CAPTURE_ELEMENT;
        }
        if sender == self.ht_scroll_bar_horz {
            self.pointer_grab_state
                .set(ScrollContainerPointerGrabState::BarHorz);

            return EventContinueControl::STOP_PROPAGATION | EventContinueControl::CAPTURE_ELEMENT;
        }

        EventContinueControl::empty()
    }

    fn on_pointer_move(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerActionArgs,
    ) -> EventContinueControl {
        match self.pointer_grab_state.get() {
            ScrollContainerPointerGrabState::None => EventContinueControl::STOP_PROPAGATION,
            ScrollContainerPointerGrabState::BarVert => {
                let (_, vp_offset_y, _, _) = context.ht_manager.translate_client_to_tree_local(
                    sender,
                    args.client_pos.x,
                    args.client_pos.y,
                    args.client_size.width,
                    args.client_size.height,
                );

                let content_h = self.content_size.height.get();
                let vp_h = self.viewport_size.height.get();
                let offset_y = ((vp_offset_y - 0.5 * vp_h * vp_h / content_h) * content_h / vp_h)
                    .clamp(0.0, content_h - vp_h);
                self.content_offset.y.set(offset_y);
                context
                    .system_link
                    .dispatch_event(Event::UpdateView { id: self.view_id });

                EventContinueControl::STOP_PROPAGATION
            }
            ScrollContainerPointerGrabState::ThumbVert { base_offset_y } => {
                let (_, vp_offset_y, _, _) = context.ht_manager.translate_client_to_tree_local(
                    self.ht_scroll_bar_vert,
                    args.client_pos.x,
                    args.client_pos.y,
                    args.client_size.width,
                    args.client_size.height,
                );

                let content_h = self.content_size.height.get();
                let vp_h = self.viewport_size.height.get();
                let offset_y =
                    ((vp_offset_y - base_offset_y) * content_h / vp_h).clamp(0.0, content_h - vp_h);
                self.content_offset.y.set(offset_y);
                context
                    .system_link
                    .dispatch_event(Event::UpdateView { id: self.view_id });

                EventContinueControl::STOP_PROPAGATION
            }
            ScrollContainerPointerGrabState::BarHorz => {
                let (vp_offset_x, _, _, _) = context.ht_manager.translate_client_to_tree_local(
                    sender,
                    args.client_pos.x,
                    args.client_pos.y,
                    args.client_size.width,
                    args.client_size.height,
                );

                let content_w = self.content_size.width.get();
                let vp_w = self.viewport_size.width.get();
                let offset_x = ((vp_offset_x - 0.5 * vp_w * vp_w / content_w) * content_w / vp_w)
                    .clamp(0.0, content_w - vp_w);
                self.content_offset.x.set(offset_x);
                context
                    .system_link
                    .dispatch_event(Event::UpdateView { id: self.view_id });

                EventContinueControl::STOP_PROPAGATION
            }
            ScrollContainerPointerGrabState::ThumbHorz { base_offset_x } => {
                let (vp_offset_x, _, _, _) = context.ht_manager.translate_client_to_tree_local(
                    self.ht_scroll_bar_horz,
                    args.client_pos.x,
                    args.client_pos.y,
                    args.client_size.width,
                    args.client_size.height,
                );

                let content_w = self.content_size.width.get();
                let vp_w = self.viewport_size.width.get();
                let offset_x =
                    ((vp_offset_x - base_offset_x) * content_w / vp_w).clamp(0.0, content_w - vp_w);
                self.content_offset.x.set(offset_x);
                context
                    .system_link
                    .dispatch_event(Event::UpdateView { id: self.view_id });

                EventContinueControl::STOP_PROPAGATION
            }
        }
    }

    fn on_drag_move(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerActionArgs,
    ) -> EventContinueControl {
        match self.pointer_grab_state.get() {
            ScrollContainerPointerGrabState::None => EventContinueControl::STOP_PROPAGATION,
            ScrollContainerPointerGrabState::BarVert => {
                let (_, vp_offset_y, _, _) = context.ht_manager.translate_client_to_tree_local(
                    sender,
                    args.client_pos.x,
                    args.client_pos.y,
                    args.client_size.width,
                    args.client_size.height,
                );

                let content_h = self.content_size.height.get();
                let vp_h = self.viewport_size.height.get();
                let offset_y = ((vp_offset_y - 0.5 * vp_h * vp_h / content_h) * content_h / vp_h)
                    .clamp(0.0, content_h - vp_h);
                self.content_offset.y.set(offset_y);
                context
                    .system_link
                    .dispatch_event(Event::UpdateView { id: self.view_id });

                EventContinueControl::STOP_PROPAGATION
            }
            ScrollContainerPointerGrabState::ThumbVert { base_offset_y } => {
                let (_, vp_offset_y, _, _) = context.ht_manager.translate_client_to_tree_local(
                    self.ht_scroll_bar_vert,
                    args.client_pos.x,
                    args.client_pos.y,
                    args.client_size.width,
                    args.client_size.height,
                );

                let content_h = self.content_size.height.get();
                let vp_h = self.viewport_size.height.get();
                let offset_y =
                    ((vp_offset_y - base_offset_y) * content_h / vp_h).clamp(0.0, content_h - vp_h);
                self.content_offset.y.set(offset_y);
                context
                    .system_link
                    .dispatch_event(Event::UpdateView { id: self.view_id });

                EventContinueControl::STOP_PROPAGATION
            }
            ScrollContainerPointerGrabState::BarHorz => {
                let (vp_offset_x, _, _, _) = context.ht_manager.translate_client_to_tree_local(
                    sender,
                    args.client_pos.x,
                    args.client_pos.y,
                    args.client_size.width,
                    args.client_size.height,
                );

                let content_w = self.content_size.width.get();
                let vp_w = self.viewport_size.width.get();
                let offset_x = ((vp_offset_x - 0.5 * vp_w * vp_w / content_w) * content_w / vp_w)
                    .clamp(0.0, content_w - vp_w);
                self.content_offset.x.set(offset_x);
                context
                    .system_link
                    .dispatch_event(Event::UpdateView { id: self.view_id });

                EventContinueControl::STOP_PROPAGATION
            }
            ScrollContainerPointerGrabState::ThumbHorz { base_offset_x } => {
                let (vp_offset_x, _, _, _) = context.ht_manager.translate_client_to_tree_local(
                    self.ht_scroll_bar_horz,
                    args.client_pos.x,
                    args.client_pos.y,
                    args.client_size.width,
                    args.client_size.height,
                );

                let content_w = self.content_size.width.get();
                let vp_w = self.viewport_size.width.get();
                let offset_x =
                    ((vp_offset_x - base_offset_x) * content_w / vp_w).clamp(0.0, content_w - vp_w);
                self.content_offset.x.set(offset_x);
                context
                    .system_link
                    .dispatch_event(Event::UpdateView { id: self.view_id });

                EventContinueControl::STOP_PROPAGATION
            }
        }
    }

    fn on_pointer_up(
        &self,
        _sender: HitTestTreeRef,
        _context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        match self
            .pointer_grab_state
            .replace(ScrollContainerPointerGrabState::None)
        {
            ScrollContainerPointerGrabState::BarVert
            | ScrollContainerPointerGrabState::ThumbVert { .. }
            | ScrollContainerPointerGrabState::BarHorz
            | ScrollContainerPointerGrabState::ThumbHorz { .. } => {
                EventContinueControl::STOP_PROPAGATION
                    | EventContinueControl::RELEASE_CAPTURE_ELEMENT
            }
            _ => EventContinueControl::STOP_PROPAGATION,
        }
    }

    fn on_scroll_wheel(
        &self,
        _sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &ScrollWheelActionArgs,
    ) -> ScrollWheelActionResponse {
        tracing::debug!(amount = args.amount, "scroll wheel");

        if args.key_modifier.contains(ModifierKey::SHIFT) {
            // horizontal mode
            let content_w = self.content_size.width.get();
            let viewport_w = self.viewport_size.width.get();
            let max_overflow = content_w - viewport_w;
            if max_overflow <= 0.0 {
                // nothing to be scrolled
                return ScrollWheelActionResponse {
                    continue_flags: EventContinueControl::STOP_PROPAGATION,
                    left_amount: args.amount,
                };
            }

            context
                .composite_tree
                .begin_mod_chain(self.ct_scroll_thumb_horz)
                .opacity_animated_from_template(
                    SCROLL_THUMB_DEACTIVATE_OPACITY_ANIM,
                    // ちょっと遅らせる
                    context.current_sec + SCROLL_FADEOUT_DELAY_SECS,
                )
                .apply();

            let offset_x = self.content_offset.x.get();
            let new_offset_x =
                (offset_x - args.amount * SCROLL_AMOUNT_MULTIPLIER).clamp(0.0, max_overflow);
            let left_amount = args.amount - (new_offset_x - offset_x);
            self.content_offset.x.set(new_offset_x);
            // HitTestTreeの更新が必要なので入力イベント処理完了後に遅延させる
            context
                .system_link
                .dispatch_event(Event::UpdateView { id: self.view_id });

            ScrollWheelActionResponse {
                continue_flags: EventContinueControl::STOP_PROPAGATION,
                left_amount,
            }
        } else {
            let content_h = self.content_size.height.get();
            let viewport_h = self.viewport_size.height.get();
            let max_overflow = content_h - viewport_h;
            if max_overflow <= 0.0 {
                // nothing to be scrolled
                return ScrollWheelActionResponse {
                    continue_flags: EventContinueControl::STOP_PROPAGATION,
                    left_amount: args.amount,
                };
            }

            context
                .composite_tree
                .begin_mod_chain(self.ct_scroll_thumb_vert)
                .opacity_animated_from_template(
                    SCROLL_THUMB_DEACTIVATE_OPACITY_ANIM,
                    // ちょっと遅らせる
                    context.current_sec + SCROLL_FADEOUT_DELAY_SECS,
                )
                .apply();

            let offset_y = self.content_offset.y.get();
            let new_offset_y =
                (offset_y - args.amount * SCROLL_AMOUNT_MULTIPLIER).clamp(0.0, max_overflow);
            let left_amount = args.amount - (new_offset_y - offset_y);
            self.content_offset.y.set(new_offset_y);
            // HitTestTreeの更新が必要なので入力イベント処理完了後に遅延させる
            context
                .system_link
                .dispatch_event(Event::UpdateView { id: self.view_id });

            ScrollWheelActionResponse {
                continue_flags: EventContinueControl::STOP_PROPAGATION,
                left_amount,
            }
        }
    }
}
impl ViewEventHandler for ScrollContainerEventHandler {
    fn update(&self, context: &mut ViewUpdateContext) {
        let offset_x = self.content_offset.x.get();
        let offset_y = self.content_offset.y.get();

        context
            .composite_tree
            .begin_mod_chain(self.ct_content_root)
            .offset_imm(-offset_x, -offset_y)
            .apply();
        context.ht_manager.get_data_mut(self.ht_content_root).left = -offset_x;
        context.ht_manager.get_data_mut(self.ht_content_root).top = -offset_y;
        self.update_thumb_position(
            context.mount_context.composite_tree,
            context.mount_context.ht_manager,
        );
    }
}
impl ScrollContainerEventHandler {
    fn recompute_scroll_bars(&self, ctx: &mut RenderContext) {
        let should_scroll_vert = self.viewport_size.height.get() < self.content_size.height.get();
        let should_scroll_horz = self.viewport_size.width.get() < self.content_size.width.get();

        if self.should_scroll_vert.replace(should_scroll_vert) != should_scroll_vert {
            if should_scroll_vert {
                ctx.composite_tree
                    .add_child(self.ct_root, self.ct_scroll_bar_vert);
                ctx.composite_tree
                    .add_child(self.ct_root, self.ct_scroll_thumb_vert);
                ctx.ht_manager
                    .add_child(self.ht_root, self.ht_scroll_bar_vert);
                ctx.ht_manager
                    .add_child(self.ht_root, self.ht_scroll_thumb_vert);
            } else {
                ctx.composite_tree.remove_child(self.ct_scroll_bar_vert);
                ctx.composite_tree.remove_child(self.ct_scroll_thumb_vert);
                ctx.ht_manager.remove_child(self.ht_scroll_bar_vert);
                ctx.ht_manager.remove_child(self.ht_scroll_thumb_vert);
            }
        }
        if self.should_scroll_horz.replace(should_scroll_horz) != should_scroll_horz {
            if should_scroll_horz {
                ctx.composite_tree
                    .add_child(self.ct_root, self.ct_scroll_bar_horz);
                ctx.composite_tree
                    .add_child(self.ct_root, self.ct_scroll_thumb_horz);
                ctx.ht_manager
                    .add_child(self.ht_root, self.ht_scroll_bar_horz);
                ctx.ht_manager
                    .add_child(self.ht_root, self.ht_scroll_thumb_horz);
            } else {
                ctx.composite_tree.remove_child(self.ct_scroll_bar_horz);
                ctx.composite_tree.remove_child(self.ct_scroll_thumb_horz);
                ctx.ht_manager.remove_child(self.ht_scroll_bar_horz);
                ctx.ht_manager.remove_child(self.ht_scroll_thumb_horz);
            }
        }

        self.content_offset.x.update(|x| {
            x.clamp(
                0.0,
                (self.content_size.width.get() - self.viewport_size.width.get()).max(0.0),
            )
        });
        self.content_offset.y.update(|x| {
            x.clamp(
                0.0,
                (self.content_size.height.get() - self.viewport_size.height.get()).max(0.0),
            )
        });
        let offset_x = self.content_offset.x.get();
        let offset_y = self.content_offset.y.get();
        ctx.composite_tree
            .begin_mod_chain(self.ct_content_root)
            .offset_imm(-offset_x, -offset_y)
            .apply();
        ctx.ht_manager.get_data_mut(self.ht_content_root).left = -offset_x;
        ctx.ht_manager.get_data_mut(self.ht_content_root).top = -offset_y;

        self.update_thumb_position(ctx.composite_tree, ctx.ht_manager);
    }

    fn update_thumb_position<E>(
        &self,
        composite_tree: &mut CompositeTree<E>,
        ht_manager: &mut HitTestTreeManager,
    ) {
        let h_vp = self.viewport_size.height.get();
        let h_content = self.content_size.height.get();
        let h_offset = self.content_offset.y.get();

        let thumb_real_y = h_offset * h_vp / h_content;
        let thumb_real_h = h_vp * h_vp / h_content;
        composite_tree
            .begin_mod_chain(self.ct_scroll_thumb_vert)
            .y_imm(thumb_real_y + SCROLL_THUMB_SPACING)
            .height_imm(thumb_real_h - SCROLL_THUMB_SPACING * 2.0)
            .apply();
        ht_manager.get_data_mut(self.ht_scroll_thumb_vert).top = thumb_real_y;
        ht_manager.get_data_mut(self.ht_scroll_thumb_vert).height = thumb_real_h;

        let w_vp = self.viewport_size.width.get();
        let w_content = self.content_size.width.get();
        let w_offset = self.content_offset.x.get();

        let thumb_real_x = w_offset * w_vp / w_content;
        let thumb_real_w = w_vp * w_vp / w_content;
        composite_tree
            .begin_mod_chain(self.ct_scroll_thumb_horz)
            .x_imm(thumb_real_x + SCROLL_THUMB_SPACING)
            .width_imm(thumb_real_w - SCROLL_THUMB_SPACING * 2.0)
            .apply();
        ht_manager.get_data_mut(self.ht_scroll_thumb_horz).left = thumb_real_x;
        ht_manager.get_data_mut(self.ht_scroll_thumb_horz).width = thumb_real_w;
    }

    fn activate_bar<E>(&self, composite_tree: &mut CompositeTree<E>, current_sec: f32) {
        if self.bar_active.replace(true) {
            // already activated
            return;
        }

        composite_tree
            .begin_mod_chain(self.ct_scroll_bar_vert)
            .opacity_animated_from_template(SCROLL_BAR_ACTIVATE_OPACITY_ANIM, current_sec)
            .apply();
        composite_tree
            .begin_mod_chain(self.ct_scroll_thumb_vert)
            .width_animated_from_template(SCROLL_THUMB_ACTIVATE_THICKNESS_ANIM, current_sec)
            .x_animated_from_template(SCROLL_THUMB_ACTIVATE_OFFSET_ANIM, current_sec)
            .composite_mode(CompositeMode::FillColor(AnimatableColor::Value(
                ACTIVE_THUMB_COLOR,
            )))
            .opacity_imm(1.0)
            .corner_radius(CornerRadius::all(
                (ACTIVE_SCROLL_BAR_THICKNESS - SCROLL_THUMB_SPACING * 2.0) * 0.5,
            ))
            .apply();
    }

    fn deactivate_bar<E>(&self, composite_tree: &mut CompositeTree<E>, current_sec: f32) {
        if !self.bar_active.replace(false) {
            // already deactivated
            return;
        }

        composite_tree
            .begin_mod_chain(self.ct_scroll_bar_vert)
            .opacity_animated_from_template(SCROLL_BAR_DEACTIVATE_OPACITY_ANIM, current_sec)
            .apply();
        composite_tree
            .begin_mod_chain(self.ct_scroll_thumb_vert)
            .width_animated_from_template(SCROLL_THUMB_DEACTIVATE_THICKNESS_ANIM, current_sec)
            .x_animated_from_template(SCROLL_THUMB_DEACTIVATE_OFFSET_ANIM, current_sec)
            .opacity_animated_from_template(
                SCROLL_THUMB_DEACTIVATE_OPACITY_ANIM,
                // ちょっと遅らせる
                current_sec + SCROLL_FADEOUT_DELAY_SECS,
            )
            .composite_mode(CompositeMode::FillColor(AnimatableColor::Value(
                INACTIVE_THUMB_COLOR,
            )))
            .corner_radius(CornerRadius::all(DEFAULT_SCROLL_BAR_THICKNESS * 0.5))
            .apply();
    }

    fn activate_bar_horz<E>(&self, composite_tree: &mut CompositeTree<E>, current_sec: f32) {
        if self.bar_active_horz.replace(true) {
            // already activated
            return;
        }

        composite_tree
            .begin_mod_chain(self.ct_scroll_bar_horz)
            .opacity_animated_from_template(SCROLL_BAR_ACTIVATE_OPACITY_ANIM, current_sec)
            .apply();
        composite_tree
            .begin_mod_chain(self.ct_scroll_thumb_horz)
            .height_animated_from_template(SCROLL_THUMB_ACTIVATE_THICKNESS_ANIM, current_sec)
            .y_animated_from_template(SCROLL_THUMB_ACTIVATE_OFFSET_ANIM, current_sec)
            .composite_mode(CompositeMode::FillColor(AnimatableColor::Value(
                ACTIVE_THUMB_COLOR,
            )))
            .opacity_imm(1.0)
            .corner_radius(CornerRadius::all(
                (ACTIVE_SCROLL_BAR_THICKNESS - SCROLL_THUMB_SPACING * 2.0) * 0.5,
            ))
            .apply();
    }

    fn deactivate_bar_horz<E>(&self, composite_tree: &mut CompositeTree<E>, current_sec: f32) {
        if !self.bar_active_horz.replace(false) {
            // already deactivated
            return;
        }

        composite_tree
            .begin_mod_chain(self.ct_scroll_bar_horz)
            .opacity_animated_from_template(SCROLL_BAR_DEACTIVATE_OPACITY_ANIM, current_sec)
            .apply();
        composite_tree
            .begin_mod_chain(self.ct_scroll_thumb_horz)
            .height_animated_from_template(SCROLL_THUMB_DEACTIVATE_THICKNESS_ANIM, current_sec)
            .y_animated_from_template(SCROLL_THUMB_DEACTIVATE_OFFSET_ANIM, current_sec)
            .opacity_animated_from_template(
                SCROLL_THUMB_DEACTIVATE_OPACITY_ANIM,
                // ちょっと遅らせる
                current_sec + SCROLL_FADEOUT_DELAY_SECS,
            )
            .composite_mode(CompositeMode::FillColor(AnimatableColor::Value(
                INACTIVE_THUMB_COLOR,
            )))
            .corner_radius(CornerRadius::all(DEFAULT_SCROLL_BAR_THICKNESS * 0.5))
            .apply();
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ScrollContainerPointerGrabState {
    None,
    BarVert,
    ThumbVert { base_offset_y: f32 },
    BarHorz,
    ThumbHorz { base_offset_x: f32 },
}
