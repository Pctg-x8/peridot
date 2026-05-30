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
        AnimatableColor, AnimatableFloat, AnimationCurve, ClipConfig, CompositeMode, CompositeRect, CompositeRectScaleFactor, CompositeTree, CompositeTreeRef, CornerRadius
    },
    uikit::{
        MountContext, MountTarget, ViewEventHandler, ViewIdentifier, ViewInitContext,
        ViewUpdateContext,
    },
    utils::{InteriorMutableLogicalUnit, LogicalUnit, Point, Rect, SafeF32, Size},
};

const DEFAULT_SCROLL_BAR_THICKNESS: f32 = 4.0;
const ACTIVE_SCROLL_BAR_THICKNESS: f32 = 8.0;
const SCROLL_THUMB_SPACING: f32 = 1.0;
const SCROLL_FADEOUT_DELAY_SECS: f32 = 0.625;
const SCROLL_FADEOUT_DURATION_SECS: f32 = 0.375;
const SCROLL_AMOUNT_MULTIPLIER: f32 = 24.0;

pub struct ScrollContainer {
    ct_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    eh: Rc<ScrollContainerEventHandler>,
}
impl ScrollContainer {
    pub fn new(ctx: &mut ViewInitContext, rect: Rect<LogicalUnit>) -> Self {
        let ct_root = ctx.mount_context.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            offset: [
                AnimatableFloat::Value(rect.left),
                AnimatableFloat::Value(rect.top),
            ],
            size: [
                AnimatableFloat::Value(rect.width),
                AnimatableFloat::Value(rect.height),
            ],
            has_bitmap: true,
            composite_mode: CompositeMode::FillColor(AnimatableColor::Value([
                1.0, 1.0, 1.0, 0.0625,
            ])),
            clip_child: Some(ClipConfig {
                left_softness: SafeF32::ZERO,
                top_softness: SafeF32::ZERO,
                right_softness: SafeF32::ZERO,
                bottom_softness: SafeF32::ZERO,
            }),
            ..Default::default()
        });
        let ht_root = ctx.ht_manager.create(HitTestTreeData {
            left: rect.left,
            top: rect.top,
            width: rect.width,
            height: rect.height,
            clip_children: true,
            ..Default::default()
        });
        let ct_content_root = ctx.mount_context.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            offset: [AnimatableFloat::Value(0.0), AnimatableFloat::Value(0.0)],
            size: [AnimatableFloat::Value(0.0), AnimatableFloat::Value(0.0)],
            ..Default::default()
        });
        let ht_content_root = ctx.ht_manager.create(HitTestTreeData {
            left: 0.0,
            top: 0.0,
            width: rect.width,
            height: rect.height,
            ..Default::default()
        });

        let ct_scroll_bar_vert = ctx.mount_context.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            offset: [
                AnimatableFloat::Value(-ACTIVE_SCROLL_BAR_THICKNESS),
                AnimatableFloat::Value(0.0),
            ],
            relative_offset_adjustment: [1.0, 0.0],
            size: [
                AnimatableFloat::Value(ACTIVE_SCROLL_BAR_THICKNESS),
                AnimatableFloat::Value(rect.height),
            ],
            has_bitmap: true,
            composite_mode: CompositeMode::FillColor(AnimatableColor::Value([
                0.75, 0.75, 0.75, 0.5,
            ])),
            opacity: AnimatableFloat::Value(0.0),
            ..Default::default()
        });
        let ct_scroll_thumb_vert = ctx.mount_context.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            offset: [
                AnimatableFloat::Value(-SCROLL_THUMB_SPACING - DEFAULT_SCROLL_BAR_THICKNESS),
                AnimatableFloat::Value(SCROLL_THUMB_SPACING),
            ],
            relative_offset_adjustment: [1.0, 0.0],
            size: [
                AnimatableFloat::Value(DEFAULT_SCROLL_BAR_THICKNESS),
                AnimatableFloat::Value(rect.height - SCROLL_THUMB_SPACING * 2.0),
            ],
            has_bitmap: true,
            composite_mode: CompositeMode::FillColor(AnimatableColor::Value([0.0, 0.0, 0.0, 0.5])),
            corner_radius: CornerRadius::all(DEFAULT_SCROLL_BAR_THICKNESS * 0.5),
            opacity: AnimatableFloat::Value(0.0),
            ..Default::default()
        });
        let ht_scroll_bar_vert = ctx.ht_manager.create(HitTestTreeData {
            left: -ACTIVE_SCROLL_BAR_THICKNESS,
            top: 0.0,
            left_adjustment_factor: 1.0,
            width: ACTIVE_SCROLL_BAR_THICKNESS,
            height: rect.height,
            ..Default::default()
        });
        let ht_scroll_thumb_vert = ctx.ht_manager.create(HitTestTreeData {
            left: -ACTIVE_SCROLL_BAR_THICKNESS,
            top: 0.0,
            left_adjustment_factor: 1.0,
            width: ACTIVE_SCROLL_BAR_THICKNESS,
            height: rect.height,
            ..Default::default()
        });

        let ct_scroll_bar_horz = ctx.mount_context.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            offset: [
                AnimatableFloat::Value(0.0),
                AnimatableFloat::Value(-ACTIVE_SCROLL_BAR_THICKNESS),
            ],
            relative_offset_adjustment: [0.0, 1.0],
            size: [
                AnimatableFloat::Value(rect.height),
                AnimatableFloat::Value(ACTIVE_SCROLL_BAR_THICKNESS),
            ],
            has_bitmap: true,
            composite_mode: CompositeMode::FillColor(AnimatableColor::Value([
                0.75, 0.75, 0.75, 0.5,
            ])),
            opacity: AnimatableFloat::Value(0.0),
            ..Default::default()
        });
        let ct_scroll_thumb_horz = ctx.mount_context.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            offset: [
                AnimatableFloat::Value(SCROLL_THUMB_SPACING),
                AnimatableFloat::Value(-SCROLL_THUMB_SPACING - DEFAULT_SCROLL_BAR_THICKNESS),
            ],
            relative_offset_adjustment: [0.0, 1.0],
            size: [
                AnimatableFloat::Value(rect.height - SCROLL_THUMB_SPACING * 2.0),
                AnimatableFloat::Value(DEFAULT_SCROLL_BAR_THICKNESS),
            ],
            has_bitmap: true,
            composite_mode: CompositeMode::FillColor(AnimatableColor::Value([0.0, 0.0, 0.0, 0.5])),
            corner_radius: CornerRadius::all(DEFAULT_SCROLL_BAR_THICKNESS * 0.5),
            opacity: AnimatableFloat::Value(0.0),
            ..Default::default()
        });
        let ht_scroll_bar_horz = ctx.ht_manager.create(HitTestTreeData {
            left: 0.0,
            top: -ACTIVE_SCROLL_BAR_THICKNESS,
            top_adjustment_factor: 1.0,
            width: rect.height,
            height: ACTIVE_SCROLL_BAR_THICKNESS,
            ..Default::default()
        });
        let ht_scroll_thumb_horz = ctx.ht_manager.create(HitTestTreeData {
            left: 0.0,
            top: -ACTIVE_SCROLL_BAR_THICKNESS,
            top_adjustment_factor: 1.0,
            width: rect.height,
            height: ACTIVE_SCROLL_BAR_THICKNESS,
            ..Default::default()
        });

        ctx.composite_tree.add_child(ct_root, ct_content_root);
        ctx.ht_manager.add_child(ht_root, ht_content_root);

        let view_id = ctx.view_registry.alloc();
        let eh = Rc::new(ScrollContainerEventHandler {
            view_id,
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
            viewport_size: Size::new_logical_interior_mutable(rect.width, rect.height),
            content_size: Size::new_logical_interior_mutable(0.0, 0.0),
            content_offset: Point::new_logical_interior_mutable(0.0, 0.0),
            pointer_grab_state: core::cell::Cell::new(ScrollContainerPointerGrabState::None),
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
        ctx.view_registry.set_event_handler(view_id, &eh);

        Self {
            ct_root,
            ht_root,
            eh,
        }
    }

    pub fn mount(&self, ctx: &mut MountContext, target: &(impl MountTarget + ?Sized)) {
        ctx.composite_tree.add_child(target.ct_root(), self.ct_root);
        ctx.ht_manager.add_child(target.ht_root(), self.ht_root);
    }

    pub fn set_content_size<E>(
        &self,
        size: Size<LogicalUnit>,
        composite_tree: &mut CompositeTree<E>,
        ht_manager: &mut HitTestTreeManager,
    ) {
        self.eh.content_size.width.set(size.width);
        self.eh.content_size.height.set(size.height);

        // mount/dismount scroll bars only if needed
        let should_scroll_vert = self.eh.viewport_size.height.get() < size.height;
        let should_scroll_horz = self.eh.viewport_size.width.get() < size.width;
        if self.eh.should_scroll_vert.replace(should_scroll_vert) != should_scroll_vert {
            if should_scroll_vert {
                composite_tree.add_child(self.ct_root, self.eh.ct_scroll_bar_vert);
                composite_tree.add_child(self.ct_root, self.eh.ct_scroll_thumb_vert);
                ht_manager.add_child(self.ht_root, self.eh.ht_scroll_bar_vert);
                ht_manager.add_child(self.ht_root, self.eh.ht_scroll_thumb_vert);
            } else {
                composite_tree.remove_child(self.eh.ct_scroll_bar_vert);
                composite_tree.remove_child(self.eh.ct_scroll_thumb_vert);
                ht_manager.remove_child(self.eh.ht_scroll_bar_vert);
                ht_manager.remove_child(self.eh.ht_scroll_thumb_vert);
            }
        }
        if self.eh.should_scroll_horz.replace(should_scroll_horz) != should_scroll_horz {
            if should_scroll_horz {
                composite_tree.add_child(self.ct_root, self.eh.ct_scroll_bar_horz);
                composite_tree.add_child(self.ct_root, self.eh.ct_scroll_thumb_horz);
                ht_manager.add_child(self.ht_root, self.eh.ht_scroll_bar_horz);
                ht_manager.add_child(self.ht_root, self.eh.ht_scroll_thumb_horz);
            } else {
                composite_tree.remove_child(self.eh.ct_scroll_bar_horz);
                composite_tree.remove_child(self.eh.ct_scroll_thumb_horz);
                ht_manager.remove_child(self.eh.ht_scroll_bar_horz);
                ht_manager.remove_child(self.eh.ht_scroll_thumb_horz);
            }
        }

        self.eh.update_thumb_position(composite_tree, ht_manager);
    }
}
impl MountTarget for ScrollContainer {
    #[inline(always)]
    fn ct_root(&self) -> CompositeTreeRef {
        self.eh.ct_content_root
    }

    #[inline(always)]
    fn ht_root(&self) -> HitTestTreeRef {
        self.eh.ht_content_root
    }
}
struct ScrollContainerEventHandler {
    view_id: ViewIdentifier,
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
            context
                .composite_tree
                .get_mut(self.ct_scroll_thumb_vert)
                .composite_mode =
                CompositeMode::FillColor(AnimatableColor::Value([0.0, 0.0, 0.0, 0.75]));
            context.composite_tree.mark_dirty(self.ct_scroll_thumb_vert);

            return EventContinueControl::STOP_PROPAGATION;
        }

        if sender == self.ht_scroll_bar_horz {
            self.deactivate_bar_horz(context.composite_tree, context.current_sec);
            return EventContinueControl::STOP_PROPAGATION;
        }
        if sender == self.ht_scroll_thumb_horz {
            self.deactivate_bar_horz(context.composite_tree, context.current_sec);
            context
                .composite_tree
                .get_mut(self.ct_scroll_thumb_horz)
                .composite_mode =
                CompositeMode::FillColor(AnimatableColor::Value([0.0, 0.0, 0.0, 0.75]));
            context.composite_tree.mark_dirty(self.ct_scroll_thumb_horz);

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
                .get_mut(self.ct_scroll_thumb_horz)
                .opacity = AnimatableFloat::Animated {
                // ちょっと遅らせる
                start_sec: context.current_sec + SCROLL_FADEOUT_DELAY_SECS,
                end_sec: context.current_sec
                    + SCROLL_FADEOUT_DELAY_SECS
                    + SCROLL_FADEOUT_DURATION_SECS,
                from_value: 1.0,
                to_value: 0.0,
                curve: AnimationCurve::Linear,
                event_on_complete: None,
            };
            context.composite_tree.mark_dirty(self.ct_scroll_thumb_horz);

            let offset_x = self.content_offset.x.get();
            let new_offset_x =
                (offset_x - args.amount * SCROLL_AMOUNT_MULTIPLIER).clamp(0.0, max_overflow);
            let left_amount = args.amount - (new_offset_x - offset_x);
            self.content_offset.x.set(new_offset_x);
            // HitTestTreeの更新が必要なので入力イベント処理完了後に遅延させる
            context
                .system_link
                .dispatch_event(Event::UpdateView { id: self.view_id });

            return ScrollWheelActionResponse {
                continue_flags: EventContinueControl::STOP_PROPAGATION,
                left_amount,
            };
        }

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
            .get_mut(self.ct_scroll_thumb_vert)
            .opacity = AnimatableFloat::Animated {
            // ちょっと遅らせる
            start_sec: context.current_sec + SCROLL_FADEOUT_DELAY_SECS,
            end_sec: context.current_sec + SCROLL_FADEOUT_DELAY_SECS + SCROLL_FADEOUT_DURATION_SECS,
            from_value: 1.0,
            to_value: 0.0,
            curve: AnimationCurve::Linear,
            event_on_complete: None,
        };
        context.composite_tree.mark_dirty(self.ct_scroll_thumb_vert);

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
impl ViewEventHandler for ScrollContainerEventHandler {
    fn update(&self, context: &mut ViewUpdateContext) {
        let offset_x = self.content_offset.x.get();
        let offset_y = self.content_offset.y.get();

        context.composite_tree.get_mut(self.ct_content_root).offset[0] =
            AnimatableFloat::Value(-offset_x);
        context.ht_manager.get_data_mut(self.ht_content_root).left = -offset_x;
        context.composite_tree.get_mut(self.ct_content_root).offset[1] =
            AnimatableFloat::Value(-offset_y);
        context.ht_manager.get_data_mut(self.ht_content_root).top = -offset_y;
        context.composite_tree.mark_dirty(self.ct_content_root);
        self.update_thumb_position(context.composite_tree, context.ht_manager);
    }
}
impl ScrollContainerEventHandler {
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
        composite_tree.get_mut(self.ct_scroll_thumb_vert).offset[1] =
            AnimatableFloat::Value(thumb_real_y + SCROLL_THUMB_SPACING);
        composite_tree.get_mut(self.ct_scroll_thumb_vert).size[1] =
            AnimatableFloat::Value(thumb_real_h - SCROLL_THUMB_SPACING * 2.0);
        ht_manager.get_data_mut(self.ht_scroll_thumb_vert).top = thumb_real_y;
        ht_manager.get_data_mut(self.ht_scroll_thumb_vert).height = thumb_real_h;
        composite_tree.mark_dirty(self.ct_scroll_thumb_vert);

        let w_vp = self.viewport_size.width.get();
        let w_content = self.content_size.width.get();
        let w_offset = self.content_offset.x.get();

        let thumb_real_x = w_offset * w_vp / w_content;
        let thumb_real_w = w_vp * w_vp / w_content;
        composite_tree.get_mut(self.ct_scroll_thumb_horz).offset[0] =
            AnimatableFloat::Value(thumb_real_x + SCROLL_THUMB_SPACING);
        composite_tree.get_mut(self.ct_scroll_thumb_horz).size[0] =
            AnimatableFloat::Value(thumb_real_w - SCROLL_THUMB_SPACING * 2.0);
        ht_manager.get_data_mut(self.ht_scroll_thumb_horz).left = thumb_real_x;
        ht_manager.get_data_mut(self.ht_scroll_thumb_horz).width = thumb_real_w;
        composite_tree.mark_dirty(self.ct_scroll_thumb_horz);
    }

    fn activate_bar<E>(&self, composite_tree: &mut CompositeTree<E>, current_sec: f32) {
        if self.bar_active.replace(true) {
            // already activated
            return;
        }

        composite_tree.get_mut(self.ct_scroll_bar_vert).opacity = AnimatableFloat::Animated {
            start_sec: current_sec,
            end_sec: current_sec + 0.1,
            from_value: 0.0,
            to_value: 1.0,
            curve: AnimationCurve::Linear,
            event_on_complete: None,
        };
        composite_tree.get_mut(self.ct_scroll_thumb_vert).size[0] = AnimatableFloat::Animated {
            start_sec: current_sec,
            end_sec: current_sec + 0.1,
            from_value: DEFAULT_SCROLL_BAR_THICKNESS,
            to_value: ACTIVE_SCROLL_BAR_THICKNESS - SCROLL_THUMB_SPACING * 2.0,
            curve: AnimationCurve::Linear,
            event_on_complete: None,
        };
        composite_tree.get_mut(self.ct_scroll_thumb_vert).offset[0] = AnimatableFloat::Animated {
            start_sec: current_sec,
            end_sec: current_sec + 0.1,
            from_value: -SCROLL_THUMB_SPACING - DEFAULT_SCROLL_BAR_THICKNESS,
            to_value: -SCROLL_THUMB_SPACING
                - (ACTIVE_SCROLL_BAR_THICKNESS - SCROLL_THUMB_SPACING * 2.0),
            curve: AnimationCurve::Linear,
            event_on_complete: None,
        };
        composite_tree
            .get_mut(self.ct_scroll_thumb_vert)
            .composite_mode =
            CompositeMode::FillColor(AnimatableColor::Value([0.0, 0.0, 0.0, 0.75]));
        composite_tree.get_mut(self.ct_scroll_thumb_vert).opacity = AnimatableFloat::Value(1.0);
        composite_tree
            .get_mut(self.ct_scroll_thumb_vert)
            .corner_radius =
            CornerRadius::all((ACTIVE_SCROLL_BAR_THICKNESS - SCROLL_THUMB_SPACING * 2.0) * 0.5);

        composite_tree.mark_dirty(self.ct_scroll_bar_vert);
        composite_tree.mark_dirty(self.ct_scroll_thumb_vert);
    }

    fn deactivate_bar<E>(&self, composite_tree: &mut CompositeTree<E>, current_sec: f32) {
        if !self.bar_active.replace(false) {
            // already deactivated
            return;
        }

        composite_tree.get_mut(self.ct_scroll_bar_vert).opacity = AnimatableFloat::Animated {
            start_sec: current_sec,
            end_sec: current_sec + 0.1,
            from_value: 1.0,
            to_value: 0.0,
            curve: AnimationCurve::Linear,
            event_on_complete: None,
        };
        composite_tree.get_mut(self.ct_scroll_thumb_vert).size[0] = AnimatableFloat::Animated {
            start_sec: current_sec,
            end_sec: current_sec + 0.1,
            from_value: ACTIVE_SCROLL_BAR_THICKNESS - SCROLL_THUMB_SPACING * 2.0,
            to_value: DEFAULT_SCROLL_BAR_THICKNESS,
            curve: AnimationCurve::Linear,
            event_on_complete: None,
        };
        composite_tree.get_mut(self.ct_scroll_thumb_vert).offset[0] = AnimatableFloat::Animated {
            start_sec: current_sec,
            end_sec: current_sec + 0.1,
            from_value: -SCROLL_THUMB_SPACING
                - (ACTIVE_SCROLL_BAR_THICKNESS - SCROLL_THUMB_SPACING * 2.0),
            to_value: -SCROLL_THUMB_SPACING - DEFAULT_SCROLL_BAR_THICKNESS,
            curve: AnimationCurve::Linear,
            event_on_complete: None,
        };
        composite_tree.get_mut(self.ct_scroll_thumb_vert).opacity = AnimatableFloat::Animated {
            // ちょっと遅らせる
            start_sec: current_sec + SCROLL_FADEOUT_DELAY_SECS,
            end_sec: current_sec + SCROLL_FADEOUT_DELAY_SECS + SCROLL_FADEOUT_DURATION_SECS,
            from_value: 1.0,
            to_value: 0.0,
            curve: AnimationCurve::Linear,
            event_on_complete: None,
        };
        composite_tree
            .get_mut(self.ct_scroll_thumb_vert)
            .composite_mode =
            CompositeMode::FillColor(AnimatableColor::Value([0.0, 0.0, 0.0, 0.5]));
        composite_tree
            .get_mut(self.ct_scroll_thumb_vert)
            .corner_radius = CornerRadius::all(DEFAULT_SCROLL_BAR_THICKNESS * 0.5);

        composite_tree.mark_dirty(self.ct_scroll_bar_vert);
        composite_tree.mark_dirty(self.ct_scroll_thumb_vert);
    }

    fn activate_bar_horz<E>(&self, composite_tree: &mut CompositeTree<E>, current_sec: f32) {
        if self.bar_active_horz.replace(true) {
            // already activated
            return;
        }

        composite_tree.get_mut(self.ct_scroll_bar_horz).opacity = AnimatableFloat::Animated {
            start_sec: current_sec,
            end_sec: current_sec + 0.1,
            from_value: 0.0,
            to_value: 1.0,
            curve: AnimationCurve::Linear,
            event_on_complete: None,
        };
        composite_tree.get_mut(self.ct_scroll_thumb_horz).size[1] = AnimatableFloat::Animated {
            start_sec: current_sec,
            end_sec: current_sec + 0.1,
            from_value: DEFAULT_SCROLL_BAR_THICKNESS,
            to_value: ACTIVE_SCROLL_BAR_THICKNESS - SCROLL_THUMB_SPACING * 2.0,
            curve: AnimationCurve::Linear,
            event_on_complete: None,
        };
        composite_tree.get_mut(self.ct_scroll_thumb_horz).offset[1] = AnimatableFloat::Animated {
            start_sec: current_sec,
            end_sec: current_sec + 0.1,
            from_value: -SCROLL_THUMB_SPACING - DEFAULT_SCROLL_BAR_THICKNESS,
            to_value: -SCROLL_THUMB_SPACING
                - (ACTIVE_SCROLL_BAR_THICKNESS - SCROLL_THUMB_SPACING * 2.0),
            curve: AnimationCurve::Linear,
            event_on_complete: None,
        };
        composite_tree
            .get_mut(self.ct_scroll_thumb_horz)
            .composite_mode =
            CompositeMode::FillColor(AnimatableColor::Value([0.0, 0.0, 0.0, 0.75]));
        composite_tree.get_mut(self.ct_scroll_thumb_horz).opacity = AnimatableFloat::Value(1.0);
        composite_tree
            .get_mut(self.ct_scroll_thumb_horz)
            .corner_radius =
            CornerRadius::all((ACTIVE_SCROLL_BAR_THICKNESS - SCROLL_THUMB_SPACING * 2.0) * 0.5);

        composite_tree.mark_dirty(self.ct_scroll_bar_horz);
        composite_tree.mark_dirty(self.ct_scroll_thumb_horz);
    }

    fn deactivate_bar_horz<E>(&self, composite_tree: &mut CompositeTree<E>, current_sec: f32) {
        if !self.bar_active_horz.replace(false) {
            // already deactivated
            return;
        }

        composite_tree.get_mut(self.ct_scroll_bar_horz).opacity = AnimatableFloat::Animated {
            start_sec: current_sec,
            end_sec: current_sec + 0.1,
            from_value: 1.0,
            to_value: 0.0,
            curve: AnimationCurve::Linear,
            event_on_complete: None,
        };
        composite_tree.get_mut(self.ct_scroll_thumb_horz).size[1] = AnimatableFloat::Animated {
            start_sec: current_sec,
            end_sec: current_sec + 0.1,
            from_value: ACTIVE_SCROLL_BAR_THICKNESS - SCROLL_THUMB_SPACING * 2.0,
            to_value: DEFAULT_SCROLL_BAR_THICKNESS,
            curve: AnimationCurve::Linear,
            event_on_complete: None,
        };
        composite_tree.get_mut(self.ct_scroll_thumb_horz).offset[1] = AnimatableFloat::Animated {
            start_sec: current_sec,
            end_sec: current_sec + 0.1,
            from_value: -SCROLL_THUMB_SPACING
                - (ACTIVE_SCROLL_BAR_THICKNESS - SCROLL_THUMB_SPACING * 2.0),
            to_value: -SCROLL_THUMB_SPACING - DEFAULT_SCROLL_BAR_THICKNESS,
            curve: AnimationCurve::Linear,
            event_on_complete: None,
        };
        composite_tree.get_mut(self.ct_scroll_thumb_horz).opacity = AnimatableFloat::Animated {
            // ちょっと遅らせる
            start_sec: current_sec + SCROLL_FADEOUT_DELAY_SECS,
            end_sec: current_sec + SCROLL_FADEOUT_DELAY_SECS + SCROLL_FADEOUT_DURATION_SECS,
            from_value: 1.0,
            to_value: 0.0,
            curve: AnimationCurve::Linear,
            event_on_complete: None,
        };
        composite_tree
            .get_mut(self.ct_scroll_thumb_horz)
            .composite_mode =
            CompositeMode::FillColor(AnimatableColor::Value([0.0, 0.0, 0.0, 0.5]));
        composite_tree
            .get_mut(self.ct_scroll_thumb_horz)
            .corner_radius = CornerRadius::all(DEFAULT_SCROLL_BAR_THICKNESS * 0.5);

        composite_tree.mark_dirty(self.ct_scroll_bar_horz);
        composite_tree.mark_dirty(self.ct_scroll_thumb_horz);
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
