use core::cell::Cell;
use std::rc::Rc;

use crate::{
    input::{
        EventContinueControl, InputEventContext,
        hittest::{
            CursorShape, HitTestTreeActionHandler, HitTestTreeData, HitTestTreeRef,
            PointerActionArgs, PointerButtonActionArgs,
        },
    },
    rendering::composite::{
        AnimatableColor, AnimatableFloat, AnimationCurve, Border, CompositeMode, CompositeRect,
        CompositeRectScaleFactor, CompositeTree, CompositeTreeRef, CornerRadius,
    },
    uikit::{
        RenderChildScheduler, RenderContext, TeardownContext, View, ViewElementSize,
        ViewIdentifier, ViewInstanceQueryableMut, ViewNewRenderElements, ViewPlacement,
    },
    utils::{LogicalUnit, Point, Rect, Size},
};

pub struct RadioButtonView {
    id: ViewIdentifier,
    eh: Option<Rc<RadioButtonEventHandler>>,
    placement: ViewPlacement,
    selected_changes: Option<bool>,
}
impl RadioButtonView {
    pub fn new(id: ViewIdentifier, placement: ViewPlacement) -> Self {
        Self {
            id,
            eh: None,
            placement,
            selected_changes: None,
        }
    }
}
impl View for RadioButtonView {
    fn render(
        &mut self,
        layout_rect: Rect<LogicalUnit>,
        ctx: &mut RenderContext,
        _sched: &mut RenderChildScheduler,
    ) -> ViewNewRenderElements {
        match self.eh {
            Some(ref eh) => {
                if let Some(selected) = self.selected_changes.take() {
                    if eh.current.replace(selected) != selected {
                        // changed
                        eh.update_mark(ctx.composite_tree, ctx.current_sec);
                    }
                }

                ViewNewRenderElements::EMPTY
            }
            None => {
                // first render
                let size = match self.placement.size {
                    ViewElementSize::Fixed(s) => s,
                    // preferred default
                    ViewElementSize::Automatic => Size::new_logical(16.0, 16.0),
                };
                let offset = Point::new_logical(
                    self.placement.location.offset.x
                        - size.width * self.placement.location.anchor[0],
                    self.placement.location.offset.y
                        - size.height * self.placement.location.anchor[1],
                );

                let ct_root = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    offset: [
                        AnimatableFloat::Value(offset.x),
                        AnimatableFloat::Value(offset.y),
                    ],
                    relative_offset_adjustment: self.placement.location.parent_anchor,
                    size: [
                        AnimatableFloat::Value(size.width),
                        AnimatableFloat::Value(size.height),
                    ],
                    has_bitmap: true,
                    composite_mode: CompositeMode::FillColor(AnimatableColor::Value([
                        1.0, 1.0, 1.0, 0.0,
                    ])),
                    border: Some(Border {
                        thickness: 0.5,
                        color: AnimatableColor::Value([1.0, 1.0, 1.0, 0.5]),
                        ..Default::default()
                    }),
                    corner_radius: CornerRadius::all(8.0),
                    ..Default::default()
                });
                let ct_mark = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    offset: [AnimatableFloat::Value(4.0), AnimatableFloat::Value(4.0)],
                    size: [
                        AnimatableFloat::Value(size.width - 8.0),
                        AnimatableFloat::Value(size.height - 8.0),
                    ],
                    has_bitmap: true,
                    composite_mode: CompositeMode::FillColor(AnimatableColor::Value([
                        1.0, 1.0, 1.0, 1.0,
                    ])),
                    corner_radius: CornerRadius::all(4.0),
                    opacity: AnimatableFloat::Value(0.0),
                    ..Default::default()
                });
                let ht_root = ctx.ht_manager.create(HitTestTreeData {
                    left: offset.x,
                    top: offset.y,
                    left_adjustment_factor: self.placement.location.parent_anchor[0],
                    top_adjustment_factor: self.placement.location.parent_anchor[1],
                    width: size.width,
                    height: size.height,
                    cursor_shape: CursorShape::Pointer,
                    ..Default::default()
                });

                ctx.composite_tree.add_child(ct_root, ct_mark);

                let eh = Rc::new(RadioButtonEventHandler {
                    view_id: self.id,
                    ct_root,
                    ct_mark,
                    ht_root,
                    current: Cell::new(false),
                });
                ctx.ht_manager.set_action_handler(ht_root, &eh);

                if let Some(selected) = self.selected_changes.take() {
                    if eh.current.replace(selected) != selected {
                        // changed
                        eh.update_mark(ctx.composite_tree, ctx.current_sec);
                    }
                }

                self.eh = Some(eh);
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

struct RadioButtonEventHandler {
    view_id: ViewIdentifier,
    ct_root: CompositeTreeRef,
    ct_mark: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    current: Cell<bool>,
}
impl HitTestTreeActionHandler for RadioButtonEventHandler {
    fn on_pointer_enter(
        &self,
        _sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerActionArgs,
    ) -> EventContinueControl {
        context
            .composite_tree
            .get_mut(self.ct_root)
            .border
            .as_mut()
            .expect("no border?")
            .color = AnimatableColor::Animated {
            from_value: [1.0, 1.0, 1.0, 0.5],
            to_value: [1.0, 1.0, 1.0, 1.0],
            sec_duration: (context.current_sec..context.current_sec + 0.1).into(),
            curve: AnimationCurve::Linear,
            event_on_complete: None,
        };
        context.composite_tree.mark_dirty(self.ct_root);

        EventContinueControl::STOP_PROPAGATION
    }

    fn on_pointer_leave(
        &self,
        _sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerActionArgs,
    ) -> EventContinueControl {
        context
            .composite_tree
            .get_mut(self.ct_root)
            .border
            .as_mut()
            .expect("no border?")
            .color = AnimatableColor::Animated {
            from_value: [1.0, 1.0, 1.0, 1.0],
            to_value: [1.0, 1.0, 1.0, 0.5],
            sec_duration: (context.current_sec..context.current_sec + 0.1).into(),
            curve: AnimationCurve::Linear,
            event_on_complete: None,
        };
        context.composite_tree.mark_dirty(self.ct_root);

        EventContinueControl::STOP_PROPAGATION
    }

    fn on_click(
        &self,
        _sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        // 自分自身をtrueにする(ViewGroupに属していないViewの場合これをしないとONにならない)
        context
            .view_instance_mut::<RadioButtonView>(self.view_id)
            .expect("query failed")
            .selected_changes = Some(true);
        context.view_render_queue.schedule(self.view_id);

        // 他をOFFに
        let other_participants = context
            .view_iter_self_group_parcitipants(self.view_id)
            .filter(|&x| x != self.view_id)
            .collect::<Vec<_>>();
        for x in other_participants {
            if let Some(inst) = context.view_instance_mut::<RadioButtonView>(x) {
                inst.selected_changes = Some(false);
                context.view_render_queue.schedule(x);
            }
        }

        EventContinueControl::STOP_PROPAGATION
    }
}
impl RadioButtonEventHandler {
    fn update_mark<E>(&self, composite_tree: &mut CompositeTree<E>, current_sec: f32) {
        if self.current.get() {
            composite_tree.get_mut(self.ct_mark).opacity = AnimatableFloat::Animated {
                from_value: 0.0,
                to_value: 1.0,
                sec_duration: (current_sec..current_sec + 0.1).into(),
                curve: AnimationCurve::Linear,
                event_on_complete: None,
            };
        } else {
            composite_tree.get_mut(self.ct_mark).opacity = AnimatableFloat::Animated {
                from_value: 1.0,
                to_value: 0.0,
                sec_duration: (current_sec..current_sec + 0.1).into(),
                curve: AnimationCurve::Linear,
                event_on_complete: None,
            };
        }
        composite_tree.mark_dirty(self.ct_mark);
    }
}
