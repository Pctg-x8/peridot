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
    uicore::{
        MeasureContext, RenderContext, TeardownContext, TypedViewIdentifier, View,
        ViewInstanceQueryableMut, ViewLayoutStateStore, ViewRenderElements, ViewRenderer,
    },
    utils::{LogicalUnit, Rect, Size},
};

pub struct RadioButtonView {
    id: TypedViewIdentifier<RadioButtonView>,
    eh: Option<Rc<RadioButtonEventHandler>>,
    selected_changes: Option<bool>,
}
impl RadioButtonView {
    pub fn new(id: TypedViewIdentifier<RadioButtonView>) -> Self {
        Self {
            id,
            eh: None,
            selected_changes: None,
        }
    }
}
impl View for RadioButtonView {
    fn render(
        &mut self,
        layout_rect: Rect<LogicalUnit>,
        ctx: &mut RenderContext,
        _layout_state: &ViewLayoutStateStore,
    ) -> ViewRenderElements {
        let e = match self.eh {
            Some(ref eh) => {
                ctx.composite_tree
                    .begin_mod_chain(eh.ct_root)
                    .offset_imm(layout_rect.left, layout_rect.top)
                    .size_imm(layout_rect.width, layout_rect.height)
                    .apply();
                ctx.ht_manager.get_data_mut(eh.ht_root).left = layout_rect.left;
                ctx.ht_manager.get_data_mut(eh.ht_root).top = layout_rect.top;
                ctx.ht_manager.get_data_mut(eh.ht_root).width = layout_rect.width;
                ctx.ht_manager.get_data_mut(eh.ht_root).height = layout_rect.height;

                if let Some(selected) = self.selected_changes.take() {
                    if eh.current.replace(selected) != selected {
                        // changed
                        eh.update_mark(ctx.composite_tree, ctx.current_sec);
                    }
                }

                eh
            }
            None => {
                // first render
                let ct_root = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    offset: [
                        AnimatableFloat::Value(layout_rect.left),
                        AnimatableFloat::Value(layout_rect.top),
                    ],
                    size: [
                        AnimatableFloat::Value(layout_rect.width),
                        AnimatableFloat::Value(layout_rect.height),
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
                        AnimatableFloat::Value(layout_rect.width - 8.0),
                        AnimatableFloat::Value(layout_rect.height - 8.0),
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
                    left: layout_rect.left,
                    top: layout_rect.top,
                    width: layout_rect.width,
                    height: layout_rect.height,
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

                &*self.eh.insert(eh)
            }
        };

        ViewRenderElements {
            composite_tree: Some(e.ct_root),
            hit_tree: Some(e.ht_root),
            ..ViewRenderElements::EMPTY
        }
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        let Some(entity) = self.eh.take() else {
            // not rendered
            return;
        };

        ctx.composite_tree.free_all(entity.ct_root);
        ctx.ht_manager.free_all(entity.ht_root);
    }

    fn measure_preferred_content_size(&self, _ctx: &mut MeasureContext) -> Size<LogicalUnit> {
        Size::new_logical(16.0, 16.0)
    }
}

struct RadioButtonEventHandler {
    view_id: TypedViewIdentifier<RadioButtonView>,
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
            .view_instance_mut(self.view_id)
            .expect("query failed")
            .selected_changes = Some(true);
        context.schedule_view_render(self.view_id);

        // 他をOFFに
        let other_participants = context
            .view_iter_self_group_parcitipants(self.view_id.into_untyped())
            .filter(|&x| x != self.view_id)
            .collect::<Vec<_>>();
        for x in other_participants {
            if let Some(inst) = context.view_instance_mut_of::<RadioButtonView>(x) {
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
