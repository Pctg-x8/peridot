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
    rendering::{
        Normalized2DStaticMeshTexture, Normalized2DStaticMeshTextureLazyInit,
        composite::{
            AnimatableColor, AnimatableFloat, AnimationCurve, Border, CompositeMode, CompositeRect,
            CompositeRectScaleFactor, CompositeRectText, CompositeRectTextHorizontalAlignment,
            CompositeRectTextRun, CompositeRectTextVerticalAlignment, CompositeTexture,
            CompositeTreeRef, CornerRadius, FloatAnimationTemplate, TextureMappingMode,
            TextureType,
        },
        text::{FontID, TextLayout},
    },
    uikit::{
        RenderContext, TeardownContext, View, ViewElementSize, ViewLayoutStateStore, ViewPlacement,
        ViewRenderElements,
    },
    utils::{LogicalUnit, Rect, Size, range_helper::range_from_len},
};

const CHECKMARK_ACTIVATE_OPACITY_ANIM: FloatAnimationTemplate = FloatAnimationTemplate {
    from_value: 0.0,
    to_value: 1.0,
    curve: AnimationCurve::Linear,
    duration: 0.1,
};
const CHECKMARK_DEACTIVATE_OPACITY_ANIM: FloatAnimationTemplate =
    CHECKMARK_ACTIVATE_OPACITY_ANIM.flip(AnimationCurve::Linear);

const CHECKMARK_ACTIVATE_SCALE_ANIM: FloatAnimationTemplate = FloatAnimationTemplate {
    from_value: 1.5,
    to_value: 1.0,
    curve: AnimationCurve::EASE_IN,
    duration: 0.15,
};
const CHECKMARK_DEACTIVATE_SCALE_ANIM: FloatAnimationTemplate =
    CHECKMARK_ACTIVATE_SCALE_ANIM.flip(AnimationCurve::EASE_IN);

static SHARED_CHECK_ICON: Normalized2DStaticMeshTextureLazyInit =
    Normalized2DStaticMeshTextureLazyInit::new(Normalized2DStaticMeshTexture {
        width: 12.0,
        height: 12.0,
        vertices: &[
            [0.0, 0.4],
            [0.0, 0.6],
            [0.4, 0.7],
            [0.4, 0.9],
            [1.0, 0.1],
            [1.0, 0.3],
        ],
        indices: &[0, 1, 2, 1, 2, 3, 2, 3, 4, 3, 4, 5],
    });

pub struct ToggleButtonView {
    entity: Option<Rc<ToggleButtonEventHandler>>,
    label: String,
}
impl ToggleButtonView {
    pub fn new(label: String) -> Self {
        Self {
            entity: None,
            label,
        }
    }
}
impl View for ToggleButtonView {
    fn render(
        &mut self,
        layout_rect: Rect<LogicalUnit>,
        ctx: &mut RenderContext,
        _layout_state: &ViewLayoutStateStore,
    ) -> ViewRenderElements {
        let e = match self.entity {
            Some(ref e) => {
                ctx.composite_tree
                    .begin_mod_chain(e.ct_root)
                    .offset_imm(layout_rect.left, layout_rect.top)
                    .size_imm(layout_rect.width, layout_rect.height)
                    .apply();
                ctx.ht_manager.get_data_mut(e.ht_root).left = layout_rect.left;
                ctx.ht_manager.get_data_mut(e.ht_root).top = layout_rect.top;
                ctx.ht_manager.get_data_mut(e.ht_root).width = layout_rect.width;
                ctx.ht_manager.get_data_mut(e.ht_root).height = layout_rect.height;

                e
            }
            None => {
                // first render
                let check_icon = SHARED_CHECK_ICON.get(
                    ctx.main_thread_texture_id_issuer,
                    ctx.system_link.rt_sender(),
                );

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
                        thickness: 1.0,
                        color: AnimatableColor::Value([1.0; 4]),
                        ..Default::default()
                    }),
                    corner_radius: CornerRadius::all(8.0),
                    text: Some(CompositeRectText {
                        runs: vec![CompositeRectTextRun {
                            content: self.label.clone(),
                            font_id: FontID::UIDefault,
                            color: AnimatableColor::Value([1.0; 4]),
                            ..Default::default()
                        }],
                        vertical_alignment: CompositeRectTextVerticalAlignment::Middle,
                        offset: [20.0, 0.0],
                        ..Default::default()
                    }),
                    ..Default::default()
                });
                let ct_check = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    offset: [
                        AnimatableFloat::Value(6.0),
                        AnimatableFloat::Value(-SHARED_CHECK_ICON.height() * 0.5),
                    ],
                    relative_offset_adjustment: [0.0, 0.5],
                    size: [
                        AnimatableFloat::Value(SHARED_CHECK_ICON.width()),
                        AnimatableFloat::Value(SHARED_CHECK_ICON.height()),
                    ],
                    pivot: [0.5, 0.5],
                    has_bitmap: true,
                    composite_mode: CompositeMode::ColorTint(
                        AnimatableColor::Value([1.0; 4]),
                        CompositeTexture {
                            id: check_icon,
                            r#type: TextureType::Mask,
                            mapping: TextureMappingMode::Stretch,
                            slice_borders: [0.0; 4],
                        },
                    ),
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

                ctx.composite_tree.add_child(ct_root, ct_check);

                let eh = Rc::new(ToggleButtonEventHandler {
                    ct_root,
                    ct_check,
                    ht_root,
                    current: Cell::new(false),
                });
                ctx.ht_manager.set_action_handler(ht_root, &eh);

                &*self.entity.insert(eh)
            }
        };

        ViewRenderElements {
            composite_tree: Some(e.ct_root),
            hit_tree: Some(e.ht_root),
            ..ViewRenderElements::EMPTY
        }
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        let Some(e) = self.entity.take() else {
            // not rendered
            return;
        };

        ctx.mount_context.composite_tree.free_all(e.ct_root);
        ctx.mount_context.ht_manager.free_all(e.ht_root);
    }

    fn measure_preferred_content_size(&self, ctx: &mut super::MeasureContext) -> Size<LogicalUnit> {
        let label_size = TextLayout::new_single(
            &self.label,
            FontID::UIDefault,
            ctx.system_link.font_set(),
            CompositeRectTextHorizontalAlignment::Start,
            None,
        )
        .size();

        // space for checkmark / rounding padding
        Size::new_logical(24.0 + label_size.width + 4.0, label_size.height + 8.0)
    }
}

struct ToggleButtonEventHandler {
    ct_root: CompositeTreeRef,
    ct_check: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    current: Cell<bool>,
}
impl HitTestTreeActionHandler for ToggleButtonEventHandler {
    fn on_pointer_enter(
        &self,
        _sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerActionArgs,
    ) -> EventContinueControl {
        context
            .composite_tree
            .begin_mod_chain(self.ct_root)
            .composite_mode(CompositeMode::FillColor(AnimatableColor::Animated {
                from_value: [1.0, 1.0, 1.0, 0.0],
                to_value: [1.0, 1.0, 1.0, 0.25],
                sec_duration: range_from_len(context.current_sec, 0.1),
                curve: AnimationCurve::Linear,
                event_on_complete: None,
            }))
            .apply();

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
            .begin_mod_chain(self.ct_root)
            .composite_mode(CompositeMode::FillColor(AnimatableColor::Animated {
                from_value: [1.0, 1.0, 1.0, 0.25],
                to_value: [1.0, 1.0, 1.0, 0.0],
                sec_duration: range_from_len(context.current_sec, 0.1),
                curve: AnimationCurve::Linear,
                event_on_complete: None,
            }))
            .apply();

        EventContinueControl::STOP_PROPAGATION
    }

    fn on_click(
        &self,
        _sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        self.current.update(|x| !x);
        let active = self.current.get();

        context
            .composite_tree
            .begin_mod_chain(self.ct_check)
            .opacity_animated_from_template(
                if active {
                    &CHECKMARK_ACTIVATE_OPACITY_ANIM
                } else {
                    &CHECKMARK_DEACTIVATE_OPACITY_ANIM
                },
                context.current_sec,
            )
            .scale_animated_from_template(
                if active {
                    &CHECKMARK_ACTIVATE_SCALE_ANIM
                } else {
                    &CHECKMARK_DEACTIVATE_SCALE_ANIM
                },
                context.current_sec,
            )
            .apply();

        EventContinueControl::STOP_PROPAGATION
    }
}

pub struct CheckboxView {
    entity: Option<Rc<CheckboxEventHandler>>,
}
impl CheckboxView {
    pub fn new() -> Self {
        Self { entity: None }
    }
}
impl View for CheckboxView {
    fn render(
        &mut self,
        layout_rect: Rect<LogicalUnit>,
        ctx: &mut RenderContext,
        _layout_state: &ViewLayoutStateStore,
    ) -> ViewRenderElements {
        let e = match self.entity {
            Some(ref e) => {
                ctx.composite_tree
                    .begin_mod_chain(e.ct_root)
                    .offset_imm(layout_rect.left, layout_rect.top)
                    .size_imm(layout_rect.width, layout_rect.height)
                    .apply();
                ctx.ht_manager.get_data_mut(e.ht_root).left = layout_rect.left;
                ctx.ht_manager.get_data_mut(e.ht_root).top = layout_rect.top;
                ctx.ht_manager.get_data_mut(e.ht_root).width = layout_rect.width;
                ctx.ht_manager.get_data_mut(e.ht_root).height = layout_rect.height;

                e
            }
            None => {
                // first render
                let check_icon = SHARED_CHECK_ICON.get(
                    ctx.main_thread_texture_id_issuer,
                    ctx.system_link.rt_sender(),
                );

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
                    corner_radius: CornerRadius::all(2.0),
                    ..Default::default()
                });
                let ct_check = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    offset: [
                        AnimatableFloat::Value(-SHARED_CHECK_ICON.width() * 0.5),
                        AnimatableFloat::Value(-SHARED_CHECK_ICON.height() * 0.5),
                    ],
                    relative_offset_adjustment: [0.5, 0.5],
                    pivot: [0.5, 0.5],
                    size: [
                        AnimatableFloat::Value(SHARED_CHECK_ICON.width()),
                        AnimatableFloat::Value(SHARED_CHECK_ICON.height()),
                    ],
                    has_bitmap: true,
                    composite_mode: CompositeMode::ColorTint(
                        AnimatableColor::Value([1.0; 4]),
                        CompositeTexture {
                            id: check_icon,
                            r#type: TextureType::Mask,
                            mapping: TextureMappingMode::Stretch,
                            slice_borders: [0.0; 4],
                        },
                    ),
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

                ctx.composite_tree.add_child(ct_root, ct_check);

                let eh = Rc::new(CheckboxEventHandler {
                    ct_root,
                    ct_check,
                    ht_root,
                    current: Cell::new(false),
                });
                ctx.ht_manager.set_action_handler(ht_root, &eh);

                &*self.entity.insert(eh)
            }
        };

        ViewRenderElements {
            composite_tree: Some(e.ct_root),
            hit_tree: Some(e.ht_root),
            ..ViewRenderElements::EMPTY
        }
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        let Some(e) = self.entity.take() else {
            // not rendered
            return;
        };

        ctx.mount_context.composite_tree.free_all(e.ct_root);
        ctx.mount_context.ht_manager.free_all(e.ht_root);
    }

    fn measure_preferred_content_size(&self, ctx: &mut super::MeasureContext) -> Size<LogicalUnit> {
        Size::new_logical(16.0, 16.0)
    }
}

struct CheckboxEventHandler {
    ct_root: CompositeTreeRef,
    ct_check: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    current: Cell<bool>,
}
impl HitTestTreeActionHandler for CheckboxEventHandler {
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
        self.current.update(|x| !x);
        let active = self.current.get();

        context
            .composite_tree
            .begin_mod_chain(self.ct_check)
            .opacity_animated_from_template(
                if active {
                    &CHECKMARK_ACTIVATE_OPACITY_ANIM
                } else {
                    &CHECKMARK_DEACTIVATE_OPACITY_ANIM
                },
                context.current_sec,
            )
            .scale_animated_from_template(
                if active {
                    &CHECKMARK_ACTIVATE_SCALE_ANIM
                } else {
                    &CHECKMARK_DEACTIVATE_SCALE_ANIM
                },
                context.current_sec,
            )
            .apply();

        EventContinueControl::STOP_PROPAGATION
    }
}
