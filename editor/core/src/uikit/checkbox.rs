use core::cell::{Cell, OnceCell};
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
        MainThreadTextureIDIssuer, Normalized2DStaticMeshTexture, RenderMessage,
        RenderMessageSender, TextureID,
        composite::{
            AnimatableColor, AnimatableFloat, AnimationCurve, Border, CompositeMode, CompositeRect,
            CompositeRectScaleFactor, CompositeRectText, CompositeRectTextRun,
            CompositeRectTextVerticalAlignment, CompositeTexture, CompositeTreeRef, CornerRadius,
            FloatAnimationTemplate, TextureMappingMode, TextureType,
        },
        text::FontID,
    },
    uikit::{MountContext, MountTarget, ViewInitContext},
    utils::{LogicalUnit, Rect, UnsafeMainThreadOnlyOnceCell, range_helper::range_from_len},
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

struct SharedCheckIcon {
    check_icon: TextureID,
}
impl SharedCheckIcon {
    const CHECK_ICON: Normalized2DStaticMeshTexture = Normalized2DStaticMeshTexture {
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
    };

    fn new(
        mt_texid_issuer: &mut MainThreadTextureIDIssuer,
        rt_sender: &RenderMessageSender,
    ) -> Self {
        let check_icon = mt_texid_issuer.issue();
        rt_sender
            .send(RenderMessage::RegisterNormalized2DStaticMeshTexture {
                id: check_icon,
                data: Self::CHECK_ICON,
            })
            .expect("rt_sender.send");

        Self { check_icon }
    }
}

static SHARED_CHECK_ICON: UnsafeMainThreadOnlyOnceCell<SharedCheckIcon> =
    UnsafeMainThreadOnlyOnceCell(OnceCell::new());

pub struct ToggleButtonView {
    eh: Rc<ToggleButtonEventHandler>,
}
impl ToggleButtonView {
    pub fn new(ctx: &mut ViewInitContext, rect: Rect<LogicalUnit>, label: String) -> Self {
        let shared_res = SHARED_CHECK_ICON.0.get_or_init(|| {
            SharedCheckIcon::new(
                ctx.main_thread_texture_id_issuer,
                ctx.system_link.rt_sender(),
            )
        });

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
            composite_mode: CompositeMode::FillColor(AnimatableColor::Value([1.0, 1.0, 1.0, 0.0])),
            border: Some(Border {
                thickness: 1.0,
                color: AnimatableColor::Value([1.0; 4]),
                ..Default::default()
            }),
            corner_radius: CornerRadius::all(8.0),
            text: Some(CompositeRectText {
                runs: vec![CompositeRectTextRun {
                    content: label,
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
        let ct_check = ctx.mount_context.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            offset: [
                AnimatableFloat::Value(6.0),
                AnimatableFloat::Value(-SharedCheckIcon::CHECK_ICON.height * 0.5),
            ],
            relative_offset_adjustment: [0.0, 0.5],
            size: [
                AnimatableFloat::Value(SharedCheckIcon::CHECK_ICON.width),
                AnimatableFloat::Value(SharedCheckIcon::CHECK_ICON.height),
            ],
            pivot: [0.5, 0.5],
            has_bitmap: true,
            composite_mode: CompositeMode::ColorTint(
                AnimatableColor::Value([1.0; 4]),
                CompositeTexture {
                    id: shared_res.check_icon,
                    r#type: TextureType::Mask,
                    mapping: TextureMappingMode::Stretch,
                    slice_borders: [0.0; 4],
                },
            ),
            opacity: AnimatableFloat::Value(0.0),
            ..Default::default()
        });
        let ht_root = ctx.ht_manager.create(HitTestTreeData {
            left: rect.left,
            top: rect.top,
            width: rect.width,
            height: rect.height,
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

        Self { eh }
    }

    pub fn mount(&self, ctx: &mut MountContext, target: &(impl MountTarget + ?Sized)) {
        ctx.composite_tree
            .add_child(target.ct_root(), self.eh.ct_root);
        ctx.ht_manager.add_child(target.ht_root(), self.eh.ht_root);
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
    eh: Rc<CheckboxEventHandler>,
}
impl CheckboxView {
    pub fn new(ctx: &mut ViewInitContext, rect: Rect<LogicalUnit>) -> Self {
        let shared_res = SHARED_CHECK_ICON.0.get_or_init(|| {
            SharedCheckIcon::new(
                ctx.main_thread_texture_id_issuer,
                ctx.system_link.rt_sender(),
            )
        });

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
            composite_mode: CompositeMode::FillColor(AnimatableColor::Value([1.0, 1.0, 1.0, 0.0])),
            border: Some(Border {
                thickness: 0.5,
                color: AnimatableColor::Value([1.0, 1.0, 1.0, 0.5]),
                ..Default::default()
            }),
            corner_radius: CornerRadius::all(2.0),
            ..Default::default()
        });
        let ct_check = ctx.mount_context.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            offset: [
                AnimatableFloat::Value(-SharedCheckIcon::CHECK_ICON.width * 0.5),
                AnimatableFloat::Value(-SharedCheckIcon::CHECK_ICON.height * 0.5),
            ],
            relative_offset_adjustment: [0.5, 0.5],
            pivot: [0.5, 0.5],
            size: [
                AnimatableFloat::Value(SharedCheckIcon::CHECK_ICON.width),
                AnimatableFloat::Value(SharedCheckIcon::CHECK_ICON.height),
            ],
            has_bitmap: true,
            composite_mode: CompositeMode::ColorTint(
                AnimatableColor::Value([1.0; 4]),
                CompositeTexture {
                    id: shared_res.check_icon,
                    r#type: TextureType::Mask,
                    mapping: TextureMappingMode::Stretch,
                    slice_borders: [0.0; 4],
                },
            ),
            opacity: AnimatableFloat::Value(0.0),
            ..Default::default()
        });
        let ht_root = ctx.ht_manager.create(HitTestTreeData {
            left: rect.left,
            top: rect.top,
            width: rect.width,
            height: rect.height,
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

        Self { eh }
    }

    pub fn mount(&self, ctx: &mut MountContext, target: &(impl MountTarget + ?Sized)) {
        ctx.composite_tree
            .add_child(target.ct_root(), self.eh.ct_root);
        ctx.ht_manager.add_child(target.ht_root(), self.eh.ht_root);
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
