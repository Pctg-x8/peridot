use std::collections::HashMap;

use crate::{
    SyncEvent, WindowHandle,
    input::hittest::{HitTestTreeData, HitTestTreeManager, HitTestTreeRef},
    rendering::composite::{
        AnimatableColor, AnimatableFloat, AnimationCurve, Border, CompositeMode, CompositeRect,
        CompositeTree, CompositeTreeRef, CornerRadius,
    },
    uikit::{MountContext, MountTarget, RawMountTarget, ViewInitContext},
    utils::{LogicalUnit, Size},
};

#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct PopupID(uuid::Uuid);
impl PopupID {
    #[inline(always)]
    fn new() -> Self {
        Self(uuid::Uuid::new_v4())
    }
}

pub trait Popup {
    fn mount(&self, ctx: &mut MountContext, parent: &RawMountTarget);
    fn rescale(&self, scale: f32, composite_tree: &mut CompositeTree<SyncEvent>);
    fn close(
        &self,
        composite_tree: &mut CompositeTree<SyncEvent>,
        ht_manager: &mut HitTestTreeManager,
        current_sec: f32,
    );
    fn unmount(&self, ctx: &mut MountContext);
}

pub struct PopupManager {
    instance_by_id: HashMap<PopupID, (Box<dyn Popup>, WindowHandle)>,
}
impl PopupManager {
    #[inline(always)]
    pub fn new() -> Self {
        Self {
            instance_by_id: HashMap::new(),
        }
    }

    pub fn open<P: Popup + 'static>(
        &mut self,
        ctx: &mut ViewInitContext,
        window: WindowHandle,
        ctor: impl FnOnce(PopupID, &mut ViewInitContext) -> P,
    ) -> PopupID {
        let id = PopupID::new();
        let instance = ctor(id, ctx);
        instance.mount(ctx, &RawMountTarget::from_typed(&window));
        self.instance_by_id.insert(id, (Box::new(instance), window));

        id
    }

    #[inline(always)]
    pub fn close(
        &self,
        id: PopupID,
        composite_tree: &mut CompositeTree<SyncEvent>,
        ht_manager: &mut HitTestTreeManager,
        current_sec: f32,
    ) -> bool {
        if let Some((instance, _)) = self.instance_by_id.get(&id) {
            instance.close(composite_tree, ht_manager, current_sec);
            true
        } else {
            false
        }
    }

    #[inline(always)]
    pub fn unmount(&mut self, ctx: &mut MountContext, id: PopupID) -> bool {
        if let Some((instance, _)) = self.instance_by_id.remove(&id) {
            instance.unmount(ctx);
            true
        } else {
            false
        }
    }

    #[inline(always)]
    pub fn rescale(
        &self,
        for_window: WindowHandle,
        scale: f32,
        composite_tree: &mut CompositeTree<SyncEvent>,
    ) {
        for (x, bound_window) in self.instance_by_id.values() {
            if bound_window == &for_window {
                x.rescale(scale, composite_tree);
            }
        }
    }
}

pub struct OverlayPopupBasicMaskView {
    ct_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
}
impl MountTarget for OverlayPopupBasicMaskView {
    #[inline(always)]
    fn ct_root(&self) -> CompositeTreeRef {
        self.ct_root
    }

    #[inline(always)]
    fn ht_root(&self) -> HitTestTreeRef {
        self.ht_root
    }
}
impl OverlayPopupBasicMaskView {
    pub const ANIMATION_DURATION: f32 = 0.125;

    pub fn new(ctx: &mut ViewInitContext) -> Self {
        let ct_root = ctx.composite_tree.create(CompositeRect {
            relative_size_adjustment: [1.0, 1.0],
            has_bitmap: true,
            composite_mode: CompositeMode::FillColorBackdropBlur(
                AnimatableColor::Value([0.0, 0.0, 0.0, 0.25]),
                AnimatableFloat::Value(3.0),
            ),
            ..Default::default()
        });
        let ht_root = ctx.ht_manager.create(HitTestTreeData {
            width_adjustment_factor: 1.0,
            height_adjustment_factor: 1.0,
            // WindowHeaderのぶん開ける(ドラッグ判定がこない)
            // TODO: ここだけ参照関係が逆になる（uikit -> ui） どうするか......
            height: -crate::ui::window_header::View::THICKNESS,
            top: crate::ui::window_header::View::THICKNESS,
            ..Default::default()
        });

        Self { ct_root, ht_root }
    }

    pub fn mount(&self, ctx: &mut MountContext, parent: &(impl MountTarget + ?Sized)) {
        ctx.composite_tree.add_child(parent.ct_root(), self.ct_root);
        ctx.ht_manager.add_child(parent.ht_root(), self.ht_root);
    }

    pub fn unmount(&self, ctx: &mut MountContext) {
        ctx.composite_tree.remove_child(self.ct_root);
        ctx.ht_manager.remove_child(self.ht_root);
    }

    pub fn play_open_animation(
        &self,
        composite_tree: &mut CompositeTree<SyncEvent>,
        current_sec: f32,
    ) {
        composite_tree.get_mut(self.ct_root).composite_mode = CompositeMode::FillColorBackdropBlur(
            AnimatableColor::Animated {
                from_value: [0.0, 0.0, 0.0, 0.0],
                to_value: [0.0, 0.0, 0.0, 0.25],
                start_sec: current_sec,
                end_sec: current_sec + Self::ANIMATION_DURATION,
                curve: AnimationCurve::Linear,
                event_on_complete: None,
            },
            AnimatableFloat::Animated {
                from_value: 0.0,
                to_value: 3.0,
                start_sec: current_sec,
                end_sec: current_sec + Self::ANIMATION_DURATION,
                curve: AnimationCurve::Linear,
                event_on_complete: None,
            },
        );
        composite_tree.mark_dirty(self.ct_root);
    }

    pub fn play_close_animation(
        &self,
        composite_tree: &mut CompositeTree<SyncEvent>,
        current_sec: f32,
    ) {
        composite_tree.get_mut(self.ct_root).composite_mode = CompositeMode::FillColorBackdropBlur(
            AnimatableColor::Animated {
                from_value: [0.0, 0.0, 0.0, 0.25],
                to_value: [0.0, 0.0, 0.0, 0.0],
                start_sec: current_sec,
                end_sec: current_sec + Self::ANIMATION_DURATION,
                curve: AnimationCurve::Linear,
                event_on_complete: None,
            },
            AnimatableFloat::Animated {
                from_value: 3.0,
                to_value: 0.0,
                start_sec: current_sec,
                end_sec: current_sec + Self::ANIMATION_DURATION,
                curve: AnimationCurve::Linear,
                event_on_complete: None,
            },
        );
        composite_tree.mark_dirty(self.ct_root);
    }
}

pub struct OverlayPopupBasicFrameView {
    ct_root: CompositeTreeRef,
    ct_shadow: CompositeTreeRef,
    ct_visual: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    size: Size<LogicalUnit>,
}
impl MountTarget for OverlayPopupBasicFrameView {
    #[inline(always)]
    fn ct_root(&self) -> CompositeTreeRef {
        self.ct_root
    }

    #[inline(always)]
    fn ht_root(&self) -> HitTestTreeRef {
        self.ht_root
    }
}
impl OverlayPopupBasicFrameView {
    pub const ANIMATION_DURATION: f32 = OverlayPopupBasicMaskView::ANIMATION_DURATION;

    pub fn new(ctx: &mut ViewInitContext, size: Size<LogicalUnit>) -> Self {
        let ct_root = ctx.mount_context.composite_tree.create(CompositeRect {
            base_scale_factor: ctx.ui_scale_factor,
            relative_offset_adjustment: [0.5, 0.5],
            size: [
                AnimatableFloat::Value(size.width),
                AnimatableFloat::Value(size.height),
            ],
            offset: [
                AnimatableFloat::Value(-size.width * 0.5),
                AnimatableFloat::Value(-size.height * 0.5),
            ],
            ..Default::default()
        });
        let ct_shadow = ctx.mount_context.composite_tree.create(CompositeRect {
            base_scale_factor: ctx.ui_scale_factor,
            relative_size_adjustment: [1.0, 1.0],
            size: [AnimatableFloat::Value(64.0), AnimatableFloat::Value(64.0)],
            offset: [
                AnimatableFloat::Value(-32.0),
                AnimatableFloat::Value(-32.0 + 12.0),
            ],
            has_bitmap: true,
            composite_mode: CompositeMode::FillColor(AnimatableColor::Value([0.0, 0.0, 0.0, 0.75])),
            corner_radius: CornerRadius::all(64.0),
            softedge: 64.0,
            ..Default::default()
        });
        let ct_visual = ctx.mount_context.composite_tree.create(CompositeRect {
            base_scale_factor: ctx.ui_scale_factor,
            relative_size_adjustment: [1.0, 1.0],
            has_bitmap: true,
            composite_mode: CompositeMode::FillColor(AnimatableColor::Value([
                0.025, 0.025, 0.025, 1.0,
            ])),
            corner_radius: CornerRadius::all(16.0),
            border: Some(Border {
                thickness: 0.5,
                color: AnimatableColor::Value([0.0, 0.0, 0.0, 1.0]),
                ..Default::default()
            }),
            ..Default::default()
        });
        let ht_root = ctx.ht_manager.create(HitTestTreeData {
            width: size.width,
            height: size.height,
            left_adjustment_factor: 0.5,
            top_adjustment_factor: 0.5,
            left: -size.width * 0.5,
            // maskでヘッダ分開けてるのをここで補正
            top: -size.height * 0.5 - crate::ui::window_header::View::THICKNESS * 0.5,
            ..Default::default()
        });

        ctx.composite_tree.add_child(ct_root, ct_shadow);
        ctx.composite_tree.add_child(ct_root, ct_visual);

        Self {
            ct_root,
            ct_shadow,
            ct_visual,
            ht_root,
            size,
        }
    }

    pub fn mount(&self, ctx: &mut MountContext, parent: &(impl MountTarget + ?Sized)) {
        ctx.composite_tree.add_child(parent.ct_root(), self.ct_root);
        ctx.ht_manager.add_child(parent.ht_root(), self.ht_root);
    }

    pub fn play_open_animation(
        &self,
        composite_tree: &mut CompositeTree<SyncEvent>,
        current_sec: f32,
    ) {
        composite_tree.get_mut(self.ct_root).offset[1] = AnimatableFloat::Animated {
            from_value: -self.size.height * 0.5 + 4.0,
            to_value: -self.size.height * 0.5,
            start_sec: current_sec,
            end_sec: current_sec + Self::ANIMATION_DURATION,
            curve: AnimationCurve::CubicBezier {
                p1: (0.5, 0.5),
                p2: (0.5, 1.0),
            },
            event_on_complete: None,
        };
        composite_tree.get_mut(self.ct_root).scale_x = AnimatableFloat::Animated {
            from_value: 0.95,
            to_value: 1.0,
            start_sec: current_sec,
            end_sec: current_sec + Self::ANIMATION_DURATION,
            curve: AnimationCurve::CubicBezier {
                p1: (0.5, 0.5),
                p2: (0.5, 1.0),
            },
            event_on_complete: None,
        };
        composite_tree.get_mut(self.ct_root).scale_y = AnimatableFloat::Animated {
            from_value: 0.95,
            to_value: 1.0,
            start_sec: current_sec,
            end_sec: current_sec + Self::ANIMATION_DURATION,
            curve: AnimationCurve::CubicBezier {
                p1: (0.5, 0.5),
                p2: (0.5, 1.0),
            },
            event_on_complete: None,
        };
        composite_tree.get_mut(self.ct_root).opacity = AnimatableFloat::Animated {
            from_value: 0.0,
            to_value: 1.0,
            start_sec: current_sec,
            end_sec: current_sec + Self::ANIMATION_DURATION,
            curve: AnimationCurve::Linear,
            event_on_complete: None,
        };
        composite_tree.mark_dirty(self.ct_root);
    }

    pub fn play_close_animation(
        &self,
        composite_tree: &mut CompositeTree<SyncEvent>,
        current_sec: f32,
        event_on_complete: SyncEvent,
    ) {
        composite_tree.get_mut(self.ct_root).offset[1] = AnimatableFloat::Animated {
            from_value: -self.size.height * 0.5,
            to_value: -self.size.height * 0.5 + 4.0,
            start_sec: current_sec,
            end_sec: current_sec + Self::ANIMATION_DURATION,
            curve: AnimationCurve::CubicBezier {
                p1: (0.5, 0.5),
                p2: (0.5, 1.0),
            },
            event_on_complete: None,
        };
        composite_tree.get_mut(self.ct_root).scale_x = AnimatableFloat::Animated {
            from_value: 1.0,
            to_value: 0.95,
            start_sec: current_sec,
            end_sec: current_sec + Self::ANIMATION_DURATION,
            curve: AnimationCurve::CubicBezier {
                p1: (0.5, 0.5),
                p2: (0.5, 1.0),
            },
            event_on_complete: None,
        };
        composite_tree.get_mut(self.ct_root).scale_y = AnimatableFloat::Animated {
            from_value: 1.0,
            to_value: 0.95,
            start_sec: current_sec,
            end_sec: current_sec + Self::ANIMATION_DURATION,
            curve: AnimationCurve::CubicBezier {
                p1: (0.5, 0.5),
                p2: (0.5, 1.0),
            },
            event_on_complete: None,
        };
        composite_tree.get_mut(self.ct_root).opacity = AnimatableFloat::Animated {
            from_value: 1.0,
            to_value: 0.0,
            start_sec: current_sec,
            end_sec: current_sec + Self::ANIMATION_DURATION,
            curve: AnimationCurve::Linear,
            event_on_complete: Some(event_on_complete),
        };
        composite_tree.mark_dirty(self.ct_root);
    }

    pub fn rescale(&self, scale: f32, composite_tree: &mut CompositeTree<SyncEvent>) {
        composite_tree.get_mut(self.ct_root).base_scale_factor = scale;
        composite_tree.get_mut(self.ct_shadow).base_scale_factor = scale;
        composite_tree.get_mut(self.ct_visual).base_scale_factor = scale;

        composite_tree.mark_dirty_all(self.ct_root);
        composite_tree.mark_dirty_all(self.ct_shadow);
        composite_tree.mark_dirty_all(self.ct_visual);
    }
}
