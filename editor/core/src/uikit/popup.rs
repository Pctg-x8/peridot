use std::collections::HashMap;

use crate::{
    SyncEvent, WindowHandle,
    input::{
        InputEventContext, KeyboardFocusGroupRef, KeyboardFocusTokenRegistry,
        hittest::{HitTestTreeData, HitTestTreeManager, HitTestTreeRef},
    },
    rendering::composite::{
        AnimatableColor, AnimatableFloat, AnimationCurve, Border, CompositeMode, CompositeRect,
        CompositeRectScaleFactor, CompositeTree, CompositeTreeRef, CornerRadius,
        FloatAnimationTemplate,
    },
    uikit::{
        RawMountTarget, RenderChildScheduler, RenderContext, TeardownContext, View, ViewIdentifier,
        ViewImmediateRenderable, ViewInitContext, ViewInstanceModifier, ViewInstanceStore,
        ViewNewRenderElements, ViewRenderQueue, ViewRenderStateStore, ViewTreeRelationStore,
        render_view_recursive, teardown_view_recursive, view_instance, view_instance_mut,
    },
    utils::{LogicalUnit, Size, range_helper::range_from_len},
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

/// ポップアップ共通ライフサイクル
pub trait Popup {
    fn root_view_id(&self) -> ViewIdentifier;

    /// UI Render Scaleが変わったときに呼ばれる
    #[allow(unused_variables)]
    fn rescale(&self, scale: f32, composite_tree: &mut CompositeTree<SyncEvent>) {}

    /// ポップアップが閉じられるときに呼ばれる
    fn close(
        &mut self,
        context: &mut PopupCloseContext,
        composite_tree: &mut CompositeTree<SyncEvent>,
        ht_manager: &mut HitTestTreeManager,
        current_sec: f32,
    );

    /// ポップアップのクローズアニメーションが終わって、インスタンスが破棄されるときに呼ばれる
    fn teardown(&mut self, ctx: &mut TeardownContext);
}

pub struct PopupCloseContext<'env> {
    pub view_instance_store: &'env mut ViewInstanceStore,
}
impl PopupCloseContext<'_> {
    #[inline(always)]
    pub fn view_instance<T: View + 'static>(&self, id: ViewIdentifier) -> Option<&T> {
        view_instance(id, self.view_instance_store)
    }

    #[inline(always)]
    pub fn view_instance_mut<T: View + 'static>(&mut self, id: ViewIdentifier) -> Option<&mut T> {
        view_instance_mut(id, self.view_instance_store)
    }
}

pub struct PopupManager {
    instance_by_id: HashMap<PopupID, (Box<dyn Popup>, WindowHandle, KeyboardFocusGroupRef)>,
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
        let popup_focus_group = ctx.keyboard_focus_registry.acquire_group();
        let instance = ctor(id, ctx);
        ctx.render_view_recursive(instance.root_view_id(), &window, popup_focus_group);
        self.instance_by_id
            .insert(id, (Box::new(instance), window, popup_focus_group));

        id
    }

    pub fn post_open_action(
        &mut self,
        target_popup_id: PopupID,
        action_context: &mut InputEventContext,
        kf_registry: &KeyboardFocusTokenRegistry,
    ) {
        if let Some((_, w, g)) = self.instance_by_id.get_mut(&target_popup_id) {
            w.keyboard_focus_state_mut()
                .push_tab_stop_group(*g, action_context, kf_registry);
        }
    }

    #[inline(always)]
    pub fn close(
        &mut self,
        id: PopupID,
        view_instance_store: &mut ViewInstanceStore,
        view_render_queue: &mut ViewRenderQueue,
        ctx: &mut RenderContext,
    ) -> bool {
        if let Some((instance, w, g)) = self.instance_by_id.get_mut(&id) {
            instance.close(
                &mut PopupCloseContext {
                    view_instance_store,
                },
                ctx.composite_tree,
                ctx.ht_manager,
                ctx.current_sec,
            );
            view_render_queue.schedule(instance.root_view_id());
            true
        } else {
            false
        }
    }

    #[inline(always)]
    pub fn teardown(
        &mut self,
        id: PopupID,
        view_instance_store: &mut ViewInstanceStore,
        view_tree_relation_store: &mut ViewTreeRelationStore,
        view_render_state_store: &mut ViewRenderStateStore,
        ctx: &mut TeardownContext,
    ) -> bool {
        if let Some((mut instance, mut w, g)) = self.instance_by_id.remove(&id) {
            w.keyboard_focus_state_mut().pop_tab_stop_group();
            ctx.mount_context.keyboard_focus_registry.release_group(g);
            instance.teardown(ctx);
            teardown_view_recursive(
                instance.root_view_id(),
                ctx,
                view_instance_store,
                view_tree_relation_store,
                view_render_state_store,
            );
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
        for (x, bound_window, _) in self.instance_by_id.values() {
            if bound_window == &for_window {
                x.rescale(scale, composite_tree);
            }
        }
    }
}

struct OverlayPopupBasicMaskViewRenderElements {
    ct_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
}

pub struct OverlayPopupBasicMaskView {
    render_elements: Option<OverlayPopupBasicMaskViewRenderElements>,
}
impl OverlayPopupBasicMaskView {
    pub const ANIMATION_DURATION: f32 = 0.125;
    const OPEN_BLUR_ANIM: FloatAnimationTemplate = FloatAnimationTemplate {
        from_value: 0.0,
        to_value: 3.0,
        curve: AnimationCurve::Linear,
        duration: Self::ANIMATION_DURATION,
    };
    const CLOSE_BLUR_ANIM: FloatAnimationTemplate =
        Self::OPEN_BLUR_ANIM.flip(AnimationCurve::Linear);

    pub fn new() -> Self {
        Self {
            render_elements: None,
        }
    }

    fn play_open_animation(
        ct_root: CompositeTreeRef,
        composite_tree: &mut CompositeTree<SyncEvent>,
        current_sec: f32,
    ) {
        composite_tree
            .begin_mod_chain(ct_root)
            .composite_mode(CompositeMode::FillColorBackdropBlur(
                AnimatableColor::Animated {
                    from_value: [0.0, 0.0, 0.0, 0.0],
                    to_value: [0.0, 0.0, 0.0, 0.25],
                    curve: AnimationCurve::Linear,
                    sec_duration: (current_sec..current_sec + Self::ANIMATION_DURATION).into(),
                    event_on_complete: None,
                },
                AnimatableFloat::from_template(&Self::OPEN_BLUR_ANIM, current_sec),
            ))
            .apply();
    }

    pub fn play_close_animation(
        &self,
        composite_tree: &mut CompositeTree<SyncEvent>,
        current_sec: f32,
    ) {
        composite_tree
            .begin_mod_chain(
                self.render_elements
                    .as_ref()
                    .expect("still not rendered?")
                    .ct_root,
            )
            .composite_mode(CompositeMode::FillColorBackdropBlur(
                AnimatableColor::Animated {
                    from_value: [0.0, 0.0, 0.0, 0.25],
                    to_value: [0.0, 0.0, 0.0, 0.0],
                    curve: AnimationCurve::Linear,
                    sec_duration: (current_sec..current_sec + Self::ANIMATION_DURATION).into(),
                    event_on_complete: None,
                },
                AnimatableFloat::from_template(&Self::CLOSE_BLUR_ANIM, current_sec),
            ))
            .apply();
    }
}
impl View for OverlayPopupBasicMaskView {
    fn render(
        &mut self,
        _self_instance: &mut ViewInstanceModifier,
        ctx: &mut RenderContext,
        sched: &mut RenderChildScheduler,
    ) -> ViewNewRenderElements {
        match self.render_elements {
            Some(ref e) => {
                sched.schedule_render_children(RawMountTarget {
                    ct_root: e.ct_root,
                    ht_root: e.ht_root,
                });

                ViewNewRenderElements::EMPTY
            }
            None => {
                // first render
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

                // play open animation at first render
                Self::play_open_animation(ct_root, ctx.composite_tree, ctx.current_sec);

                self.render_elements =
                    Some(OverlayPopupBasicMaskViewRenderElements { ct_root, ht_root });

                sched.schedule_render_children(RawMountTarget { ct_root, ht_root });
                ViewNewRenderElements {
                    composite_tree: Some(ct_root),
                    hit_tree: Some(ht_root),
                    ..ViewNewRenderElements::EMPTY
                }
            }
        }
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        let Some(e) = self.render_elements.take() else {
            // not rendered
            return;
        };

        ctx.mount_context.composite_tree.free_all(e.ct_root);
        ctx.mount_context.ht_manager.free_all(e.ht_root);
    }
}

struct OverlayPopupBasicFrameViewRenderElements {
    ct_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
}

pub struct OverlayPopupBasicFrameView {
    render_elements: Option<OverlayPopupBasicFrameViewRenderElements>,
    size: Size<LogicalUnit>,
}
impl OverlayPopupBasicFrameView {
    pub const ANIMATION_DURATION: f32 = OverlayPopupBasicMaskView::ANIMATION_DURATION;
    const OPEN_SCALE_ANIM: FloatAnimationTemplate = FloatAnimationTemplate {
        from_value: 0.95,
        to_value: 1.0,
        curve: AnimationCurve::CubicBezier {
            p1: (0.5, 0.5),
            p2: (0.5, 1.0),
        },
        duration: Self::ANIMATION_DURATION,
    };
    const OPEN_OPACITY_ANIM: FloatAnimationTemplate = FloatAnimationTemplate {
        from_value: 0.0,
        to_value: 1.0,
        curve: AnimationCurve::Linear,
        duration: Self::ANIMATION_DURATION,
    };
    const CLOSE_SCALE_ANIM: FloatAnimationTemplate =
        Self::OPEN_SCALE_ANIM.flip(AnimationCurve::CubicBezier {
            p1: (0.5, 0.5),
            p2: (0.5, 1.0),
        });
    const CLOSE_OPACITY_ANIM: FloatAnimationTemplate =
        Self::OPEN_OPACITY_ANIM.flip(AnimationCurve::Linear);

    pub fn new(size: Size<LogicalUnit>) -> Self {
        Self {
            render_elements: None,
            size,
        }
    }

    fn play_open_animation(
        ct_root: CompositeTreeRef,
        size: &Size<LogicalUnit>,
        composite_tree: &mut CompositeTree<SyncEvent>,
        current_sec: f32,
    ) {
        composite_tree
            .begin_mod_chain(ct_root)
            .y(AnimatableFloat::Animated {
                from_value: -size.height * 0.5 + 4.0,
                to_value: -size.height * 0.5,
                curve: AnimationCurve::CubicBezier {
                    p1: (0.5, 0.5),
                    p2: (0.5, 1.0),
                },
                sec_duration: range_from_len(current_sec, Self::ANIMATION_DURATION),
                event_on_complete: None,
            })
            .scale_animated_from_template(&Self::OPEN_SCALE_ANIM, current_sec)
            .opacity_animated_from_template(&Self::OPEN_OPACITY_ANIM, current_sec)
            .apply();
    }

    pub fn play_close_animation(
        &self,
        composite_tree: &mut CompositeTree<SyncEvent>,
        current_sec: f32,
        event_on_complete: SyncEvent,
    ) {
        composite_tree
            .begin_mod_chain(
                self.render_elements
                    .as_ref()
                    .expect("still not rendered?")
                    .ct_root,
            )
            .y(AnimatableFloat::Animated {
                from_value: -self.size.height * 0.5,
                to_value: -self.size.height * 0.5 + 4.0,
                curve: AnimationCurve::CubicBezier {
                    p1: (0.5, 0.5),
                    p2: (0.5, 1.0),
                },
                sec_duration: range_from_len(current_sec, Self::ANIMATION_DURATION),
                event_on_complete: None,
            })
            .scale_animated_from_template(&Self::CLOSE_SCALE_ANIM, current_sec)
            .opacity_animated_from_template_with_completion(
                &Self::CLOSE_OPACITY_ANIM,
                current_sec,
                event_on_complete,
            )
            .apply();
    }
}
impl View for OverlayPopupBasicFrameView {
    fn render(
        &mut self,
        _self_instance: &mut ViewInstanceModifier,
        ctx: &mut RenderContext,
        sched: &mut RenderChildScheduler,
    ) -> ViewNewRenderElements {
        match self.render_elements {
            Some(ref e) => {
                // TODO: reflect changes

                sched.schedule_render_children(RawMountTarget {
                    ct_root: e.ct_root,
                    ht_root: e.ht_root,
                });
                ViewNewRenderElements::EMPTY
            }
            None => {
                // first render
                let ct_root = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    relative_offset_adjustment: [0.5, 0.5],
                    size: [
                        AnimatableFloat::Value(self.size.width),
                        AnimatableFloat::Value(self.size.height),
                    ],
                    offset: [
                        AnimatableFloat::Value(-self.size.width * 0.5),
                        AnimatableFloat::Value(-self.size.height * 0.5),
                    ],
                    ..Default::default()
                });
                let ct_shadow = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    relative_size_adjustment: [1.0, 1.0],
                    size: [AnimatableFloat::Value(64.0), AnimatableFloat::Value(64.0)],
                    offset: [
                        AnimatableFloat::Value(-32.0),
                        AnimatableFloat::Value(-32.0 + 12.0),
                    ],
                    has_bitmap: true,
                    composite_mode: CompositeMode::FillColor(AnimatableColor::Value([
                        0.0, 0.0, 0.0, 0.75,
                    ])),
                    corner_radius: CornerRadius::all(64.0),
                    softedge: 64.0,
                    ..Default::default()
                });
                let ct_visual = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    relative_size_adjustment: [1.0, 1.0],
                    has_bitmap: true,
                    composite_mode: CompositeMode::FillColor(AnimatableColor::Value([
                        0.1, 0.1, 0.1, 1.0,
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
                    width: self.size.width,
                    height: self.size.height,
                    left_adjustment_factor: 0.5,
                    top_adjustment_factor: 0.5,
                    left: -self.size.width * 0.5,
                    // maskでヘッダ分開けてるのをここで補正
                    top: -self.size.height * 0.5 - crate::ui::window_header::View::THICKNESS * 0.5,
                    ..Default::default()
                });

                ctx.composite_tree.add_child(ct_root, ct_shadow);
                ctx.composite_tree.add_child(ct_root, ct_visual);

                // play animation on first render
                Self::play_open_animation(ct_root, &self.size, ctx.composite_tree, ctx.current_sec);

                self.render_elements =
                    Some(OverlayPopupBasicFrameViewRenderElements { ct_root, ht_root });

                sched.schedule_render_children(RawMountTarget { ct_root, ht_root });
                ViewNewRenderElements {
                    composite_tree: Some(ct_root),
                    hit_tree: Some(ht_root),
                    ..ViewNewRenderElements::EMPTY
                }
            }
        }
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        let Some(render_elements) = self.render_elements.take() else {
            // not rendered
            return;
        };

        ctx.mount_context
            .composite_tree
            .free_all(render_elements.ct_root);
        ctx.mount_context
            .ht_manager
            .free_all(render_elements.ht_root);
    }
}
