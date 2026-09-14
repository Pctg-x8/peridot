use std::collections::{HashMap, HashSet};

use shared::{LogicalUnit, Point, Rect, Size, range_from_len};

use crate::{
    SyncEvent, WindowHandle,
    input::{
        InputEventContext, KeyboardFocusGroupRef, KeyboardFocusTokenRegistry,
        hittest::{HitTestTreeData, HitTestTreeRef},
    },
    rendering::composite::{
        AnimatableColor, AnimatableFloat, AnimationCurve, Border, CompositeMode, CompositeRect,
        CompositeRectScaleFactor, CompositeTree, CompositeTreeRef, CornerRadius,
        FloatAnimationTemplate,
    },
    uicore::{
        RenderContext, TeardownContext, View, ViewConstructor, ViewIdentifier, ViewInitContext,
        ViewInstanceQueryable, ViewInstanceQueryableMut, ViewInstanceStore, ViewLayoutStateStore,
        ViewRenderElements, ViewRenderStateStore, ViewTreeRelationStore, render_view_with_base,
        teardown_view_recursive, view_instance, view_instance_mut, view_layout_mut,
        view_set_visibility,
    },
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
    #[allow(unused_variables)]
    fn close(&mut self, context: &mut PopupCloseContext) {}

    /// ポップアップのクローズアニメーションが終わって、インスタンスが破棄されるときに呼ばれる
    fn teardown(&mut self, ctx: &mut TeardownContext);
}

pub struct PopupCloseContext<'env> {
    pub view_instance_store: &'env mut ViewInstanceStore,
}
impl ViewInstanceQueryable for PopupCloseContext<'_> {
    #[inline(always)]
    fn view_instance_of<T: View + 'static>(&self, id: ViewIdentifier) -> Option<&T> {
        view_instance(id, self.view_instance_store)
    }
}
impl ViewInstanceQueryableMut for PopupCloseContext<'_> {
    #[inline(always)]
    fn view_instance_mut_of<T: View + 'static>(&mut self, id: ViewIdentifier) -> Option<&mut T> {
        view_instance_mut(id, self.view_instance_store)
    }

    #[inline(always)]
    fn view_set_visibility_untyped(&mut self, id: ViewIdentifier, visible: bool) {
        view_set_visibility(id, visible, self.view_instance_store);
    }

    #[inline(always)]
    fn view_layout_mut_untyped(&mut self, id: ViewIdentifier) -> Option<&mut super::ViewLayout> {
        view_layout_mut(id, self.view_instance_store)
    }
}

pub struct PopupManager {
    instance_by_id: HashMap<PopupID, (Box<dyn Popup>, WindowHandle, KeyboardFocusGroupRef)>,
    pending_update: HashSet<PopupID>,
}
impl PopupManager {
    pub fn new() -> Self {
        Self {
            instance_by_id: HashMap::new(),
            pending_update: HashSet::new(),
        }
    }

    pub fn open<P: Popup + 'static>(
        &mut self,
        ctx: &mut ViewInitContext,
        window: WindowHandle,
        ctor: impl FnOnce(PopupID, &mut ViewInitContext) -> P,
    ) -> PopupID {
        let id = PopupID::new();
        self.instance_by_id.insert(
            id,
            (
                Box::new(ctor(id, ctx)),
                window,
                ctx.keyboard_focus_registry.acquire_group(),
            ),
        );
        self.pending_update.insert(id);

        id
    }

    pub fn post_open_action(
        target_popup_id: PopupID,
        action_context: &mut InputEventContext,
        kf_registry: &KeyboardFocusTokenRegistry,
    ) {
        let Some(&mut (_, mut w, g)) = action_context
            .popup_manager
            .instance_by_id
            .get_mut(&target_popup_id)
        else {
            return;
        };

        w.keyboard_focus_state_mut()
            .push_tab_stop_group(g, action_context, kf_registry);
    }

    pub fn close(&mut self, id: PopupID, view_instance_store: &mut ViewInstanceStore) -> bool {
        let Some(&mut (ref mut instance, _, _)) = self.instance_by_id.get_mut(&id) else {
            tracing::warn!(?id, "closing invalid popup");
            return false;
        };

        instance.close(&mut PopupCloseContext {
            view_instance_store,
        });
        self.pending_update.insert(id);
        true
    }

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
            ctx.keyboard_focus_registry.release_group(g);
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

    pub fn update_views(
        &mut self,
        ctx: &mut RenderContext,
        view_instance_store: &mut ViewInstanceStore,
        view_tree_relation_store: &ViewTreeRelationStore,
        view_layout_state_store: &mut ViewLayoutStateStore,
        view_render_state_store: &mut ViewRenderStateStore,
    ) {
        for id in self.pending_update.drain() {
            let Some(&(ref instance, ref w, g)) = self.instance_by_id.get(&id) else {
                tracing::warn!(?id, "updating invalid popup");
                continue;
            };

            render_view_with_base(
                instance.root_view_id(),
                ctx,
                w,
                g,
                Rect::from_lt_size(Point::new_logical(0.0, 0.0), w.client_size()),
                view_instance_store,
                view_tree_relation_store,
                view_layout_state_store,
                view_render_state_store,
            );
        }
    }

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

pub struct OverlayPopupBasicMaskViewInit;
impl ViewConstructor for OverlayPopupBasicMaskViewInit {
    type ConcreteView = OverlayPopupBasicMaskView;

    fn construct(self, _id: super::TypedViewIdentifier<Self::ConcreteView>) -> Self::ConcreteView {
        OverlayPopupBasicMaskView {
            render_elements: None,
            active_state: OverlayPopupBasicMaskViewState::Opening,
            pending_state: None,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum OverlayPopupBasicMaskViewState {
    Opening,
    Closing,
}

pub struct OverlayPopupBasicMaskView {
    render_elements: Option<OverlayPopupBasicMaskViewRenderElements>,
    active_state: OverlayPopupBasicMaskViewState,
    pending_state: Option<OverlayPopupBasicMaskViewState>,
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

    pub fn play_close_animation(&mut self) {
        self.pending_state = Some(OverlayPopupBasicMaskViewState::Closing);
    }
}
impl View for OverlayPopupBasicMaskView {
    fn render(
        &mut self,
        _layout_rect: Rect<LogicalUnit>,
        ctx: &mut RenderContext,
        _layout_state: &ViewLayoutStateStore,
    ) -> ViewRenderElements {
        let (e, retrigger_anim) = match self.render_elements {
            Some(ref e) => (e, false),
            None => {
                // first render
                let ct_root = CompositeRect::build()
                    .expand_full()
                    .composite(CompositeMode::FillColorBackdropBlur(
                        AnimatableColor::Value([0.0, 0.0, 0.0, 0.25]),
                        AnimatableFloat::Value(3.0),
                    ))
                    .create(ctx.composite_tree);
                let ht_root = HitTestTreeData::build()
                    .expand_full()
                    // WindowHeaderのぶん開ける(ドラッグ判定がこない)
                    // TODO: ここだけ参照関係が逆になる（uicore -> ui） どうするか......
                    .height(-crate::ui::window_header::View::THICKNESS)
                    .top(crate::ui::window_header::View::THICKNESS)
                    .create(ctx.ht_manager);

                (
                    &*self
                        .render_elements
                        .insert(OverlayPopupBasicMaskViewRenderElements { ct_root, ht_root }),
                    true,
                )
            }
        };

        let state_changed = match self.pending_state.take() {
            None => false,
            Some(st) => core::mem::replace(&mut self.active_state, st) != st,
        };
        if state_changed || retrigger_anim {
            match self.active_state {
                OverlayPopupBasicMaskViewState::Opening => {
                    ctx.composite_tree
                        .begin_mod_chain(e.ct_root)
                        .composite_mode(CompositeMode::FillColorBackdropBlur(
                            AnimatableColor::Animated {
                                from_value: [0.0, 0.0, 0.0, 0.0],
                                to_value: [0.0, 0.0, 0.0, 0.25],
                                curve: AnimationCurve::Linear,
                                sec_duration: (ctx.current_sec
                                    ..ctx.current_sec + Self::ANIMATION_DURATION)
                                    .into(),
                                event_on_complete: None,
                            },
                            AnimatableFloat::from_template(&Self::OPEN_BLUR_ANIM, ctx.current_sec),
                        ))
                        .apply();
                }
                OverlayPopupBasicMaskViewState::Closing => {
                    ctx.composite_tree
                        .begin_mod_chain(e.ct_root)
                        .composite_mode(CompositeMode::FillColorBackdropBlur(
                            AnimatableColor::Animated {
                                from_value: [0.0, 0.0, 0.0, 0.25],
                                to_value: [0.0, 0.0, 0.0, 0.0],
                                curve: AnimationCurve::Linear,
                                sec_duration: (ctx.current_sec
                                    ..ctx.current_sec + Self::ANIMATION_DURATION)
                                    .into(),
                                event_on_complete: None,
                            },
                            AnimatableFloat::from_template(&Self::CLOSE_BLUR_ANIM, ctx.current_sec),
                        ))
                        .apply();
                }
            }
        }

        ViewRenderElements {
            composite_tree: Some(e.ct_root),
            hit_tree: Some(e.ht_root),
            ..ViewRenderElements::EMPTY
        }
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        let Some(e) = self.render_elements.take() else {
            // not rendered
            return;
        };

        ctx.composite_tree.free(e.ct_root);
        ctx.ht_manager.free(e.ht_root);
    }

    fn measure_preferred_content_size(
        &self,
        _ctx: &mut super::MeasureContext,
    ) -> Size<LogicalUnit> {
        Size::new_logical(0.0, 0.0)
    }

    fn create_new_layout_layer(&self) -> bool {
        true
    }
}

struct OverlayPopupBasicFrameViewRenderElements {
    ct_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
}

pub struct OverlayPopupBasicFrameViewInit {
    pub size: Size<LogicalUnit>,
}
impl ViewConstructor for OverlayPopupBasicFrameViewInit {
    type ConcreteView = OverlayPopupBasicFrameView;

    fn construct(self, _id: super::TypedViewIdentifier<Self::ConcreteView>) -> Self::ConcreteView {
        OverlayPopupBasicFrameView {
            render_elements: None,
            size: self.size,
            active_state: OverlayPopupBasicFrameViewState::Opening,
            pending_state: None,
            close_transition_done_event: None,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum OverlayPopupBasicFrameViewState {
    Opening,
    Closing,
}

pub struct OverlayPopupBasicFrameView {
    render_elements: Option<OverlayPopupBasicFrameViewRenderElements>,
    size: Size<LogicalUnit>,
    active_state: OverlayPopupBasicFrameViewState,
    pending_state: Option<OverlayPopupBasicFrameViewState>,
    close_transition_done_event: Option<SyncEvent>,
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

    pub fn play_close_animation(&mut self, event_on_complete: SyncEvent) {
        self.pending_state = Some(OverlayPopupBasicFrameViewState::Closing);
        self.close_transition_done_event = Some(event_on_complete);
    }
}
impl View for OverlayPopupBasicFrameView {
    fn render(
        &mut self,
        layout_rect: Rect<LogicalUnit>,
        ctx: &mut RenderContext,
        _layout_state: &ViewLayoutStateStore,
    ) -> ViewRenderElements {
        let (e, retrigger_anim) = match self.render_elements {
            Some(ref e) => {
                // TODO: reflect changes

                (e, false)
            }
            None => {
                // first render
                let ct_root = CompositeRect::build()
                    .size_imm(layout_rect.width, layout_rect.height)
                    .centering()
                    .create(ctx.composite_tree);
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

                (
                    &*self
                        .render_elements
                        .insert(OverlayPopupBasicFrameViewRenderElements { ct_root, ht_root }),
                    true,
                )
            }
        };

        let state_changed = match self.pending_state.take() {
            None => false,
            Some(st) => core::mem::replace(&mut self.active_state, st) != st,
        };
        if state_changed || retrigger_anim {
            match self.active_state {
                OverlayPopupBasicFrameViewState::Opening => {
                    ctx.composite_tree
                        .begin_mod_chain(e.ct_root)
                        .y(AnimatableFloat::Animated {
                            from_value: -self.size.height * 0.5 + 4.0,
                            to_value: -self.size.height * 0.5,
                            curve: AnimationCurve::CubicBezier {
                                p1: (0.5, 0.5),
                                p2: (0.5, 1.0),
                            },
                            sec_duration: range_from_len(ctx.current_sec, Self::ANIMATION_DURATION),
                            event_on_complete: None,
                        })
                        .scale_animated_from_template(&Self::OPEN_SCALE_ANIM, ctx.current_sec)
                        .opacity_animated_from_template(&Self::OPEN_OPACITY_ANIM, ctx.current_sec)
                        .apply();
                }
                OverlayPopupBasicFrameViewState::Closing => {
                    ctx.composite_tree
                        .begin_mod_chain(e.ct_root)
                        .y(AnimatableFloat::Animated {
                            from_value: -self.size.height * 0.5,
                            to_value: -self.size.height * 0.5 + 4.0,
                            curve: AnimationCurve::CubicBezier {
                                p1: (0.5, 0.5),
                                p2: (0.5, 1.0),
                            },
                            sec_duration: range_from_len(ctx.current_sec, Self::ANIMATION_DURATION),
                            event_on_complete: None,
                        })
                        .scale_animated_from_template(&Self::CLOSE_SCALE_ANIM, ctx.current_sec)
                        .opacity_animated_from_template_with_completion(
                            &Self::CLOSE_OPACITY_ANIM,
                            ctx.current_sec,
                            self.close_transition_done_event
                                .take()
                                .expect("no close transition done event bound"),
                        )
                        .apply();
                }
            }
        }

        ViewRenderElements {
            composite_tree: Some(e.ct_root),
            hit_tree: Some(e.ht_root),
            ..ViewRenderElements::EMPTY
        }
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        let Some(e) = self.render_elements.take() else {
            // not rendered
            return;
        };

        ctx.composite_tree.free(e.ct_root);
        ctx.ht_manager.free(e.ht_root);
    }

    fn measure_preferred_content_size(
        &self,
        _ctx: &mut super::MeasureContext,
    ) -> Size<LogicalUnit> {
        Size::new_logical(0.0, 0.0)
    }

    fn create_new_layout_layer(&self) -> bool {
        true
    }
}
