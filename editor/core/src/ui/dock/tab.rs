//! Dockのタブコンポーネント

use core::cell::Cell;
use std::rc::Rc;

use shared::{LogicalUnit, Point, Rect, Size};

use crate::{
    SystemLink,
    input::{
        EventContinueControl, InputEventContext,
        hittest::{
            HitTestTreeActionHandler, HitTestTreeData, HitTestTreeRef, PointerActionArgs,
            PointerButton, PointerButtonActionArgs,
        },
    },
    rendering::{
        composite::{
            AnimatableColor, AnimationCurve, CompositeMode, CompositeRect, CompositeRectText,
            CompositeRectTextRun, CompositeTree, CompositeTreeRef, CornerRadius,
            FloatAnimationTemplate, Gradient, GradientRef,
        },
        text::{FontID, TextLayout},
    },
    ui::dock::{DESIGN_METRICS, Dock, DockID},
    uicore::{
        MeasureContext, RenderContext, TeardownContext, TypedViewIdentifier, View, ViewConstructor,
        ViewIdentifier, ViewInstanceQueryable, ViewInstanceQueryableMut, ViewInstanceStore,
        ViewLayout, ViewLayoutStateStore, ViewRelationQueryable, ViewRenderElements,
        ViewRenderQueue, ViewRenderer,
    },
    utils::UnsafeMainThreadOnlyOnceCell,
};

pub(super) struct PaneGroupTabStripViewInit;
impl ViewConstructor for PaneGroupTabStripViewInit {
    type ConcreteView = PaneGroupTabStripView;

    fn construct(
        self,
        _id: crate::uicore::TypedViewIdentifier<Self::ConcreteView>,
    ) -> Self::ConcreteView {
        PaneGroupTabStripView { entity: None }
    }
}

/// Paneのグループのタブ部分を管理するView
pub(super) struct PaneGroupTabStripView {
    entity: Option<PaneGroupTabStripViewEntity>,
}
impl Drop for PaneGroupTabStripView {
    fn drop(&mut self) {
        if self.entity.is_some() {
            tracing::warn!("PaneGroupTabStripView dropped while still rendered")
        }
    }
}
impl View for PaneGroupTabStripView {
    fn render(
        &mut self,
        layout_rect: Rect<LogicalUnit>,
        ctx: &mut RenderContext,
        _layout_state: &ViewLayoutStateStore,
    ) -> ViewRenderElements {
        let e = match self.entity {
            Some(ref e) => {
                // placement changed
                ctx.composite_tree
                    .begin_mod_chain(e.ct_root)
                    .rect_imm(layout_rect.clone())
                    .apply();
                ctx.ht_manager.mod_chain(e.ht_root).rect(layout_rect);

                e
            }
            None => {
                // first render
                let ct_root = CompositeRect::build()
                    .rect_imm(layout_rect.clone())
                    .clip_child_hard()
                    .create(ctx.composite_tree);
                let ht_root = HitTestTreeData::build()
                    .rect(layout_rect)
                    .create(ctx.ht_manager);

                &*self
                    .entity
                    .insert(PaneGroupTabStripViewEntity { ct_root, ht_root })
            }
        };

        ViewRenderElements {
            composite_tree: Some(e.ct_root),
            hit_tree: Some(e.ht_root),
            ..ViewRenderElements::EMPTY
        }
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        let Some(entity) = self.entity.take() else {
            // not rendered
            return;
        };

        ctx.composite_tree.free(entity.ct_root);
        ctx.ht_manager.free(entity.ht_root);
    }

    fn measure_preferred_content_size(&self, _ctx: &mut MeasureContext) -> Size<LogicalUnit> {
        Size::new_logical(0.0, DESIGN_METRICS.tab_height())
    }

    fn create_new_layout_layer(&self) -> bool {
        true
    }
}

struct PaneGroupTabStripViewEntity {
    ct_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
}

/// タブの初期化データ
pub(super) struct PaneGroupTabViewInit {
    pub label: String,
    pub dock: DockID,
    pub active: bool,
}
impl ViewConstructor for PaneGroupTabViewInit {
    type ConcreteView = PaneGroupTabView;

    fn construct(self, id: TypedViewIdentifier<Self::ConcreteView>) -> Self::ConcreteView {
        PaneGroupTabView {
            id,
            entity: None,
            label: self.label,
            place: Point::new_logical(0.0, 0.0),
            dock: self.dock,
            active: self.active,
        }
    }
}

/// タブ
pub(super) struct PaneGroupTabView {
    id: TypedViewIdentifier<PaneGroupTabView>,
    entity: Option<Rc<PaneGroupTabEventHandler>>,
    label: String,
    place: Point<LogicalUnit>,
    dock: DockID,
    active: bool,
}
impl PaneGroupTabView {
    /// 幅を計算する
    pub(super) fn compute_width(label: &str, syslink: &SystemLink) -> f32 {
        TextLayout::measure_visual_width(label, FontID::UIDefault, syslink.font_set())
            + DESIGN_METRICS.tab_padding_x * 2.0
    }

    /// 配置
    pub(super) fn place(&mut self, pos: Point<LogicalUnit>) {
        self.place = pos;
    }

    /// アクティブ表示の切り替え
    pub(super) fn set_active(&mut self, active: bool) {
        self.active = active;
    }

    pub(super) fn rebind_dock(&mut self, dock: DockID) {
        self.dock = dock;
        if let Some(ref entity) = self.entity {
            // 紐づいてるdockはrenderを待たず直接アップデートしちゃう（表示には関係ないものなので）
            entity.dock.set(dock);
        }
    }

    const UNDERLINE_ACTIVATE_ANIM: FloatAnimationTemplate = FloatAnimationTemplate {
        from_value: 0.0,
        to_value: 1.0,
        curve: AnimationCurve::Linear,
        duration: 0.1,
    };
    const UNDERLINE_ACTIVATE_SCALEX_ANIM: FloatAnimationTemplate = FloatAnimationTemplate {
        from_value: 0.0,
        to_value: 1.0,
        curve: AnimationCurve::EASE_OUT_HARD,
        duration: 0.2,
    };
    const UNDERLINE_DEACTIVATE_SCALEX_ANIM: FloatAnimationTemplate =
        Self::UNDERLINE_ACTIVATE_SCALEX_ANIM.flip(AnimationCurve::EASE_IN);
}
impl View for PaneGroupTabView {
    fn render(
        &mut self,
        _layout_rect: Rect<LogicalUnit>,
        ctx: &mut RenderContext,
        _layout_state: &ViewLayoutStateStore,
    ) -> ViewRenderElements {
        let e = match self.entity {
            Some(ref e) => {
                ctx.composite_tree
                    .begin_mod_chain(e.ct_root)
                    .offset_imm(self.place.x, self.place.y)
                    .apply();
                ctx.ht_manager.get_data_mut(e.ht_root).left = self.place.x;
                ctx.ht_manager.get_data_mut(e.ht_root).top = self.place.y;

                if e.active.replace(self.active) != self.active {
                    if self.active {
                        ctx.composite_tree
                            .begin_mod_chain(e.ct_underline)
                            .scale_x_animated_from_template(
                                &Self::UNDERLINE_ACTIVATE_SCALEX_ANIM,
                                ctx.current_sec,
                            )
                            .opacity_animated_from_template(
                                &Self::UNDERLINE_ACTIVATE_ANIM,
                                ctx.current_sec,
                            )
                            .apply();
                        ctx.composite_tree
                            .begin_mod_chain(e.ct_active)
                            .composite_mode(CompositeMode::FillColor(AnimatableColor::Animated {
                                from_value: [1.0, 1.0, 1.0, 0.0],
                                to_value: [1.0, 1.0, 1.0, 0.1],
                                sec_duration: (ctx.current_sec..ctx.current_sec + 0.2).into(),
                                curve: AnimationCurve::Linear,
                                event_on_complete: None,
                            }))
                            .apply();
                    } else {
                        ctx.composite_tree
                            .begin_mod_chain(e.ct_underline)
                            .scale_x_animated_from_template(
                                &Self::UNDERLINE_DEACTIVATE_SCALEX_ANIM,
                                ctx.current_sec,
                            )
                            .apply();
                        ctx.composite_tree
                            .begin_mod_chain(e.ct_active)
                            .composite_mode(CompositeMode::FillColor(AnimatableColor::Animated {
                                from_value: [1.0, 1.0, 1.0, 0.1],
                                to_value: [1.0, 1.0, 1.0, 0.0],
                                sec_duration: (ctx.current_sec..ctx.current_sec + 0.2).into(),
                                curve: AnimationCurve::Linear,
                                event_on_complete: None,
                            }))
                            .apply();
                    }
                }

                e
            }
            None => {
                // first render
                let active_gradient = pane_group_tab_active_gradient(ctx.composite_tree);
                let size = Size::new_logical(
                    Self::compute_width(&self.label, ctx.system_link),
                    DESIGN_METRICS.tab_height(),
                );

                let ct_root = CompositeRect::build()
                    .offset_imm(self.place.x, self.place.y)
                    .size_imm(size.width, size.height)
                    .composite_fill_color_imm([1.0, 1.0, 1.0, 0.0])
                    .corner_radius(CornerRadius::all(DESIGN_METRICS.tab_rounding))
                    .text(
                        CompositeRectText::build()
                            .run(
                                CompositeRectTextRun::build(self.label.clone())
                                    .color_imm([1.0, 1.0, 1.0, 1.0]),
                            )
                            .vertical_middle()
                            .horizontal_middle(),
                    )
                    .create(ctx.composite_tree);
                let ct_active = CompositeRect::build()
                    .expand_full()
                    .composite_fill_color_imm([1.0, 1.0, 1.0, if self.active { 0.1 } else { 0.0 }])
                    .corner_radius(CornerRadius::all(DESIGN_METRICS.tab_rounding))
                    .create(ctx.composite_tree);
                let ct_underline = CompositeRect::build()
                    .expand_full()
                    .composite(CompositeMode::FillLinearGradient(active_gradient))
                    .corner_radius(CornerRadius::all(DESIGN_METRICS.tab_rounding))
                    .scale_x_imm(if self.active { 1.0 } else { 0.0 })
                    .create(ctx.composite_tree);
                let ht_root = HitTestTreeData::build()
                    .interactive_defaults()
                    .left(self.place.x)
                    .top(self.place.y)
                    .width(size.width)
                    .height(size.height)
                    .create(ctx.ht_manager);

                ctx.composite_tree.add_child(ct_root, ct_active);
                ctx.composite_tree.add_child(ct_root, ct_underline);

                let eh = Rc::new(PaneGroupTabEventHandler {
                    view_id: self.id,
                    dock: Cell::new(self.dock),
                    ct_root,
                    ct_active,
                    ct_underline,
                    ht_root,
                    size,
                    active: Cell::new(self.active),
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
        let Some(entity) = self.entity.take() else {
            // not rendered
            return;
        };

        ctx.composite_tree.remove_child(entity.ct_root);
        ctx.ht_manager.remove_child(entity.ht_root);

        ctx.composite_tree.free_all(entity.ct_root);
        ctx.ht_manager.free_all(entity.ht_root);
    }

    fn measure_preferred_content_size(&self, ctx: &mut MeasureContext) -> Size<LogicalUnit> {
        Size::new_logical(
            Self::compute_width(&self.label, ctx.system_link),
            DESIGN_METRICS.tab_height(),
        )
    }
}

/// タブViewのイベントハンドラ
struct PaneGroupTabEventHandler {
    /// このタブViewのID
    view_id: TypedViewIdentifier<PaneGroupTabView>,
    /// このタブが属しているDockのID
    dock: Cell<DockID>,
    /// ビジュアルツリー ルート
    ct_root: CompositeTreeRef,
    /// ビジュアルツリー アクティブ表示
    ct_active: CompositeTreeRef,
    /// ビジュアルツリー 下線
    ct_underline: CompositeTreeRef,
    /// 入力ツリー ルート
    ht_root: HitTestTreeRef,
    /// 大きさ
    size: Size<LogicalUnit>,
    /// アクティブ状態か？
    active: Cell<bool>,
}
impl HitTestTreeActionHandler for PaneGroupTabEventHandler {
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
                sec_duration: (context.current_sec..context.current_sec + 0.1).into(),
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
                sec_duration: (context.current_sec..context.current_sec + 0.1).into(),
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
        args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        if args.button == PointerButton::Primary {
            let Dock::Fill {
                group_view_controller,
                ..
            } = context.dock_store.get_mut(self.dock.get())
            else {
                unreachable!("tab on non-fill dock?");
            };

            struct LocalContext<'a> {
                view_instance_store: &'a mut ViewInstanceStore,
                view_render_queue: &'a mut ViewRenderQueue,
            }
            impl ViewInstanceQueryableMut for LocalContext<'_> {
                #[inline(always)]
                fn view_instance_mut_of<T: View + 'static>(
                    &mut self,
                    id: ViewIdentifier,
                ) -> Option<&mut T> {
                    crate::uicore::view_instance_mut(id, self.view_instance_store)
                }

                #[inline(always)]
                fn view_set_visibility_untyped(&mut self, id: ViewIdentifier, visible: bool) {
                    crate::uicore::view_set_visibility(id, visible, self.view_instance_store);
                }

                #[inline(always)]
                fn view_layout_mut_untyped(
                    &mut self,
                    id: ViewIdentifier,
                ) -> Option<&mut ViewLayout> {
                    crate::uicore::view_layout_mut(id, self.view_instance_store)
                }
            }
            impl ViewRenderer for LocalContext<'_> {
                #[inline(always)]
                fn schedule_view_render_untyped(&mut self, target: ViewIdentifier) {
                    self.view_render_queue.schedule(target)
                }
            }
            group_view_controller.select_tab(
                self.view_id,
                &mut LocalContext {
                    view_instance_store: context.view_instance_store,
                    view_render_queue: context.view_render_queue,
                },
            );
        } else {
            /*context.system_link.dispatch_event(Event::MenuOpen {
                parent: context
                    .ht_manager
                    .query_root_window(sender)
                    .expect("not mounted"),
                items: vec![
                    crate::uikit::MenuItem::Command {
                        label: "Entry1".into(),
                        command_id: 0,
                    },
                    crate::uikit::MenuItem::Command {
                        label: "Entry2".into(),
                        command_id: 1,
                    },
                    crate::uikit::MenuItem::Separator,
                    crate::uikit::MenuItem::Command {
                        label: "Entry3".into(),
                        command_id: 2,
                    },
                    crate::uikit::MenuItem::Heading {
                        label: "Head".into(),
                    },
                    crate::uikit::MenuItem::SubMenu {
                        label: "Sub".into(),
                        items: vec![crate::uikit::MenuItem::Command {
                            label: "SubEntry1".into(),
                            command_id: 4,
                        }],
                    },
                    crate::uikit::MenuItem::Command {
                        label: "Entry4".into(),
                        command_id: 3,
                    },
                ],
                surface_pos: args.client_pos,
            });*/
        }

        EventContinueControl::STOP_PROPAGATION
    }

    fn on_drag_start(
        &self,
        _sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        if args.button != PointerButton::Primary {
            return EventContinueControl::empty();
        }

        let dock = self.dock.get();
        let preview_rect = context.dock_store.get_computed_state(dock).rect.clone();
        let Dock::Fill {
            group_view_controller,
            ..
        } = context.dock_store.get_mut(dock)
        else {
            unreachable!("tab on non-fill dock?");
        };

        let tab_index = group_view_controller
            .tab_index(self.view_id)
            .expect("not in any group");
        let tab_strip_view = group_view_controller.tab_strip_view;
        let content_ht_root = context
            .view_instance(tab_strip_view)
            .expect("query failed")
            .entity
            .as_ref()
            .expect("not rendered")
            .ht_root;
        let initiator = context
            .ht_manager
            .query_root_window(content_ht_root)
            .expect("not mounted");

        let (state, popover_rect) = super::begin_preview(
            preview_rect,
            self.size.clone(),
            &args.client_pos,
            initiator,
            dock,
            tab_index,
        );
        let root_layout = context
            .view_layout_untyped(
                context
                    .view_get_parent(tab_strip_view)
                    .expect("view not mounted?"),
            )
            .expect("query failed");
        let dock_basepoint = Point::new_logical(root_layout.left_offset, root_layout.top_offset);
        context.system_link.begin_pane_drag(
            initiator,
            &args.pointer_id,
            state.offset,
            &popover_rect.ref_with_offset(dock_basepoint),
        );
        context.store_docking_preview_state(state);

        EventContinueControl::STOP_PROPAGATION
    }
}

static PANE_GROUP_TAB_ACTIVE_GRADIENT: UnsafeMainThreadOnlyOnceCell<GradientRef> =
    UnsafeMainThreadOnlyOnceCell(std::cell::OnceCell::new());
fn pane_group_tab_active_gradient<E>(composite_tree: &mut CompositeTree<E>) -> GradientRef {
    *PANE_GROUP_TAB_ACTIVE_GRADIENT.0.get_or_init(|| {
        composite_tree.create_gradient(Gradient::Linear {
            start_color: [0.0, 0.5, 1.0, 0.0],
            end_color: [0.0, 0.75, 1.2, 1.0],
            start_pos_relative: [0.0, 0.8],
            end_pos_relative: [0.0, 1.0],
        })
    })
}
