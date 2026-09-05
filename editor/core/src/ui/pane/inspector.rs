use std::{cell::Cell, rc::Rc};

use model::{
    Application, ApplicationAccess, ApplicationMutableAccess, ApplicationMutation,
    ObjectSelectionState,
};
use peridot_math::Vector3;

use crate::{
    input::{
        EventContinueControl,
        hittest::{CursorShape, HitTestTreeActionHandler, HitTestTreeData, HitTestTreeRef},
    },
    rendering::{
        composite::{
            AnimatableColor, AnimatableFloat, AnimationCurve, CompositeMode, CompositeRect,
            CompositeRectScaleFactor, CompositeRectText, CompositeRectTextRun, CompositeTreeRef,
        },
        text::FontID,
    },
    ui::dock::PaneContentResizeContext,
    uikit::{
        ContainerView, ContainerViewInit, NumericInputView, NumericInputViewIO,
        NumericInputViewInit, ScrollContainer, ScrollContainerInit, StaticTextView,
        StaticTextViewInit, TeardownContext, TextInputView, TextInputViewIO, TextInputViewInit,
        TypedViewIdentifier, View, ViewFeedbackContext, ViewFeedbackHandler,
        ViewFeedbackPerformAtomic, ViewFeedbackRegisterable, ViewIdentifier,
        ViewImmediateTeardownable, ViewInitContext, ViewInstanceQueryable,
        ViewInstanceQueryableMut, ViewLayoutChild, ViewLayoutFlowAlignment, ViewLayoutFlowBasis,
        ViewLayoutFlowDirection, ViewLayoutFlowJustify, ViewLayoutOverflow, ViewRegisterable,
        ViewRelationControllable, ViewRenderer, ViewSize, checkbox::CheckmarkVisual,
    },
    utils::{LogicalUnit, Rect, Size},
};

pub struct Presenter {
    eh: Rc<EventHandler>,
}
impl Presenter {
    pub const ID: &str = internal_pane_identifier!("Inspector");

    pub fn new(ctx: &mut ViewInitContext) -> Self {
        let eh = Rc::new_cyclic(|eh| {
            let selected_object_label = ctx.construct_view(
                StaticTextViewInit {
                    content: "No selection".into(),
                    ..Default::default()
                },
                |_| [],
            );
            let selected_object_name =
                ctx.construct_view(TextInputViewInit::new(eh.clone()), |_| []);
            ctx.view_layout_mut(selected_object_name)
                .expect("query failed")
                .width = ViewSize::FillAvailable;
            ctx.view_layout_mut(selected_object_name)
                .expect("query failed")
                .height = ViewSize::Fixed(20.0);

            let root_content_view = ctx.construct_view(ContainerViewInit, |_| {
                [
                    selected_object_label.into_untyped(),
                    selected_object_name.into_untyped(),
                ]
            });
            {
                let l = ctx
                    .view_layout_mut(root_content_view)
                    .expect("query failed");
                l.padding.set_all(8.0);
                l.child = ViewLayoutChild::Flow {
                    direction: ViewLayoutFlowDirection::Vertical,
                    alignment: ViewLayoutFlowAlignment::Start,
                    justify: ViewLayoutFlowJustify::Start,
                    overflow: ViewLayoutOverflow::Overflow,
                    gap: 0.0,
                };
            }

            let content_view = ctx.construct_view(ContainerViewInit, |_| []);
            {
                let l = ctx.view_layout_mut(content_view).expect("query failed");
                l.child = ViewLayoutChild::Flow {
                    direction: ViewLayoutFlowDirection::Vertical,
                    alignment: ViewLayoutFlowAlignment::Start,
                    justify: ViewLayoutFlowJustify::Start,
                    overflow: ViewLayoutOverflow::Overflow,
                    gap: 4.0,
                };
                l.padding.left = 8.0;
                l.padding.right = 8.0;
            }

            let label = ctx.construct_view(
                StaticTextViewInit {
                    content: "POSITION".into(),
                    font: FontID::UIFormLiftedLabel,
                    ..Default::default()
                },
                |_| [],
            );
            ctx.view_set_parent(label, content_view);
            let position_editor = Vec3EditorComponent::new(ctx, eh.clone());
            ctx.view_set_parent(position_editor.root_view, content_view);

            let label = ctx.construct_view(
                StaticTextViewInit {
                    content: "ROTATION".into(),
                    font: FontID::UIFormLiftedLabel,
                    ..Default::default()
                },
                |_| [],
            );
            ctx.view_set_parent(label, content_view);
            let rotation_editor = Vec3EditorComponent::new(ctx, eh.clone());
            ctx.view_set_parent(rotation_editor.root_view, content_view);

            let label = ctx.construct_view(
                StaticTextViewInit {
                    content: "SCALE".into(),
                    font: FontID::UIFormLiftedLabel,
                    ..Default::default()
                },
                |_| [],
            );
            ctx.view_set_parent(label, content_view);
            let scale_editor = Vec3EditorComponent::new(ctx, eh.clone());
            ctx.view_set_parent(scale_editor.root_view, content_view);

            let render_section_header = ctx.construct_view_direct(|_| {
                Box::new(SectionHeaderView::new("Render".into(), eh.clone()))
            });
            ctx.view_layout_mut(render_section_header)
                .expect("query failed")
                .width = ViewSize::FillAvailable;
            ctx.view_set_parent(render_section_header, content_view);

            let label = ctx.construct_view(
                StaticTextViewInit {
                    content: "SHAPE".into(),
                    font: FontID::UIFormLiftedLabel,
                    ..Default::default()
                },
                |_| [],
            );
            let shape_selector = ctx.construct_view_direct(|id| {
                Box::new(crate::uikit::dropdown_box::View::new(
                    id,
                    eh.clone(),
                    vec![
                        "Plane".into(),
                        "Cube".into(),
                        "Sphere".into(),
                        "Cylinder".into(),
                        "Capsule".into(),
                    ],
                ))
            });
            ctx.view_set_parent(label, content_view);
            ctx.view_set_parent(shape_selector, content_view);

            ctx.view_layout_mut(content_view)
                .expect("query failed")
                .width = ViewSize::Fixed(128.0 + 16.0);
            let items_container_view = ctx
                .construct_view(ScrollContainerInit::new(content_view), |_| {
                    [content_view.into_untyped()]
                });
            let l = ctx
                .view_layout_mut(items_container_view)
                .expect("query failed");
            l.width = ViewSize::FillAvailable;
            l.height = ViewSize::FillAvailable;
            l.flow_basis = ViewLayoutFlowBasis::Flexible(1.0);

            EventHandler {
                object_selection_changed: Cell::new(false),
                items_container_mounted: Cell::new(false),
                root_content_view,
                selected_object_label,
                selected_object_name,
                object_name_editing: Cell::new(false),
                items_container_view,
                items_content_view: content_view,
                vec3_editors: vec![position_editor, rotation_editor, scale_editor],
                render_section_header_view: render_section_header,
                render_shape_selector_view: shape_selector,
            }
        });
        eh.subscribe_view_feedbacks(ctx);

        Self { eh }
    }
}
impl crate::ui::dock::PaneContentPresenter for Presenter {
    fn id(&self) -> String {
        Self::ID.into()
    }

    fn name(&self) -> String {
        "Inspector".into()
    }

    fn root_view_id(&self) -> ViewIdentifier {
        self.eh.root_content_view.into_untyped()
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        self.eh.unsubscribe_view_feedbacks(ctx);
    }

    fn resize(&self, new_size: &Size<LogicalUnit>, context: &mut PaneContentResizeContext) {
        let content_width = new_size.width.max(128.0);
        context
            .view_layout_mut(self.eh.root_content_view)
            .expect("query failed")
            .width = ViewSize::Fixed(content_width);
        context
            .view_layout_mut(self.eh.root_content_view)
            .expect("query failed")
            .height = ViewSize::Fixed(new_size.height);
        context
            .view_layout_mut(self.eh.items_content_view)
            .expect("query failed")
            .width = ViewSize::Fixed(content_width - 16.0);
        context.schedule_view_render(self.eh.root_content_view);
    }
}

struct Vec3EditorComponent {
    root_view: TypedViewIdentifier<ContainerView>,
    x: TypedViewIdentifier<NumericInputView>,
    y: TypedViewIdentifier<NumericInputView>,
    z: TypedViewIdentifier<NumericInputView>,
}
impl Vec3EditorComponent {
    pub fn new(
        ctx: &mut (impl ViewRegisterable + ViewRelationControllable + ViewInstanceQueryableMut + ?Sized),
        value_io: std::rc::Weak<impl NumericInputViewIO + 'static>,
    ) -> Self {
        let x = ctx.construct_view(
            NumericInputViewInit {
                value: value_io.clone(),
                ..Default::default()
            },
            |_| [],
        );
        {
            let l = ctx.view_layout_mut(x).expect("query failed");
            l.flow_basis = ViewLayoutFlowBasis::Flexible(1.0);
            l.width = ViewSize::FillAvailable;
        }
        let y = ctx.construct_view(
            NumericInputViewInit {
                value: value_io.clone(),
                ..Default::default()
            },
            |_| [],
        );
        {
            let l = ctx.view_layout_mut(y).expect("query failed");
            l.flow_basis = ViewLayoutFlowBasis::Flexible(1.0);
            l.width = ViewSize::FillAvailable;
        }
        let z = ctx.construct_view(
            NumericInputViewInit {
                value: value_io.clone(),
                ..Default::default()
            },
            |_| [],
        );
        {
            let l = ctx.view_layout_mut(z).expect("query failed");
            l.flow_basis = ViewLayoutFlowBasis::Flexible(1.0);
            l.width = ViewSize::FillAvailable;
        }

        let root_view = ctx.construct_view(ContainerViewInit, |ctx| {
            [
                ctx.construct_view(
                    StaticTextViewInit {
                        content: "X".into(),
                        ..Default::default()
                    },
                    |_| [],
                )
                .into_untyped(),
                x.into_untyped(),
                ctx.construct_view(
                    StaticTextViewInit {
                        content: "Y".into(),
                        ..Default::default()
                    },
                    |_| [],
                )
                .into_untyped(),
                y.into_untyped(),
                ctx.construct_view(
                    StaticTextViewInit {
                        content: "Z".into(),
                        ..Default::default()
                    },
                    |_| [],
                )
                .into_untyped(),
                z.into_untyped(),
            ]
        });
        {
            let l = ctx.view_layout_mut(root_view).expect("query failed");
            l.width = ViewSize::FillAvailable;
            l.child = ViewLayoutChild::Flow {
                direction: ViewLayoutFlowDirection::Horizontal,
                alignment: Default::default(),
                justify: Default::default(),
                overflow: Default::default(),
                gap: 4.0,
            };
        }

        Self { root_view, x, y, z }
    }
}

struct SectionHeaderView {
    name: String,
    event_handler: std::rc::Weak<EventHandler>,
    entity: Option<Rc<SectionHeaderViewEntity>>,
    checked: bool,
    next_checked_with_transition: bool,
}
impl SectionHeaderView {
    pub fn new(name: String, event_handler: std::rc::Weak<EventHandler>) -> Self {
        Self {
            name,
            event_handler,
            entity: None,
            checked: false,
            next_checked_with_transition: true,
        }
    }

    /// Returns `true` if the checked state changed.
    pub fn set_checked(&mut self, checked: bool, with_transition: bool) -> bool {
        let changed = core::mem::replace(&mut self.checked, checked) != checked;
        self.next_checked_with_transition = with_transition;
        changed
    }
}
impl View for SectionHeaderView {
    fn render(
        &mut self,
        layout_rect: Rect<LogicalUnit>,
        ctx: &mut crate::uikit::RenderContext,
        _layout_state: &crate::uikit::ViewLayoutStateStore,
    ) -> crate::uikit::ViewRenderElements {
        let entity = match self.entity {
            Some(ref e) => {
                if core::mem::replace(&mut self.next_checked_with_transition, true) {
                    e.checkmark
                        .set(self.checked, ctx.composite_tree, ctx.current_sec);
                } else {
                    e.checkmark
                        .set_without_transition(self.checked, ctx.composite_tree);
                }

                ctx.composite_tree
                    .begin_mod_chain(e.ct_root)
                    .rect_imm(layout_rect.clone())
                    .apply();
                ctx.ht_manager.get_data_mut(e.ht_root).left = layout_rect.left;
                ctx.ht_manager.get_data_mut(e.ht_root).top = layout_rect.top;
                ctx.ht_manager.get_data_mut(e.ht_root).width = layout_rect.width;
                ctx.ht_manager.get_data_mut(e.ht_root).height = layout_rect.height;

                e
            }
            None => {
                // first render
                let ct_root = CompositeRect::build()
                    .rect_imm(layout_rect.clone())
                    .composite_fill_color_imm([1.0, 1.0, 1.0, 0.0])
                    .text(
                        CompositeRectText::build()
                            .run(
                                CompositeRectTextRun::build(self.name.clone())
                                    .color_imm([1.0, 1.0, 1.0, 1.0]),
                            )
                            .vertical_middle()
                            .shift_left(24.0),
                    )
                    .create(ctx.composite_tree);
                let ct_topline = CompositeRect::build()
                    .size_imm(0.0, 1.0)
                    .expand_width()
                    .composite_fill_color_imm([1.0, 1.0, 1.0, 0.25])
                    .create(ctx.composite_tree);
                let ct_bottomline = CompositeRect::build()
                    .size_imm(0.0, 1.0)
                    .expand_width()
                    .anchor_parent_bottom()
                    .composite_fill_color_imm([1.0, 1.0, 1.0, 0.25])
                    .create(ctx.composite_tree);
                let checkmark = CheckmarkVisual::new(
                    ctx.composite_tree,
                    ctx.main_thread_texture_id_issuer,
                    ctx.system_link.rt_sender(),
                    || CompositeRect {
                        scale_factor: CompositeRectScaleFactor::UI,
                        relative_offset_adjustment: [0.0, 0.5],
                        offset: [
                            AnimatableFloat::Value((24.0 - 12.0) * 0.5),
                            AnimatableFloat::Value(-6.0),
                        ],
                        size: [AnimatableFloat::Value(12.0), AnimatableFloat::Value(12.0)],
                        ..Default::default()
                    },
                );
                ctx.composite_tree.add_child(ct_root, ct_topline);
                ctx.composite_tree.add_child(ct_root, ct_bottomline);
                ctx.composite_tree.add_child(ct_root, checkmark.ct());

                let ht_root = ctx.ht_manager.create(HitTestTreeData {
                    left: layout_rect.left,
                    top: layout_rect.top,
                    width: layout_rect.width,
                    height: layout_rect.height,
                    cursor_shape: CursorShape::Pointer,
                    ..Default::default()
                });

                let entity = Rc::new(SectionHeaderViewEntity {
                    parent_event_handler: self.event_handler.clone(),
                    ct_root,
                    ht_root,
                    checkmark,
                });
                ctx.ht_manager.set_action_handler(ht_root, &entity);

                // 最初はトランジションなしでマークを反映する
                entity
                    .checkmark
                    .set_without_transition(self.checked, ctx.composite_tree);

                &*self.entity.insert(entity)
            }
        };

        crate::uikit::ViewRenderElements {
            composite_tree: Some(entity.ct_root),
            hit_tree: Some(entity.ht_root),
            ..crate::uikit::ViewRenderElements::EMPTY
        }
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        let Some(entity) = self.entity.take() else {
            // not rendered
            return;
        };

        ctx.composite_tree.free_all(entity.ct_root);
        ctx.ht_manager.free_all(entity.ht_root);
    }

    fn measure_preferred_content_size(
        &self,
        _ctx: &mut crate::uikit::MeasureContext,
    ) -> Size<LogicalUnit> {
        Size::new_logical(0.0, 24.0)
    }
}

struct SectionHeaderViewEntity {
    parent_event_handler: std::rc::Weak<EventHandler>,
    ct_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    checkmark: CheckmarkVisual,
}
impl HitTestTreeActionHandler for SectionHeaderViewEntity {
    fn on_pointer_enter(
        &self,
        _sender: HitTestTreeRef,
        context: &mut crate::input::InputEventContext,
        _args: &crate::input::hittest::PointerActionArgs,
    ) -> EventContinueControl {
        context
            .composite_tree
            .begin_mod_chain(self.ct_root)
            .composite_mode(CompositeMode::FillColor(AnimatableColor::Animated {
                from_value: [1.0, 1.0, 1.0, 0.0],
                to_value: [1.0, 1.0, 1.0, 0.125],
                curve: AnimationCurve::Linear,
                event_on_complete: None,
                sec_duration: (context.current_sec..context.current_sec + 0.1).into(),
            }))
            .apply();

        EventContinueControl::STOP_PROPAGATION
    }

    fn on_pointer_leave(
        &self,
        _sender: HitTestTreeRef,
        context: &mut crate::input::InputEventContext,
        _args: &crate::input::hittest::PointerActionArgs,
    ) -> EventContinueControl {
        context
            .composite_tree
            .begin_mod_chain(self.ct_root)
            .composite_mode(CompositeMode::FillColor(AnimatableColor::Animated {
                from_value: [1.0, 1.0, 1.0, 0.125],
                to_value: [1.0, 1.0, 1.0, 0.0],
                curve: AnimationCurve::Linear,
                event_on_complete: None,
                sec_duration: (context.current_sec..context.current_sec + 0.1).into(),
            }))
            .apply();

        EventContinueControl::STOP_PROPAGATION
    }

    fn on_pointer_down(
        &self,
        _sender: HitTestTreeRef,
        _context: &mut crate::input::InputEventContext,
        _args: &crate::input::hittest::PointerButtonActionArgs,
    ) -> EventContinueControl {
        EventContinueControl::STOP_PROPAGATION
    }

    fn on_pointer_up(
        &self,
        _sender: HitTestTreeRef,
        _context: &mut crate::input::InputEventContext,
        _args: &crate::input::hittest::PointerButtonActionArgs,
    ) -> EventContinueControl {
        EventContinueControl::STOP_PROPAGATION
    }

    fn on_click(
        &self,
        _sender: HitTestTreeRef,
        context: &mut crate::input::InputEventContext,
        _args: &crate::input::hittest::PointerButtonActionArgs,
    ) -> EventContinueControl {
        let Some(parent) = self.parent_event_handler.upgrade() else {
            return EventContinueControl::empty();
        };

        parent.on_toggle_render_enable(context);
        EventContinueControl::STOP_PROPAGATION
    }
}

struct EventHandler {
    object_selection_changed: Cell<bool>,
    items_container_mounted: Cell<bool>,
    root_content_view: TypedViewIdentifier<ContainerView>,
    selected_object_label: TypedViewIdentifier<StaticTextView>,
    selected_object_name: TypedViewIdentifier<TextInputView>,
    object_name_editing: Cell<bool>,
    items_container_view: TypedViewIdentifier<ScrollContainer>,
    items_content_view: TypedViewIdentifier<ContainerView>,
    vec3_editors: Vec<Vec3EditorComponent>,
    render_section_header_view: TypedViewIdentifier<SectionHeaderView>,
    render_shape_selector_view: TypedViewIdentifier<crate::uikit::dropdown_box::View>,
}
impl EventHandler {
    fn subscribe_view_feedbacks(
        self: &std::rc::Rc<Self>,
        env: &mut (impl ViewFeedbackRegisterable + ?Sized),
    ) {
        env.subscribe_view_feedback::<ViewFeedbackPerformAtomic>(self);
        env.subscribe_view_feedback::<model::ViewFeedbackObjectSelectionChanged>(self);
        env.subscribe_view_feedback::<model::ViewFeedbackObjectNameChanged>(self);
        env.subscribe_view_feedback::<model::ViewFeedbackObjectDataChanged>(self);
    }

    fn unsubscribe_view_feedbacks(
        self: &std::rc::Rc<Self>,
        env: &mut (impl ViewFeedbackRegisterable + ?Sized),
    ) {
        env.unsubscribe_view_feedback::<ViewFeedbackPerformAtomic>(self);
        env.unsubscribe_view_feedback::<model::ViewFeedbackObjectSelectionChanged>(self);
        env.unsubscribe_view_feedback::<model::ViewFeedbackObjectNameChanged>(self);
        env.unsubscribe_view_feedback::<model::ViewFeedbackObjectDataChanged>(self);
    }

    fn revalidate_all(
        &self,
        with_transition: bool,
        env: &mut (
                 impl ViewRenderer
                 + ViewInstanceQueryable
                 + ViewInstanceQueryableMut
                 + ApplicationAccess
                 + ?Sized
             ),
    ) {
        for x in self.vec3_editors.iter() {
            env.view_instance(x.x).expect("query failed").revalidate();
            env.view_instance(x.y).expect("query failed").revalidate();
            env.view_instance(x.z).expect("query failed").revalidate();
        }

        let render_enabled = model::selected_object_render_is_enabled(env);
        if env
            .view_instance_mut(self.render_section_header_view)
            .expect("query failed")
            .set_checked(render_enabled, with_transition)
        {
            // should re-render
            env.schedule_view_render(self.render_section_header_view);
        }

        env.view_instance_mut(self.render_shape_selector_view)
            .expect("query failed")
            .revalidate();
        env.schedule_view_render(self.root_content_view);
    }

    fn on_toggle_render_enable(&self, ctx: &mut (impl ApplicationMutableAccess + ?Sized)) {
        model::toggle_selected_object_render_enable(ctx);
    }
}
impl ViewFeedbackHandler<ViewFeedbackPerformAtomic> for EventHandler {
    fn accept_feedback<'a, 'h>(
        &self,
        _feedback: &ViewFeedbackPerformAtomic,
        context: &mut ViewFeedbackContext<'a, 'h>,
    ) {
        let object_selection_changed = self.object_selection_changed.replace(false);

        if object_selection_changed {
            match model::selection_state(context) {
                ObjectSelectionState::None => {
                    context
                        .view_instance_mut(self.selected_object_label)
                        .expect("query failed")
                        .set_text("No selection".into());
                    context
                        .view_instance_mut(self.selected_object_name)
                        .expect("query failed")
                        .revalidate();

                    // remove items_container_view from tree
                    context.teardown_view_recursive(self.items_container_view);
                    context.view_detach_parent(self.items_container_view);
                    self.items_container_mounted.set(false);
                }
                ObjectSelectionState::Single { id } => {
                    let object_label_text = format!("Object {id}");

                    context
                        .view_instance_mut(self.selected_object_label)
                        .expect("query failed")
                        .set_text(object_label_text);
                    context
                        .view_instance_mut(self.selected_object_name)
                        .expect("query failed")
                        .revalidate();

                    if !self.items_container_mounted.replace(true) {
                        context.view_set_parent(self.items_container_view, self.root_content_view);
                    }

                    self.revalidate_all(false, context);
                }
                ObjectSelectionState::Multiple => {
                    context
                        .view_instance_mut(self.selected_object_label)
                        .expect("query failed")
                        .set_text("Multiple selection".into());
                    context
                        .view_instance_mut(self.selected_object_name)
                        .expect("query failed")
                        .revalidate();

                    // remove items_container_view from tree
                    context.teardown_view_recursive(self.items_container_view);
                    context.view_detach_parent(self.items_container_view);
                    self.items_container_mounted.set(false);
                }
            }

            context.schedule_view_render(self.root_content_view);
        }
    }
}
impl ViewFeedbackHandler<model::ViewFeedbackObjectSelectionChanged> for EventHandler {
    fn accept_feedback<'a, 'h>(
        &self,
        _feedback: &model::ViewFeedbackObjectSelectionChanged,
        _context: &mut ViewFeedbackContext<'a, 'h>,
    ) {
        self.object_selection_changed.set(true);
    }
}
impl ViewFeedbackHandler<model::ViewFeedbackObjectNameChanged> for EventHandler {
    fn accept_feedback<'a, 'h>(
        &self,
        _feedback: &model::ViewFeedbackObjectNameChanged,
        context: &mut ViewFeedbackContext<'a, 'h>,
    ) {
        if !self.object_name_editing.replace(false) {
            // 自分以外からの変更通知
            context
                .view_instance_mut(self.selected_object_name)
                .expect("query failed")
                .revalidate();
            context.schedule_view_render(self.selected_object_name);
        }
    }
}
impl ViewFeedbackHandler<model::ViewFeedbackObjectDataChanged> for EventHandler {
    fn accept_feedback<'a, 'h>(
        &self,
        _feedback: &model::ViewFeedbackObjectDataChanged,
        context: &mut ViewFeedbackContext<'a, 'h>,
    ) {
        self.revalidate_all(true, context);
    }
}
impl TextInputViewIO for EventHandler {
    fn text(&self, requester: ViewIdentifier, application: &Application) -> String {
        if requester == self.vec3_editors[0].x {
            // pos x
            format!(
                "{:.3}",
                model::selected_object_local_translate_x(application)
            )
        } else if requester == self.vec3_editors[0].y {
            // pos y
            format!(
                "{:.3}",
                model::selected_object_local_translate_y(application)
            )
        } else if requester == self.vec3_editors[0].z {
            // pos z
            format!(
                "{:.3}",
                model::selected_object_local_translate_z(application)
            )
        } else if requester == self.vec3_editors[1].x {
            // rotate x
            format!("{:.3}", model::selected_object_local_rotate_x(application))
        } else if requester == self.vec3_editors[1].y {
            // rotate y
            format!("{:.3}", model::selected_object_local_rotate_y(application))
        } else if requester == self.vec3_editors[1].z {
            // rotate z
            format!("{:.3}", model::selected_object_local_rotate_z(application))
        } else if requester == self.vec3_editors[2].x {
            // scale x
            format!("{:.3}", model::selected_object_local_scale_x(application))
        } else if requester == self.vec3_editors[2].y {
            // scale y
            format!("{:.3}", model::selected_object_local_scale_y(application))
        } else if requester == self.vec3_editors[2].z {
            // scale z
            format!("{:.3}", model::selected_object_local_scale_z(application))
        } else if requester == self.selected_object_name {
            model::selected_object_name(application)
                .unwrap_or("")
                .into()
        } else {
            "-".into()
        }
    }

    fn set_text(
        &self,
        sender: ViewIdentifier,
        application: &mut ApplicationMutation,
        input: String,
    ) {
        if sender == self.vec3_editors[0].x {
            // pos x
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            model::set_selected_object_local_translate_x(application, v);
        } else if sender == self.vec3_editors[0].y {
            // pos y
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            model::set_selected_object_local_translate_y(application, v);
        } else if sender == self.vec3_editors[0].z {
            // pos z
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            model::set_selected_object_local_translate_z(application, v);
        } else if sender == self.vec3_editors[1].x {
            // rotate x
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            model::set_selected_object_local_rotation_x(application, v);
        } else if sender == self.vec3_editors[1].y {
            // rotate y
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            model::set_selected_object_local_rotation_y(application, v);
        } else if sender == self.vec3_editors[1].z {
            // rotate z
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            model::set_selected_object_local_rotation_z(application, v);
        } else if sender == self.vec3_editors[2].x {
            // scale x
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            model::set_selected_object_local_scale_x(application, v);
        } else if sender == self.vec3_editors[2].y {
            // scale y
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            model::set_selected_object_local_scale_y(application, v);
        } else if sender == self.vec3_editors[2].z {
            // scale z
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            model::set_selected_object_local_scale_z(application, v);
        } else if sender == self.selected_object_name {
            // Note: compositioning中にテキストセットするのを想定してないのでループバックしてこないようにする
            self.object_name_editing.set(true);
            model::set_selected_object_name(application, input);
        }
    }
}
impl NumericInputViewIO for EventHandler {
    fn set_delta(&self, sender: ViewIdentifier, application: &mut ApplicationMutation, delta: f32) {
        if sender == self.vec3_editors[0].x {
            // pos x
            model::apply_selected_object_local_translate_delta(
                application,
                Vector3(delta * 0.1, 0.0, 0.0),
            );
        } else if sender == self.vec3_editors[0].y {
            // pos y
            model::apply_selected_object_local_translate_delta(
                application,
                Vector3(0.0, delta * 0.1, 0.0),
            );
        } else if sender == self.vec3_editors[0].z {
            // pos z
            model::apply_selected_object_local_translate_delta(
                application,
                Vector3(0.0, 0.0, delta * 0.1),
            );
        } else if sender == self.vec3_editors[1].x {
            // rotate x
            model::apply_selected_object_local_rotate_delta(application, Vector3(delta, 0.0, 0.0));
        } else if sender == self.vec3_editors[1].y {
            // rotate y
            model::apply_selected_object_local_rotate_delta(application, Vector3(0.0, delta, 0.0));
        } else if sender == self.vec3_editors[1].z {
            // rotate z
            model::apply_selected_object_local_rotate_delta(application, Vector3(0.0, 0.0, delta));
        } else if sender == self.vec3_editors[2].x {
            // scale x
            model::apply_selected_object_local_scale_delta(
                application,
                Vector3(delta * 0.1, 0.0, 0.0),
            );
        } else if sender == self.vec3_editors[2].y {
            // scale y
            model::apply_selected_object_local_scale_delta(
                application,
                Vector3(0.0, delta * 0.1, 0.0),
            );
        } else if sender == self.vec3_editors[2].z {
            // scale z
            model::apply_selected_object_local_scale_delta(
                application,
                Vector3(0.0, 0.0, delta * 0.1),
            );
        }
    }
}
impl crate::uikit::dropdown_box::IO for EventHandler {
    fn selected_index(&self, requester: ViewIdentifier, application: &Application) -> usize {
        if requester == self.render_shape_selector_view {
            return model::selected_object_render_shape(application)
                .unwrap_or(model::ObjectRenderShape::Cube) as _;
        }

        tracing::warn!(?requester, "receiving from unknown view");
        0
    }

    fn on_selected_index_change(
        &self,
        sender: ViewIdentifier,
        index: usize,
        application: &mut ApplicationMutation,
    ) {
        if sender == self.render_shape_selector_view {
            model::set_selected_object_render_shape(application, unsafe {
                core::mem::transmute(u8::try_from(index).expect("too large value"))
            });
            return;
        }

        tracing::warn!(?sender, "receiving from unknown view");
    }
}
