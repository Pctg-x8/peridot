use std::{cell::Cell, rc::Rc};

use peridot_math::Vector3;

use crate::{
    input::{
        EventContinueControl,
        hittest::{CursorShape, HitTestTreeActionHandler, HitTestTreeData, HitTestTreeRef},
    },
    model::{
        Application, ApplicationMutableAccess, ApplicationMutation, ObjectSelectionState,
        ViewFeedbackObjectDataChanged, ViewFeedbackObjectNameChanged,
        ViewFeedbackObjectSelectionChanged,
    },
    rendering::{
        composite::{
            AnimatableColor, AnimatableFloat, AnimationCurve, CompositeMode, CompositeRect,
            CompositeRectScaleFactor, CompositeRectText, CompositeRectTextRun,
            CompositeRectTextVerticalAlignment, CompositeTreeRef,
        },
        text::FontID,
    },
    ui::dock::PaneContentResizeContext,
    uikit::{
        ContainerView, NumericInputView, NumericInputViewIO, NumericInputViewInit, ScrollContainer,
        StaticTextView, TeardownContext, TextInputView, TextInputViewIO, View, ViewFeedbackContext,
        ViewFeedbackHandler, ViewFeedbackPerformAtomic, ViewFeedbackRegisterable, ViewIdentifier,
        ViewImmediateTeardownable, ViewInitContext, ViewInstanceQueryable,
        ViewInstanceQueryableMut, ViewLayoutChild, ViewLayoutFlowAlignment, ViewLayoutFlowBasis,
        ViewLayoutFlowDirection, ViewLayoutFlowJustify, ViewLayoutOverflow, ViewRegisterable,
        ViewRelationControllable, ViewRenderer, ViewSize, checkbox::CheckmarkVisual,
    },
    utils::{LogicalUnit, Point, Rect, Size},
};

pub struct Presenter {
    eh: Rc<EventHandler>,
}
impl Presenter {
    pub const ID: &str = internal_pane_identifier!("Inspector");

    pub fn new(ctx: &mut ViewInitContext) -> Self {
        let eh = Rc::new_cyclic(|eh| {
            let root_content_view = ctx.construct_view(|_| Box::new(ContainerView));
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

            let selected_object_label =
                ctx.construct_view(|_| Box::new(StaticTextView::new("No selection".into())));
            let selected_object_name =
                ctx.construct_view(|id| Box::new(TextInputView::new(id, eh.clone())));
            ctx.view_layout_mut(selected_object_name)
                .expect("query failed")
                .width = ViewSize::FillAvailable;
            ctx.view_layout_mut(selected_object_name)
                .expect("query failed")
                .height = ViewSize::Fixed(20.0);
            ctx.view_set_parent(selected_object_label, root_content_view);
            ctx.view_set_parent(selected_object_name, root_content_view);

            let content_view = ctx.construct_view(|_| Box::new(ContainerView));
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

            let label = ctx.construct_view(|_| {
                let mut v = Box::new(StaticTextView::new("POSITION".into()));
                v.set_font(FontID::UIFormLiftedLabel);
                v
            });
            ctx.view_set_parent(label, content_view);
            let position_editor = Vec3EditorComponent::new(ctx, eh.clone());
            ctx.view_set_parent(position_editor.root_view, content_view);

            let label = ctx.construct_view(|_| {
                let mut v = Box::new(StaticTextView::new("ROTATION".into()));
                v.set_font(FontID::UIFormLiftedLabel);
                v
            });
            ctx.view_set_parent(label, content_view);
            let rotation_editor = Vec3EditorComponent::new(ctx, eh.clone());
            ctx.view_set_parent(rotation_editor.root_view, content_view);

            let label = ctx.construct_view(|_| {
                let mut v = Box::new(StaticTextView::new("SCALE".into()));
                v.set_font(FontID::UIFormLiftedLabel);
                v
            });
            ctx.view_set_parent(label, content_view);
            let scale_editor = Vec3EditorComponent::new(ctx, eh.clone());
            ctx.view_set_parent(scale_editor.root_view, content_view);

            let render_section_header = ctx
                .construct_view(|_| Box::new(SectionHeaderView::new("Render".into(), eh.clone())));
            ctx.view_layout_mut(render_section_header)
                .expect("query failed")
                .width = ViewSize::FillAvailable;
            ctx.view_set_parent(render_section_header, content_view);

            let label = ctx.construct_view(|_| {
                let mut v = Box::new(StaticTextView::new("SHAPE".into()));
                v.set_font(FontID::UIFormLiftedLabel);
                v
            });
            let shape_selector = ctx.construct_view(|_| {
                Box::new(crate::uikit::dropdown_box::View::new(vec![
                    "Cube".into(),
                    "Sphere".into(),
                    "Cylinder".into(),
                    "Capsule".into(),
                ]))
            });
            ctx.view_set_parent(label, content_view);
            ctx.view_set_parent(shape_selector, content_view);

            ctx.view_layout_mut(content_view)
                .expect("query failed")
                .width = ViewSize::Fixed(128.0 + 16.0);
            let items_container_view = ctx.construct_view(|id| {
                Box::new(ScrollContainer::new(
                    id,
                    Rect::from_lt_size(
                        Point::new_logical(0.0, 8.0 + 12.0 + 20.0 + 8.0),
                        Size::new_logical(128.0, 128.0),
                    ),
                    content_view,
                ))
            });
            ctx.view_set_parent(content_view, items_container_view);

            ctx.view_layout_mut(root_content_view)
                .expect("query failed")
                .width = ViewSize::Fixed(128.0);

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
                numeric_input_view_ids: vec![],
                render_section_header_view: render_section_header,
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
        self.eh.root_content_view
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
            .view_layout_mut(self.eh.items_content_view)
            .expect("query failed")
            .width = ViewSize::Fixed(content_width);
        context
            .view_instance_mut::<ScrollContainer>(self.eh.items_container_view)
            .expect("query failed")
            .resize(Size::new_logical(
                new_size.width,
                new_size.height - 8.0 - 12.0 - 12.0 - 8.0,
            ));
        context.schedule_view_render(self.eh.root_content_view);
    }
}

struct Vec3EditorComponent {
    root_view: ViewIdentifier,
    x: ViewIdentifier,
    y: ViewIdentifier,
    z: ViewIdentifier,
}
impl Vec3EditorComponent {
    pub fn new(
        ctx: &mut (impl ViewRegisterable + ViewRelationControllable + ViewInstanceQueryableMut + ?Sized),
        value_io: std::rc::Weak<impl NumericInputViewIO + 'static>,
    ) -> Self {
        let root_view = ctx.construct_view(|_| Box::new(ContainerView));
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

        let label = ctx.construct_view(|_| Box::new(StaticTextView::new("X".into())));
        ctx.view_set_parent(label, root_view);
        let x = ctx.construct_view(|id| {
            Box::new(NumericInputView::new(
                id,
                NumericInputViewInit {
                    value: value_io.clone(),
                    ..Default::default()
                },
            ))
        });
        {
            let l = ctx.view_layout_mut(x).expect("query failed");
            l.flow_basis = ViewLayoutFlowBasis::Flexible(1.0);
            l.width = ViewSize::FillAvailable;
        }
        ctx.view_set_parent(x, root_view);
        let label = ctx.construct_view(|_| Box::new(StaticTextView::new("Y".into())));
        ctx.view_set_parent(label, root_view);
        let y = ctx.construct_view(|id| {
            Box::new(NumericInputView::new(
                id,
                NumericInputViewInit {
                    value: value_io.clone(),
                    ..Default::default()
                },
            ))
        });
        {
            let l = ctx.view_layout_mut(y).expect("query failed");
            l.flow_basis = ViewLayoutFlowBasis::Flexible(1.0);
            l.width = ViewSize::FillAvailable;
        }
        ctx.view_set_parent(y, root_view);
        let label = ctx.construct_view(|_| Box::new(StaticTextView::new("Z".into())));
        ctx.view_set_parent(label, root_view);
        let z = ctx.construct_view(|id| {
            Box::new(NumericInputView::new(
                id,
                NumericInputViewInit {
                    value: value_io.clone(),
                    ..Default::default()
                },
            ))
        });
        {
            let l = ctx.view_layout_mut(z).expect("query failed");
            l.flow_basis = ViewLayoutFlowBasis::Flexible(1.0);
            l.width = ViewSize::FillAvailable;
        }
        ctx.view_set_parent(z, root_view);

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

                e
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
                    text: Some(CompositeRectText {
                        runs: vec![CompositeRectTextRun {
                            content: self.name.clone(),
                            color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                            ..Default::default()
                        }],
                        vertical_alignment: CompositeRectTextVerticalAlignment::Middle,
                        offset: [24.0, 0.0],
                        ..Default::default()
                    }),
                    ..Default::default()
                });
                let ct_topline = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    size: [AnimatableFloat::Value(0.0), AnimatableFloat::Value(1.0)],
                    relative_size_adjustment: [1.0, 0.0],
                    has_bitmap: true,
                    composite_mode: CompositeMode::FillColor(AnimatableColor::Value([
                        1.0, 1.0, 1.0, 0.25,
                    ])),
                    ..Default::default()
                });
                let ct_bottomline = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    size: [AnimatableFloat::Value(0.0), AnimatableFloat::Value(1.0)],
                    relative_size_adjustment: [1.0, 0.0],
                    relative_offset_adjustment: [0.0, 1.0],
                    has_bitmap: true,
                    composite_mode: CompositeMode::FillColor(AnimatableColor::Value([
                        1.0, 1.0, 1.0, 0.25,
                    ])),
                    ..Default::default()
                });
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

        ctx.mount_context.composite_tree.free_all(entity.ct_root);
        ctx.mount_context.ht_manager.free_all(entity.ht_root);
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
    root_content_view: ViewIdentifier,
    selected_object_label: ViewIdentifier,
    selected_object_name: ViewIdentifier,
    object_name_editing: Cell<bool>,
    items_container_view: ViewIdentifier,
    items_content_view: ViewIdentifier,
    vec3_editors: Vec<Vec3EditorComponent>,
    numeric_input_view_ids: Vec<ViewIdentifier>,
    render_section_header_view: ViewIdentifier,
}
impl EventHandler {
    fn subscribe_view_feedbacks(
        self: &std::rc::Rc<Self>,
        env: &mut (impl ViewFeedbackRegisterable + ?Sized),
    ) {
        env.subscribe_view_feedback::<ViewFeedbackPerformAtomic>(self);
        env.subscribe_view_feedback::<ViewFeedbackObjectSelectionChanged>(self);
        env.subscribe_view_feedback::<ViewFeedbackObjectNameChanged>(self);
        env.subscribe_view_feedback::<ViewFeedbackObjectDataChanged>(self);
    }

    fn unsubscribe_view_feedbacks(
        self: &std::rc::Rc<Self>,
        env: &mut (impl ViewFeedbackRegisterable + ?Sized),
    ) {
        env.unsubscribe_view_feedback::<ViewFeedbackPerformAtomic>(self);
        env.unsubscribe_view_feedback::<ViewFeedbackObjectSelectionChanged>(self);
        env.unsubscribe_view_feedback::<ViewFeedbackObjectNameChanged>(self);
        env.unsubscribe_view_feedback::<ViewFeedbackObjectDataChanged>(self);
    }

    fn on_toggle_render_enable(&self, ctx: &mut (impl ApplicationMutableAccess + ?Sized)) {
        crate::model::toggle_selected_object_render_enable(ctx);
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
            match crate::model::selection_state(context) {
                ObjectSelectionState::None => {
                    context
                        .view_instance_mut::<StaticTextView>(self.selected_object_label)
                        .expect("query failed")
                        .set_text("No selection".into());
                    context
                        .view_instance_mut::<TextInputView>(self.selected_object_name)
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
                        .view_instance_mut::<StaticTextView>(self.selected_object_label)
                        .expect("query failed")
                        .set_text(object_label_text);
                    context
                        .view_instance_mut::<TextInputView>(self.selected_object_name)
                        .expect("query failed")
                        .revalidate();

                    if !self.items_container_mounted.replace(true) {
                        context.view_set_parent(self.items_container_view, self.root_content_view);
                    }

                    for x in self.vec3_editors.iter() {
                        context
                            .view_instance::<NumericInputView>(x.x)
                            .expect("query failed")
                            .revalidate();
                        context
                            .view_instance::<NumericInputView>(x.y)
                            .expect("query failed")
                            .revalidate();
                        context
                            .view_instance::<NumericInputView>(x.z)
                            .expect("query failed")
                            .revalidate();
                    }

                    for &x in self.numeric_input_view_ids.iter() {
                        context
                            .view_instance::<NumericInputView>(x)
                            .expect("query failed")
                            .revalidate();
                    }

                    let render_enabled = crate::model::selected_object_render_is_enabled(context);
                    if context
                        .view_instance_mut::<SectionHeaderView>(self.render_section_header_view)
                        .expect("query failed")
                        .set_checked(render_enabled, false)
                    {
                        // should re-render
                        context.schedule_view_render(self.render_section_header_view);
                    }
                }
                ObjectSelectionState::Multiple => {
                    context
                        .view_instance_mut::<StaticTextView>(self.selected_object_label)
                        .expect("query failed")
                        .set_text("Multiple selection".into());
                    context
                        .view_instance_mut::<TextInputView>(self.selected_object_name)
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
impl ViewFeedbackHandler<ViewFeedbackObjectSelectionChanged> for EventHandler {
    fn accept_feedback<'a, 'h>(
        &self,
        _feedback: &ViewFeedbackObjectSelectionChanged,
        _context: &mut ViewFeedbackContext<'a, 'h>,
    ) {
        self.object_selection_changed.set(true);
    }
}
impl ViewFeedbackHandler<ViewFeedbackObjectNameChanged> for EventHandler {
    fn accept_feedback<'a, 'h>(
        &self,
        _feedback: &ViewFeedbackObjectNameChanged,
        context: &mut ViewFeedbackContext<'a, 'h>,
    ) {
        if !self.object_name_editing.replace(false) {
            // 自分以外からの変更通知
            context
                .view_instance_mut::<TextInputView>(self.selected_object_name)
                .expect("query failed")
                .revalidate();
            context.schedule_view_render(self.selected_object_name);
        }
    }
}
impl ViewFeedbackHandler<ViewFeedbackObjectDataChanged> for EventHandler {
    fn accept_feedback<'a, 'h>(
        &self,
        _feedback: &ViewFeedbackObjectDataChanged,
        context: &mut ViewFeedbackContext<'a, 'h>,
    ) {
        let render_enabled = crate::model::selected_object_render_is_enabled(context);
        if context
            .view_instance_mut::<SectionHeaderView>(self.render_section_header_view)
            .expect("query failed")
            .set_checked(render_enabled, true)
        {
            // should re-render
            context.schedule_view_render(self.render_section_header_view);
        }
    }
}
impl TextInputViewIO for EventHandler {
    fn text(&self, requester: ViewIdentifier, application: &Application) -> String {
        if requester == self.vec3_editors[0].x {
            // pos x
            format!(
                "{:.3}",
                crate::model::selected_object_local_translate_x(application)
            )
        } else if requester == self.vec3_editors[0].y {
            // pos y
            format!(
                "{:.3}",
                crate::model::selected_object_local_translate_y(application)
            )
        } else if requester == self.vec3_editors[0].z {
            // pos z
            format!(
                "{:.3}",
                crate::model::selected_object_local_translate_z(application)
            )
        } else if requester == self.vec3_editors[1].x {
            // rotate x
            format!(
                "{:.3}",
                crate::model::selected_object_local_rotate_x(application)
            )
        } else if requester == self.vec3_editors[1].y {
            // rotate y
            format!(
                "{:.3}",
                crate::model::selected_object_local_rotate_y(application)
            )
        } else if requester == self.vec3_editors[1].z {
            // rotate z
            format!(
                "{:.3}",
                crate::model::selected_object_local_rotate_z(application)
            )
        } else if requester == self.vec3_editors[2].x {
            // scale x
            format!(
                "{:.3}",
                crate::model::selected_object_local_scale_x(application)
            )
        } else if requester == self.vec3_editors[2].y {
            // scale y
            format!(
                "{:.3}",
                crate::model::selected_object_local_scale_y(application)
            )
        } else if requester == self.vec3_editors[2].z {
            // scale z
            format!(
                "{:.3}",
                crate::model::selected_object_local_scale_z(application)
            )
        } else if requester == self.selected_object_name {
            crate::model::selected_object_name(application)
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
            crate::model::set_selected_object_local_translate_x(application, v);
        } else if sender == self.vec3_editors[0].y {
            // pos y
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            crate::model::set_selected_object_local_translate_y(application, v);
        } else if sender == self.vec3_editors[0].z {
            // pos z
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            crate::model::set_selected_object_local_translate_z(application, v);
        } else if sender == self.vec3_editors[1].x {
            // rotate x
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            crate::model::set_selected_object_local_rotation_x(application, v);
        } else if sender == self.vec3_editors[1].y {
            // rotate y
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            crate::model::set_selected_object_local_rotation_y(application, v);
        } else if sender == self.vec3_editors[1].z {
            // rotate z
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            crate::model::set_selected_object_local_rotation_z(application, v);
        } else if sender == self.vec3_editors[2].x {
            // scale x
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            crate::model::set_selected_object_local_scale_x(application, v);
        } else if sender == self.vec3_editors[2].y {
            // scale y
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            crate::model::set_selected_object_local_scale_y(application, v);
        } else if sender == self.vec3_editors[2].z {
            // scale z
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            crate::model::set_selected_object_local_scale_z(application, v);
        } else if sender == self.selected_object_name {
            // Note: compositioning中にテキストセットするのを想定してないのでループバックしてこないようにする
            self.object_name_editing.set(true);
            crate::model::set_selected_object_name(application, input);
        }
    }
}
impl NumericInputViewIO for EventHandler {
    fn set_delta(&self, sender: ViewIdentifier, application: &mut ApplicationMutation, delta: f32) {
        if sender == self.vec3_editors[0].x {
            // pos x
            crate::model::apply_selected_object_local_translate_delta(
                application,
                Vector3(delta * 0.1, 0.0, 0.0),
            );
        } else if sender == self.vec3_editors[0].y {
            // pos y
            crate::model::apply_selected_object_local_translate_delta(
                application,
                Vector3(0.0, delta * 0.1, 0.0),
            );
        } else if sender == self.vec3_editors[0].z {
            // pos z
            crate::model::apply_selected_object_local_translate_delta(
                application,
                Vector3(0.0, 0.0, delta * 0.1),
            );
        } else if sender == self.vec3_editors[1].x {
            // rotate x
            crate::model::apply_selected_object_local_rotate_delta(
                application,
                Vector3(delta, 0.0, 0.0),
            );
        } else if sender == self.vec3_editors[1].y {
            // rotate y
            crate::model::apply_selected_object_local_rotate_delta(
                application,
                Vector3(0.0, delta, 0.0),
            );
        } else if sender == self.vec3_editors[1].z {
            // rotate z
            crate::model::apply_selected_object_local_rotate_delta(
                application,
                Vector3(0.0, 0.0, delta),
            );
        } else if sender == self.vec3_editors[2].x {
            // scale x
            crate::model::apply_selected_object_local_scale_delta(
                application,
                Vector3(delta * 0.1, 0.0, 0.0),
            );
        } else if sender == self.vec3_editors[2].y {
            // scale y
            crate::model::apply_selected_object_local_scale_delta(
                application,
                Vector3(0.0, delta * 0.1, 0.0),
            );
        } else if sender == self.vec3_editors[2].z {
            // scale z
            crate::model::apply_selected_object_local_scale_delta(
                application,
                Vector3(0.0, 0.0, delta * 0.1),
            );
        }
    }
}
