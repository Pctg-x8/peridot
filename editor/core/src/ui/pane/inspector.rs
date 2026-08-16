use std::{cell::Cell, rc::Rc};

use crate::{
    Application, ApplicationMutation, ViewFeedbackObjectSelectionChanged,
    rendering::text::FontID,
    ui::dock::PaneContentResizeContext,
    uikit::{
        CheckboxView, ContainerView, NumericInputView, NumericInputViewIO, NumericInputViewInit,
        ScrollContainer, StaticTextView, TeardownContext, TextInputViewIO, ViewFeedbackContext,
        ViewFeedbackHandler, ViewFeedbackPerformAtomic, ViewIdentifier, ViewInitContext,
        ViewInstanceQueryableMut, ViewLayoutChild, ViewLayoutFlowAlignment, ViewLayoutFlowBasis,
        ViewLayoutFlowDirection, ViewLayoutFlowJustify, ViewLayoutOverflow, ViewRegisterable,
        ViewRelationControllable, ViewRenderer, ViewSize,
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
            let selected_object_name_label =
                ctx.construct_view(|_| Box::new(StaticTextView::new(String::new())));
            ctx.view_set_parent(selected_object_label, root_content_view);
            ctx.view_set_parent(selected_object_name_label, root_content_view);

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

            let render_checkbox = ctx.construct_view(|_| Box::new(CheckboxView::new()));
            let section_label =
                ctx.construct_view(|_| Box::new(StaticTextView::new("Render".into())));
            ctx.view_set_parent(render_checkbox, content_view);
            ctx.view_set_parent(section_label, content_view);

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
                        Point::new_logical(0.0, 8.0 + 12.0 + 12.0 + 8.0),
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
                selected_object_name_label,
                items_container_view,
                items_content_view: content_view,
                vec3_editors: vec![position_editor, rotation_editor, scale_editor],
                numeric_input_view_ids: vec![],
            }
        });
        ctx.subscribe_view_feedback::<ViewFeedbackPerformAtomic>(&eh);
        ctx.subscribe_view_feedback::<ViewFeedbackObjectSelectionChanged>(&eh);

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
        ctx.unsubscribe_view_feedback::<ViewFeedbackPerformAtomic>(&self.eh);
        ctx.unsubscribe_view_feedback::<ViewFeedbackObjectSelectionChanged>(&self.eh);
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

struct EventHandler {
    object_selection_changed: Cell<bool>,
    items_container_mounted: Cell<bool>,
    root_content_view: ViewIdentifier,
    selected_object_label: ViewIdentifier,
    selected_object_name_label: ViewIdentifier,
    items_container_view: ViewIdentifier,
    items_content_view: ViewIdentifier,
    vec3_editors: Vec<Vec3EditorComponent>,
    numeric_input_view_ids: Vec<ViewIdentifier>,
}
impl ViewFeedbackHandler<ViewFeedbackPerformAtomic> for EventHandler {
    fn accept_feedback<'a, 'h>(
        &self,
        _feedback: &ViewFeedbackPerformAtomic,
        context: &mut ViewFeedbackContext<'a, 'h>,
    ) {
        let object_selection_changed = self.object_selection_changed.replace(false);

        if object_selection_changed {
            match context.application.selected_objects.len() {
                0 => {
                    context
                        .view_instance_mut::<StaticTextView>(self.selected_object_label)
                        .expect("query failed")
                        .set_text("No selection".into());
                    context
                        .view_instance_mut::<StaticTextView>(self.selected_object_name_label)
                        .expect("query failed")
                        .set_text(String::new());

                    // remove items_container_view from tree
                    context.teardown_view_recursive(self.items_container_view);
                    context.view_detach_parent(self.items_container_view);
                    self.items_container_mounted.set(false);
                }
                1 => {
                    let id = *unsafe {
                        context
                            .application
                            .selected_objects
                            .iter()
                            .next()
                            .unwrap_unchecked()
                    };
                    context
                        .view_instance_mut::<StaticTextView>(self.selected_object_label)
                        .expect("query failed")
                        .set_text(format!("Object {id}"));
                    let name_label_text = format!("Name: {}", context.application.object(id).name);
                    context
                        .view_instance_mut::<StaticTextView>(self.selected_object_name_label)
                        .expect("query failed")
                        .set_text(name_label_text);

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
                }
                _ => {
                    context
                        .view_instance_mut::<StaticTextView>(self.selected_object_label)
                        .expect("query failed")
                        .set_text("Multiple selection".into());
                    context
                        .view_instance_mut::<StaticTextView>(self.selected_object_name_label)
                        .expect("query failed")
                        .set_text(String::new());

                    // remove items_container_view from tree
                    context.teardown_view_recursive(self.items_container_view);
                    context.view_detach_parent(self.items_container_view);
                    self.items_container_mounted.set(false);
                }
            }

            context.schedule_render(self.root_content_view);
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
impl TextInputViewIO for EventHandler {
    fn text(&self, requester: ViewIdentifier, application: &Application) -> String {
        // TODO: multi-select
        let Some(&selected) = application.selected_objects.iter().next() else {
            return "-".into();
        };

        if requester == self.vec3_editors[0].x {
            // pos x
            format!("{:.3}", application.object(selected).local_position.0)
        } else if requester == self.vec3_editors[0].y {
            // pos y
            format!("{:.3}", application.object(selected).local_position.1)
        } else if requester == self.vec3_editors[0].z {
            // pos z
            format!("{:.3}", application.object(selected).local_position.2)
        } else if requester == self.vec3_editors[1].x {
            // rotate x
            format!("{:.3}", application.object(selected).local_rotation_euler.0)
        } else if requester == self.vec3_editors[1].y {
            // rotate y
            format!("{:.3}", application.object(selected).local_rotation_euler.1)
        } else if requester == self.vec3_editors[1].z {
            // rotate z
            format!("{:.3}", application.object(selected).local_rotation_euler.2)
        } else if requester == self.vec3_editors[2].x {
            // scale x
            format!("{:.3}", application.object(selected).local_scale.0)
        } else if requester == self.vec3_editors[2].y {
            // scale y
            format!("{:.3}", application.object(selected).local_scale.1)
        } else if requester == self.vec3_editors[2].z {
            // scale z
            format!("{:.3}", application.object(selected).local_scale.2)
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
        // TODO: multi-select
        let Some(&selected) = application.selected_objects.iter().next() else {
            return;
        };

        if sender == self.vec3_editors[0].x {
            // pos x
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            application.object_modify_data(selected, |o| o.local_position.0 = v);
        } else if sender == self.vec3_editors[0].y {
            // pos y
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            application.object_modify_data(selected, |o| o.local_position.1 = v);
        } else if sender == self.vec3_editors[0].z {
            // pos z
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            application.object_modify_data(selected, |o| o.local_position.2 = v);
        } else if sender == self.vec3_editors[1].x {
            // rotate x
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            application.object_modify_data(selected, |o| o.local_rotation_euler.0 = v);
        } else if sender == self.vec3_editors[1].y {
            // rotate y
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            application.object_modify_data(selected, |o| o.local_rotation_euler.1 = v);
        } else if sender == self.vec3_editors[1].z {
            // rotate z
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            application.object_modify_data(selected, |o| o.local_rotation_euler.2 = v);
        } else if sender == self.vec3_editors[2].x {
            // scale x
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            application.object_modify_data(selected, |o| o.local_scale.0 = v);
        } else if sender == self.vec3_editors[2].y {
            // scale y
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            application.object_modify_data(selected, |o| o.local_scale.1 = v);
        } else if sender == self.vec3_editors[2].z {
            // scale z
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            application.object_modify_data(selected, |o| o.local_scale.2 = v);
        }
    }
}
impl NumericInputViewIO for EventHandler {
    fn set_delta(&self, sender: ViewIdentifier, application: &mut ApplicationMutation, delta: f32) {
        // TODO: multi-select
        let Some(&selected) = application.selected_objects.iter().next() else {
            return;
        };

        if sender == self.vec3_editors[0].x {
            // pos x
            application.object_modify_data(selected, |o| o.local_position.0 += delta * 0.1);
        } else if sender == self.vec3_editors[0].y {
            // pos y
            application.object_modify_data(selected, |o| o.local_position.1 += delta * 0.1);
        } else if sender == self.vec3_editors[0].z {
            // pos z
            application.object_modify_data(selected, |o| o.local_position.2 += delta * 0.1);
        } else if sender == self.vec3_editors[1].x {
            // rotate x
            application.object_modify_data(selected, |o| o.local_rotation_euler.0 += delta);
        } else if sender == self.vec3_editors[1].y {
            // rotate y
            application.object_modify_data(selected, |o| o.local_rotation_euler.1 += delta);
        } else if sender == self.vec3_editors[1].z {
            // rotate z
            application.object_modify_data(selected, |o| o.local_rotation_euler.2 += delta);
        } else if sender == self.vec3_editors[2].x {
            // scale x
            application.object_modify_data(selected, |o| o.local_scale.0 += delta * 0.1);
        } else if sender == self.vec3_editors[2].y {
            // scale y
            application.object_modify_data(selected, |o| o.local_scale.1 += delta * 0.1);
        } else if sender == self.vec3_editors[2].z {
            // scale z
            application.object_modify_data(selected, |o| o.local_scale.2 += delta * 0.1);
        }
    }
}
