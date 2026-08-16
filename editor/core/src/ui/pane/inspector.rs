use std::{cell::Cell, rc::Rc};

use crate::{
    Application, ApplicationMutation, ViewFeedbackObjectSelectionChanged,
    rendering::text::FontID,
    ui::dock::PaneContentResizeContext,
    uikit::{
        CheckboxView, ContainerView, NumericInputView, NumericInputViewIO, NumericInputViewInit,
        ScrollContainer, StaticTextView, TeardownContext, TextInputViewIO, ViewFeedbackContext,
        ViewFeedbackHandler, ViewFeedbackPerformAtomic, ViewIdentifier, ViewInitContext,
        ViewInstanceQueryableMut, ViewLayoutChild, ViewLayoutFlowAlignment,
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
        let root_content_view = ctx.construct_view(|_| Box::new(ContainerView));

        let selected_object_label =
            ctx.construct_view(|_| Box::new(StaticTextView::new("No selection".into())));
        let selected_object_name_label =
            ctx.construct_view(|_| Box::new(StaticTextView::new(String::new())));
        ctx.view_set_parent(selected_object_label, root_content_view);
        ctx.view_set_parent(selected_object_name_label, root_content_view);

        let eh = Rc::new_cyclic(|eh| {
            let content_view = ctx.construct_view(|_| Box::new(ContainerView));
            ctx.view_layout_mut(content_view)
                .expect("query failed")
                .child = ViewLayoutChild::Flow {
                direction: ViewLayoutFlowDirection::Vertical,
                alignment: ViewLayoutFlowAlignment::Start,
                justify: ViewLayoutFlowJustify::Start,
                overflow: ViewLayoutOverflow::Overflow,
                gap: 4.0,
            };

            let label = ctx.construct_view(|_| {
                let mut v = Box::new(StaticTextView::new("POSITION".into()));
                v.set_font(FontID::UIFormLiftedLabel);
                v
            });
            let input_container = ctx.construct_view(|_| Box::new(ContainerView));
            ctx.view_layout_mut(input_container)
                .expect("query failed")
                .child = ViewLayoutChild::Flow {
                direction: ViewLayoutFlowDirection::Horizontal,
                alignment: ViewLayoutFlowAlignment::Start,
                justify: ViewLayoutFlowJustify::Start,
                overflow: ViewLayoutOverflow::Overflow,
                gap: 4.0,
            };
            let local_position_x_input_view = ctx.construct_view(|id| {
                Box::new(NumericInputView::new(
                    id,
                    NumericInputViewInit {
                        value: eh.clone(),
                        ..Default::default()
                    },
                ))
            });
            let local_position_y_input_view = ctx.construct_view(|id| {
                Box::new(NumericInputView::new(
                    id,
                    NumericInputViewInit {
                        value: eh.clone(),
                        ..Default::default()
                    },
                ))
            });
            let local_position_z_input_view = ctx.construct_view(|id| {
                Box::new(NumericInputView::new(
                    id,
                    NumericInputViewInit {
                        value: eh.clone(),
                        ..Default::default()
                    },
                ))
            });
            ctx.view_set_parent(label, content_view);
            ctx.view_set_parent(local_position_x_input_view, input_container);
            ctx.view_set_parent(local_position_y_input_view, input_container);
            ctx.view_set_parent(local_position_z_input_view, input_container);
            ctx.view_set_parent(input_container, content_view);

            let label = ctx.construct_view(|_| {
                let mut v = Box::new(StaticTextView::new("ROTATION".into()));
                v.set_font(FontID::UIFormLiftedLabel);
                v
            });
            let local_rotation_x_input_view = ctx.construct_view(|id| {
                Box::new(NumericInputView::new(
                    id,
                    NumericInputViewInit {
                        value: eh.clone(),
                        ..Default::default()
                    },
                ))
            });
            let local_rotation_y_input_view = ctx.construct_view(|id| {
                Box::new(NumericInputView::new(
                    id,
                    NumericInputViewInit {
                        value: eh.clone(),
                        ..Default::default()
                    },
                ))
            });
            let local_rotation_z_input_view = ctx.construct_view(|id| {
                Box::new(NumericInputView::new(
                    id,
                    NumericInputViewInit {
                        value: eh.clone(),
                        ..Default::default()
                    },
                ))
            });
            ctx.view_set_parent(label, content_view);
            ctx.view_set_parent(local_rotation_x_input_view, content_view);
            ctx.view_set_parent(local_rotation_y_input_view, content_view);
            ctx.view_set_parent(local_rotation_z_input_view, content_view);

            let label = ctx.construct_view(|_| {
                let mut v = Box::new(StaticTextView::new("SCALE".into()));
                v.set_font(FontID::UIFormLiftedLabel);
                v
            });
            let local_scale_x_input_view = ctx.construct_view(|id| {
                Box::new(NumericInputView::new(
                    id,
                    NumericInputViewInit {
                        value: eh.clone(),
                        ..Default::default()
                    },
                ))
            });
            let local_scale_y_input_view = ctx.construct_view(|id| {
                Box::new(NumericInputView::new(
                    id,
                    NumericInputViewInit {
                        value: eh.clone(),
                        ..Default::default()
                    },
                ))
            });
            let local_scale_z_input_view = ctx.construct_view(|id| {
                Box::new(NumericInputView::new(
                    id,
                    NumericInputViewInit {
                        value: eh.clone(),
                        ..Default::default()
                    },
                ))
            });
            ctx.view_set_parent(label, content_view);
            ctx.view_set_parent(local_scale_x_input_view, content_view);
            ctx.view_set_parent(local_scale_y_input_view, content_view);
            ctx.view_set_parent(local_scale_z_input_view, content_view);

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
                        Point::new_logical(0.0, 8.0 + 12.0 + 12.0),
                        Size::new_logical(128.0, 128.0),
                    ),
                    content_view,
                ))
            });
            ctx.view_set_parent(content_view, items_container_view);

            ctx.view_layout_mut(root_content_view)
                .expect("query failed")
                .width = ViewSize::Fixed(128.0);
            let root_container_view = ctx.construct_view(|id| {
                Box::new(ScrollContainer::new(
                    id,
                    Rect::from_lt_size(
                        Point::new_logical(0.0, 0.0),
                        Size::new_logical(128.0, 128.0),
                    ),
                    root_content_view,
                ))
            });
            ctx.view_set_parent(root_content_view, root_container_view);

            EventHandler {
                object_selection_changed: Cell::new(false),
                items_container_mounted: Cell::new(false),
                root_container_view,
                root_content_view,
                selected_object_label,
                selected_object_name_label,
                items_container_view,
                items_content_view: content_view,
                numeric_input_view_ids: vec![
                    local_position_x_input_view,
                    local_position_y_input_view,
                    local_position_z_input_view,
                    local_rotation_x_input_view,
                    local_rotation_y_input_view,
                    local_rotation_z_input_view,
                    local_scale_x_input_view,
                    local_scale_y_input_view,
                    local_scale_z_input_view,
                ],
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
        self.eh.root_container_view
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        ctx.unsubscribe_view_feedback::<ViewFeedbackPerformAtomic>(&self.eh);
        ctx.unsubscribe_view_feedback::<ViewFeedbackObjectSelectionChanged>(&self.eh);
    }

    fn resize(&self, new_size: &Size<LogicalUnit>, context: &mut PaneContentResizeContext) {
        context
            .view_instance_mut::<ScrollContainer>(self.eh.root_container_view)
            .expect("query failed")
            .resize(*new_size);
        context
            .view_layout_mut(self.eh.root_content_view)
            .expect("query failed")
            .width = ViewSize::Fixed(new_size.width.max(128.0));
        context
            .view_instance_mut::<ScrollContainer>(self.eh.items_container_view)
            .expect("query failed")
            .resize(Size::new_logical(
                new_size.width,
                new_size.height - 8.0 - 12.0 - 12.0,
            ));
        context
            .view_layout_mut(self.eh.items_content_view)
            .expect("query failed")
            .width = ViewSize::Fixed(new_size.width.max(128.0));
        context.schedule_view_render(self.eh.root_container_view);
    }
}

struct EventHandler {
    object_selection_changed: Cell<bool>,
    items_container_mounted: Cell<bool>,
    root_container_view: ViewIdentifier,
    root_content_view: ViewIdentifier,
    selected_object_label: ViewIdentifier,
    selected_object_name_label: ViewIdentifier,
    items_container_view: ViewIdentifier,
    items_content_view: ViewIdentifier,
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
                        context
                            .view_set_parent(self.items_container_view, self.root_container_view);
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

            context.schedule_render(self.root_container_view);
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

        if requester == self.numeric_input_view_ids[0] {
            // pos x
            format!("{:.3}", application.object(selected).local_position.0)
        } else if requester == self.numeric_input_view_ids[1] {
            // pos y
            format!("{:.3}", application.object(selected).local_position.1)
        } else if requester == self.numeric_input_view_ids[2] {
            // pos z
            format!("{:.3}", application.object(selected).local_position.2)
        } else if requester == self.numeric_input_view_ids[3] {
            // rotate x
            format!("{:.3}", application.object(selected).local_rotation_euler.0)
        } else if requester == self.numeric_input_view_ids[4] {
            // rotate y
            format!("{:.3}", application.object(selected).local_rotation_euler.1)
        } else if requester == self.numeric_input_view_ids[5] {
            // rotate z
            format!("{:.3}", application.object(selected).local_rotation_euler.2)
        } else if requester == self.numeric_input_view_ids[6] {
            // scale x
            format!("{:.3}", application.object(selected).local_scale.0)
        } else if requester == self.numeric_input_view_ids[7] {
            // scale y
            format!("{:.3}", application.object(selected).local_scale.1)
        } else if requester == self.numeric_input_view_ids[8] {
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

        if sender == self.numeric_input_view_ids[0] {
            // pos x
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            application.object_modify_data(selected, |o| o.local_position.0 = v);
        } else if sender == self.numeric_input_view_ids[1] {
            // pos y
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            application.object_modify_data(selected, |o| o.local_position.1 = v);
        } else if sender == self.numeric_input_view_ids[2] {
            // pos z
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            application.object_modify_data(selected, |o| o.local_position.2 = v);
        } else if sender == self.numeric_input_view_ids[3] {
            // rotate x
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            application.object_modify_data(selected, |o| o.local_rotation_euler.0 = v);
        } else if sender == self.numeric_input_view_ids[4] {
            // rotate y
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            application.object_modify_data(selected, |o| o.local_rotation_euler.1 = v);
        } else if sender == self.numeric_input_view_ids[5] {
            // rotate z
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            application.object_modify_data(selected, |o| o.local_rotation_euler.2 = v);
        } else if sender == self.numeric_input_view_ids[6] {
            // scale x
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            application.object_modify_data(selected, |o| o.local_scale.0 = v);
        } else if sender == self.numeric_input_view_ids[7] {
            // scale y
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            application.object_modify_data(selected, |o| o.local_scale.1 = v);
        } else if sender == self.numeric_input_view_ids[8] {
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

        if sender == self.numeric_input_view_ids[0] {
            // pos x
            application.object_modify_data(selected, |o| o.local_position.0 += delta * 0.1);
        } else if sender == self.numeric_input_view_ids[1] {
            // pos y
            application.object_modify_data(selected, |o| o.local_position.1 += delta * 0.1);
        } else if sender == self.numeric_input_view_ids[2] {
            // pos z
            application.object_modify_data(selected, |o| o.local_position.2 += delta * 0.1);
        } else if sender == self.numeric_input_view_ids[3] {
            // rotate x
            application.object_modify_data(selected, |o| o.local_rotation_euler.0 += delta);
        } else if sender == self.numeric_input_view_ids[4] {
            // rotate y
            application.object_modify_data(selected, |o| o.local_rotation_euler.1 += delta);
        } else if sender == self.numeric_input_view_ids[5] {
            // rotate z
            application.object_modify_data(selected, |o| o.local_rotation_euler.2 += delta);
        } else if sender == self.numeric_input_view_ids[6] {
            // scale x
            application.object_modify_data(selected, |o| o.local_scale.0 += delta * 0.1);
        } else if sender == self.numeric_input_view_ids[7] {
            // scale y
            application.object_modify_data(selected, |o| o.local_scale.1 += delta * 0.1);
        } else if sender == self.numeric_input_view_ids[8] {
            // scale z
            application.object_modify_data(selected, |o| o.local_scale.2 += delta * 0.1);
        }
    }
}
