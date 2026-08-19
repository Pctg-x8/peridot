use std::{
    cell::{Cell, RefCell},
    collections::HashSet,
    rc::Rc,
};

use crate::{
    Event, MENU_COMMAND_ID_OBJECT_CREATE_CAPSULE, MENU_COMMAND_ID_OBJECT_CREATE_CUBE,
    MENU_COMMAND_ID_OBJECT_CREATE_CYLINDER, MENU_COMMAND_ID_OBJECT_CREATE_PLANE,
    MENU_COMMAND_ID_OBJECT_CREATE_SP_TERRAIN, MENU_COMMAND_ID_OBJECT_CREATE_SPHERE,
    MENU_COMMAND_ID_OBJECT_DESTROY_SELECTED,
    input::{
        EventContinueControl, InputEventContext, ModifierKey,
        hittest::{
            CursorShape, HitTestTreeActionHandler, HitTestTreeData, HitTestTreeRef,
            PointerActionArgs, PointerButton, PointerButtonActionArgs,
        },
    },
    model::{
        ObjectID, ViewFeedbackObjectNameChanged, ViewFeedbackObjectSelectionChanged,
        ViewFeedbackObjectTreeChanged,
    },
    rendering::composite::{
        AnimatableColor, AnimationCurve, CompositeMode, CompositeRect, CompositeRectText,
        CompositeRectTextRun, CompositeTreeRef,
    },
    ui::dock::PaneContentResizeContext,
    uikit::{
        MenuItem, TeardownContext, TypedViewIdentifier, ViewFeedbackContext, ViewFeedbackHandler,
        ViewFeedbackPerformAtomic, ViewFeedbackRegisterable, ViewIdentifier,
        ViewImmediateTeardownable, ViewInitContext, ViewInstanceQueryable,
        ViewInstanceQueryableMut, ViewLayoutChild, ViewLayoutFlowDirection, ViewRegisterable,
        ViewRelationControllable, ViewRenderElements, ViewRenderer, ViewSize,
    },
    utils::{LogicalUnit, Rect, Size},
};

pub struct Presenter {
    root_view_id: TypedViewIdentifier<View>,
    eh: Rc<ObjectTreePaneEventHandler>,
}
impl Presenter {
    pub const ID: &str = internal_pane_identifier!("ObjectTree");

    pub fn new(ctx: &mut ViewInitContext) -> Self {
        let root_view_id = ctx.construct_view(|_| Box::new(View::new()));
        {
            let l = ctx.view_layout_mut(root_view_id).expect("query failed");
            l.child = ViewLayoutChild::Flow {
                direction: ViewLayoutFlowDirection::Vertical,
                alignment: Default::default(),
                justify: Default::default(),
                overflow: Default::default(),
                gap: 0.0,
            };
        }

        let eh = Rc::new(ObjectTreePaneEventHandler {
            root_view_id,
            object_tree_changed: Cell::new(false),
            changed_object_ids: RefCell::new(HashSet::new()),
            row_views: RefCell::new(Vec::new()),
        });
        ctx.subscribe_view_feedback::<ViewFeedbackPerformAtomic>(&eh);
        ctx.subscribe_view_feedback::<ViewFeedbackObjectTreeChanged>(&eh);
        ctx.subscribe_view_feedback::<ViewFeedbackObjectNameChanged>(&eh);

        Self { eh, root_view_id }
    }
}
impl crate::ui::dock::PaneContentPresenter for Presenter {
    fn id(&self) -> String {
        Self::ID.into()
    }

    fn name(&self) -> String {
        "Object Tree".into()
    }

    fn root_view_id(&self) -> ViewIdentifier {
        self.root_view_id.into_untyped()
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        ctx.unsubscribe_view_feedback::<ViewFeedbackPerformAtomic>(&self.eh);
        ctx.unsubscribe_view_feedback::<ViewFeedbackObjectTreeChanged>(&self.eh);
        ctx.unsubscribe_view_feedback::<ViewFeedbackObjectNameChanged>(&self.eh);
    }

    fn resize(&self, new_size: &Size<LogicalUnit>, context: &mut PaneContentResizeContext) {
        context
            .view_layout_mut(self.root_view_id)
            .expect("query failed")
            .width = ViewSize::Fixed(new_size.width);
        context.schedule_view_render(self.root_view_id);
    }
}

struct View {
    entity: Option<Rc<ViewEntity>>,
}
impl View {
    pub fn new() -> Self {
        Self { entity: None }
    }
}
impl crate::uikit::View for View {
    fn render(
        &mut self,
        _layout_rect: Rect<LogicalUnit>,
        ctx: &mut crate::uikit::RenderContext,
        _layout_state: &crate::uikit::ViewLayoutStateStore,
    ) -> ViewRenderElements {
        let e = match self.entity {
            Some(ref e) => e,
            None => {
                // first render
                let ct_root = CompositeRect::build()
                    .expand_full()
                    .create(ctx.composite_tree);
                let ht_root = ctx.ht_manager.create(HitTestTreeData {
                    width_adjustment_factor: 1.0,
                    height_adjustment_factor: 1.0,
                    ..Default::default()
                });
                let entity = Rc::new(ViewEntity { ct_root, ht_root });
                ctx.ht_manager.set_action_handler(ht_root, &entity);

                &*self.entity.insert(entity)
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

        ctx.mount_context.composite_tree.free(entity.ct_root);
        ctx.mount_context.ht_manager.free(entity.ht_root);
    }

    fn measure_preferred_content_size(
        &self,
        _ctx: &mut crate::uikit::MeasureContext,
    ) -> Size<LogicalUnit> {
        Size::new_logical(0.0, 0.0)
    }

    fn create_new_layout_layer(&self) -> bool {
        true
    }
}

struct ViewEntity {
    ct_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
}
impl HitTestTreeActionHandler for ViewEntity {
    fn on_pointer_down(
        &self,
        _sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        if args.button == PointerButton::Secondary {
            context.system_link.dispatch_event(Event::MenuOpen {
                parent: context
                    .ht_manager
                    .query_root_window(self.ht_root)
                    .expect("not mounted"),
                items: vec![
                    MenuItem::Heading {
                        label: "Create Object".into(),
                    },
                    MenuItem::Command {
                        label: "Plane".into(),
                        command_id: MENU_COMMAND_ID_OBJECT_CREATE_PLANE,
                    },
                    MenuItem::Command {
                        label: "Cube".into(),
                        command_id: MENU_COMMAND_ID_OBJECT_CREATE_CUBE,
                    },
                    MenuItem::Command {
                        label: "Sphere".into(),
                        command_id: MENU_COMMAND_ID_OBJECT_CREATE_SPHERE,
                    },
                    MenuItem::Command {
                        label: "Cylinder".into(),
                        command_id: MENU_COMMAND_ID_OBJECT_CREATE_CYLINDER,
                    },
                    MenuItem::Command {
                        label: "Capsule".into(),
                        command_id: MENU_COMMAND_ID_OBJECT_CREATE_CAPSULE,
                    },
                    MenuItem::SubMenu {
                        label: "Special".into(),
                        items: vec![MenuItem::Command {
                            label: "Terrain".into(),
                            command_id: MENU_COMMAND_ID_OBJECT_CREATE_SP_TERRAIN,
                        }],
                    },
                ],
                surface_pos: args.client_pos,
            });

            return EventContinueControl::STOP_PROPAGATION;
        }

        EventContinueControl::empty()
    }
}

struct ObjectTreePaneEventHandler {
    root_view_id: TypedViewIdentifier<View>,
    object_tree_changed: Cell<bool>,
    changed_object_ids: RefCell<HashSet<ObjectID>>,
    row_views: RefCell<Vec<TypedViewIdentifier<ObjectRowView>>>,
}
impl ViewFeedbackHandler<ViewFeedbackPerformAtomic> for ObjectTreePaneEventHandler {
    fn accept_feedback<'a, 'h>(
        &self,
        _feedback: &ViewFeedbackPerformAtomic,
        context: &mut ViewFeedbackContext<'a, 'h>,
    ) {
        let object_tree_changed = self.object_tree_changed.replace(false);
        let changed_object_ids = self
            .changed_object_ids
            .borrow_mut()
            .drain()
            .collect::<Vec<_>>();

        if object_tree_changed {
            let mut row_views = self.row_views.borrow_mut();
            for x in row_views.drain(..) {
                context.teardown_view_recursive(x);
                context.free_view(x);
            }
            for (x, name) in crate::model::object_tree_content(context.application) {
                let rv =
                    context.construct_view(|id| Box::new(ObjectRowView::new(id, x, name.into())));
                context.view_layout_mut(rv).expect("query failed").width = ViewSize::FillAvailable;
                context.view_set_parent(rv, self.root_view_id);
                row_views.push(rv);
            }

            context.schedule_view_render(self.root_view_id);
        } else {
            for oid in changed_object_ids {
                for &view in self.row_views.borrow().iter() {
                    if context
                        .view_instance(view)
                        .expect("query failed")
                        .assigned_object
                        == oid
                    {
                        let name = crate::model::object_name(context, oid).into();

                        context
                            .view_instance_mut(view)
                            .expect("query failed")
                            .set_label(name);
                        context.schedule_view_render(view);
                        break;
                    }
                }
            }
        }
    }
}
impl ViewFeedbackHandler<ViewFeedbackObjectTreeChanged> for ObjectTreePaneEventHandler {
    fn accept_feedback<'a, 'h>(
        &self,
        _feedback: &ViewFeedbackObjectTreeChanged,
        _context: &mut ViewFeedbackContext<'a, 'h>,
    ) {
        self.object_tree_changed.set(true);
    }
}
impl ViewFeedbackHandler<ViewFeedbackObjectNameChanged> for ObjectTreePaneEventHandler {
    fn accept_feedback<'a, 'h>(
        &self,
        feedback: &ViewFeedbackObjectNameChanged,
        _context: &mut ViewFeedbackContext<'a, 'h>,
    ) {
        self.changed_object_ids.borrow_mut().insert(feedback.0);
    }
}

struct ObjectRowView {
    id: TypedViewIdentifier<Self>,
    assigned_object: ObjectID,
    eh: Option<Rc<ObjectRowEventHandler>>,
    label: String,
    label_changed: bool,
}
impl ObjectRowView {
    const ITEM_HEIGHT: f32 = 20.0;

    fn new(id: TypedViewIdentifier<Self>, assigned_object: ObjectID, init_label: String) -> Self {
        Self {
            id,
            assigned_object,
            eh: None,
            label: init_label,
            label_changed: false,
        }
    }

    fn set_label(&mut self, label: String) {
        self.label = label;
        self.label_changed = true;
    }
}
impl crate::uikit::View for ObjectRowView {
    fn render(
        &mut self,
        layout_rect: Rect<LogicalUnit>,
        ctx: &mut crate::uikit::RenderContext,
        _layout_state: &crate::uikit::ViewLayoutStateStore,
    ) -> ViewRenderElements {
        let selected = crate::model::object_is_selected(ctx, self.assigned_object);

        let e = match self.eh {
            // TODO: reflect state changes
            Some(ref e) => {
                if core::mem::replace(&mut self.label_changed, false) {
                    // label changed
                    ctx.composite_tree
                        .begin_mod_chain(e.ct_label_hover)
                        .text_run(
                            CompositeRectTextRun::build(self.label.clone())
                                .color_imm([1.0, 1.0, 1.0, 1.0]),
                        )
                        .apply();
                }

                if e.selection_lit.replace(selected) != selected {
                    // selected changed
                    if selected {
                        ctx.composite_tree
                            .begin_mod_chain(e.ct_root)
                            .composite_mode(CompositeMode::FillColor(AnimatableColor::Animated {
                                from_value: [0.0, 0.25, 1.0, 0.0],
                                to_value: [0.0, 0.25, 1.0, 1.0],
                                curve: AnimationCurve::EASE_OUT,
                                event_on_complete: None,
                                sec_duration: (ctx.current_sec..ctx.current_sec + 0.1).into(),
                            }))
                            .apply();
                    } else {
                        ctx.composite_tree
                            .begin_mod_chain(e.ct_root)
                            .composite_mode(CompositeMode::FillColor(AnimatableColor::Animated {
                                from_value: [0.0, 0.25, 1.0, 1.0],
                                to_value: [0.0, 0.25, 1.0, 0.0],
                                curve: AnimationCurve::EASE_OUT,
                                event_on_complete: None,
                                sec_duration: (ctx.current_sec..ctx.current_sec + 0.1).into(),
                            }))
                            .apply();
                    }
                }

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
                let ct_root = CompositeRect::build()
                    .use_ui_scale()
                    .rect_imm(layout_rect.clone())
                    .composite_fill_color_imm([0.0, 0.25, 1.0, if selected { 1.0 } else { 0.0 }])
                    .create(ctx.composite_tree);
                let ct_label_hover = CompositeRect::build()
                    .expand_full()
                    .composite_fill_color_imm([0.0; 4])
                    .text(
                        CompositeRectText::build()
                            .run(
                                CompositeRectTextRun::build(self.label.clone()).color_imm([1.0; 4]),
                            )
                            .vertical_middle(),
                    )
                    .create(ctx.composite_tree);
                let ht_root = ctx.ht_manager.create(HitTestTreeData {
                    left: layout_rect.left,
                    top: layout_rect.top,
                    width: layout_rect.width,
                    height: layout_rect.height,
                    cursor_shape: CursorShape::Pointer,
                    ..Default::default()
                });

                ctx.composite_tree.add_child(ct_root, ct_label_hover);

                let eh = Rc::new(ObjectRowEventHandler {
                    view_id: self.id,
                    assigned_object: self.assigned_object,
                    selection_lit: Cell::new(selected),
                    ct_root,
                    ct_label_hover,
                    ht_root,
                });
                ctx.ht_manager.set_action_handler(ht_root, &eh);
                ctx.subscribe_view_feedback::<ViewFeedbackObjectSelectionChanged>(&eh);

                &*self.eh.insert(eh)
            }
        };

        ViewRenderElements {
            composite_tree: Some(e.ct_root),
            hit_tree: Some(e.ht_root),
            ..ViewRenderElements::EMPTY
        }
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        let Some(entity) = self.eh.take() else {
            // not rendered
            return;
        };

        ctx.unsubscribe_view_feedback::<ViewFeedbackObjectSelectionChanged>(&entity);
        ctx.mount_context.composite_tree.free_all(entity.ct_root);
        ctx.mount_context.ht_manager.free_all(entity.ht_root);
    }

    fn measure_preferred_content_size(
        &self,
        _ctx: &mut crate::uikit::MeasureContext,
    ) -> Size<LogicalUnit> {
        Size::new_logical(0.0, Self::ITEM_HEIGHT)
    }
}

struct ObjectRowEventHandler {
    view_id: TypedViewIdentifier<ObjectRowView>,
    assigned_object: ObjectID,
    selection_lit: Cell<bool>,
    ct_root: CompositeTreeRef,
    ct_label_hover: CompositeTreeRef,
    ht_root: HitTestTreeRef,
}
impl HitTestTreeActionHandler for ObjectRowEventHandler {
    fn on_pointer_enter(
        &self,
        _sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerActionArgs,
    ) -> EventContinueControl {
        context
            .composite_tree
            .begin_mod_chain(self.ct_label_hover)
            .composite_mode(CompositeMode::FillColor(AnimatableColor::Animated {
                from_value: [1.0, 1.0, 1.0, 0.0],
                to_value: [1.0, 1.0, 1.0, 0.125],
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
            .begin_mod_chain(self.ct_label_hover)
            .composite_mode(CompositeMode::FillColor(AnimatableColor::Animated {
                from_value: [1.0, 1.0, 1.0, 0.125],
                to_value: [1.0, 1.0, 1.0, 0.0],
                sec_duration: (context.current_sec..context.current_sec + 0.1).into(),
                curve: AnimationCurve::Linear,
                event_on_complete: None,
            }))
            .apply();

        EventContinueControl::STOP_PROPAGATION
    }

    fn on_pointer_down(
        &self,
        _sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        if args.key_modifier.contains(ModifierKey::CONTROL) {
            crate::model::toggle_object_selection_additive(context, self.assigned_object);
        } else {
            crate::model::select_object(context, self.assigned_object);
        }

        if args.button == PointerButton::Secondary {
            context.system_link.dispatch_event(Event::MenuOpen {
                parent: context
                    .ht_manager
                    .query_root_window(self.ht_root)
                    .expect("not mounted"),
                items: vec![
                    MenuItem::Command {
                        label: "Destroy".into(),
                        command_id: MENU_COMMAND_ID_OBJECT_DESTROY_SELECTED,
                    },
                    MenuItem::Heading {
                        label: "Create Child Object".into(),
                    },
                    MenuItem::Command {
                        label: "Plane".into(),
                        command_id: MENU_COMMAND_ID_OBJECT_CREATE_PLANE,
                    },
                    MenuItem::Command {
                        label: "Cube".into(),
                        command_id: MENU_COMMAND_ID_OBJECT_CREATE_CUBE,
                    },
                    MenuItem::Command {
                        label: "Sphere".into(),
                        command_id: MENU_COMMAND_ID_OBJECT_CREATE_SPHERE,
                    },
                    MenuItem::Command {
                        label: "Cylinder".into(),
                        command_id: MENU_COMMAND_ID_OBJECT_CREATE_CYLINDER,
                    },
                    MenuItem::Command {
                        label: "Capsule".into(),
                        command_id: MENU_COMMAND_ID_OBJECT_CREATE_CAPSULE,
                    },
                    MenuItem::SubMenu {
                        label: "Special".into(),
                        items: vec![MenuItem::Command {
                            label: "Terrain".into(),
                            command_id: MENU_COMMAND_ID_OBJECT_CREATE_SP_TERRAIN,
                        }],
                    },
                ],
                surface_pos: args.client_pos,
            });
        }

        EventContinueControl::STOP_PROPAGATION
    }
}
impl ViewFeedbackHandler<ViewFeedbackObjectSelectionChanged> for ObjectRowEventHandler {
    fn accept_feedback<'a, 'h>(
        &self,
        _feedback: &ViewFeedbackObjectSelectionChanged,
        context: &mut ViewFeedbackContext<'a, 'h>,
    ) {
        context.schedule_view_render(self.view_id);
    }
}
