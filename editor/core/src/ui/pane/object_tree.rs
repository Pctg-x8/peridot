use std::{
    cell::{Cell, RefCell},
    rc::Rc,
};

use crate::{
    Event, MENU_COMMAND_ID_OBJECT_CREATE_CAPSULE, MENU_COMMAND_ID_OBJECT_CREATE_CUBE,
    MENU_COMMAND_ID_OBJECT_CREATE_CYLINDER, MENU_COMMAND_ID_OBJECT_CREATE_SP_TERRAIN,
    MENU_COMMAND_ID_OBJECT_CREATE_SPHERE, ObjectID, ViewFeedbackObjectSelectionChanged,
    ViewFeedbackObjectTreeChanged,
    input::{
        EventContinueControl, InputEventContext, ModifierKey,
        hittest::{
            CursorShape, HitTestTreeActionHandler, HitTestTreeData, HitTestTreeRef,
            PointerActionArgs, PointerButton, PointerButtonActionArgs,
        },
    },
    rendering::composite::{
        AnimatableColor, AnimatableFloat, AnimationCurve, CompositeMode, CompositeRect,
        CompositeRectScaleFactor, CompositeRectText, CompositeRectTextRun,
        CompositeRectTextVerticalAlignment, CompositeTreeRef,
    },
    uikit::{
        MenuItem, RawMountTarget, TeardownContext, ViewFeedbackContext, ViewFeedbackHandler,
        ViewFeedbackPerformAtomic, ViewIdentifier, ViewInitContext, ViewNewRenderElements,
        ViewRegisterable,
    },
};

pub struct Presenter {
    root_view_id: ViewIdentifier,
    eh: Rc<ObjectTreePaneEventHandler>,
}
impl Presenter {
    pub const ID: &str = internal_pane_identifier!("ObjectTree");

    pub fn new(ctx: &mut ViewInitContext) -> Self {
        let root_view_id = ctx.construct_view(|_| Box::new(View::new()));

        let eh = Rc::new(ObjectTreePaneEventHandler {
            root_view_id,
            object_tree_changed: Cell::new(false),
            object_selection_changed: Cell::new(false),
            row_views: RefCell::new(Vec::new()),
        });
        ctx.subscribe_view_feedback::<ViewFeedbackPerformAtomic>(&eh);
        ctx.subscribe_view_feedback::<ViewFeedbackObjectTreeChanged>(&eh);
        ctx.subscribe_view_feedback::<ViewFeedbackObjectSelectionChanged>(&eh);

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
        self.root_view_id
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        ctx.unsubscribe_view_feedback::<ViewFeedbackPerformAtomic>(&self.eh);
        ctx.unsubscribe_view_feedback::<ViewFeedbackObjectTreeChanged>(&self.eh);
        ctx.unsubscribe_view_feedback::<ViewFeedbackObjectSelectionChanged>(&self.eh);
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
        _self_instance: &mut crate::uikit::ViewInstanceModifier,
        ctx: &mut crate::uikit::RenderContext,
        sched: &mut crate::uikit::RenderChildScheduler,
    ) -> ViewNewRenderElements {
        match self.entity {
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
                    ..Default::default()
                });
                let ht_root = ctx.ht_manager.create(HitTestTreeData {
                    width_adjustment_factor: 1.0,
                    height_adjustment_factor: 1.0,
                    ..Default::default()
                });
                let entity = Rc::new(ViewEntity { ct_root, ht_root });
                ctx.ht_manager.set_action_handler(ht_root, &entity);

                self.entity = Some(entity);
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
        let Some(entity) = self.entity.take() else {
            // not rendered
            return;
        };

        ctx.mount_context.composite_tree.free(entity.ct_root);
        ctx.mount_context.ht_manager.free(entity.ht_root);
    }
}

struct ViewEntity {
    ct_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
}
impl HitTestTreeActionHandler for ViewEntity {
    fn on_click(
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
    root_view_id: ViewIdentifier,
    object_tree_changed: Cell<bool>,
    object_selection_changed: Cell<bool>,
    row_views: RefCell<Vec<ViewIdentifier>>,
}
impl ViewFeedbackHandler<ViewFeedbackPerformAtomic> for ObjectTreePaneEventHandler {
    fn accept_feedback<'a, 'h>(
        &self,
        _feedback: &ViewFeedbackPerformAtomic,
        context: &mut ViewFeedbackContext<'a, 'h>,
    ) {
        let object_tree_changed = self.object_tree_changed.replace(false);
        let object_selection_changed = self.object_selection_changed.replace(false);

        if object_tree_changed {
            let mut row_views = self.row_views.borrow_mut();
            for x in row_views.drain(..) {
                context.teardown_view_recursive(x);
                context.view_init_context.free_view(x);
            }
            for (n, &x) in context.application.root_objects.iter().enumerate() {
                let o = context.application.object(x);
                let rv = context.view_init_context.construct_view(|_| {
                    Box::new(ObjectRowView::new(
                        x,
                        o.name.clone(),
                        n as f32 * ObjectRowView::ITEM_HEIGHT,
                        context.application.object_is_selected(x),
                    ))
                });
                context.view_set_parent(rv, self.root_view_id);
                row_views.push(rv);
            }

            context.schedule_render(self.root_view_id);
        }

        if object_selection_changed {
            for &x in self.row_views.borrow().iter() {
                let o = context
                    .view_instance::<ObjectRowView>(x)
                    .expect("query failed")
                    .assigned_object;
                let selected = context.application.object_is_selected(o);
                context
                    .view_instance_mut::<ObjectRowView>(x)
                    .expect("query failed")
                    .selected = selected;
                context.schedule_render(x);
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
impl ViewFeedbackHandler<ViewFeedbackObjectSelectionChanged> for ObjectTreePaneEventHandler {
    fn accept_feedback<'a, 'h>(
        &self,
        _feedback: &ViewFeedbackObjectSelectionChanged,
        _context: &mut ViewFeedbackContext<'a, 'h>,
    ) {
        self.object_selection_changed.set(true);
    }
}

struct ObjectRowView {
    assigned_object: ObjectID,
    eh: Option<Rc<ObjectTreeObjectRowEventHandler>>,
    label: String,
    y: f32,
    selected: bool,
}
impl ObjectRowView {
    const ITEM_HEIGHT: f32 = 20.0;

    fn new(
        assigned_object: ObjectID,
        init_label: String,
        init_y: f32,
        init_selected: bool,
    ) -> Self {
        Self {
            assigned_object,
            eh: None,
            label: init_label,
            y: init_y,
            selected: init_selected,
        }
    }
}
impl crate::uikit::View for ObjectRowView {
    fn render(
        &mut self,
        _self_instance: &mut crate::uikit::ViewInstanceModifier,
        ctx: &mut crate::uikit::RenderContext,
        _sched: &mut crate::uikit::RenderChildScheduler,
    ) -> ViewNewRenderElements {
        match self.eh {
            // TODO: reflect state changes
            Some(ref e) => {
                if e.selection_lit.replace(self.selected) != self.selected {
                    // selected changed
                    if self.selected {
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

                ViewNewRenderElements::EMPTY
            }
            None => {
                // first render
                let ct_root = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    offset: [AnimatableFloat::Value(0.0), AnimatableFloat::Value(self.y)],
                    size: [
                        AnimatableFloat::Value(0.0),
                        AnimatableFloat::Value(Self::ITEM_HEIGHT),
                    ],
                    relative_size_adjustment: [1.0, 0.0],
                    has_bitmap: true,
                    composite_mode: CompositeMode::FillColor(AnimatableColor::Value([
                        0.0,
                        0.25,
                        1.0,
                        if self.selected { 1.0 } else { 0.0 },
                    ])),
                    ..Default::default()
                });
                let ct_label_hover = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    relative_size_adjustment: [1.0, 1.0],
                    has_bitmap: true,
                    composite_mode: CompositeMode::FillColor(AnimatableColor::Value([0.0; 4])),
                    text: Some(CompositeRectText {
                        runs: vec![CompositeRectTextRun {
                            content: self.label.clone(),
                            color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                            ..Default::default()
                        }],
                        vertical_alignment: CompositeRectTextVerticalAlignment::Middle,
                        ..Default::default()
                    }),
                    ..Default::default()
                });
                let ht_root = ctx.ht_manager.create(HitTestTreeData {
                    top: self.y,
                    height: Self::ITEM_HEIGHT,
                    width_adjustment_factor: 1.0,
                    cursor_shape: CursorShape::Pointer,
                    ..Default::default()
                });

                let eh = Rc::new(ObjectTreeObjectRowEventHandler {
                    assigned_object: self.assigned_object,
                    selection_lit: Cell::new(self.selected),
                    ct_root,
                    ct_label_hover,
                    ht_root,
                });
                ctx.ht_manager.set_action_handler(eh.ht_root, &eh);

                ctx.composite_tree.add_child(eh.ct_root, eh.ct_label_hover);

                self.eh = Some(eh);
                ViewNewRenderElements {
                    composite_tree: Some(ct_root),
                    hit_tree: Some(ht_root),
                    ..ViewNewRenderElements::EMPTY
                }
            }
        }
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        let Some(entity) = self.eh.take() else {
            // not rendered
            return;
        };

        ctx.mount_context.composite_tree.free_all(entity.ct_root);
        ctx.mount_context.ht_manager.free_all(entity.ht_root);
    }
}

struct ObjectTreeObjectRowEventHandler {
    assigned_object: ObjectID,
    selection_lit: Cell<bool>,
    ct_root: CompositeTreeRef,
    ct_label_hover: CompositeTreeRef,
    ht_root: HitTestTreeRef,
}
impl HitTestTreeActionHandler for ObjectTreeObjectRowEventHandler {
    fn on_pointer_enter(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerActionArgs,
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
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerActionArgs,
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

    fn on_click(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        if args.button == PointerButton::Primary {
            if args.key_modifier.contains(ModifierKey::CONTROL) {
                context
                    .application
                    .toggle_object_selection_additive(self.assigned_object);
            } else {
                context.application.select_object(self.assigned_object);
            }

            return EventContinueControl::STOP_PROPAGATION;
        }

        EventContinueControl::empty()
    }
}
