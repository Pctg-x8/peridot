use std::{
    cell::{Cell, RefCell},
    rc::Rc,
};

use crate::{
    EmptyView, Event, ObjectID, ViewFeedbackObjectSelectionChanged, ViewFeedbackObjectTreeChanged,
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
        CompositeRectTextVerticalAlignment, CompositeTree, CompositeTreeRef,
    },
    uikit::{
        MenuItem, MountContext, MountTarget, RawMountTarget, TeardownContext, ViewFeedbackContext,
        ViewFeedbackHandler, ViewFeedbackPerformAtomic, ViewIdentifier, ViewInitContext,
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
        let ct_root = ctx.composite_tree.create(CompositeRect {
            relative_size_adjustment: [1.0, 1.0],
            ..Default::default()
        });
        let ht_root = ctx.ht_manager.create(HitTestTreeData {
            width_adjustment_factor: 1.0,
            height_adjustment_factor: 1.0,
            ..Default::default()
        });

        let ht_context_menu_receiver = ctx.ht_manager.create(HitTestTreeData {
            width_adjustment_factor: 1.0,
            height_adjustment_factor: 1.0,
            ..Default::default()
        });

        let eh = Rc::new(ObjectTreePaneEventHandler {
            ct_root,
            ht_root,
            ht_context_menu_receiver,
            object_tree_changed: Cell::new(false),
            object_selection_changed: Cell::new(false),
            row_views: RefCell::new(Vec::new()),
        });
        ctx.ht_manager
            .set_action_handler(eh.ht_context_menu_receiver, &eh);
        ctx.subscribe_view_feedback::<ViewFeedbackPerformAtomic>(&eh);
        ctx.subscribe_view_feedback::<ViewFeedbackObjectTreeChanged>(&eh);
        ctx.subscribe_view_feedback::<ViewFeedbackObjectSelectionChanged>(&eh);

        ctx.ht_manager
            .add_child(eh.ht_root, eh.ht_context_menu_receiver);

        Self {
            eh,
            root_view_id: ctx.construct_view(|_| Box::new(EmptyView)),
        }
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

    // fn mount(&self, ctx: &mut MountContext, target: &RawMountTarget) {
    //     ctx.composite_tree
    //         .add_child(target.ct_root(), self.eh.ct_root);
    //     ctx.ht_manager.add_child(target.ht_root(), self.eh.ht_root);
    // }

    // fn unmount(&self, ctx: &mut MountContext) {
    //     ctx.composite_tree.remove_child(self.eh.ct_root);
    //     ctx.ht_manager.remove_child(self.eh.ht_root);
    // }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        ctx.unsubscribe_view_feedback::<ViewFeedbackPerformAtomic>(&self.eh);
        ctx.unsubscribe_view_feedback::<ViewFeedbackObjectTreeChanged>(&self.eh);
        ctx.unsubscribe_view_feedback::<ViewFeedbackObjectSelectionChanged>(&self.eh);

        ctx.mount_context.composite_tree.free_all(self.eh.ct_root);
        ctx.mount_context.ht_manager.free_all(self.eh.ht_root)
    }
}

pub const MENU_COMMAND_ID_OBJECT_CREATE_CUBE: u64 = 1;
pub const MENU_COMMAND_ID_OBJECT_CREATE_SPHERE: u64 = 2;
pub const MENU_COMMAND_ID_OBJECT_CREATE_CYLINDER: u64 = 3;
pub const MENU_COMMAND_ID_OBJECT_CREATE_CAPSULE: u64 = 4;
pub const MENU_COMMAND_ID_OBJECT_CREATE_SP_TERRAIN: u64 = 10;

struct ObjectTreePaneEventHandler {
    ct_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    ht_context_menu_receiver: HitTestTreeRef,
    object_tree_changed: Cell<bool>,
    object_selection_changed: Cell<bool>,
    row_views: RefCell<Vec<ObjectTreeObjectRowView>>,
}
impl HitTestTreeActionHandler for ObjectTreePaneEventHandler {
    fn on_click(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        if args.button == PointerButton::Secondary {
            context.system_link.dispatch_event(Event::MenuOpen {
                parent: context
                    .ht_manager
                    .query_root_window(self.ht_context_menu_receiver)
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
                x.unmount(&mut context.view_init_context);
                x.teardown(&mut context.view_init_context.make_teardown_context());
            }
            for (n, &x) in context.application.root_objects.iter().enumerate() {
                let o = context.application.object(x);
                let rv = ObjectTreeObjectRowView::new(
                    &mut context.view_init_context,
                    x,
                    o.name.clone(),
                    n as f32 * ObjectTreeObjectRowView::ITEM_HEIGHT,
                    context.application.object_is_selected(x),
                );
                rv.mount(
                    &mut context.view_init_context,
                    &RawMountTarget {
                        ht_root: self.ht_root,
                        ct_root: self.ct_root,
                    },
                );
                row_views.push(rv);
            }
        }

        if object_selection_changed {
            for x in self.row_views.borrow().iter() {
                x.eh.update_selected(
                    context.application.object_is_selected(x.eh.assigned_object),
                    context.view_init_context.mount_context.composite_tree,
                    context.view_init_context.current_sec,
                );
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

struct ObjectTreeObjectRowView {
    eh: Rc<ObjectTreeObjectRowEventHandler>,
}
impl ObjectTreeObjectRowView {
    const ITEM_HEIGHT: f32 = 20.0;

    fn new(
        ctx: &mut ViewInitContext,
        assigned_object: ObjectID,
        init_label: String,
        init_y: f32,
        init_selected: bool,
    ) -> Self {
        let ct_root = ctx.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            offset: [AnimatableFloat::Value(0.0), AnimatableFloat::Value(init_y)],
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
                if init_selected { 1.0 } else { 0.0 },
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
                    content: init_label,
                    color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                    ..Default::default()
                }],
                vertical_alignment: CompositeRectTextVerticalAlignment::Middle,
                ..Default::default()
            }),
            ..Default::default()
        });
        let ht_root = ctx.ht_manager.create(HitTestTreeData {
            top: init_y,
            height: Self::ITEM_HEIGHT,
            width_adjustment_factor: 1.0,
            cursor_shape: CursorShape::Pointer,
            ..Default::default()
        });

        let eh = Rc::new(ObjectTreeObjectRowEventHandler {
            assigned_object,
            selection_lit: Cell::new(init_selected),
            ct_root,
            ct_label_hover,
            ht_root,
        });
        ctx.ht_manager.set_action_handler(eh.ht_root, &eh);

        ctx.composite_tree.add_child(eh.ct_root, eh.ct_label_hover);

        Self { eh }
    }

    fn mount(&self, ctx: &mut MountContext, target: &(impl MountTarget + ?Sized)) {
        ctx.composite_tree
            .add_child(target.ct_root(), self.eh.ct_root);
        ctx.ht_manager.add_child(target.ht_root(), self.eh.ht_root);
    }

    fn unmount(&self, ctx: &mut MountContext) {
        ctx.composite_tree.remove_child(self.eh.ct_root);
        ctx.ht_manager.remove_child(self.eh.ht_root);
    }

    fn teardown(self, ctx: &mut TeardownContext) {
        ctx.mount_context.composite_tree.free_all(self.eh.ct_root);
        ctx.mount_context.ht_manager.free_all(self.eh.ht_root);
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
impl ObjectTreeObjectRowEventHandler {
    fn update_selected<E>(
        &self,
        selected: bool,
        composite_tree: &mut CompositeTree<E>,
        current_sec: f32,
    ) {
        if self.selection_lit.replace(selected) == selected {
            // not changed
            return;
        }

        if selected {
            composite_tree
                .begin_mod_chain(self.ct_root)
                .composite_mode(CompositeMode::FillColor(AnimatableColor::Animated {
                    from_value: [0.0, 0.25, 1.0, 0.0],
                    to_value: [0.0, 0.25, 1.0, 1.0],
                    curve: AnimationCurve::EASE_OUT,
                    event_on_complete: None,
                    sec_duration: (current_sec..current_sec + 0.1).into(),
                }))
                .apply();
        } else {
            composite_tree
                .begin_mod_chain(self.ct_root)
                .composite_mode(CompositeMode::FillColor(AnimatableColor::Animated {
                    from_value: [0.0, 0.25, 1.0, 1.0],
                    to_value: [0.0, 0.25, 1.0, 0.0],
                    curve: AnimationCurve::EASE_OUT,
                    event_on_complete: None,
                    sec_duration: (current_sec..current_sec + 0.1).into(),
                }))
                .apply();
        }
    }
}
