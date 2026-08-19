use std::{
    collections::{BTreeSet, HashSet, VecDeque},
    num::NonZeroUsize,
};

use peridot_math::{One, Vector3F32};

use crate::uikit::{ViewFeedbackContext, ViewFeedbackRegistry};

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ObjectID(NonZeroUsize);
impl core::fmt::Display for ObjectID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{}", self.0.get())
    }
}
impl ObjectID {
    const fn from_array_index(v: usize) -> Self {
        Self(unsafe { NonZeroUsize::new_unchecked(v.checked_add(1).expect("too many objects!")) })
    }

    const fn into_array_index(self) -> usize {
        self.0.get() - 1
    }
}

pub enum ObjectRenderShape {
    Cube,
    Sphere,
    Cylinder,
    Capsule,
}

pub struct Object {
    parent: Option<ObjectID>,
    children: Vec<ObjectID>,
    name: String,
    local_position: peridot_math::Vector3F32,
    local_rotation_euler: peridot_math::Vector3F32,
    local_scale: peridot_math::Vector3F32,
    world_matrix: peridot_math::Matrix4F32,
    render_enabled: bool,
    render_shape: ObjectRenderShape,
}
impl Object {
    fn new(name: String) -> Self {
        Self {
            parent: None,
            children: Vec::new(),
            name,
            local_position: peridot_math::Vector3(0.0, 0.0, 0.0),
            local_rotation_euler: peridot_math::Vector3(0.0, 0.0, 0.0),
            local_scale: peridot_math::Vector3(1.0, 1.0, 1.0),
            world_matrix: peridot_math::Matrix4F32::ONE,
            render_enabled: true,
            render_shape: ObjectRenderShape::Cube,
        }
    }

    fn reset(&mut self) {
        self.name = String::new();
        self.children = Vec::new();
        self.parent = None;
    }
}

/// Logical Application Model
pub struct Application {
    objects: Vec<Object>,
    free_object_indices: BTreeSet<usize>,
    root_objects: Vec<ObjectID>,
    selected_objects: HashSet<ObjectID>,
    preview_edit_tool_type: PreviewEditToolType,
}
impl Application {
    pub fn new() -> Self {
        Self {
            objects: Vec::new(),
            free_object_indices: BTreeSet::new(),
            root_objects: Vec::new(),
            selected_objects: HashSet::new(),
            preview_edit_tool_type: PreviewEditToolType::Translate,
        }
    }

    pub fn sync(&self, feedback_queue: &mut VecDeque<ViewFeedback>) {
        feedback_queue.extend([
            ViewFeedback::object_tree_changed(),
            ViewFeedback::object_selection_changed(),
            ViewFeedback::preview_edit_tool_type_changed(),
        ]);
    }

    fn alloc_object(&mut self, o: Object) -> ObjectID {
        if let Some(index) = self.free_object_indices.pop_first() {
            self.objects[index] = o;
            self.root_objects.push(ObjectID::from_array_index(index));
            return ObjectID::from_array_index(index);
        }

        let index = self.objects.len();
        self.objects.push(o);
        self.root_objects.push(ObjectID::from_array_index(index));
        ObjectID::from_array_index(index)
    }

    fn free_object(&mut self, id: ObjectID) {
        // detach from registry
        match self.objects[id.into_array_index()].parent.take() {
            Some(parent) => {
                self.objects[parent.into_array_index()]
                    .children
                    .retain(|&oid| oid != id);
            }
            None => {
                self.root_objects.retain(|&oid| oid != id);
            }
        }

        self.free_object_indices.insert(id.into_array_index());
        self.objects[id.into_array_index()].reset();

        // TODO: compactionの頻度を減らすかはあとで検討
        self.compaction_objects();
    }

    fn free_object_selected(&mut self) {
        for id in self.selected_objects.drain() {
            // detach from registry
            match self.objects[id.into_array_index()].parent.take() {
                Some(parent) => {
                    self.objects[parent.into_array_index()]
                        .children
                        .retain(|&oid| oid != id);
                }
                None => {
                    self.root_objects.retain(|&oid| oid != id);
                }
            }

            self.free_object_indices.insert(id.into_array_index());
            self.objects[id.into_array_index()].reset();
        }

        // TODO: compactionの頻度を減らすかはあとで検討
        self.compaction_objects();
    }

    fn compaction_objects(&mut self) {
        // objectsのうしろにいるfreeを解放
        while !self.objects.is_empty() && self.free_object_indices.remove(&(self.objects.len() - 1))
        {
            self.objects.pop();
        }

        self.objects.shrink_to_fit();
    }

    pub fn object(&self, id: ObjectID) -> &Object {
        &self.objects[id.into_array_index()]
    }
}

pub trait ApplicationAccess {
    fn application(&self) -> &Application;
}
impl ApplicationAccess for Application {
    #[inline(always)]
    fn application(&self) -> &Application {
        self
    }
}

pub trait ApplicationMutableAccess: ApplicationAccess {
    fn application_mut(&mut self) -> &mut Application;
    fn dispatch_view_feedback(&mut self, feedback: ViewFeedback);
}

pub fn object_is_selected(env: &(impl ApplicationAccess + ?Sized), id: ObjectID) -> bool {
    env.application().selected_objects.contains(&id)
}

pub fn object_tree_content(
    env: &(impl ApplicationAccess + ?Sized),
) -> impl Iterator<Item = (usize, ObjectID, &str)> {
    env.application()
        .root_objects
        .iter()
        .enumerate()
        .map(move |(n, &id)| (n, id, env.application().object(id).name.as_str()))
}

pub enum ObjectSelectionState<'a> {
    None,
    Single { id: ObjectID, name: &'a str },
    Multiple,
}
pub fn selection_state<'a>(env: &'a (impl ApplicationAccess + ?Sized)) -> ObjectSelectionState<'a> {
    match env.application().selected_objects.len() {
        0 => ObjectSelectionState::None,
        1 => {
            let id = *env
                .application()
                .selected_objects
                .iter()
                .next()
                .expect("no selection?");

            ObjectSelectionState::Single {
                id,
                name: &env.application().object(id).name,
            }
        }
        _ => ObjectSelectionState::Multiple,
    }
}

pub fn preview_edit_tool_type(env: &(impl ApplicationAccess + ?Sized)) -> PreviewEditToolType {
    env.application().preview_edit_tool_type
}

// TODO: multiple selection
pub fn selected_object_local_translate_x(env: &(impl ApplicationAccess + ?Sized)) -> f32 {
    let id = env
        .application()
        .selected_objects
        .iter()
        .next()
        .copied()
        .expect("no selection");
    env.application().object(id).local_position.0
}

pub fn selected_object_local_translate_y(env: &(impl ApplicationAccess + ?Sized)) -> f32 {
    let id = env
        .application()
        .selected_objects
        .iter()
        .next()
        .copied()
        .expect("no selection");
    env.application().object(id).local_position.1
}

pub fn selected_object_local_translate_z(env: &(impl ApplicationAccess + ?Sized)) -> f32 {
    let id = env
        .application()
        .selected_objects
        .iter()
        .next()
        .copied()
        .expect("no selection");
    env.application().object(id).local_position.2
}

pub fn selected_object_local_rotate_x(env: &(impl ApplicationAccess + ?Sized)) -> f32 {
    let id = env
        .application()
        .selected_objects
        .iter()
        .next()
        .copied()
        .expect("no selection");
    env.application().object(id).local_rotation_euler.0
}

pub fn selected_object_local_rotate_y(env: &(impl ApplicationAccess + ?Sized)) -> f32 {
    let id = env
        .application()
        .selected_objects
        .iter()
        .next()
        .copied()
        .expect("no selection");
    env.application().object(id).local_rotation_euler.1
}

pub fn selected_object_local_rotate_z(env: &(impl ApplicationAccess + ?Sized)) -> f32 {
    let id = env
        .application()
        .selected_objects
        .iter()
        .next()
        .copied()
        .expect("no selection");
    env.application().object(id).local_rotation_euler.2
}

pub fn selected_object_local_scale_x(env: &(impl ApplicationAccess + ?Sized)) -> f32 {
    let id = env
        .application()
        .selected_objects
        .iter()
        .next()
        .copied()
        .expect("no selection");
    env.application().object(id).local_scale.0
}

pub fn selected_object_local_scale_y(env: &(impl ApplicationAccess + ?Sized)) -> f32 {
    let id = env
        .application()
        .selected_objects
        .iter()
        .next()
        .copied()
        .expect("no selection");
    env.application().object(id).local_scale.1
}

pub fn selected_object_local_scale_z(env: &(impl ApplicationAccess + ?Sized)) -> f32 {
    let id = env
        .application()
        .selected_objects
        .iter()
        .next()
        .copied()
        .expect("no selection");
    env.application().object(id).local_scale.2
}

pub fn object_create(env: &mut (impl ApplicationMutableAccess + ?Sized), name: String) -> ObjectID {
    let id = env.application_mut().alloc_object(Object::new(name));
    env.dispatch_view_feedback(ViewFeedback::object_tree_changed());
    id
}

pub fn object_destroy_selected(env: &mut (impl ApplicationMutableAccess + ?Sized)) {
    env.application_mut().free_object_selected();
    env.dispatch_view_feedback(ViewFeedback::object_tree_changed());
    env.dispatch_view_feedback(ViewFeedback::object_selection_changed());
}

pub fn object_set_parent(
    env: &mut (impl ApplicationMutableAccess + ?Sized),
    id: ObjectID,
    parent: ObjectID,
) {
    match env.application_mut().objects[id.into_array_index()]
        .parent
        .replace(parent)
    {
        None => {
            // detach from root
            env.application_mut().root_objects.retain(|&oid| oid != id);
        }
        Some(old_parent) if old_parent == parent => {
            // already linked
            return;
        }
        Some(old_parent) => {
            // detach from old parent
            env.application_mut().objects[old_parent.into_array_index()]
                .children
                .retain(|&oid| oid != id);
        }
    }

    env.application_mut().objects[parent.into_array_index()]
        .children
        .push(id);
    env.dispatch_view_feedback(ViewFeedback::object_tree_changed());
}

pub fn object_detach_parent(env: &mut (impl ApplicationMutableAccess + ?Sized), child: ObjectID) {
    let Some(parent) = env.application_mut().objects[child.into_array_index()]
        .parent
        .take()
    else {
        // already on root
        return;
    };

    env.application_mut().objects[parent.into_array_index()]
        .children
        .retain(|&id| id != child);
    env.application_mut().root_objects.push(parent);

    env.dispatch_view_feedback(ViewFeedback::object_tree_changed());
}

pub fn object_modify_data(
    env: &mut (impl ApplicationMutableAccess + ?Sized),
    id: ObjectID,
    updater: impl FnOnce(&mut Object),
) {
    updater(&mut env.application_mut().objects[id.into_array_index()]);
    env.dispatch_view_feedback(ViewFeedback::object_data_changed(id));
}

// TODO: multiple selection
pub fn set_selected_object_local_translate_x(
    env: &mut (impl ApplicationMutableAccess + ?Sized),
    v: f32,
) {
    let Some(&selected) = env.application_mut().selected_objects.iter().next() else {
        return;
    };

    object_modify_data(env, selected, |o| o.local_position.0 = v);
}

pub fn set_selected_object_local_translate_y(
    env: &mut (impl ApplicationMutableAccess + ?Sized),
    v: f32,
) {
    let Some(&selected) = env.application_mut().selected_objects.iter().next() else {
        return;
    };

    object_modify_data(env, selected, |o| o.local_position.1 = v);
}

pub fn set_selected_object_local_translate_z(
    env: &mut (impl ApplicationMutableAccess + ?Sized),
    v: f32,
) {
    let Some(&selected) = env.application_mut().selected_objects.iter().next() else {
        return;
    };

    object_modify_data(env, selected, |o| o.local_position.2 = v);
}

pub fn set_selected_object_local_rotation_x(
    env: &mut (impl ApplicationMutableAccess + ?Sized),
    v: f32,
) {
    let Some(&selected) = env.application_mut().selected_objects.iter().next() else {
        return;
    };

    object_modify_data(env, selected, |o| o.local_rotation_euler.0 = v);
}

pub fn set_selected_object_local_rotation_y(
    env: &mut (impl ApplicationMutableAccess + ?Sized),
    v: f32,
) {
    let Some(&selected) = env.application_mut().selected_objects.iter().next() else {
        return;
    };

    object_modify_data(env, selected, |o| o.local_rotation_euler.1 = v);
}

pub fn set_selected_object_local_rotation_z(
    env: &mut (impl ApplicationMutableAccess + ?Sized),
    v: f32,
) {
    let Some(&selected) = env.application_mut().selected_objects.iter().next() else {
        return;
    };

    object_modify_data(env, selected, |o| o.local_rotation_euler.2 = v);
}

pub fn set_selected_object_local_scale_x(
    env: &mut (impl ApplicationMutableAccess + ?Sized),
    v: f32,
) {
    let Some(&selected) = env.application_mut().selected_objects.iter().next() else {
        return;
    };

    object_modify_data(env, selected, |o| o.local_scale.0 = v);
}

pub fn set_selected_object_local_scale_y(
    env: &mut (impl ApplicationMutableAccess + ?Sized),
    v: f32,
) {
    let Some(&selected) = env.application_mut().selected_objects.iter().next() else {
        return;
    };

    object_modify_data(env, selected, |o| o.local_scale.1 = v);
}

pub fn set_selected_object_local_scale_z(
    env: &mut (impl ApplicationMutableAccess + ?Sized),
    v: f32,
) {
    let Some(&selected) = env.application_mut().selected_objects.iter().next() else {
        return;
    };

    object_modify_data(env, selected, |o| o.local_scale.2 = v);
}

pub fn apply_selected_object_local_translate_delta(
    env: &mut (impl ApplicationMutableAccess + ?Sized),
    delta: Vector3F32,
) {
    let Some(&selected) = env.application_mut().selected_objects.iter().next() else {
        return;
    };

    object_modify_data(env, selected, |o| o.local_position += delta);
}

pub fn apply_selected_object_local_rotate_delta(
    env: &mut (impl ApplicationMutableAccess + ?Sized),
    delta: Vector3F32,
) {
    let Some(&selected) = env.application_mut().selected_objects.iter().next() else {
        return;
    };

    object_modify_data(env, selected, |o| o.local_rotation_euler += delta);
}

pub fn apply_selected_object_local_scale_delta(
    env: &mut (impl ApplicationMutableAccess + ?Sized),
    delta: Vector3F32,
) {
    let Some(&selected) = env.application_mut().selected_objects.iter().next() else {
        return;
    };

    object_modify_data(env, selected, |o| o.local_scale += delta);
}

pub fn selected_object_render_is_enabled(env: &(impl ApplicationAccess + ?Sized)) -> bool {
    let Some(&selected) = env.application().selected_objects.iter().next() else {
        return false;
    };

    env.application().object(selected).render_enabled
}

pub fn toggle_selected_object_render_enable(env: &mut (impl ApplicationMutableAccess + ?Sized)) {
    let Some(&selected) = env.application_mut().selected_objects.iter().next() else {
        return;
    };

    object_modify_data(env, selected, |o| o.render_enabled = !o.render_enabled);
}

pub fn select_object(env: &mut (impl ApplicationMutableAccess + ?Sized), id: ObjectID) {
    if env.application_mut().selected_objects.len() == 1
        && env
            .application_mut()
            .selected_objects
            .iter()
            .next()
            .is_some_and(|&x| x == id)
    {
        // already selected
        return;
    }

    env.application_mut().selected_objects.clear();
    env.application_mut().selected_objects.insert(id);
    env.dispatch_view_feedback(ViewFeedback::object_selection_changed());
}

pub fn toggle_object_selection_additive(
    env: &mut (impl ApplicationMutableAccess + ?Sized),
    id: ObjectID,
) {
    if !env.application_mut().selected_objects.insert(id) {
        // selecting
        env.application_mut().selected_objects.remove(&id);
    }

    env.dispatch_view_feedback(ViewFeedback::object_selection_changed());
}

pub fn set_preview_edit_tool_type(
    env: &mut (impl ApplicationMutableAccess + ?Sized),
    tool_type: PreviewEditToolType,
) {
    env.application_mut().preview_edit_tool_type = tool_type;
    env.dispatch_view_feedback(ViewFeedback::preview_edit_tool_type_changed());
}

pub struct ApplicationMutation<'a> {
    pub state: &'a mut Application,
    pub view_feedbacks: &'a mut VecDeque<ViewFeedback>,
}
impl core::ops::Deref for ApplicationMutation<'_> {
    type Target = Application;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        self.state
    }
}
impl ApplicationAccess for ApplicationMutation<'_> {
    #[inline(always)]
    fn application(&self) -> &Application {
        self.state
    }
}
impl ApplicationMutableAccess for ApplicationMutation<'_> {
    #[inline(always)]
    fn application_mut(&mut self) -> &mut Application {
        self.state
    }

    #[inline(always)]
    fn dispatch_view_feedback(&mut self, feedback: ViewFeedback) {
        self.view_feedbacks.push_back(feedback);
    }
}

pub enum ViewFeedback {
    ObjectTreeChanged(ViewFeedbackObjectTreeChanged),
    ObjectSelectionChanged(ViewFeedbackObjectSelectionChanged),
    ObjectDataChanged(ViewFeedbackObjectDataChanged),
    PreviewEditToolTypeChanged(ViewFeedbackPreviewEditToolTypeChanged),
}
impl ViewFeedback {
    pub const fn object_tree_changed() -> Self {
        Self::ObjectTreeChanged(ViewFeedbackObjectTreeChanged)
    }

    pub const fn object_selection_changed() -> Self {
        Self::ObjectSelectionChanged(ViewFeedbackObjectSelectionChanged)
    }

    pub const fn object_data_changed(object_id: ObjectID) -> Self {
        Self::ObjectDataChanged(ViewFeedbackObjectDataChanged(object_id))
    }

    pub const fn preview_edit_tool_type_changed() -> Self {
        Self::PreviewEditToolTypeChanged(ViewFeedbackPreviewEditToolTypeChanged)
    }

    pub fn dispatch(self, registry: &ViewFeedbackRegistry, context: &mut ViewFeedbackContext) {
        match self {
            Self::ObjectTreeChanged(o) => registry.dispatch(o, context),
            Self::ObjectSelectionChanged(o) => registry.dispatch(o, context),
            Self::ObjectDataChanged(o) => registry.dispatch(o, context),
            Self::PreviewEditToolTypeChanged(o) => registry.dispatch(o, context),
        }
    }
}

#[derive(Clone)]
pub struct ViewFeedbackObjectTreeChanged;

pub struct ViewFeedbackObjectSelectionChanged;

pub struct ViewFeedbackObjectDataChanged(pub ObjectID);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PreviewEditToolType {
    Translate,
    Rotate,
    Scale,
}

pub struct ViewFeedbackPreviewEditToolTypeChanged;
