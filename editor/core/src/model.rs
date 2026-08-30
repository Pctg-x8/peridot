use std::{
    collections::{BTreeSet, HashSet, VecDeque},
    num::NonZeroUsize,
};

use peridot_math::{Matrix4, Matrix4F32, One, Quaternion, Ray3, Sphere3, Vector3F32};

use crate::uikit::ViewFeedbackQueue;

pub mod asset_explorer;

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ObjectID(NonZeroUsize);
impl core::fmt::Display for ObjectID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{}", self.0.get())
    }
}
impl ObjectID {
    pub const fn from_array_index(v: usize) -> Self {
        Self(unsafe { NonZeroUsize::new_unchecked(v.checked_add(1).expect("too many objects!")) })
    }

    pub const fn into_array_index(self) -> usize {
        self.0.get() - 1
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum ObjectRenderShape {
    Plane,
    Cube,
    Sphere,
    Cylinder,
    Capsule,
}

pub struct Object {
    pub parent: Option<ObjectID>,
    pub children: Vec<ObjectID>,
    name: String,
    pub local_position: peridot_math::Vector3F32,
    pub local_rotation_euler: peridot_math::Vector3F32,
    pub local_scale: peridot_math::Vector3F32,
    pub world_matrix: peridot_math::Matrix4F32,
    pub render_enabled: bool,
    pub render_shape: ObjectRenderShape,
    pub render_id: Option<usize>,
    pub render_dirty: bool,
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
            render_id: None,
            render_dirty: true,
        }
    }

    fn duplicate_single(&self) -> Self {
        Self {
            parent: None,
            children: Vec::new(),
            name: self.name.clone(),
            local_position: self.local_position,
            local_rotation_euler: self.local_rotation_euler,
            local_scale: self.local_scale,
            world_matrix: self.world_matrix.clone(),
            render_enabled: self.render_enabled,
            render_shape: self.render_shape,
            render_id: None,
            render_dirty: true,
        }
    }

    fn reset(&mut self) {
        self.name = String::new();
        self.children = Vec::new();
        self.parent = None;
    }

    #[inline(always)]
    pub fn compute_local_matrix(&self) -> Matrix4F32 {
        Matrix4::trs(
            self.local_position,
            Quaternion::from_euler_zyx(
                self.local_rotation_euler * (core::f32::consts::TAU / 360.0),
            ),
            self.local_scale,
        )
    }

    pub fn hittest_ray(&self, ray: &Ray3<f32>) -> bool {
        if !self.render_enabled {
            // no hittest geometry
            return false;
        }

        match self.render_shape {
            ObjectRenderShape::Plane => {
                // TODO: plane test
                false
            }
            ObjectRenderShape::Cube => {
                // TODO: cube test
                false
            }
            ObjectRenderShape::Sphere => {
                // TODO: sphere test
                false
            }
            ObjectRenderShape::Cylinder => {
                // TODO: cylinder test
                false
            }
            ObjectRenderShape::Capsule => {
                // TODO: capsule test
                false
            }
        }
    }
}

/// Logical Application Model
pub struct Application {
    pub objects: Vec<Object>,
    free_object_indices: BTreeSet<usize>,
    root_objects: Vec<ObjectID>,
    pub selected_objects: HashSet<ObjectID>,
    preview_edit_tool_type: PreviewEditToolType,
    pub removed_object_render_ids: Vec<usize>,
    pub world_matrix_recompute_targets: HashSet<ObjectID>,
    asset_explorer: self::asset_explorer::State,
}
impl Application {
    pub fn new() -> Self {
        Self {
            objects: Vec::new(),
            free_object_indices: BTreeSet::new(),
            root_objects: Vec::new(),
            selected_objects: HashSet::new(),
            preview_edit_tool_type: PreviewEditToolType::Translate,
            removed_object_render_ids: Vec::new(),
            world_matrix_recompute_targets: HashSet::new(),
            asset_explorer: self::asset_explorer::State::new(),
        }
    }

    pub fn sync(&self, feedback_queue: &mut ViewFeedbackQueue) {
        feedback_queue.push(ViewFeedbackObjectTreeChanged);
        feedback_queue.push(ViewFeedbackObjectSelectionChanged);
        feedback_queue.push(ViewFeedbackPreviewEditToolTypeChanged);
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
        self.removed_object_render_ids
            .extend(self.objects[id.into_array_index()].render_id.take());
        self.objects[id.into_array_index()].reset();

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

    fn should_world_matrix_recompute(&mut self, id: ObjectID) {
        self.world_matrix_recompute_targets.insert(id);
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
    fn dispatch_view_feedback<T: 'static>(&mut self, feedback: T);
}

pub fn object_is_selected(env: &(impl ApplicationAccess + ?Sized), id: ObjectID) -> bool {
    env.application().selected_objects.contains(&id)
}

pub fn object_tree_content(
    env: &(impl ApplicationAccess + ?Sized),
) -> Vec<(ObjectID, &str, usize)> {
    let mut results = Vec::new();
    let mut process_stack = VecDeque::new();
    process_stack.extend(env.application().root_objects.iter().map(|&id| (id, 0)));
    while let Some((id, depth)) = process_stack.pop_front() {
        results.push((
            id,
            env.application().objects[id.into_array_index()]
                .name
                .as_str(),
            depth,
        ));
        for &child in env.application().objects[id.into_array_index()]
            .children
            .iter()
            .rev()
        {
            process_stack.push_front((child, depth + 1));
        }
    }

    results
}

pub enum ObjectSelectionState {
    None,
    Single { id: ObjectID },
    Multiple,
}
pub fn selection_state(env: &(impl ApplicationAccess + ?Sized)) -> ObjectSelectionState {
    match env.application().selected_objects.len() {
        0 => ObjectSelectionState::None,
        1 => {
            let id = *env
                .application()
                .selected_objects
                .iter()
                .next()
                .expect("no selection?");

            ObjectSelectionState::Single { id }
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

pub fn object_create_of_shape(
    env: &mut (impl ApplicationMutableAccess + ?Sized),
    name: String,
    shape: ObjectRenderShape,
) -> ObjectID {
    let id = env.application_mut().alloc_object(Object::new(name));
    env.application_mut().objects[id.into_array_index()].render_shape = shape;
    env.dispatch_view_feedback(ViewFeedbackObjectTreeChanged);
    id
}

pub fn object_create_of_shape_children_of_selected(
    env: &mut (impl ApplicationMutableAccess + ?Sized),
    name: String,
    shape: ObjectRenderShape,
) -> Option<ObjectID> {
    let Some(&selected) = env.application_mut().selected_objects.iter().next() else {
        return None;
    };

    let id = object_create_of_shape(env, name, shape);
    object_set_parent(env, id, selected);
    Some(id)
}

pub fn object_duplicate_selected(
    env: &mut (impl ApplicationMutableAccess + ?Sized),
) -> Option<ObjectID> {
    let Some(&selected) = env.application_mut().selected_objects.iter().next() else {
        return None;
    };

    // root object
    let new_object = env.application().objects[selected.into_array_index()].duplicate_single();
    let id = env.application_mut().alloc_object(new_object);
    if let Some(parent) = env.application().objects[selected.into_array_index()].parent {
        object_set_parent(env, id, parent);
    }

    // recursibly duplicate children
    let mut process_stack = Vec::new();
    process_stack.extend(
        env.application().objects[selected.into_array_index()]
            .children
            .iter()
            .map(|&child| (child, id)),
    );
    while let Some((src_id, parent)) = process_stack.pop() {
        let new_object = env.application().objects[src_id.into_array_index()].duplicate_single();
        let id = env.application_mut().alloc_object(new_object);
        object_set_parent(env, id, parent);
        process_stack.extend(
            env.application().objects[src_id.into_array_index()]
                .children
                .iter()
                .map(|&child| (child, id)),
        );
    }

    env.dispatch_view_feedback(ViewFeedbackObjectTreeChanged);
    Some(id)
}

pub fn object_destroy_selected(env: &mut (impl ApplicationMutableAccess + ?Sized)) {
    let state = env.application_mut();
    for id in state.selected_objects.drain() {
        let mut destroy_targets = Vec::new();
        let mut process_stack = Vec::new();
        process_stack.push(id);
        while let Some(id) = process_stack.pop() {
            destroy_targets.push(id);
            process_stack.extend(
                state.objects[id.into_array_index()]
                    .children
                    .iter()
                    .copied(),
            );
        }

        // 子から消していく
        while let Some(id) = destroy_targets.pop() {
            // detach from registry
            match state.objects[id.into_array_index()].parent.take() {
                Some(parent) => {
                    state.objects[parent.into_array_index()]
                        .children
                        .retain(|&oid| oid != id);
                }
                None => {
                    state.root_objects.retain(|&oid| oid != id);
                }
            }

            state.free_object_indices.insert(id.into_array_index());
            state
                .removed_object_render_ids
                .extend(state.objects[id.into_array_index()].render_id.take());
            state.objects[id.into_array_index()].reset();
        }
    }

    // TODO: compactionの頻度を減らすかはあとで検討
    state.compaction_objects();

    env.dispatch_view_feedback(ViewFeedbackObjectTreeChanged);
    env.dispatch_view_feedback(ViewFeedbackObjectSelectionChanged);
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
    env.application_mut().should_world_matrix_recompute(id);
    env.dispatch_view_feedback(ViewFeedbackObjectTreeChanged);
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
    env.application_mut().root_objects.push(child);
    env.application_mut().should_world_matrix_recompute(child);

    env.dispatch_view_feedback(ViewFeedbackObjectTreeChanged);
}

pub fn object_modify_data(
    env: &mut (impl ApplicationMutableAccess + ?Sized),
    id: ObjectID,
    updater: impl FnOnce(&mut Object),
) {
    updater(&mut env.application_mut().objects[id.into_array_index()]);
    env.dispatch_view_feedback(ViewFeedbackObjectDataChanged(id));
}

pub fn object_name(env: &(impl ApplicationAccess + ?Sized), id: ObjectID) -> &str {
    &env.application().objects[id.into_array_index()].name
}

pub fn selected_object_name(env: &(impl ApplicationAccess + ?Sized)) -> Option<&str> {
    match env.application().selected_objects.len() {
        0 => None,
        1 => {
            let id = env.application().selected_objects.iter().next().unwrap();
            let name = &env.application().objects[id.into_array_index()].name;
            Some(name.as_str())
        }
        _ => None,
    }
}

// TODO: multiple selection
pub fn set_selected_object_name(env: &mut (impl ApplicationMutableAccess + ?Sized), name: String) {
    let Some(&selected) = env.application_mut().selected_objects.iter().next() else {
        return;
    };

    env.application_mut().objects[selected.into_array_index()].name = name;
    env.dispatch_view_feedback(ViewFeedbackObjectNameChanged(selected));
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
    env.application_mut()
        .should_world_matrix_recompute(selected);
}

pub fn set_selected_object_local_translate_y(
    env: &mut (impl ApplicationMutableAccess + ?Sized),
    v: f32,
) {
    let Some(&selected) = env.application_mut().selected_objects.iter().next() else {
        return;
    };

    object_modify_data(env, selected, |o| o.local_position.1 = v);
    env.application_mut()
        .should_world_matrix_recompute(selected);
}

pub fn set_selected_object_local_translate_z(
    env: &mut (impl ApplicationMutableAccess + ?Sized),
    v: f32,
) {
    let Some(&selected) = env.application_mut().selected_objects.iter().next() else {
        return;
    };

    object_modify_data(env, selected, |o| o.local_position.2 = v);
    env.application_mut()
        .should_world_matrix_recompute(selected);
}

pub fn set_selected_object_local_rotation_x(
    env: &mut (impl ApplicationMutableAccess + ?Sized),
    v: f32,
) {
    let Some(&selected) = env.application_mut().selected_objects.iter().next() else {
        return;
    };

    object_modify_data(env, selected, |o| o.local_rotation_euler.0 = v);
    env.application_mut()
        .should_world_matrix_recompute(selected);
}

pub fn set_selected_object_local_rotation_y(
    env: &mut (impl ApplicationMutableAccess + ?Sized),
    v: f32,
) {
    let Some(&selected) = env.application_mut().selected_objects.iter().next() else {
        return;
    };

    object_modify_data(env, selected, |o| o.local_rotation_euler.1 = v);
    env.application_mut()
        .should_world_matrix_recompute(selected);
}

pub fn set_selected_object_local_rotation_z(
    env: &mut (impl ApplicationMutableAccess + ?Sized),
    v: f32,
) {
    let Some(&selected) = env.application_mut().selected_objects.iter().next() else {
        return;
    };

    object_modify_data(env, selected, |o| o.local_rotation_euler.2 = v);
    env.application_mut()
        .should_world_matrix_recompute(selected);
}

pub fn set_selected_object_local_scale_x(
    env: &mut (impl ApplicationMutableAccess + ?Sized),
    v: f32,
) {
    let Some(&selected) = env.application_mut().selected_objects.iter().next() else {
        return;
    };

    object_modify_data(env, selected, |o| o.local_scale.0 = v);
    env.application_mut()
        .should_world_matrix_recompute(selected);
}

pub fn set_selected_object_local_scale_y(
    env: &mut (impl ApplicationMutableAccess + ?Sized),
    v: f32,
) {
    let Some(&selected) = env.application_mut().selected_objects.iter().next() else {
        return;
    };

    object_modify_data(env, selected, |o| o.local_scale.1 = v);
    env.application_mut()
        .should_world_matrix_recompute(selected);
}

pub fn set_selected_object_local_scale_z(
    env: &mut (impl ApplicationMutableAccess + ?Sized),
    v: f32,
) {
    let Some(&selected) = env.application_mut().selected_objects.iter().next() else {
        return;
    };

    object_modify_data(env, selected, |o| o.local_scale.2 = v);
    env.application_mut()
        .should_world_matrix_recompute(selected);
}

pub fn set_selected_object_local_scale(
    env: &mut (impl ApplicationMutableAccess + ?Sized),
    v: Vector3F32,
) {
    let Some(&selected) = env.application_mut().selected_objects.iter().next() else {
        return;
    };

    object_modify_data(env, selected, |o| o.local_scale = v);
    env.application_mut()
        .should_world_matrix_recompute(selected);
}

pub fn apply_selected_object_local_translate_delta(
    env: &mut (impl ApplicationMutableAccess + ?Sized),
    delta: Vector3F32,
) {
    let Some(&selected) = env.application_mut().selected_objects.iter().next() else {
        return;
    };

    object_modify_data(env, selected, |o| o.local_position += delta);
    env.application_mut()
        .should_world_matrix_recompute(selected);
}

pub fn apply_selected_object_local_rotate_delta(
    env: &mut (impl ApplicationMutableAccess + ?Sized),
    delta: Vector3F32,
) {
    let Some(&selected) = env.application_mut().selected_objects.iter().next() else {
        return;
    };

    object_modify_data(env, selected, |o| o.local_rotation_euler += delta);
    env.application_mut()
        .should_world_matrix_recompute(selected);
}

pub fn apply_selected_object_local_scale_delta(
    env: &mut (impl ApplicationMutableAccess + ?Sized),
    delta: Vector3F32,
) {
    let Some(&selected) = env.application_mut().selected_objects.iter().next() else {
        return;
    };

    object_modify_data(env, selected, |o| o.local_scale += delta);
    env.application_mut()
        .should_world_matrix_recompute(selected);
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

    object_modify_data(env, selected, |o| {
        o.render_enabled = !o.render_enabled;
        o.render_dirty = true;
    });
}

pub fn selected_object_render_shape(
    env: &(impl ApplicationAccess + ?Sized),
) -> Option<ObjectRenderShape> {
    let Some(&selected) = env.application().selected_objects.iter().next() else {
        return None;
    };

    Some(env.application().object(selected).render_shape)
}

/// TODO: multiple select
pub fn set_selected_object_render_shape(
    env: &mut (impl ApplicationMutableAccess + ?Sized),
    shape: ObjectRenderShape,
) {
    let Some(&selected) = env.application_mut().selected_objects.iter().next() else {
        return;
    };

    object_modify_data(env, selected, |o| {
        o.render_shape = shape;
        o.render_dirty = true;
    });
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
    env.dispatch_view_feedback(ViewFeedbackObjectSelectionChanged);
}

pub fn toggle_object_selection_additive(
    env: &mut (impl ApplicationMutableAccess + ?Sized),
    id: ObjectID,
) {
    if !env.application_mut().selected_objects.insert(id) {
        // selecting
        env.application_mut().selected_objects.remove(&id);
    }

    env.dispatch_view_feedback(ViewFeedbackObjectSelectionChanged);
}

pub fn object_deselect_all(env: &mut (impl ApplicationMutableAccess + ?Sized)) {
    env.application_mut().selected_objects.clear();
    env.dispatch_view_feedback(ViewFeedbackObjectSelectionChanged);
}

pub fn set_preview_edit_tool_type(
    env: &mut (impl ApplicationMutableAccess + ?Sized),
    tool_type: PreviewEditToolType,
) {
    env.application_mut().preview_edit_tool_type = tool_type;
    env.dispatch_view_feedback(ViewFeedbackPreviewEditToolTypeChanged);
}

pub struct ApplicationMutation<'a> {
    pub state: &'a mut Application,
    pub view_feedbacks: &'a mut ViewFeedbackQueue,
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
    fn dispatch_view_feedback<T: 'static>(&mut self, feedback: T) {
        self.view_feedbacks.push(feedback);
    }
}

pub struct ViewFeedbackObjectTreeChanged;
pub struct ViewFeedbackObjectSelectionChanged;
pub struct ViewFeedbackObjectNameChanged(pub ObjectID);
pub struct ViewFeedbackObjectDataChanged(pub ObjectID);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PreviewEditToolType {
    Translate,
    Rotate,
    Scale,
}

pub struct ViewFeedbackPreviewEditToolTypeChanged;
