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
            render_enabled: false,
            render_shape: ObjectRenderShape::Cube,
        }
    }

    fn reset(&mut self) {
        self.name = String::new();
        self.children = Vec::new();
        self.parent = None;
    }
}

pub enum ObjectSelectionState<'a> {
    None,
    Single { id: ObjectID, name: &'a str },
    Multiple,
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

    fn compaction_objects(&mut self) {
        // objectsのうしろにいるfreeを解放
        while self.free_object_indices.remove(&(self.objects.len() - 1)) {
            self.objects.pop();
        }

        self.objects.shrink_to_fit();
    }

    pub fn object(&self, id: ObjectID) -> &Object {
        &self.objects[id.into_array_index()]
    }

    pub fn object_is_selected(&self, id: ObjectID) -> bool {
        self.selected_objects.contains(&id)
    }

    pub fn object_tree_content(&self) -> impl Iterator<Item = (usize, ObjectID, &str)> {
        self.root_objects
            .iter()
            .enumerate()
            .map(|(n, &id)| (n, id, self.object(id).name.as_str()))
    }

    pub fn selection_state<'a>(&'a self) -> ObjectSelectionState<'a> {
        match self.selected_objects.len() {
            0 => ObjectSelectionState::None,
            1 => {
                let id = *self.selected_objects.iter().next().expect("no selection?");

                ObjectSelectionState::Single {
                    id,
                    name: &self.object(id).name,
                }
            }
            _ => ObjectSelectionState::Multiple,
        }
    }

    pub const fn preview_edit_tool_type(&self) -> PreviewEditToolType {
        self.preview_edit_tool_type
    }

    // TODO: multiple selection
    pub fn selected_object_local_translate_x(&self) -> f32 {
        let id = self
            .selected_objects
            .iter()
            .next()
            .copied()
            .expect("no selection");
        self.object(id).local_position.0
    }

    pub fn selected_object_local_translate_y(&self) -> f32 {
        let id = self
            .selected_objects
            .iter()
            .next()
            .copied()
            .expect("no selection");
        self.object(id).local_position.1
    }

    pub fn selected_object_local_translate_z(&self) -> f32 {
        let id = self
            .selected_objects
            .iter()
            .next()
            .copied()
            .expect("no selection");
        self.object(id).local_position.2
    }

    pub fn selected_object_local_rotate_x(&self) -> f32 {
        let id = self
            .selected_objects
            .iter()
            .next()
            .copied()
            .expect("no selection");
        self.object(id).local_rotation_euler.0
    }

    pub fn selected_object_local_rotate_y(&self) -> f32 {
        let id = self
            .selected_objects
            .iter()
            .next()
            .copied()
            .expect("no selection");
        self.object(id).local_rotation_euler.1
    }

    pub fn selected_object_local_rotate_z(&self) -> f32 {
        let id = self
            .selected_objects
            .iter()
            .next()
            .copied()
            .expect("no selection");
        self.object(id).local_rotation_euler.2
    }

    pub fn selected_object_local_scale_x(&self) -> f32 {
        let id = self
            .selected_objects
            .iter()
            .next()
            .copied()
            .expect("no selection");
        self.object(id).local_scale.0
    }

    pub fn selected_object_local_scale_y(&self) -> f32 {
        let id = self
            .selected_objects
            .iter()
            .next()
            .copied()
            .expect("no selection");
        self.object(id).local_scale.1
    }

    pub fn selected_object_local_scale_z(&self) -> f32 {
        let id = self
            .selected_objects
            .iter()
            .next()
            .copied()
            .expect("no selection");
        self.object(id).local_scale.2
    }
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
impl ApplicationMutation<'_> {
    pub fn object_create(&mut self, name: String) -> ObjectID {
        let id = self.state.alloc_object(Object::new(name));
        self.view_feedbacks
            .push_back(ViewFeedback::object_tree_changed());
        id
    }

    pub fn object_destroy(&mut self, id: ObjectID) {
        self.state.free_object(id);
        self.view_feedbacks
            .push_back(ViewFeedback::object_tree_changed());
    }

    pub fn object_set_parent(&mut self, id: ObjectID, parent: ObjectID) {
        match self.state.objects[id.into_array_index()]
            .parent
            .replace(parent)
        {
            None => {
                // detach from root
                self.state.root_objects.retain(|&oid| oid != id);
            }
            Some(old_parent) if old_parent == parent => {
                // already linked
                return;
            }
            Some(old_parent) => {
                // detach from old parent
                self.state.objects[old_parent.into_array_index()]
                    .children
                    .retain(|&oid| oid != id);
            }
        }

        self.state.objects[parent.into_array_index()]
            .children
            .push(id);
        self.view_feedbacks
            .push_back(ViewFeedback::object_tree_changed());
    }

    pub fn object_detach_parent(&mut self, child: ObjectID) {
        let Some(parent) = self.state.objects[child.into_array_index()].parent.take() else {
            // already on root
            return;
        };

        self.state.objects[parent.into_array_index()]
            .children
            .retain(|&id| id != child);
        self.state.root_objects.push(parent);

        self.view_feedbacks
            .push_back(ViewFeedback::object_tree_changed());
    }

    pub fn object_modify_data(&mut self, id: ObjectID, updater: impl FnOnce(&mut Object)) {
        updater(&mut self.state.objects[id.into_array_index()]);
        self.view_feedbacks
            .push_back(ViewFeedback::object_data_changed(id));
    }

    // TODO: multiple selection
    pub fn set_selected_object_local_translate_x(&mut self, v: f32) {
        let Some(&selected) = self.selected_objects.iter().next() else {
            return;
        };

        self.object_modify_data(selected, |o| o.local_position.0 = v);
    }

    pub fn set_selected_object_local_translate_y(&mut self, v: f32) {
        let Some(&selected) = self.selected_objects.iter().next() else {
            return;
        };

        self.object_modify_data(selected, |o| o.local_position.1 = v);
    }

    pub fn set_selected_object_local_translate_z(&mut self, v: f32) {
        let Some(&selected) = self.selected_objects.iter().next() else {
            return;
        };

        self.object_modify_data(selected, |o| o.local_position.2 = v);
    }

    pub fn set_selected_object_local_rotation_x(&mut self, v: f32) {
        let Some(&selected) = self.selected_objects.iter().next() else {
            return;
        };

        self.object_modify_data(selected, |o| o.local_rotation_euler.0 = v);
    }

    pub fn set_selected_object_local_rotation_y(&mut self, v: f32) {
        let Some(&selected) = self.selected_objects.iter().next() else {
            return;
        };

        self.object_modify_data(selected, |o| o.local_rotation_euler.1 = v);
    }

    pub fn set_selected_object_local_rotation_z(&mut self, v: f32) {
        let Some(&selected) = self.selected_objects.iter().next() else {
            return;
        };

        self.object_modify_data(selected, |o| o.local_rotation_euler.2 = v);
    }

    pub fn set_selected_object_local_scale_x(&mut self, v: f32) {
        let Some(&selected) = self.selected_objects.iter().next() else {
            return;
        };

        self.object_modify_data(selected, |o| o.local_scale.0 = v);
    }

    pub fn set_selected_object_local_scale_y(&mut self, v: f32) {
        let Some(&selected) = self.selected_objects.iter().next() else {
            return;
        };

        self.object_modify_data(selected, |o| o.local_scale.1 = v);
    }

    pub fn set_selected_object_local_scale_z(&mut self, v: f32) {
        let Some(&selected) = self.selected_objects.iter().next() else {
            return;
        };

        self.object_modify_data(selected, |o| o.local_scale.2 = v);
    }

    pub fn apply_selected_object_local_translate_delta(&mut self, delta: Vector3F32) {
        let Some(&selected) = self.selected_objects.iter().next() else {
            return;
        };

        self.object_modify_data(selected, |o| o.local_position += delta);
    }

    pub fn apply_selected_object_local_rotate_delta(&mut self, delta: Vector3F32) {
        let Some(&selected) = self.selected_objects.iter().next() else {
            return;
        };

        self.object_modify_data(selected, |o| o.local_rotation_euler += delta);
    }

    pub fn apply_selected_object_local_scale_delta(&mut self, delta: Vector3F32) {
        let Some(&selected) = self.selected_objects.iter().next() else {
            return;
        };

        self.object_modify_data(selected, |o| o.local_scale += delta);
    }

    pub fn select_object(&mut self, id: ObjectID) {
        if self.state.selected_objects.len() == 1
            && self
                .state
                .selected_objects
                .iter()
                .next()
                .is_some_and(|&x| x == id)
        {
            // already selected
            return;
        }

        self.state.selected_objects.clear();
        self.state.selected_objects.insert(id);
        self.view_feedbacks
            .push_back(ViewFeedback::object_selection_changed());
    }

    pub fn toggle_object_selection_additive(&mut self, id: ObjectID) {
        if !self.state.selected_objects.insert(id) {
            // selecting
            self.state.selected_objects.remove(&id);
        }

        self.view_feedbacks
            .push_back(ViewFeedback::object_selection_changed());
    }

    pub fn clear_selection(&mut self) {
        if self.state.selected_objects.is_empty() {
            // already cleared
            return;
        }

        self.state.selected_objects.clear();
        self.view_feedbacks
            .push_back(ViewFeedback::object_selection_changed());
    }

    pub fn set_preview_edit_tool_type(&mut self, tool_type: PreviewEditToolType) {
        self.state.preview_edit_tool_type = tool_type;
        self.view_feedbacks
            .push_back(ViewFeedback::preview_edit_tool_type_changed());
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
