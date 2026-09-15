use core::cell::Cell;
use std::{
    collections::{BTreeSet, HashMap},
    rc::Rc,
};

use bitflags::Flags;
use model::{ApplicationMutation, ObjectID, ObjectRenderShape, PreviewEditToolType};
use shared::{LogicalUnit, Point, Rect, Size};

use crate::{
    input::{
        EventContinueControl, FocusTargetToken, InputEventContext, KeyInputCode,
        KeyInputEventHandler, ModifierKey,
        hittest::{
            CursorShape, GrabDeltaMoveActionArgs, HitTestTreeActionHandler, HitTestTreeData,
            HitTestTreeRef, PointerActionArgs, PointerButtonActionArgs, ScrollWheelActionArgs,
            ScrollWheelActionResponse,
        },
    },
    rendering::{
        composite::{
            AnimatableColor, AnimatableFloat, AnimationCurve, Border, CompositeMode, CompositeRect,
            CompositeRectScaleFactor, CompositeRectText, CompositeRectTextHorizontalAlignment,
            CompositeRectTextRun, CompositeRectTextVerticalAlignment, CompositeTreeRef,
            CornerRadius,
        },
        preview::{
            CommittedMeshData, CommittedRenderData, HandlePointing, HandleShape, IndexType,
            handle::{
                ROTATION_HANDLE_HITSPHERE, SCALE_HANDLE_HITBOX_CENTER, SCALE_HANDLE_HITBOX_X,
                SCALE_HANDLE_HITBOX_Y, SCALE_HANDLE_HITBOX_Z, TRANSLATE_HANDLE_HITBOX_X,
                TRANSLATE_HANDLE_HITBOX_Y, TRANSLATE_HANDLE_HITBOX_Z,
            },
        },
    },
    ui::dock::{PaneContentPresenter, PaneContentResizeContext},
    uicore::{
        MeasureContext, RenderContext, TeardownContext, TypedViewIdentifier, View,
        ViewFeedbackContext, ViewFeedbackHandler, ViewFeedbackRegisterable, ViewIdentifier,
        ViewInitContext, ViewInstanceQueryableMut, ViewLayoutStateStore, ViewRegisterable,
        ViewRelationControllable, ViewRenderElements, ViewRenderer,
    },
};

bitflags::bitflags! {
    #[derive(Clone, Copy)]
    pub struct PreviewKeyInputState : u8 {
        const W = 0x01;
        const A = 0x02;
        const S = 0x04;
        const D = 0x08;
        const SHIFT = 0x10;
        const CONTROL = 0x20;
    }
}

pub struct InputState {
    new_viewport_size: Option<Size<LogicalUnit>>,
    scroll_amount: f32,
    grabbing: bool,
    clicked: bool,
    grab_delta: Point<LogicalUnit>,
    key_input: PreviewKeyInputState,
    pointer_pos: Option<Point<LogicalUnit>>,
}
impl InputState {
    pub fn new() -> Self {
        Self {
            new_viewport_size: None,
            scroll_amount: 0.0,
            grabbing: false,
            clicked: false,
            grab_delta: Point::new_logical(0.0, 0.0),
            key_input: PreviewKeyInputState::empty(),
            pointer_pos: None,
        }
    }
}

pub enum ManipulationState {
    None,
    Camera,
    Translate {
        pointing: HandlePointing,
        base_object_pos: peridot_math::Vector3F32,
        base_cursor_pos: peridot_math::Vector3F32,
        grab_sum: Point<LogicalUnit>,
    },
    Rotate {
        pointing: HandlePointing,
        base_object_rot: peridot_math::Vector3F32,
        base_cursor_pos: peridot_math::Vector3F32,
        grab_sum: Point<LogicalUnit>,
    },
    Scale {
        pointing: HandlePointing,
        base_object_scale: peridot_math::Vector3F32,
        base_cursor_pos: peridot_math::Vector3F32,
        grab_sum: Point<LogicalUnit>,
    },
}

pub struct MainThreadState {
    manipulation_state: ManipulationState,
    latched_key_motion_amplifier: Option<f32>,
    render_shape_to_mesh_id: HashMap<ObjectRenderShape, usize>,
    last_available_mesh_id: usize,
    free_mesh_ids: BTreeSet<usize>,
    last_available_render_id: usize,
    free_render_ids: BTreeSet<usize>,
}
impl MainThreadState {
    pub fn new() -> Self {
        Self {
            manipulation_state: ManipulationState::None,
            latched_key_motion_amplifier: None,
            render_shape_to_mesh_id: HashMap::new(),
            last_available_mesh_id: 0,
            free_mesh_ids: BTreeSet::new(),
            last_available_render_id: 0,
            free_render_ids: BTreeSet::new(),
        }
    }

    #[profiler::instrument("MainThread.Preview.Update")]
    pub fn update(
        &mut self,
        committed_state: &mut crate::rendering::preview::CommittedState,
        input: &mut InputState,
        application: &mut ApplicationMutation,
    ) {
        if let Some(new_viewport_size) = input.new_viewport_size.take() {
            committed_state.viewport_size = new_viewport_size;
        }

        let scroll_amount = core::mem::replace(&mut input.scroll_amount, 0.0);
        let grab_delta = core::mem::replace(&mut input.grab_delta, Point::new_logical(0.0, 0.0));
        let clicked = core::mem::replace(&mut input.clicked, false);

        loop {
            match self.manipulation_state {
                ManipulationState::None => {
                    if scroll_amount != 0.0 {
                        // move by scroll
                        let amplifier =
                            5.0f32.powf(if committed_state.main_camera.position.1 == 0.0 {
                                0.0
                            } else {
                                committed_state.main_camera.position.1.abs().log10().floor()
                            });
                        committed_state.main_camera.position = committed_state.main_camera.position
                            + committed_state.main_camera.forward()
                                * 0.25
                                * amplifier
                                * scroll_amount;
                        committed_state.main_camera_dirtified = true;
                    }

                    if clicked && let Some(pointer_pos) = input.pointer_pos {
                        // TODO: 必要なら最適化する

                        let ray = committed_state.main_camera.viewport_point_to_world_ray(
                            peridot_math::Vector2(
                                pointer_pos.x / committed_state.viewport_size.width,
                                pointer_pos.y / committed_state.viewport_size.height,
                            ),
                            committed_state.viewport_size.width
                                / committed_state.viewport_size.height,
                        );
                        let mut selected_oid = None;
                        for (oid, o) in application.state.objects.iter().enumerate() {
                            if o.hittest_ray(&ray) {
                                selected_oid = Some(ObjectID::from_array_index(oid));
                                break;
                            }
                        }

                        match selected_oid {
                            Some(oid) => {
                                model::select_object(application, oid);
                            }
                            None => {
                                model::object_deselect_all(application);
                            }
                        }
                    }

                    if input.grabbing {
                        // grab start on this frame

                        if let Some(&selected) = application.selected_objects.iter().next()
                            && let Some(pointer_pos) = input.pointer_pos
                        {
                            let current_handle_shape =
                                match model::preview_edit_tool_type(application) {
                                    PreviewEditToolType::Translate => HandleShape::Translation,
                                    PreviewEditToolType::Rotate => HandleShape::Rotation,
                                    PreviewEditToolType::Scale => HandleShape::Scale,
                                };

                            let handle_matrix =
                                &application.objects[selected.into_array_index()].world_matrix;
                            let handle_pos = peridot_math::Vector3(
                                handle_matrix.0[3],
                                handle_matrix.1[3],
                                handle_matrix.2[3],
                            );

                            let ray = committed_state.main_camera.viewport_point_to_world_ray(
                                peridot_math::Vector2(
                                    pointer_pos.x / committed_state.viewport_size.width,
                                    pointer_pos.y / committed_state.viewport_size.height,
                                ),
                                committed_state.viewport_size.width
                                    / committed_state.viewport_size.height,
                            );

                            let handle_scale =
                                (committed_state.main_camera.position - handle_pos).len();
                            if let Some(pointing) = Self::hittest_with_handle(
                                current_handle_shape,
                                handle_scale,
                                &handle_pos,
                                &ray,
                            ) {
                                self.manipulation_state = match current_handle_shape {
                                    HandleShape::Translation => ManipulationState::Translate {
                                        pointing,
                                        base_object_pos: application.objects
                                            [selected.into_array_index()]
                                        .local_position,
                                        base_cursor_pos: committed_state
                                            .main_camera
                                            .viewport_point_to_world_point(
                                                peridot_math::Vector2(
                                                    pointer_pos.x
                                                        / committed_state.viewport_size.width,
                                                    pointer_pos.y
                                                        / committed_state.viewport_size.height,
                                                ),
                                                committed_state.viewport_size.width
                                                    / committed_state.viewport_size.height,
                                            ),
                                        grab_sum: pointer_pos,
                                    },
                                    HandleShape::Rotation => ManipulationState::Rotate {
                                        pointing,
                                        base_object_rot: application.objects
                                            [selected.into_array_index()]
                                        .local_rotation_euler,
                                        base_cursor_pos: committed_state
                                            .main_camera
                                            .viewport_point_to_world_point(
                                                peridot_math::Vector2(
                                                    pointer_pos.x
                                                        / committed_state.viewport_size.width,
                                                    pointer_pos.y
                                                        / committed_state.viewport_size.height,
                                                ),
                                                committed_state.viewport_size.width
                                                    / committed_state.viewport_size.height,
                                            ),
                                        grab_sum: pointer_pos,
                                    },
                                    HandleShape::Scale => ManipulationState::Scale {
                                        pointing,
                                        base_object_scale: application.objects
                                            [selected.into_array_index()]
                                        .local_scale,
                                        base_cursor_pos: committed_state
                                            .main_camera
                                            .viewport_point_to_world_point(
                                                peridot_math::Vector2(
                                                    pointer_pos.x
                                                        / committed_state.viewport_size.width,
                                                    pointer_pos.y
                                                        / committed_state.viewport_size.height,
                                                ),
                                                committed_state.viewport_size.width
                                                    / committed_state.viewport_size.height,
                                            ),
                                        grab_sum: pointer_pos,
                                    },
                                };
                                break;
                            }
                        }

                        self.manipulation_state = ManipulationState::Camera;
                        continue;
                    } else {
                        break;
                    }
                }
                ManipulationState::Camera => {
                    if grab_delta.x != 0.0 || grab_delta.y != 0.0 {
                        // rotate by grab
                        committed_state.main_camera.rotation = committed_state.main_camera.rotation
                            * peridot_math::Quaternion::new(
                                grab_delta.y * 0.5f32.to_radians(),
                                peridot_math::Matrix3::from(committed_state.main_camera.rotation)
                                    * peridot_math::Vector3::left(),
                            )
                            * peridot_math::Quaternion::new(
                                grab_delta.x * 0.5f32.to_radians(),
                                peridot_math::Vector3::down(),
                            );
                        committed_state.main_camera_dirtified = true;
                    }

                    if scroll_amount != 0.0 {
                        // move by scroll
                        let amplifier =
                            5.0f32.powf(if committed_state.main_camera.position.1 == 0.0 {
                                0.0
                            } else {
                                committed_state.main_camera.position.1.abs().log10().floor()
                            });
                        committed_state.main_camera.position = committed_state.main_camera.position
                            + committed_state.main_camera.forward()
                                * 0.25
                                * amplifier
                                * scroll_amount;
                        committed_state.main_camera_dirtified = true;
                    }

                    if input.grabbing {
                        let mut key_forwards = 0.0f32;
                        let mut key_rights = 0.0f32;
                        let mut key_y_motions = 0.0f32;
                        if input.key_input.contains(PreviewKeyInputState::W) {
                            key_forwards += 1.0;
                        }
                        if input.key_input.contains(PreviewKeyInputState::S) {
                            key_forwards -= 1.0;
                        }
                        if input.key_input.contains(PreviewKeyInputState::D) {
                            key_rights += 1.0;
                        }
                        if input.key_input.contains(PreviewKeyInputState::A) {
                            key_rights -= 1.0;
                        }
                        if input.key_input.contains(PreviewKeyInputState::SHIFT) {
                            key_y_motions += 1.0;
                        }
                        if input.key_input.contains(PreviewKeyInputState::CONTROL) {
                            key_y_motions -= 1.0;
                        }

                        if key_forwards != 0.0 || key_rights != 0.0 || key_y_motions != 0.0 {
                            // move by key
                            let amplifier =
                                *self.latched_key_motion_amplifier.get_or_insert_with(|| {
                                    2.5f32.powf(if committed_state.main_camera.position.1 == 0.0 {
                                        0.0
                                    } else {
                                        committed_state.main_camera.position.1.abs().log10().floor()
                                    })
                                });
                            committed_state.main_camera.position = committed_state
                                .main_camera
                                .position
                                + committed_state.main_camera.forward()
                                    * (0.25 * amplifier * key_forwards)
                                + committed_state.main_camera.right()
                                    * (0.25 * amplifier * key_rights)
                                + peridot_math::Vector3(0.0, key_y_motions * 0.25 * amplifier, 0.0);
                            committed_state.main_camera_dirtified = true;
                        } else {
                            self.latched_key_motion_amplifier = None;
                        }
                    } else {
                        self.latched_key_motion_amplifier = None;
                        self.manipulation_state = ManipulationState::None;
                    }

                    break;
                }
                ManipulationState::Translate {
                    pointing,
                    base_object_pos,
                    base_cursor_pos,
                    ref mut grab_sum,
                } => {
                    const SENSITIVITY: f32 = 25.0;

                    if !input.grabbing {
                        self.manipulation_state = ManipulationState::None;
                        continue;
                    }

                    *grab_sum = grab_sum.with_offset(grab_delta);
                    let cursor_pos = committed_state.main_camera.viewport_point_to_world_point(
                        peridot_math::Vector2(
                            grab_sum.x / committed_state.viewport_size.width,
                            grab_sum.y / committed_state.viewport_size.height,
                        ),
                        committed_state.viewport_size.width / committed_state.viewport_size.height,
                    );
                    let move_delta = (cursor_pos - base_cursor_pos) * SENSITIVITY;

                    match pointing {
                        HandlePointing::X => {
                            model::set_selected_object_local_translate_x(
                                application,
                                base_object_pos.0 + move_delta.0,
                            );
                        }
                        HandlePointing::Y => {
                            model::set_selected_object_local_translate_y(
                                application,
                                base_object_pos.1 + move_delta.1,
                            );
                        }
                        HandlePointing::Z => {
                            model::set_selected_object_local_translate_z(
                                application,
                                base_object_pos.2 + move_delta.2,
                            );
                        }
                        HandlePointing::All => {
                            // nop for translate
                        }
                    }

                    break;
                }
                ManipulationState::Rotate {
                    pointing,
                    base_object_rot,
                    base_cursor_pos,
                    ref mut grab_sum,
                } => {
                    if !input.grabbing {
                        self.manipulation_state = ManipulationState::None;
                        continue;
                    }
                    const SENSITIVITY: f32 = 90.0;

                    *grab_sum = grab_sum.with_offset(grab_delta);
                    let cursor_pos = committed_state.main_camera.viewport_point_to_world_point(
                        peridot_math::Vector2(
                            grab_sum.x / committed_state.viewport_size.width,
                            grab_sum.y / committed_state.viewport_size.height,
                        ),
                        committed_state.viewport_size.width / committed_state.viewport_size.height,
                    );
                    let move_delta = (cursor_pos - base_cursor_pos) * SENSITIVITY;

                    // TODO: ここ見る軸はこれであってるか？
                    match pointing {
                        HandlePointing::X => {
                            model::set_selected_object_local_rotation_x(
                                application,
                                base_object_rot.0 - move_delta.1,
                            );
                        }
                        HandlePointing::Y => {
                            model::set_selected_object_local_rotation_y(
                                application,
                                base_object_rot.1 + move_delta.0,
                            );
                        }
                        HandlePointing::Z => {
                            model::set_selected_object_local_rotation_z(
                                application,
                                base_object_rot.2 - move_delta.1,
                            );
                        }
                        HandlePointing::All => {
                            // nop for rotation
                        }
                    }

                    break;
                }
                ManipulationState::Scale {
                    pointing,
                    base_object_scale,
                    base_cursor_pos,
                    ref mut grab_sum,
                } => {
                    if !input.grabbing {
                        self.manipulation_state = ManipulationState::None;
                        continue;
                    }
                    const SENSITIVITY: f32 = 25.0;

                    *grab_sum = grab_sum.with_offset(grab_delta);
                    let cursor_pos = committed_state.main_camera.viewport_point_to_world_point(
                        peridot_math::Vector2(
                            grab_sum.x / committed_state.viewport_size.width,
                            grab_sum.y / committed_state.viewport_size.height,
                        ),
                        committed_state.viewport_size.width / committed_state.viewport_size.height,
                    );
                    let move_delta = (cursor_pos - base_cursor_pos) * SENSITIVITY;

                    match pointing {
                        HandlePointing::X => {
                            model::set_selected_object_local_scale_x(
                                application,
                                base_object_scale.0 + move_delta.0,
                            );
                        }
                        HandlePointing::Y => {
                            model::set_selected_object_local_scale_y(
                                application,
                                base_object_scale.1 + move_delta.1,
                            );
                        }
                        HandlePointing::Z => {
                            model::set_selected_object_local_scale_z(
                                application,
                                base_object_scale.2 + move_delta.2,
                            );
                        }
                        HandlePointing::All => {
                            let scale_all = move_delta.len();
                            model::set_selected_object_local_scale(
                                application,
                                base_object_scale
                                    + peridot_math::Vector3(scale_all, scale_all, scale_all),
                            );
                        }
                    }

                    break;
                }
            }
        }

        let mut process_stack = Vec::new();
        process_stack.extend(application.world_matrix_recompute_targets.iter().copied());
        while let Some(id) = process_stack.pop() {
            match application.objects[id.into_array_index()].parent {
                None => {
                    // this is root object: compute direct matrix
                    let o = &mut application.state.objects[id.into_array_index()];
                    o.world_matrix = o.compute_local_matrix();
                    o.render_dirty = true;
                }
                Some(parent_id) => {
                    if application
                        .state
                        .world_matrix_recompute_targets
                        .contains(&parent_id)
                    {
                        // parent is scheduled to be updated the world matrix
                        continue;
                    }

                    let parent_matrix = application.state.objects[parent_id.into_array_index()]
                        .world_matrix
                        .clone();
                    let o = &mut application.state.objects[id.into_array_index()];
                    o.world_matrix = parent_matrix * o.compute_local_matrix();
                    o.render_dirty = true;
                }
            }

            application.state.world_matrix_recompute_targets.remove(&id);
            process_stack.extend(
                application.state.objects[id.into_array_index()]
                    .children
                    .iter()
                    .copied(),
            );
        }

        for o in application.state.removed_object_render_ids.drain(..) {
            committed_state.removed_render_data.insert(o);
        }

        for o in application.state.objects.iter_mut() {
            if core::mem::replace(&mut o.render_dirty, false) {
                // update object render data
                if !o.render_enabled {
                    if let Some(current_render_id) = o.render_id.take() {
                        committed_state
                            .removed_render_data
                            .insert(current_render_id);
                        self.free_mesh_ids.insert(current_render_id);
                    }
                } else {
                    let mesh_id = *self
                        .render_shape_to_mesh_id
                        .entry(o.render_shape)
                        .or_insert_with(|| {
                            if let Some(rid) = self.free_mesh_ids.pop_first() {
                                committed_state
                                    .dirty_meshes
                                    .insert(rid, mesh_data_for_render_shape(o.render_shape));
                                return rid;
                            }

                            let rid = self.last_available_mesh_id;
                            self.last_available_mesh_id += 1;
                            committed_state
                                .pushed_meshes
                                .push(mesh_data_for_render_shape(o.render_shape));
                            rid
                        });

                    match o.render_id {
                        None => {
                            // first render
                            o.render_id =
                                Some(if let Some(rid) = self.free_render_ids.pop_first() {
                                    committed_state.dirty_render_data.insert(
                                        rid,
                                        CommittedRenderData {
                                            object_to_world: o.world_matrix.clone(),
                                            mesh_id,
                                        },
                                    );
                                    rid
                                } else {
                                    let rid = self.last_available_render_id;
                                    self.last_available_render_id += 1;
                                    committed_state
                                        .pushed_render_data
                                        .push(CommittedRenderData {
                                            object_to_world: o.world_matrix.clone(),
                                            mesh_id,
                                        });
                                    rid
                                });
                        }
                        Some(rid) => {
                            // update existing
                            committed_state.dirty_render_data.insert(
                                rid,
                                CommittedRenderData {
                                    object_to_world: o.world_matrix.clone(),
                                    mesh_id,
                                },
                            );
                        }
                    }
                }
            }
        }

        let current_handle_shape;
        // TODO: handle for multiple selected?(中心に置くとかになるかな)
        if let Some(&selected) = application.selected_objects.iter().next() {
            let handle_matrix = application.objects[selected.into_array_index()]
                .world_matrix
                .clone();
            let handle_pos =
                peridot_math::Vector3(handle_matrix.0[3], handle_matrix.1[3], handle_matrix.2[3]);
            let handle_matrix = peridot_math::Matrix4::translation(handle_pos);
            if handle_matrix != committed_state.handle_to_world_transform {
                committed_state.handle_to_world_transform = handle_matrix;
                committed_state.handle_data_dirtified = true;
            }

            current_handle_shape = Some(match model::preview_edit_tool_type(application) {
                PreviewEditToolType::Translate => HandleShape::Translation,
                PreviewEditToolType::Rotate => HandleShape::Rotation,
                PreviewEditToolType::Scale => HandleShape::Scale,
            });

            if !input.grabbing {
                let current_handle_pointing = if let Some(pointer_pos) = input.pointer_pos {
                    let ray = committed_state.main_camera.viewport_point_to_world_ray(
                        peridot_math::Vector2(
                            pointer_pos.x / committed_state.viewport_size.width,
                            pointer_pos.y / committed_state.viewport_size.height,
                        ),
                        committed_state.viewport_size.width / committed_state.viewport_size.height,
                    );

                    let handle_scale = (committed_state.main_camera.position - handle_pos).len();
                    Self::hittest_with_handle(
                        unsafe { current_handle_shape.unwrap_unchecked() },
                        handle_scale,
                        &handle_pos,
                        &ray,
                    )
                } else {
                    None
                };

                if current_handle_pointing != committed_state.handle_pointing {
                    committed_state.handle_pointing = current_handle_pointing;
                    committed_state.handle_data_dirtified = true;
                }
            }
        } else {
            current_handle_shape = None;
        }
        if current_handle_shape != committed_state.handle_shape {
            committed_state.handle_shape = current_handle_shape;
            committed_state.handle_data_dirtified = true;
        }
    }

    fn hittest_with_handle(
        shape: HandleShape,
        scale: f32,
        pos: &peridot_math::Vector3F32,
        ray: &peridot_math::Ray3<f32>,
    ) -> Option<HandlePointing> {
        match shape {
            HandleShape::Translation => {
                let scale = peridot_math::Vector3(scale, scale, scale);
                let bbox_x = TRANSLATE_HANDLE_HITBOX_X.scale(&scale).translate(pos);
                let bbox_y = TRANSLATE_HANDLE_HITBOX_Y.scale(&scale).translate(pos);
                let bbox_z = TRANSLATE_HANDLE_HITBOX_Z.scale(&scale).translate(pos);

                if bbox_x.intersect(ray).is_some() {
                    Some(HandlePointing::X)
                } else if bbox_y.intersect(ray).is_some() {
                    Some(HandlePointing::Y)
                } else if bbox_z.intersect(ray).is_some() {
                    Some(HandlePointing::Z)
                } else {
                    None
                }
            }
            HandleShape::Rotation => {
                let hit_sphere = ROTATION_HANDLE_HITSPHERE.scale(scale).translate(pos);
                if let Some(tr) = hit_sphere.intersect(ray) {
                    const SENSIBLE_WIDTH: f32 = 0.02;
                    let p = ray.point(tr.start) - *pos;
                    if -SENSIBLE_WIDTH * scale <= p.0 && p.0 <= SENSIBLE_WIDTH * scale {
                        Some(HandlePointing::X)
                    } else if -SENSIBLE_WIDTH * scale <= p.1 && p.1 <= SENSIBLE_WIDTH * scale {
                        Some(HandlePointing::Y)
                    } else if -SENSIBLE_WIDTH * scale <= p.2 && p.2 <= SENSIBLE_WIDTH * scale {
                        Some(HandlePointing::Z)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            HandleShape::Scale => {
                let scale = peridot_math::Vector3(scale, scale, scale);
                let bbox_x = SCALE_HANDLE_HITBOX_X.scale(&scale).translate(pos);
                let bbox_y = SCALE_HANDLE_HITBOX_Y.scale(&scale).translate(pos);
                let bbox_z = SCALE_HANDLE_HITBOX_Z.scale(&scale).translate(pos);
                let bbox_center = SCALE_HANDLE_HITBOX_CENTER.scale(&scale).translate(pos);

                if bbox_x.intersect(ray).is_some() {
                    Some(HandlePointing::X)
                } else if bbox_y.intersect(ray).is_some() {
                    Some(HandlePointing::Y)
                } else if bbox_z.intersect(ray).is_some() {
                    Some(HandlePointing::Z)
                } else if bbox_center.intersect(ray).is_some() {
                    Some(HandlePointing::All)
                } else {
                    None
                }
            }
        }
    }
}

const PLANE_VERTICES: &[[peridot_math::Vector4F32; 2]] = &[
    [
        peridot_math::Vector4(-0.5, 0.0, -0.5, 1.0),
        peridot_math::Vector4(0.0, 1.0, 0.0, 0.0),
    ],
    [
        peridot_math::Vector4(0.5, 0.0, -0.5, 1.0),
        peridot_math::Vector4(0.0, 1.0, 0.0, 0.0),
    ],
    [
        peridot_math::Vector4(0.5, 0.0, 0.5, 1.0),
        peridot_math::Vector4(0.0, 1.0, 0.0, 0.0),
    ],
    [
        peridot_math::Vector4(-0.5, 0.0, 0.5, 1.0),
        peridot_math::Vector4(0.0, 1.0, 0.0, 0.0),
    ],
];
const PLANE_INDICES: &[u16] = &[0, 1, 2, 2, 3, 0];

const CUBE_VERTICES: &[[peridot_math::Vector4F32; 2]] = &[
    // +x
    [
        peridot_math::Vector4(0.5, 0.5, 0.5, 1.0),
        peridot_math::Vector4(1.0, 0.0, 0.0, 0.0),
    ],
    [
        peridot_math::Vector4(0.5, 0.5, -0.5, 1.0),
        peridot_math::Vector4(1.0, 0.0, 0.0, 0.0),
    ],
    [
        peridot_math::Vector4(0.5, -0.5, 0.5, 1.0),
        peridot_math::Vector4(1.0, 0.0, 0.0, 0.0),
    ],
    [
        peridot_math::Vector4(0.5, -0.5, -0.5, 1.0),
        peridot_math::Vector4(1.0, 0.0, 0.0, 0.0),
    ],
    // -x
    [
        peridot_math::Vector4(-0.5, 0.5, 0.5, 1.0),
        peridot_math::Vector4(-1.0, 0.0, 0.0, 0.0),
    ],
    [
        peridot_math::Vector4(-0.5, -0.5, 0.5, 1.0),
        peridot_math::Vector4(-1.0, 0.0, 0.0, 0.0),
    ],
    [
        peridot_math::Vector4(-0.5, 0.5, -0.5, 1.0),
        peridot_math::Vector4(-1.0, 0.0, 0.0, 0.0),
    ],
    [
        peridot_math::Vector4(-0.5, -0.5, -0.5, 1.0),
        peridot_math::Vector4(-1.0, 0.0, 0.0, 0.0),
    ],
    // +y
    [
        peridot_math::Vector4(0.5, 0.5, 0.5, 1.0),
        peridot_math::Vector4(0.0, 1.0, 0.0, 0.0),
    ],
    [
        peridot_math::Vector4(-0.5, 0.5, 0.5, 1.0),
        peridot_math::Vector4(0.0, 1.0, 0.0, 0.0),
    ],
    [
        peridot_math::Vector4(0.5, 0.5, -0.5, 1.0),
        peridot_math::Vector4(0.0, 1.0, 0.0, 0.0),
    ],
    [
        peridot_math::Vector4(-0.5, 0.5, -0.5, 1.0),
        peridot_math::Vector4(0.0, 1.0, 0.0, 0.0),
    ],
    // -y
    [
        peridot_math::Vector4(0.5, -0.5, 0.5, 1.0),
        peridot_math::Vector4(0.0, -1.0, 0.0, 0.0),
    ],
    [
        peridot_math::Vector4(0.5, -0.5, -0.5, 1.0),
        peridot_math::Vector4(0.0, -1.0, 0.0, 0.0),
    ],
    [
        peridot_math::Vector4(-0.5, -0.5, 0.5, 1.0),
        peridot_math::Vector4(0.0, -1.0, 0.0, 0.0),
    ],
    [
        peridot_math::Vector4(-0.5, -0.5, -0.5, 1.0),
        peridot_math::Vector4(0.0, -1.0, 0.0, 0.0),
    ],
    // +z
    [
        peridot_math::Vector4(0.5, 0.5, 0.5, 1.0),
        peridot_math::Vector4(0.0, 0.0, 1.0, 0.0),
    ],
    [
        peridot_math::Vector4(0.5, -0.5, 0.5, 1.0),
        peridot_math::Vector4(0.0, 0.0, 1.0, 0.0),
    ],
    [
        peridot_math::Vector4(-0.5, 0.5, 0.5, 1.0),
        peridot_math::Vector4(0.0, 0.0, 1.0, 0.0),
    ],
    [
        peridot_math::Vector4(-0.5, -0.5, 0.5, 1.0),
        peridot_math::Vector4(0.0, 0.0, 1.0, 0.0),
    ],
    // -z
    [
        peridot_math::Vector4(0.5, 0.5, -0.5, 1.0),
        peridot_math::Vector4(0.0, 0.0, -1.0, 0.0),
    ],
    [
        peridot_math::Vector4(-0.5, 0.5, -0.5, 1.0),
        peridot_math::Vector4(0.0, 0.0, -1.0, 0.0),
    ],
    [
        peridot_math::Vector4(0.5, -0.5, -0.5, 1.0),
        peridot_math::Vector4(0.0, 0.0, -1.0, 0.0),
    ],
    [
        peridot_math::Vector4(-0.5, -0.5, -0.5, 1.0),
        peridot_math::Vector4(0.0, 0.0, -1.0, 0.0),
    ],
];
const CUBE_INDICES: &[u16] = &[
    0, 1, 2, 2, 1, 3, // +x
    4, 5, 6, 6, 5, 7, // -x
    8, 9, 10, 10, 9, 11, // +y
    12, 13, 14, 14, 13, 15, // -y
    16, 17, 18, 18, 17, 19, // +z
    20, 21, 22, 22, 21, 23, // -z
];

fn mesh_data_for_render_shape(shape: ObjectRenderShape) -> CommittedMeshData {
    match shape {
        ObjectRenderShape::Plane => {
            let mut vbuf_bytes = vec![0u8; size_of_val(PLANE_VERTICES)];
            let mut ibuf_bytes = vec![0u8; size_of_val(PLANE_INDICES)];
            unsafe {
                vbuf_bytes.as_mut_ptr().copy_from_nonoverlapping(
                    PLANE_VERTICES.as_ptr().cast(),
                    size_of_val(PLANE_VERTICES),
                );
                ibuf_bytes.as_mut_ptr().copy_from_nonoverlapping(
                    PLANE_INDICES.as_ptr().cast(),
                    size_of_val(PLANE_INDICES),
                );
            }

            CommittedMeshData {
                vertices: std::sync::Arc::from(vbuf_bytes),
                vertex_stride: size_of::<[peridot_math::Vector4F32; 2]>(),
                indices: std::sync::Arc::from(ibuf_bytes),
                index_type: IndexType::U16,
                sub_mesh_ranges: std::sync::Arc::new([core::range::Range::from(0..6)]),
            }
        }
        ObjectRenderShape::Cube => {
            let mut vbuf_bytes = vec![0u8; size_of_val(CUBE_VERTICES)];
            let mut ibuf_bytes = vec![0u8; size_of_val(CUBE_INDICES)];
            unsafe {
                vbuf_bytes.as_mut_ptr().copy_from_nonoverlapping(
                    CUBE_VERTICES.as_ptr().cast(),
                    size_of_val(CUBE_VERTICES),
                );
                ibuf_bytes.as_mut_ptr().copy_from_nonoverlapping(
                    CUBE_INDICES.as_ptr().cast(),
                    size_of_val(CUBE_INDICES),
                );
            }

            CommittedMeshData {
                vertices: std::sync::Arc::from(vbuf_bytes),
                vertex_stride: size_of::<[peridot_math::Vector4F32; 2]>(),
                indices: std::sync::Arc::from(ibuf_bytes),
                index_type: IndexType::U16,
                sub_mesh_ranges: std::sync::Arc::new([core::range::Range::from(0..36)]),
            }
        }
        ObjectRenderShape::Sphere => {
            const HDIV: usize = 20;
            const VDIV: usize = 10;

            let vertex_count = HDIV * (VDIV + 1);
            let index_count = (HDIV * VDIV) * 6;
            let mut vbuf_bytes =
                vec![0u8; size_of::<[peridot_math::Vector4F32; 2]>() * vertex_count];
            let mut ibuf_bytes = vec![0u8; size_of::<u16>() * index_count];
            tracing::debug!(vertex_count, index_count);
            unsafe {
                let vt = vbuf_bytes
                    .as_mut_ptr()
                    .cast::<[peridot_math::Vector4F32; 2]>();
                let ix = ibuf_bytes.as_mut_ptr().cast::<u16>();

                // TODO: v = 0とv = VDIV - 1を特殊処理したほうがよさそう(形状がfanになる)
                for v in 0..=VDIV {
                    for h in 0..HDIV {
                        let ix_base = (h + v * HDIV) * 6;

                        let (y, yc) =
                            (core::f32::consts::PI * (v as f32 / VDIV as f32 - 0.5)).sin_cos();
                        let (x, z) = (core::f32::consts::TAU * h as f32 / HDIV as f32).sin_cos();
                        let n = peridot_math::Vector3(x * yc, y, z * yc).normalize();

                        vt.add(h + v * HDIV).write_unaligned([
                            peridot_math::Vector4(x * yc * 0.5, y * 0.5, z * yc * 0.5, 1.0),
                            n.clone().with_w(0.0),
                        ]);
                        if v < VDIV {
                            let v0 = v;
                            let v1 = v + 1;
                            let h0 = h;
                            let h1 = (h + 1) % HDIV;
                            ix.add(ix_base + 0).write_unaligned((h0 + v0 * HDIV) as _);
                            ix.add(ix_base + 2).write_unaligned((h1 + v0 * HDIV) as _);
                            ix.add(ix_base + 1).write_unaligned((h1 + v1 * HDIV) as _);
                            ix.add(ix_base + 3).write_unaligned((h0 + v0 * HDIV) as _);
                            ix.add(ix_base + 5).write_unaligned((h1 + v1 * HDIV) as _);
                            ix.add(ix_base + 4).write_unaligned((h0 + v1 * HDIV) as _);
                        }
                    }
                }
            }

            CommittedMeshData {
                vertices: std::sync::Arc::from(vbuf_bytes),
                vertex_stride: size_of::<[peridot_math::Vector4F32; 2]>(),
                indices: std::sync::Arc::from(ibuf_bytes),
                index_type: IndexType::U16,
                sub_mesh_ranges: std::sync::Arc::new([core::range::Range::from(
                    0..index_count as u32,
                )]),
            }
        }
        ObjectRenderShape::Cylinder => {
            const DIV_COUNT: usize = 16;

            let vertex_count = 2 + DIV_COUNT * 2 + DIV_COUNT * 2;
            let index_count = (DIV_COUNT * 3) * 2 + (DIV_COUNT * 6);
            let mut vbuf_bytes =
                vec![0u8; size_of::<[peridot_math::Vector4F32; 2]>() * vertex_count];
            let mut ibuf_bytes = vec![0u8; size_of::<u16>() * index_count];
            unsafe {
                let v = vbuf_bytes
                    .as_mut_ptr()
                    .cast::<[peridot_math::Vector4F32; 2]>();
                let i = ibuf_bytes.as_mut_ptr().cast::<u16>();

                // top/bottom center point
                v.add(0).write_unaligned([
                    peridot_math::Vector4(0.0, 0.5, 0.0, 1.0),
                    peridot_math::Vector4(0.0, 1.0, 0.0, 0.0),
                ]);
                v.add(1).write_unaligned([
                    peridot_math::Vector4(0.0, -0.5, 0.0, 1.0),
                    peridot_math::Vector4(0.0, -1.0, 0.0, 0.0),
                ]);

                let top_plane_vts_index_base = 2;
                let top_plane_ix_base = 0;
                let bottom_plane_vts_index_base = top_plane_vts_index_base + DIV_COUNT;
                let bottom_plane_ix_base = top_plane_ix_base + DIV_COUNT * 3;
                let side_plane_vts_index_base = bottom_plane_vts_index_base + DIV_COUNT;
                let side_plane_ix_base = bottom_plane_ix_base + DIV_COUNT * 3;
                for n in 0..DIV_COUNT {
                    let th = core::f32::consts::TAU * n as f32 / DIV_COUNT as f32;
                    let (s, c) = th.sin_cos();

                    // top/bottom plane
                    v.add(top_plane_vts_index_base + n).write_unaligned([
                        peridot_math::Vector4(s * 0.5, 0.5, c * 0.5, 1.0),
                        peridot_math::Vector4(0.0, 1.0, 0.0, 0.0),
                    ]);
                    v.add(bottom_plane_vts_index_base + n).write_unaligned([
                        peridot_math::Vector4(s * 0.5, -0.5, c * 0.5, 1.0),
                        peridot_math::Vector4(0.0, -1.0, 0.0, 0.0),
                    ]);
                    i.add(top_plane_ix_base + n * 3 + 0).write_unaligned(0);
                    i.add(top_plane_ix_base + n * 3 + 1)
                        .write_unaligned((top_plane_vts_index_base + (n + 1) % DIV_COUNT) as _);
                    i.add(top_plane_ix_base + n * 3 + 2)
                        .write_unaligned((top_plane_vts_index_base + n) as _);
                    i.add(bottom_plane_ix_base + n * 3 + 0).write_unaligned(1);
                    i.add(bottom_plane_ix_base + n * 3 + 1)
                        .write_unaligned((bottom_plane_vts_index_base + n) as _);
                    i.add(bottom_plane_ix_base + n * 3 + 2)
                        .write_unaligned((bottom_plane_vts_index_base + (n + 1) % DIV_COUNT) as _);

                    // side plane
                    v.add(side_plane_vts_index_base + n * 2 + 0)
                        .write_unaligned([
                            peridot_math::Vector4(s * 0.5, 0.5, c * 0.5, 1.0),
                            peridot_math::Vector4(s, 0.0, c, 0.0),
                        ]);
                    v.add(side_plane_vts_index_base + n * 2 + 1)
                        .write_unaligned([
                            peridot_math::Vector4(s * 0.5, -0.5, c * 0.5, 1.0),
                            peridot_math::Vector4(s, 0.0, c, 0.0),
                        ]);
                    i.add(side_plane_ix_base + n * 6 + 0)
                        .write_unaligned((side_plane_vts_index_base + n * 2 + 0) as _);
                    i.add(side_plane_ix_base + n * 6 + 1).write_unaligned(
                        (side_plane_vts_index_base + ((n + 1) % DIV_COUNT) * 2 + 0) as _,
                    );
                    i.add(side_plane_ix_base + n * 6 + 2)
                        .write_unaligned((side_plane_vts_index_base + n * 2 + 1) as _);
                    i.add(side_plane_ix_base + n * 6 + 3).write_unaligned(
                        (side_plane_vts_index_base + ((n + 1) % DIV_COUNT) * 2 + 0) as _,
                    );
                    i.add(side_plane_ix_base + n * 6 + 4).write_unaligned(
                        (side_plane_vts_index_base + ((n + 1) % DIV_COUNT) * 2 + 1) as _,
                    );
                    i.add(side_plane_ix_base + n * 6 + 5)
                        .write_unaligned((side_plane_vts_index_base + n * 2 + 1) as _);
                }
            }

            CommittedMeshData {
                vertices: std::sync::Arc::from(vbuf_bytes),
                vertex_stride: size_of::<[peridot_math::Vector4F32; 2]>(),
                indices: std::sync::Arc::from(ibuf_bytes),
                index_type: IndexType::U16,
                sub_mesh_ranges: std::sync::Arc::new([core::range::Range::from(
                    0..index_count as u32,
                )]),
            }
        }
        ObjectRenderShape::Capsule => {
            const HDIV: usize = 20;
            const VDIV: usize = 3;

            let vertex_count = 2 + (HDIV * VDIV) * 2;
            let index_count = HDIV * 6 + (HDIV * VDIV) * 12 + HDIV * 6;
            let mut vbuf_bytes =
                vec![0u8; size_of::<[peridot_math::Vector4F32; 2]>() * vertex_count];
            let mut ibuf_bytes = vec![0u8; size_of::<u16>() * index_count];
            tracing::debug!(vertex_count, index_count);
            unsafe {
                let vt = vbuf_bytes
                    .as_mut_ptr()
                    .cast::<[peridot_math::Vector4F32; 2]>();
                let ix = ibuf_bytes.as_mut_ptr().cast::<u16>();

                // peaks
                vt.write_unaligned([
                    peridot_math::Vector4(0.0, 0.5, 0.0, 1.0),
                    peridot_math::Vector4(0.0, 1.0, 0.0, 0.0),
                ]);
                vt.add(1).write_unaligned([
                    peridot_math::Vector4(0.0, -0.5, 0.0, 1.0),
                    peridot_math::Vector4(0.0, -1.0, 0.0, 0.0),
                ]);

                // first v layer(v 0 -> 1)
                let v_base = 2;
                for h in 0..HDIV {
                    let (y, yc) = (core::f32::consts::PI * (0.5 / VDIV as f32 - 0.5)).sin_cos();
                    let (x, z) = (core::f32::consts::TAU * h as f32 / HDIV as f32).sin_cos();
                    let n = peridot_math::Vector3(x * yc, y, z * yc).normalize();
                    vt.add(v_base + h * 2 + 0).write_unaligned([
                        peridot_math::Vector4(x * yc * 0.25, y * 0.25 - 0.25, z * yc * 0.25, 1.0),
                        n.with_w(0.0),
                    ]);
                    let (y, yc) = (core::f32::consts::PI * (-0.5 / VDIV as f32 + 0.5)).sin_cos();
                    let n = peridot_math::Vector3(x * yc, y, z * yc).normalize();
                    vt.add(v_base + h * 2 + 1).write_unaligned([
                        peridot_math::Vector4(x * yc * 0.25, y * 0.25 + 0.25, z * yc * 0.25, 1.0),
                        n.with_w(0.0),
                    ]);

                    ix.add(h * 6 + 0).write_unaligned(1);
                    ix.add(h * 6 + 1).write_unaligned((v_base + h * 2 + 0) as _);
                    ix.add(h * 6 + 2)
                        .write_unaligned((v_base + ((h + 1) % HDIV) * 2 + 0) as _);
                    ix.add(h * 6 + 3).write_unaligned(0);
                    ix.add(h * 6 + 5).write_unaligned((v_base + h * 2 + 1) as _);
                    ix.add(h * 6 + 4)
                        .write_unaligned((v_base + ((h + 1) % HDIV) * 2 + 1) as _);
                }

                // middle v layers
                for v in 2..=VDIV {
                    for h in 0..HDIV {
                        let ix_base = (h + (v - 1) * HDIV) * 12;

                        let (y, yc) = (core::f32::consts::PI
                            * (0.5 * v as f32 / VDIV as f32 - 0.5))
                            .sin_cos();
                        let (x, z) = (core::f32::consts::TAU * h as f32 / HDIV as f32).sin_cos();
                        let n = peridot_math::Vector3(x * yc, y, z * yc).normalize();

                        vt.add(v_base + (h + (v - 1) * HDIV) * 2 + 0)
                            .write_unaligned([
                                peridot_math::Vector4(
                                    x * yc * 0.25,
                                    y * 0.25 - 0.25,
                                    z * yc * 0.25,
                                    1.0,
                                ),
                                n.clone().with_w(0.0),
                            ]);
                        let v0 = v - 2;
                        let v1 = v - 1;
                        let h0 = h;
                        let h1 = (h + 1) % HDIV;
                        ix.add(ix_base + 0)
                            .write_unaligned((v_base + (h0 + v0 * HDIV) * 2 + 0) as _);
                        ix.add(ix_base + 2)
                            .write_unaligned((v_base + (h1 + v0 * HDIV) * 2 + 0) as _);
                        ix.add(ix_base + 1)
                            .write_unaligned((v_base + (h1 + v1 * HDIV) * 2 + 0) as _);
                        ix.add(ix_base + 3)
                            .write_unaligned((v_base + (h0 + v0 * HDIV) * 2 + 0) as _);
                        ix.add(ix_base + 5)
                            .write_unaligned((v_base + (h1 + v1 * HDIV) * 2 + 0) as _);
                        ix.add(ix_base + 4)
                            .write_unaligned((v_base + (h0 + v1 * HDIV) * 2 + 0) as _);

                        let (y, yc) = (core::f32::consts::PI
                            * (-0.5 * v as f32 / VDIV as f32 + 0.5))
                            .sin_cos();
                        let n = peridot_math::Vector3(x * yc, y, z * yc).normalize();

                        vt.add(v_base + (h + (v - 1) * HDIV) * 2 + 1)
                            .write_unaligned([
                                peridot_math::Vector4(
                                    x * yc * 0.25,
                                    y * 0.25 + 0.25,
                                    z * yc * 0.25,
                                    1.0,
                                ),
                                n.clone().with_w(0.0),
                            ]);
                        let v0 = v - 2;
                        let v1 = v - 1;
                        let h0 = h;
                        let h1 = (h + 1) % HDIV;
                        ix.add(ix_base + 6)
                            .write_unaligned((v_base + (h0 + v0 * HDIV) * 2 + 1) as _);
                        ix.add(ix_base + 7)
                            .write_unaligned((v_base + (h1 + v0 * HDIV) * 2 + 1) as _);
                        ix.add(ix_base + 8)
                            .write_unaligned((v_base + (h1 + v1 * HDIV) * 2 + 1) as _);
                        ix.add(ix_base + 9)
                            .write_unaligned((v_base + (h0 + v0 * HDIV) * 2 + 1) as _);
                        ix.add(ix_base + 10)
                            .write_unaligned((v_base + (h1 + v1 * HDIV) * 2 + 1) as _);
                        ix.add(ix_base + 11)
                            .write_unaligned((v_base + (h0 + v1 * HDIV) * 2 + 1) as _);
                    }
                }

                // side planes
                for h in 0..HDIV {
                    let ix_base = HDIV * 6 + (HDIV * VDIV) * 12 + h * 6;
                    let v_base0 = v_base + (HDIV * (VDIV - 1) + h) * 2;
                    let v_base1 = v_base + (HDIV * (VDIV - 1) + (h + 1) % HDIV) * 2;

                    ix.add(ix_base + 0).write_unaligned((v_base0 + 0) as _);
                    ix.add(ix_base + 1).write_unaligned((v_base0 + 1) as _);
                    ix.add(ix_base + 2).write_unaligned((v_base1 + 0) as _);
                    ix.add(ix_base + 3).write_unaligned((v_base1 + 0) as _);
                    ix.add(ix_base + 5).write_unaligned((v_base1 + 1) as _);
                    ix.add(ix_base + 4).write_unaligned((v_base0 + 1) as _);
                }
            }

            CommittedMeshData {
                vertices: std::sync::Arc::from(vbuf_bytes),
                vertex_stride: size_of::<[peridot_math::Vector4F32; 2]>(),
                indices: std::sync::Arc::from(ibuf_bytes),
                index_type: IndexType::U16,
                sub_mesh_ranges: std::sync::Arc::new([core::range::Range::from(
                    0..index_count as u32,
                )]),
            }
        }
    }
}

struct PreviewToolSelectorButtonView {
    round_top: bool,
    round_bottom: bool,
    pos: Point<LogicalUnit>,
    label: String,
    bound_tool_type: PreviewEditToolType,
    entity: Option<Rc<PreviewToolSelectorButtonViewEntity>>,
    selecting: bool,
}
impl PreviewToolSelectorButtonView {
    const SIZE: f32 = 24.0;
    const ROUNDING: f32 = 8.0;
    const SELECTING_COLOR: [f32; 4] = [0.25, 0.5, 1.0, 0.5];
    const DESELECTING_COLOR: [f32; 4] = [0.25, 0.25, 0.25, 0.5];

    fn new(
        round_top: bool,
        round_bottom: bool,
        pos: Point<LogicalUnit>,
        label: String,
        bound_tool_type: PreviewEditToolType,
    ) -> Self {
        Self {
            round_top,
            round_bottom,
            pos,
            label,
            bound_tool_type,
            entity: None,
            selecting: false,
        }
    }

    fn set_selecting(&mut self, selecting: bool) {
        self.selecting = selecting;
    }
}
impl View for PreviewToolSelectorButtonView {
    fn render(
        &mut self,
        _layout_rect: Rect<LogicalUnit>,
        ctx: &mut RenderContext,
        _layout_state: &ViewLayoutStateStore,
    ) -> ViewRenderElements {
        let e = match self.entity {
            Some(ref e) => {
                if self.selecting != e.selecting.replace(self.selecting) {
                    // TODO: reflect selecting
                    ctx.composite_tree
                        .begin_mod_chain(e.ct_root)
                        .composite_mode(CompositeMode::FillColorBackdropBlur(
                            AnimatableColor::Animated {
                                from_value: if self.selecting {
                                    Self::DESELECTING_COLOR
                                } else {
                                    Self::SELECTING_COLOR
                                },
                                to_value: if self.selecting {
                                    Self::SELECTING_COLOR
                                } else {
                                    Self::DESELECTING_COLOR
                                },
                                curve: AnimationCurve::Linear,
                                event_on_complete: None,
                                sec_duration: (ctx.current_sec..ctx.current_sec + 0.1).into(),
                            },
                            AnimatableFloat::Value(3.0),
                        ))
                        .apply();
                }

                e
            }
            None => {
                // first render
                let rounding = match (self.round_top, self.round_bottom) {
                    (false, false) => CornerRadius::all(0.0),
                    (true, false) => CornerRadius {
                        left_top: [Self::ROUNDING, Self::ROUNDING],
                        right_top: [Self::ROUNDING, Self::ROUNDING],
                        left_bottom: [0.0, 0.0],
                        right_bottom: [0.0, 0.0],
                    },
                    (false, true) => CornerRadius {
                        left_top: [0.0, 0.0],
                        right_top: [0.0, 0.0],
                        left_bottom: [Self::ROUNDING, Self::ROUNDING],
                        right_bottom: [Self::ROUNDING, Self::ROUNDING],
                    },
                    (true, true) => CornerRadius {
                        left_top: [Self::ROUNDING, Self::ROUNDING],
                        right_top: [Self::ROUNDING, Self::ROUNDING],
                        left_bottom: [Self::ROUNDING, Self::ROUNDING],
                        right_bottom: [Self::ROUNDING, Self::ROUNDING],
                    },
                };

                let ct_root = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    size: [
                        AnimatableFloat::Value(Self::SIZE),
                        AnimatableFloat::Value(Self::SIZE),
                    ],
                    offset: [
                        AnimatableFloat::Value(self.pos.x),
                        AnimatableFloat::Value(self.pos.y),
                    ],
                    has_bitmap: true,
                    composite_mode: CompositeMode::FillColorBackdropBlur(
                        AnimatableColor::Value(if self.selecting {
                            Self::SELECTING_COLOR
                        } else {
                            Self::DESELECTING_COLOR
                        }),
                        AnimatableFloat::Value(3.0),
                    ),
                    corner_radius: rounding.clone(),
                    border: Some(Border {
                        thickness: 1.0,
                        color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                        ..Default::default()
                    }),
                    text: Some(CompositeRectText {
                        runs: vec![CompositeRectTextRun {
                            content: self.label.clone(),
                            color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                            ..Default::default()
                        }],
                        horizontal_alignment: CompositeRectTextHorizontalAlignment::Middle,
                        vertical_alignment: CompositeRectTextVerticalAlignment::Middle,
                        ..Default::default()
                    }),
                    ..Default::default()
                });
                let ct_hover_lit = ctx.composite_tree.create(CompositeRect {
                    relative_size_adjustment: [1.0, 1.0],
                    has_bitmap: true,
                    composite_mode: CompositeMode::FillColor(AnimatableColor::Value([
                        1.0, 1.0, 1.0, 0.0,
                    ])),
                    corner_radius: rounding,
                    ..Default::default()
                });
                let ht_root = ctx.ht_manager.create(HitTestTreeData {
                    width: Self::SIZE,
                    height: Self::SIZE,
                    left: self.pos.x,
                    top: self.pos.y,
                    cursor_shape: CursorShape::Pointer,
                    ..Default::default()
                });
                ctx.composite_tree.add_child(ct_root, ct_hover_lit);

                let entity = Rc::new(PreviewToolSelectorButtonViewEntity {
                    ct_root,
                    ct_hover_lit,
                    ht_root,
                    bound_tool_type: self.bound_tool_type,
                    selecting: Cell::new(self.selecting),
                });
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
            // not rendering
            return;
        };

        ctx.composite_tree.free_all(entity.ct_root);
        ctx.ht_manager.free_all(entity.ht_root);
    }

    fn measure_preferred_content_size(&self, _ctx: &mut MeasureContext) -> Size<LogicalUnit> {
        Size::new_logical(Self::SIZE, Self::SIZE)
    }
}

struct PreviewToolSelectorButtonViewEntity {
    ct_root: CompositeTreeRef,
    ct_hover_lit: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    bound_tool_type: PreviewEditToolType,
    selecting: Cell<bool>,
}
impl HitTestTreeActionHandler for PreviewToolSelectorButtonViewEntity {
    fn on_pointer_enter(
        &self,
        _sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerActionArgs,
    ) -> EventContinueControl {
        context
            .composite_tree
            .begin_mod_chain(self.ct_hover_lit)
            .composite_mode(CompositeMode::FillColor(AnimatableColor::Animated {
                from_value: [1.0, 1.0, 1.0, 0.0],
                to_value: [1.0, 1.0, 1.0, 0.1],
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
        context: &mut InputEventContext,
        _args: &PointerActionArgs,
    ) -> EventContinueControl {
        context
            .composite_tree
            .begin_mod_chain(self.ct_hover_lit)
            .composite_mode(CompositeMode::FillColor(AnimatableColor::Animated {
                from_value: [1.0, 1.0, 1.0, 0.1],
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
        _context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        EventContinueControl::STOP_PROPAGATION
    }

    fn on_pointer_up(
        &self,
        _sender: HitTestTreeRef,
        _context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        EventContinueControl::STOP_PROPAGATION
    }

    fn on_drag_start(
        &self,
        _sender: HitTestTreeRef,
        _context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        EventContinueControl::STOP_PROPAGATION
    }

    fn on_click(
        &self,
        _sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        model::set_preview_edit_tool_type(context, self.bound_tool_type);

        EventContinueControl::STOP_PROPAGATION
    }
}

pub struct Presenter {
    root_view_id: TypedViewIdentifier<MainView>,
    feedback_receiver: Rc<PreviewPaneFeedbackReceiver>,
}
impl Presenter {
    pub const ID: &str = internal_pane_identifier!("Preview");

    pub fn new(ctx: &mut ViewInitContext, input_state: *mut InputState) -> Self {
        let root_view = ctx.construct_view_direct(|_| Box::new(MainView::new(input_state)));
        let translate_control_button = ctx.construct_view_direct(|_| {
            Box::new(PreviewToolSelectorButtonView::new(
                true,
                false,
                Point::new_logical(8.0, 8.0),
                "T".into(),
                PreviewEditToolType::Translate,
            ))
        });
        let rotate_control_button = ctx.construct_view_direct(|_| {
            Box::new(PreviewToolSelectorButtonView::new(
                false,
                false,
                Point::new_logical(8.0, 8.0 + 24.0 - 1.0),
                "R".into(),
                PreviewEditToolType::Rotate,
            ))
        });
        let scale_control_button = ctx.construct_view_direct(|_| {
            Box::new(PreviewToolSelectorButtonView::new(
                false,
                true,
                Point::new_logical(8.0, 8.0 + 48.0 - 2.0),
                "S".into(),
                PreviewEditToolType::Scale,
            ))
        });
        ctx.view_set_parent(translate_control_button, root_view);
        ctx.view_set_parent(rotate_control_button, root_view);
        ctx.view_set_parent(scale_control_button, root_view);

        let feedback_receiver = Rc::new(PreviewPaneFeedbackReceiver {
            translate_tool_button_view_id: translate_control_button,
            rotate_tool_button_view_id: rotate_control_button,
            scale_tool_button_view_id: scale_control_button,
        });
        ctx.subscribe_view_feedback::<model::ViewFeedbackPreviewEditToolTypeChanged>(
            &feedback_receiver,
        );

        Self {
            root_view_id: root_view,
            feedback_receiver,
        }
    }
}
impl PaneContentPresenter for Presenter {
    fn id(&self) -> String {
        Self::ID.into()
    }

    fn name(&self) -> String {
        "Preview".into()
    }

    fn root_view_id(&self) -> ViewIdentifier {
        self.root_view_id.into_untyped()
    }

    fn resize(&self, new_size: &Size<LogicalUnit>, context: &mut PaneContentResizeContext) {
        unsafe {
            &mut *context
                .view_instance_mut(self.root_view_id)
                .expect("query failed")
                .input_state
        }
        .new_viewport_size = Some(new_size.clone());
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        ctx.unsubscribe_view_feedback::<model::ViewFeedbackPreviewEditToolTypeChanged>(
            &self.feedback_receiver,
        );
    }
}

pub struct PreviewPaneFeedbackReceiver {
    translate_tool_button_view_id: TypedViewIdentifier<PreviewToolSelectorButtonView>,
    rotate_tool_button_view_id: TypedViewIdentifier<PreviewToolSelectorButtonView>,
    scale_tool_button_view_id: TypedViewIdentifier<PreviewToolSelectorButtonView>,
}
impl ViewFeedbackHandler<model::ViewFeedbackPreviewEditToolTypeChanged>
    for PreviewPaneFeedbackReceiver
{
    fn accept_feedback<'a, 'h, 'sys>(
        &self,
        _feedback: &model::ViewFeedbackPreviewEditToolTypeChanged,
        context: &mut ViewFeedbackContext<'a, 'sys>,
    ) {
        let is_selecting = model::preview_edit_tool_type(context) == PreviewEditToolType::Translate;
        context
            .view_instance_mut(self.translate_tool_button_view_id)
            .expect("query failed")
            .set_selecting(is_selecting);
        context.schedule_view_render(self.translate_tool_button_view_id);

        let is_selecting = model::preview_edit_tool_type(context) == PreviewEditToolType::Rotate;
        context
            .view_instance_mut(self.rotate_tool_button_view_id)
            .expect("query failed")
            .set_selecting(is_selecting);
        context.schedule_view_render(self.rotate_tool_button_view_id);

        let is_selecting = model::preview_edit_tool_type(context) == PreviewEditToolType::Scale;
        context
            .view_instance_mut(self.scale_tool_button_view_id)
            .expect("query failed")
            .set_selecting(is_selecting);
        context.schedule_view_render(self.scale_tool_button_view_id);
    }
}

struct MainView {
    input_state: *mut InputState,
    entity: Option<Rc<PreviewViewEntity>>,
}
impl MainView {
    pub fn new(input_state: *mut InputState) -> Self {
        Self {
            input_state,
            entity: None,
        }
    }
}
impl View for MainView {
    fn render(
        &mut self,
        _layout_rect: Rect<LogicalUnit>,
        ctx: &mut RenderContext,
        _layout_state: &ViewLayoutStateStore,
    ) -> ViewRenderElements {
        let e = match self.entity {
            Some(ref e) => e,
            None => {
                // first render
                let kf_token = ctx.keyboard_focus_registry.acquire_token();
                let ct_root = ctx.composite_tree.create(CompositeRect {
                    // has_bitmap: true,
                    custom_render_token: Some(crate::rendering::PREVIEW_COMPOSITE),
                    relative_size_adjustment: [1.0, 1.0],
                    ..Default::default()
                });
                let ht_root = ctx.ht_manager.create(HitTestTreeData {
                    width_adjustment_factor: 1.0,
                    height_adjustment_factor: 1.0,
                    keyboard_focus: Some(kf_token),
                    ..Default::default()
                });

                let entity = Rc::new(PreviewViewEntity {
                    ct_root,
                    ht_root,
                    kf_token,
                    input_state: self.input_state,
                });
                ctx.ht_manager.set_action_handler(ht_root, &entity);
                ctx.keyboard_focus_registry
                    .set_event_handler(kf_token, &entity);

                &*self.entity.insert(entity)
            }
        };

        ViewRenderElements {
            composite_tree: Some(e.ct_root),
            hit_tree: Some(e.ht_root),
            keyboard_focus: Some(e.kf_token),
            ..ViewRenderElements::EMPTY
        }
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        let Some(entity) = self.entity.take() else {
            // not rendered
            return;
        };

        ctx.composite_tree.free(entity.ct_root);
        ctx.ht_manager.free(entity.ht_root);
        ctx.keyboard_focus_registry.release_token(entity.kf_token);
    }

    fn measure_preferred_content_size(&self, _ctx: &mut MeasureContext) -> Size<LogicalUnit> {
        Size::new_logical(0.0, 0.0)
    }

    fn create_new_layout_layer(&self) -> bool {
        true
    }
}

struct PreviewViewEntity {
    ct_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    kf_token: FocusTargetToken,
    input_state: *mut InputState,
}
impl HitTestTreeActionHandler for PreviewViewEntity {
    fn on_scroll_wheel(
        &self,
        _sender: HitTestTreeRef,
        _context: &mut InputEventContext,
        args: &ScrollWheelActionArgs,
    ) -> ScrollWheelActionResponse {
        unsafe { &mut *self.input_state }.scroll_amount += args.amount;

        ScrollWheelActionResponse {
            left_amount: 0.0,
            continue_flags: EventContinueControl::STOP_PROPAGATION,
        }
    }

    fn on_pointer_leave(
        &self,
        _sender: HitTestTreeRef,
        _context: &mut InputEventContext,
        _args: &PointerActionArgs,
    ) -> EventContinueControl {
        unsafe { &mut *self.input_state }.pointer_pos = None;
        EventContinueControl::empty()
    }

    fn on_pointer_move(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerActionArgs,
    ) -> EventContinueControl {
        let (x, y, _, _) = context.ht_manager.translate_client_to_tree_local(
            sender,
            args.client_pos.x,
            args.client_pos.y,
            args.client_size.width,
            args.client_size.height,
        );
        unsafe { &mut *self.input_state }.pointer_pos = Some(Point::new_logical(x, y));

        EventContinueControl::STOP_PROPAGATION
    }

    fn on_pointer_down(
        &self,
        _sender: HitTestTreeRef,
        _context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        EventContinueControl::STOP_PROPAGATION
    }

    fn on_pointer_up(
        &self,
        _sender: HitTestTreeRef,
        _context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        EventContinueControl::STOP_PROPAGATION
    }

    fn on_drag_start(
        &self,
        _sender: HitTestTreeRef,
        _context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        unsafe { &mut *self.input_state }.grabbing = true;
        EventContinueControl::GRAB_POINTER | EventContinueControl::STOP_PROPAGATION
    }

    fn on_drag_end(
        &self,
        _sender: HitTestTreeRef,
        _context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        unsafe { &mut *self.input_state }.grabbing = false;
        EventContinueControl::RELEASE_CAPTURE_ELEMENT | EventContinueControl::STOP_PROPAGATION
    }

    fn grab_delta_move(
        &self,
        _sender: HitTestTreeRef,
        _context: &mut InputEventContext,
        args: &GrabDeltaMoveActionArgs,
    ) -> EventContinueControl {
        let st = unsafe { &mut *self.input_state };
        st.grab_delta.x += args.delta.x;
        st.grab_delta.y += args.delta.y;

        EventContinueControl::STOP_PROPAGATION
    }

    fn on_click(
        &self,
        _sender: HitTestTreeRef,
        _context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        unsafe { &mut *self.input_state }.clicked = true;

        EventContinueControl::STOP_PROPAGATION
    }
}
impl KeyInputEventHandler for PreviewViewEntity {
    fn focus_released(&self, _context: &mut InputEventContext) {
        unsafe { &mut *self.input_state }.key_input.clear();
    }

    fn keydown(
        &self,
        _context: &mut InputEventContext,
        code: KeyInputCode,
        _modifier: ModifierKey,
    ) {
        match code {
            KeyInputCode::Character(c) if c.eq_ignore_ascii_case(&'w') => {
                self.set_key(PreviewKeyInputState::W);
            }
            KeyInputCode::Character(c) if c.eq_ignore_ascii_case(&'a') => {
                self.set_key(PreviewKeyInputState::A);
            }
            KeyInputCode::Character(c) if c.eq_ignore_ascii_case(&'s') => {
                self.set_key(PreviewKeyInputState::S);
            }
            KeyInputCode::Character(c) if c.eq_ignore_ascii_case(&'d') => {
                self.set_key(PreviewKeyInputState::D);
            }
            KeyInputCode::RightShift | KeyInputCode::LeftShift => {
                self.set_key(PreviewKeyInputState::SHIFT);
            }
            KeyInputCode::RightControl | KeyInputCode::LeftControl => {
                self.set_key(PreviewKeyInputState::CONTROL);
            }
            _ => (),
        }
    }

    fn keyup(&self, _context: &mut InputEventContext, code: KeyInputCode, _modifier: ModifierKey) {
        tracing::debug!(?code, "keyup");
        match code {
            KeyInputCode::Character(c) if c.eq_ignore_ascii_case(&'w') => {
                self.unset_key(PreviewKeyInputState::W);
            }
            KeyInputCode::Character(c) if c.eq_ignore_ascii_case(&'a') => {
                self.unset_key(PreviewKeyInputState::A);
            }
            KeyInputCode::Character(c) if c.eq_ignore_ascii_case(&'s') => {
                self.unset_key(PreviewKeyInputState::S);
            }
            KeyInputCode::Character(c) if c.eq_ignore_ascii_case(&'d') => {
                self.unset_key(PreviewKeyInputState::D);
            }
            KeyInputCode::RightShift | KeyInputCode::LeftShift => {
                self.unset_key(PreviewKeyInputState::SHIFT);
            }
            KeyInputCode::RightControl | KeyInputCode::LeftControl => {
                self.unset_key(PreviewKeyInputState::CONTROL);
            }
            _ => (),
        }
    }
}
impl PreviewViewEntity {
    fn set_key(&self, key: PreviewKeyInputState) {
        unsafe { &mut *self.input_state }.key_input.insert(key);
    }

    fn unset_key(&self, key: PreviewKeyInputState) {
        unsafe { &mut *self.input_state }.key_input.remove(key);
    }
}
