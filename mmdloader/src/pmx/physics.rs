
use super::{GlobalizedStrings, LoadingError, Vec3, IndexValue, Header, TypedReader, IndexSize};
use std::io::Read;
use std::mem::transmute;

pub enum ShapeType { Sphere, Box, Capsule }
pub enum PhysicsMode { FollowBone, Physics, PhysicsWithBone }
pub struct RigidBody
{
    pub name: GlobalizedStrings, pub related_bone_index: Option<IndexValue>, pub group_id: i8,
    pub non_collision_mask: i16, pub shape: ShapeType, pub shape_size: Vec3, pub shape_position: Vec3,
    pub shape_rotation: Vec3, pub mass: f32, pub move_atten: f32, pub rotation_damp: f32, pub repulsion: f32,
    pub friction_force: f32, pub mode: PhysicsMode
}
impl RigidBody
{
    pub fn read<R: Read>(reader: &mut R, header: &Header) -> Result<Self, LoadingError> {
        let name = header.string_reader.read_globalized(reader)?;
        let mut bytes = vec![0u8; header.index_sizes.bone.bytesize() + 1 + 2 + 1 + 4 * (3 * 3 + 5) + 1];
        reader.read_exact(&mut bytes)?;
        let shape = match bytes[header.index_sizes.bone.bytesize() + 1 + 2] {
            0 => ShapeType::Sphere, 1 => ShapeType::Box, 2 => ShapeType::Capsule,
            v => return Err(LoadingError::UnknownShapeType(v))
        };
        let mode = match bytes[bytes.len() - 1] {
            0 => PhysicsMode::FollowBone, 1 => PhysicsMode::Physics, 2 => PhysicsMode::PhysicsWithBone,
            v => return Err(LoadingError::UnknownPhysicsMode(v))
        };

        Ok(RigidBody
        {
            name, related_bone_index: IndexValue::from_bytes(&bytes, header.index_sizes.bone),
            group_id: unsafe { *(bytes.as_ptr().add(header.index_sizes.bone.bytesize()) as *const _) },
            non_collision_mask: unsafe { *(bytes.as_ptr().add(header.index_sizes.bone.bytesize() + 1) as *const _) },
            shape,
            shape_size: Vec3::from_bytes(&bytes[header.index_sizes.bone.bytesize() + 1 + 2 + 1..]),
            shape_position: Vec3::from_bytes(&bytes[header.index_sizes.bone.bytesize() + 1 + 2 + 1 + 4 * 3..]),
            shape_rotation: Vec3::from_bytes(&bytes[header.index_sizes.bone.bytesize() + 1 + 2 + 1 + 4 * 3 * 2..]),
            mass: f32::from_bytes(&bytes[header.index_sizes.bone.bytesize()+1+2+1+4*3*3..]),
            move_atten: f32::from_bytes(&bytes[header.index_sizes.bone.bytesize()+1+2+1+4*3*3+4..]),
            rotation_damp: f32::from_bytes(&bytes[header.index_sizes.bone.bytesize()+1+2+1+4*3*3+4*2..]),
            repulsion: f32::from_bytes(&bytes[header.index_sizes.bone.bytesize()+1+2+1+4*3*3+4*3..]),
            friction_force: f32::from_bytes(&bytes[header.index_sizes.bone.bytesize()+1+2+1+4*3*3+4*4..]), mode
        })
    }
}

pub enum JointType { Spring6, SixDof, P2P, ConeTwist, Slider, Hinge }
pub struct Joint {
    pub name: GlobalizedStrings, pub _type: JointType,
    pub rigid_body_indices: (Option<IndexValue>, Option<IndexValue>),
    pub position: Vec3, pub rotation: Vec3, pub position_min: Vec3, pub position_max: Vec3,
    pub rotation_min: Vec3, pub rotation_max: Vec3, pub position_spring: Vec3,
    pub rotation_spring: Vec3
}
impl Joint {
    pub fn read<R: Read>(reader: &mut R, header: &Header) -> Result<Self, LoadingError> {
        let name = header.string_reader.read_globalized(reader)?;
        let mut bytes = vec![0u8; header.index_sizes.rigid_body.bytesize() * 2 + 1 + 4 * 3 * 8];
        reader.read_exact(&mut bytes)?;
        let _type = match bytes[0]
        {
            0 => JointType::Spring6, 1 => JointType::SixDof, 2 => JointType::P2P, 3 => JointType::ConeTwist,
            4 => JointType::Slider, 5 => JointType::Hinge, v => return Err(LoadingError::UnknonwnJointType(v))
        };
        let rigid_body_indices = (IndexValue::from_bytes(&bytes[1..], header.index_sizes.rigid_body),
            IndexValue::from_bytes(&bytes[1 + header.index_sizes.rigid_body.bytesize()..],
                header.index_sizes.rigid_body));
        
        Ok(Joint
        {
            name, _type, rigid_body_indices,
            position: Vec3::from_bytes(&bytes[1 + header.index_sizes.rigid_body.bytesize() * 2 ..]),
            rotation: Vec3::from_bytes(&bytes[1 + header.index_sizes.rigid_body.bytesize() * 2 + 4 * 3 ..]),
            position_min: Vec3::from_bytes(&bytes[1 + header.index_sizes.rigid_body.bytesize() * 2 + 4 * 3 * 2 ..]),
            position_max: Vec3::from_bytes(&bytes[1 + header.index_sizes.rigid_body.bytesize() * 2 + 4 * 3 * 3 ..]),
            rotation_min: Vec3::from_bytes(&bytes[1 + header.index_sizes.rigid_body.bytesize() * 2 + 4 * 3 * 4 ..]),
            rotation_max: Vec3::from_bytes(&bytes[1 + header.index_sizes.rigid_body.bytesize() * 2 + 4 * 3 * 5 ..]),
            position_spring: Vec3::from_bytes(
                &bytes[1 + header.index_sizes.rigid_body.bytesize() * 2 + 4 * 3 * 6 ..]),
            rotation_spring: Vec3::from_bytes(
                &bytes[1 + header.index_sizes.rigid_body.bytesize() * 2 + 4 * 3 * 7 ..])
        })
    }
}

pub enum SoftbodyShapeType { TriMesh, Rope }
pub struct SoftbodyFlags(u8);
impl SoftbodyFlags
{
    pub fn b_link(&self) -> bool { (self.0 & 0x01) != 0 }
    pub fn cluster_creation(&self) -> bool { (self.0 & 0x02) != 0 }
    pub fn link_crossing(&self) -> bool { (self.0 & 0x04) != 0 }
}
pub enum AeroDynamicsModel { Point, TwoSidedV, OneSidedV, TwoSidedF, OneSidedF }
pub struct AnchorRigidBody { pub index: Option<IndexValue>, pub vindex: u32, pub near_mode: i8 }
pub struct VertexPin(u32);
pub struct Softbody
{
    pub name: GlobalizedStrings, pub shape: SoftbodyShapeType, pub mindex: Option<IndexValue>,
    pub group: i8, pub non_collision_mask: i16, pub flags: SoftbodyFlags, pub b_link_create_distance: i32,
    pub cluster_count: i32, pub total_mass: f32, pub collision_margin: f32, pub aerodynamics_model: AeroDynamicsModel,
    pub velocities_correction_factor: f32, pub dampling_coefficient: f32, pub drag_coefficient: f32,
    pub lift_coefficient: f32, pub pressure_coefficient: f32, pub volume_conversation_coefficient: f32,
    pub dynamic_friction_coeffecient: f32, pub pose_matching_coefficient: f32, pub rigid_contracts_hardness: f32,
    pub kinetic_contracts_hardness: f32, pub soft_contracts_hardness: f32, pub anchors_hardness: f32,
    pub soft_vs_rigid_hardness: f32, pub soft_vs_kinetic_hardness: f32, pub soft_vs_soft_hardness: f32,
    pub soft_vs_soft_impulse_split: f32, pub soft_vs_rigid_impulse_split: f32, pub soft_vs_kinetic_impulse_split: f32,
    pub velocities_solver_iterations: i32, pub positions_solver_iterations: i32, pub drift_solver_iterations: i32,
    pub cluster_solver_iteratons: i32, pub linear_stiffness_coefficient: i32,
    pub area_angular_stiffness_coefficient: i32, pub volume_stiffness_coefficient: i32,
    pub anchor_rigid_bodies: Vec<AnchorRigidBody>, pub vertex_pins: Vec<VertexPin>
}
impl Softbody
{
    pub fn read<R: Read>(reader: &mut R, header: &Header) -> Result<Self, LoadingError> {
        let name = header.string_reader.read_globalized(reader)?;
        let mut bytes = vec![0u8; header.index_sizes.material.bytesize() + 3 + 2 + 4 * 29];
        reader.read_exact(&mut bytes)?;
        let shape = match bytes[0]
        {
            0 => SoftbodyShapeType::TriMesh, 1 => SoftbodyShapeType::Rope,
            v => return Err(LoadingError::UnknownShapeType(v))
        };
        let aerodynamics_model = match bytes[5 + 4 * 4 + header.index_sizes.material.bytesize()]
        {
            0 => AeroDynamicsModel::Point, 1 => AeroDynamicsModel::TwoSidedV, 2 => AeroDynamicsModel::OneSidedV,
            3 => AeroDynamicsModel::TwoSidedF, 4 => AeroDynamicsModel::OneSidedF,
            v => return Err(LoadingError::UnknownAerodynamicsMode(v))
        };
        let mindex = IndexValue::from_bytes(&bytes[1..], header.index_sizes.material);
        let group = <i8 as TypedReader>::from_bytes(&bytes[1 + header.index_sizes.material.bytesize()..]);
        let non_collision_mask = i16::from_bytes(&bytes[1 + header.index_sizes.material.bytesize() + 1..]);
        let flags = SoftbodyFlags(bytes[1 + header.index_sizes.material.bytesize() + 1 + 2]);
        let anchor_rigid_body_count = <i32 as TypedReader>::from_bytes(&bytes[bytes.len() - 4..]);
        let mut anchor_rigid_bodies = Vec::with_capacity(anchor_rigid_body_count as _);
        println!("rigid body count: {}", anchor_rigid_body_count);
        for _ in 0 .. anchor_rigid_body_count
        {
            let mut buf =
                vec![0u8; header.index_sizes.rigid_body.bytesize() + header.index_sizes.vertex.bytesize() + 1];
            reader.read_exact(&mut buf)?;
            anchor_rigid_bodies.push(AnchorRigidBody
            {
                index: IndexValue::from_bytes(&buf, header.index_sizes.rigid_body),
                vindex: header.index_sizes.rigid_body.decode_bytes_unsigned(
                    &buf[header.index_sizes.rigid_body.bytesize()..]),
                near_mode: unsafe { transmute(buf[buf.len() - 1]) }
            });
        }
        let vertex_pin_count = i32::read_value1(reader)?;
        let vertex_pins = match header.index_sizes.vertex
        {
            IndexSize::Byte =>
            {
                let mut bytes = vec![0u8; vertex_pin_count as _];
                reader.read_exact(&mut bytes)?;
                bytes.into_iter().map(|v| VertexPin(u32::from(v))).collect()
            },
            IndexSize::Short =>
            {
                let mut elements = vec![0u16; vertex_pin_count as _];
                let sink = unsafe
                {
                    std::slice::from_raw_parts_mut(elements.as_mut_ptr() as _, (vertex_pin_count * 2) as _)
                };
                reader.read_exact(sink).map(move |_| elements.into_iter().map(|v| VertexPin(u32::from(v))).collect())?
            },
            IndexSize::Long =>
            {
                let mut elements = vec![0u32; vertex_pin_count as _];
                let sink = unsafe
                {
                    std::slice::from_raw_parts_mut(elements.as_mut_ptr() as _, (vertex_pin_count * 4) as _)
                };

                reader.read_exact(sink).map(move |_| elements.into_iter().map(VertexPin).collect())?
            }
        };

        Ok(Softbody
        {
            name, shape, mindex, group, non_collision_mask, flags,
            b_link_create_distance: <i32 as TypedReader>::from_bytes(&bytes),
            cluster_count: <i32 as TypedReader>::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4..]),
            total_mass: f32::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*2..]),
            collision_margin: f32::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*2..]),
            aerodynamics_model,
            velocities_correction_factor: f32::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*4..]),
            dampling_coefficient: f32::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*5..]),
            drag_coefficient: f32::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*6..]),
            lift_coefficient: f32::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*7..]),
            pressure_coefficient: f32::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*8..]),
            volume_conversation_coefficient:
                f32::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*9..]),
            dynamic_friction_coeffecient: f32::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*10..]),
            pose_matching_coefficient: f32::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*11..]),
            rigid_contracts_hardness: f32::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*12..]),
            kinetic_contracts_hardness: f32::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*13..]),
            soft_contracts_hardness: f32::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*14..]),
            anchors_hardness: f32::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*15..]),
            soft_vs_rigid_hardness: f32::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*16..]),
            soft_vs_kinetic_hardness: f32::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*17..]),
            soft_vs_soft_hardness: f32::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*18..]),
            soft_vs_rigid_impulse_split: f32::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*19..]),
            soft_vs_kinetic_impulse_split: f32::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*20..]),
            soft_vs_soft_impulse_split: f32::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*21..]),
            velocities_solver_iterations:
                <i32 as TypedReader>::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*22..]),
            positions_solver_iterations:
                <i32 as TypedReader>::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*23..]),
            drift_solver_iterations:
                <i32 as TypedReader>::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*24..]),
            cluster_solver_iteratons:
                <i32 as TypedReader>::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*25..]),
            linear_stiffness_coefficient:
                <i32 as TypedReader>::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*26..]),
            area_angular_stiffness_coefficient:
                <i32 as TypedReader>::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*27..]),
            volume_stiffness_coefficient:
                <i32 as TypedReader>::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*28..]),
            anchor_rigid_bodies, vertex_pins
        })
    }
}
