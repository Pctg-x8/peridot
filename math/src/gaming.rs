//! Peridot Extended Mathematics: Gaming Utils(Camera, ModelMatrix)

use numtraits::*;
use linarg::*;
use std::ops::{Range, Neg};

pub enum ProjectionMethod { Orthographic { size: f32 }, Perspective { fov: f32 } }
pub struct Camera {
    pub projection: ProjectionMethod, pub position: Vector3F32, pub rotation: QuaternionF32,
    pub depth_range: Range<f32>
}

impl Camera {
    pub fn projection_matrix(&self) -> Matrix4F32 {
        match self.projection {
            ProjectionMethod::Perspective { fov } => {
                Matrix4::ONE
            },
            ProjectionMethod::Orthographic { size } => {
                Matrix4::ONE
            }
        }
    }
    pub fn view_matrix(&self) -> Matrix4F32 {
        Matrix4F32::translation(-self.position.clone()) * Matrix4F32::from(self.rotation.clone().neg())
    }
}

