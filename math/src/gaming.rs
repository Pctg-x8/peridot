//! Peridot Extended Mathematics: Gaming Utils(Camera, ModelMatrix)

use numtraits::*;
use linarg::*;
use std::ops::Range;

pub enum ProjectionMethod { Orthographic { size: f32 }, Perspective { fov: f32 } }
pub struct Camera {
    pub projection: ProjectionMethod, pub position: Vector3F32, pub rotation: QuaternionF32,
    pub depth_range: Range<f32>
}

impl Camera {
    pub fn projection_matrix(&self) -> Matrix4F32 {
        match self.projection {
            ProjectionMethod::Perspective { fov } => {
                
            },
            ProjectionMethod::Orthographic { size } => {

            }
        }
    }
    pub fn view_matrix(&self) -> Matrix4F32 {
        Matrix4F32::ONE.translate(-self.position).rotate(self.rotation.neg().into())
    }
}

