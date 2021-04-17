//! Peridot Extended Mathematics: Gaming Utils(Camera, ModelMatrix)

use crate::linarg::*;
use std::ops::Range;
use crate::{One, Zero};

/// How the camera will project vertices?
pub enum ProjectionMethod {
    /// The orthographic projection
    Orthographic { size: f32 },
    /// The perspective projection. requires fov(unit: radians)
    Perspective { fov: f32 },
    /// UI layouting optimized projection: (0, 0)-(design_width, design_height) will be mapped to (-1, -1)-(1, 1)
    /// This projection ignores aspect ratio.
    UI { design_width: f32, design_height: f32 }
}
/// A camera
/// ## Examples
/// 
/// ```
/// # use peridot_math::*;
/// let c = Camera {
///     projection: Some(ProjectionMethod::Orthographic { size: 5.0 }),
///     position: Vector3::ZERO, rotation: Quaternion::ONE,
///     depth_range: 1.0 .. 9.0
/// };
/// let (mv, mp) = c.matrixes(1.0);
/// assert_eq!(mv.clone() * Vector3(5.0, 0.0, 1.0), Vector4(5.0, 0.0, 1.0, 1.0));
/// assert_eq!(mp * mv * Vector3(5.0, 0.0, 1.0), Vector4(1.0, 0.0, 0.0, 1.0));
/// ```
pub struct Camera {
    /// Projection method of the camera. `None` indicates no projection(only adjust aspect ratio)
    pub projection: Option<ProjectionMethod>,
    /// Eye position of the camera.
    pub position: Vector3F32,
    /// Eye direction of the camera.
    pub rotation: QuaternionF32,
    /// Z range to be rendered.
    pub depth_range: Range<f32>
}

impl Camera {
    /// calculates the camera projection matrix
    pub fn projection_matrix(&self, aspect_wh: f32) -> Matrix4F32 {
        match self.projection {
            Some(ProjectionMethod::Perspective { fov }) => {
                let scaling_tan = (fov / 2.0).tan();
                let zdiff = self.depth_range.end - self.depth_range.start;
                let zscale = (self.depth_range.end / zdiff,
                    -(self.depth_range.end * self.depth_range.start) / zdiff);
                
                Matrix4(
                    [(aspect_wh * scaling_tan).recip(), 0.0, 0.0, 0.0], [0.0, -scaling_tan.recip(), 0.0, 0.0],
                    [0.0, 0.0, zscale.0, zscale.1], [0.0, 0.0, 1.0, 0.0]
                )
            },
            Some(ProjectionMethod::Orthographic { size }) => {
                let zdiff = self.depth_range.end - self.depth_range.start;
                let t = Matrix4::translation(Vector3(0.0, 0.0, -self.depth_range.start));
                let s = Matrix4::scale(Vector4((aspect_wh * size).recip(), -size.recip(), zdiff.recip(), 1.0));

                s * t
            },
            Some(ProjectionMethod::UI { design_width, design_height }) => {
                let zdiff = self.depth_range.end - self.depth_range.start;
                let t = Matrix4::translation(Vector3(-1.0, -1.0, 0.0));
                let s = Matrix4::scale(Vector4(2.0 / design_width, 2.0 / design_height, zdiff.recip(), 1.0));

                t * s
            },
            None => Matrix4::scale(Vector4(aspect_wh.recip(), 1.0, 1.0, 1.0))
        }
    }
    /// calculates the camera view matrix
    pub fn view_matrix(&self) -> Matrix4F32 {
        Matrix4F32::from(-self.rotation.clone()) * Matrix4F32::translation(-self.position.clone())
    }
    /// calculates the camera transform(view and projection) matrix
    pub fn view_projection_matrix(&self, aspect_wh: f32) -> Matrix4F32 {
        let (v, p) = self.matrixes(aspect_wh);
        p * v
    }
    /// calculates the camera view matrix and the projection matrix(returns in this order)
    pub fn matrixes(&self, aspect_wh: f32) -> (Matrix4F32, Matrix4F32) {
        (self.view_matrix(), self.projection_matrix(aspect_wh))
    }

    /// Sets rotation of the camera to look at a point
    pub fn look_at(&mut self, target: Vector3F32) {
        let eyedir = (target - self.position.clone()).normalize();
        let basedir = Vector3(0.0f32, 0.0, 1.0);
        
        let axis = basedir.cross(&eyedir).normalize();
        let angle = basedir.dot(eyedir).acos();
        self.rotation = Quaternion::new(-angle, axis);
    }
}
impl Default for Camera {
    /// Default value of the Camera, that has identity view transform and Perspective projection with fov=60deg.
    fn default() -> Self {
        Camera {
            projection: Some(ProjectionMethod::Perspective { fov: 60.0f32.to_radians() }),
            position: Vector3::ZERO,
            rotation: Quaternion::ONE,
            depth_range: 0.0 .. 1.0
        }
    }
}
