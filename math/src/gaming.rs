//! Peridot Extended Mathematics: Gaming Utils(Camera, ModelMatrix)

use linarg::*;
use std::ops::Range;

pub enum ProjectionMethod { Orthographic { size: f32 }, Perspective { fov: f32 } }
/// A camera
/// ## Examples
/// 
/// ```
/// # use peridot_math::*;
/// let c = Camera {
///     projection: ProjectionMethod::Orthographic { size: 5.0 }, position: Vector3::ZERO, rotation: Quaternion::ONE,
///     depth_range: 1.0 .. 9.0
/// };
/// let (mv, mp) = c.matrixes();
/// assert_eq!(mv.clone() * Vector3(5.0, 0.0, 1.0), Vector4(5.0, 0.0, 1.0, 1.0));
/// assert_eq!(mp * mv * Vector3(5.0, 0.0, 1.0), Vector4(1.0, 0.0, 0.0, 1.0));
/// ```
pub struct Camera {
    pub projection: ProjectionMethod, pub position: Vector3F32, pub rotation: QuaternionF32,
    pub depth_range: Range<f32>
}

impl Camera {
    /// calculates the camera projection matrix
    pub fn projection_matrix(&self) -> Matrix4F32 {
        match self.projection {
            ProjectionMethod::Perspective { fov } => {
                let scaling = (fov / 2.0).tan().recip();
                let zdiff = self.depth_range.end - self.depth_range.start;
                let zscale = (self.depth_range.end / zdiff,
                    -(self.depth_range.end * self.depth_range.start) / zdiff);
                
                Matrix4([scaling, 0.0, 0.0, 0.0], [0.0, scaling, 0.0, 0.0],
                    [0.0, 0.0, zscale.0, zscale.1], [0.0, 0.0, 1.0, 0.0])
            },
            ProjectionMethod::Orthographic { size } => {
                let zdiff = self.depth_range.end - self.depth_range.start;
                let t = Matrix4::translation(Vector3(0.0, 0.0, -self.depth_range.start));
                let s = Matrix4::scale(Vector4(size.recip(), size.recip(), zdiff.recip(), 1.0));

                s * t
            }
        }
    }
    /// calculates the camera view matrix
    pub fn view_matrix(&self) -> Matrix4F32 {
        Matrix4F32::from(-self.rotation.clone()) * Matrix4F32::translation(-self.position.clone())
    }
    /// calculates the camera view matrix and the projection matrix(returns in this order)
    pub fn matrixes(&self) -> (Matrix4F32, Matrix4F32) {
        (self.view_matrix(), self.projection_matrix())
    }

    /// Sets rotation of the camera to look at a point
    pub fn look_at(&mut self, target: Vector3F32)
    {
        let eyedir = (target - self.position.clone()).normalize();
        let basedir = Vector3(0.0f32, 0.0, 1.0);
        
        let axis = basedir.cross(&eyedir).normalize();
        let angle = basedir.dot(&eyedir).acos();
        println!("angle: {} axis: {:?}", angle, axis);
        self.rotation = Quaternion::new(-angle, axis);
    }
}
