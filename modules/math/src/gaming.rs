//! Peridot Extended Mathematics: Gaming Utils(Camera, ModelMatrix)

use crate::linarg::*;
use crate::{One, Zero};
use std::ops::Range;

/// How the camera will project vertices?
#[derive(Debug, Clone)]
pub enum ProjectionMethod {
    /// The orthographic projection
    Orthographic { size: f32 },
    /// The perspective projection. requires fov(unit: radians)
    Perspective { fov: f32 },
    /// The perspective projection but computed from physically-based units(millimeters).
    Physical {
        focal_length: f32,
        sensor_width: f32,
        sensor_height: f32,
        screen_fitting: PhysicalScreenFitting,
    },
    /// UI layouting optimized projection: (0, 0)-(design_width, design_height) will be mapped to (-1, -1)-(1, 1)
    /// This projection ignores aspect ratio.
    UI {
        design_width: f32,
        design_height: f32,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum PhysicalScreenFitting {
    // 縦のみ合わせる
    Vertical,
    // 横のみ合わせる
    Horizontal,
    // 画面からはみ出さないように調整
    Shrink,
    // 画面をすべて覆うように調整
    Enlarge,
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
    pub depth_range: Range<f32>,
}

impl Camera {
    /// calculates the camera projection matrix
    pub fn projection_matrix(&self, aspect_wh: f32) -> Matrix4F32 {
        match self.projection {
            Some(ProjectionMethod::Perspective { fov }) => {
                let scaling_tan = (fov / 2.0).tan();
                let zdiff = self.depth_range.end - self.depth_range.start;
                let zscale = (
                    self.depth_range.end / zdiff,
                    -(self.depth_range.end * self.depth_range.start) / zdiff,
                );

                Matrix4(
                    [(aspect_wh * scaling_tan).recip(), 0.0, 0.0, 0.0],
                    [0.0, -scaling_tan.recip(), 0.0, 0.0],
                    [0.0, 0.0, zscale.0, zscale.1],
                    [0.0, 0.0, 1.0, 0.0],
                )
            }
            Some(ProjectionMethod::Orthographic { size }) => {
                let zdiff = self.depth_range.end - self.depth_range.start;
                let t = Matrix4::translation(Vector3(0.0, 0.0, -self.depth_range.start));
                let s = Matrix4::scale(Vector4(
                    (aspect_wh * size).recip(),
                    -size.recip(),
                    zdiff.recip(),
                    1.0,
                ));

                s * t
            }
            Some(ProjectionMethod::Physical {
                focal_length,
                sensor_width,
                sensor_height,
                screen_fitting,
            }) => {
                let focal_length_meters = focal_length / 1000.0;
                let sensor_width_meters = sensor_width / 1000.0;
                let sensor_height_meters = sensor_height / 1000.0;
                let zd = self.depth_range.end - self.depth_range.start;

                let (scaling_x, scaling_y);
                match screen_fitting {
                    PhysicalScreenFitting::Vertical => {
                        // let scaling_tan = sensor_height_meters / focal_length_meters;

                        // scaling_x = (aspect_wh * scaling_tan).recip();
                        scaling_x = focal_length_meters / (aspect_wh * sensor_height_meters);
                        // scaling_y = scaling_tan.recip();
                        scaling_y = focal_length_meters / sensor_height_meters;
                    }
                    PhysicalScreenFitting::Horizontal => {
                        // let scaling_tan = sensor_width_meters / focal_length_meters;

                        // scaling_x = scaling_tan.recip();
                        scaling_x = focal_length_meters / sensor_width_meters;
                        // scaling_y = (scaling_tan / aspect_wh).recip();
                        scaling_y = (focal_length_meters * aspect_wh) / sensor_width_meters;
                    }
                    PhysicalScreenFitting::Shrink => {
                        let sensor_aspect_wh = sensor_width / sensor_height;
                        if sensor_aspect_wh < aspect_wh {
                            // let scaling_tan = sensor_height_meters / focal_length_meters;

                            // scaling_x = (aspect_wh * scaling_tan).recip();
                            scaling_x = focal_length_meters / (aspect_wh * sensor_height_meters);
                            // scaling_y = scaling_tan.recip();
                            scaling_y = focal_length_meters / sensor_height_meters;
                        } else {
                            // let scaling_tan = sensor_width_meters / focal_length_meters;

                            // scaling_x = scaling_tan.recip();
                            scaling_x = focal_length_meters / sensor_width_meters;
                            // scaling_y = (scaling_tan / aspect_wh).recip();
                            scaling_y = (focal_length_meters * aspect_wh) / sensor_width_meters;
                        }
                    }
                    PhysicalScreenFitting::Enlarge => {
                        let sensor_aspect_wh = sensor_width / sensor_height;
                        if sensor_aspect_wh > aspect_wh {
                            // let scaling_tan = sensor_height_meters / focal_length_meters;

                            // scaling_x = (aspect_wh * scaling_tan).recip();
                            scaling_x = focal_length_meters / (aspect_wh * sensor_height_meters);
                            // scaling_y = scaling_tan.recip();
                            scaling_y = focal_length_meters / sensor_height_meters;
                        } else {
                            // let scaling_tan = sensor_width_meters / focal_length_meters;

                            // scaling_x = scaling_tan.recip();
                            scaling_x = focal_length_meters / sensor_width_meters;
                            // scaling_y = (scaling_tan / aspect_wh).recip();
                            scaling_y = (focal_length_meters * aspect_wh) / sensor_width_meters;
                        }
                    }
                }

                // z = (z - znear) / (zfar - znear) = z / (zfar - znear) - znear / (zfar - znear)

                Matrix4(
                    [scaling_x, 0.0, 0.0, 0.0],
                    [0.0, -scaling_y, 0.0, 0.0],
                    [0.0, 0.0, zd.recip(), -self.depth_range.start / zd],
                    [0.0, 0.0, 1.0, 0.0],
                )
            }
            Some(ProjectionMethod::UI {
                design_width,
                design_height,
            }) => {
                let zdiff = self.depth_range.end - self.depth_range.start;
                let t = Matrix4::translation(Vector3(-1.0, -1.0, 0.0));
                let s = Matrix4::scale(Vector4(
                    2.0 / design_width,
                    2.0 / design_height,
                    zdiff.recip(),
                    1.0,
                ));

                t * s
            }
            None => Matrix4::scale(Vector4(aspect_wh.recip(), 1.0, 1.0, 1.0)),
        }
    }
    /// calculates the camera view matrix
    pub fn view_matrix(&self) -> Matrix4F32 {
        Matrix4F32::from(-self.rotation) * Matrix4F32::translation(-self.position)
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
        let eyedir = (target - self.position).normalize();
        let basedir = Vector3(0.0f32, 0.0, 1.0);

        let axis = basedir.cross(&eyedir);
        if axis.len2() == 0.0 {
            // same direction as basedir
            self.rotation = Quaternion::<f32>::ONE;
            return;
        }
        let angle = basedir.dot(eyedir).acos();
        self.rotation = Quaternion::<f32>::new(-angle, axis.normalize());
    }
}
impl Default for Camera {
    /// Default value of the Camera, that has identity view transform and Perspective projection with fov=60deg.
    fn default() -> Self {
        Camera {
            projection: Some(ProjectionMethod::Perspective {
                fov: 60.0f32.to_radians(),
            }),
            position: Vector3::ZERO,
            rotation: Quaternion::ONE,
            depth_range: 0.0..1.0,
        }
    }
}
