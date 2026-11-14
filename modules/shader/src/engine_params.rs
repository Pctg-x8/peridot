#[repr(C)]
#[derive(Debug, Clone)]
pub struct ObjectParameters {
    pub transform_matrix: peridot_math::Matrix4F32,
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct CameraParameters {
    pub view_projection_matrix: peridot_math::Matrix4F32,
}
