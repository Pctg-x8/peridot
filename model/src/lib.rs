
extern crate peridot_math;
use peridot_math::{Vector3, Vector3F32};
use std::borrow::Cow;

/// Provides model data(vertices, indices...)
pub trait ModelData {
    /// The data type of a vertex
    type Vertex: Clone;
    /// THe data type of a index, use u16 as default
    type Index: Clone;

    /// Returns vertices of the model, or generates vertices(for procedural models)
    fn vertices(&self) -> Cow<[Self::Vertex]>;
    /// Returns indices of the model, or generates indices(can be `None` if the model has no indices)
    fn indices(&self) -> Option<Cow<[Self::Index]>> { None }
}

#[repr(C)] #[derive(Clone)] pub struct PositionWithNormal { pub position: Vector3F32, pub normal: Vector3F32 }

fn gen_cube_mesh(size: Vector3F32) -> [PositionWithNormal; 6 * 6] {
    let Vector3(w, h, d) = size * 0.5;

    [
        PositionWithNormal { position: Vector3(w,  h,  d), normal: Vector3::RIGHT },
        PositionWithNormal { position: Vector3(w,  h, -d), normal: Vector3::RIGHT },
        PositionWithNormal { position: Vector3(w, -h,  d), normal: Vector3::RIGHT },
        PositionWithNormal { position: Vector3(w, -h,  d), normal: Vector3::RIGHT },
        PositionWithNormal { position: Vector3(w,  h, -d), normal: Vector3::RIGHT },
        PositionWithNormal { position: Vector3(w, -h, -d), normal: Vector3::RIGHT },

        PositionWithNormal { position: Vector3( w, h,  d), normal: Vector3::DOWN },
        PositionWithNormal { position: Vector3( w, h, -d), normal: Vector3::DOWN },
        PositionWithNormal { position: Vector3(-w, h,  d), normal: Vector3::DOWN },
        PositionWithNormal { position: Vector3(-w, h,  d), normal: Vector3::DOWN },
        PositionWithNormal { position: Vector3( w, h, -d), normal: Vector3::DOWN },
        PositionWithNormal { position: Vector3(-w, h, -d), normal: Vector3::DOWN },

        PositionWithNormal { position: Vector3( w, h,  d), normal: Vector3::FORWARD },
        PositionWithNormal { position: Vector3( w, -h, d), normal: Vector3::FORWARD },
        PositionWithNormal { position: Vector3(-w, h,  d), normal: Vector3::FORWARD },
        PositionWithNormal { position: Vector3(-w, h,  d), normal: Vector3::FORWARD },
        PositionWithNormal { position: Vector3( w, -h, d), normal: Vector3::FORWARD },
        PositionWithNormal { position: Vector3(-w, -h, d), normal: Vector3::FORWARD },

        PositionWithNormal { position: Vector3(-w,  h,  d), normal: -Vector3F32::RIGHT },
        PositionWithNormal { position: Vector3(-w,  h, -d), normal: -Vector3F32::RIGHT },
        PositionWithNormal { position: Vector3(-w, -h,  d), normal: -Vector3F32::RIGHT },
        PositionWithNormal { position: Vector3(-w, -h,  d), normal: -Vector3F32::RIGHT },
        PositionWithNormal { position: Vector3(-w,  h, -d), normal: -Vector3F32::RIGHT },
        PositionWithNormal { position: Vector3(-w, -h, -d), normal: -Vector3F32::RIGHT },

        PositionWithNormal { position: Vector3( w, -h,  d), normal: -Vector3F32::DOWN },
        PositionWithNormal { position: Vector3( w, -h, -d), normal: -Vector3F32::DOWN },
        PositionWithNormal { position: Vector3(-w, -h,  d), normal: -Vector3F32::DOWN },
        PositionWithNormal { position: Vector3(-w, -h,  d), normal: -Vector3F32::DOWN },
        PositionWithNormal { position: Vector3( w, -h, -d), normal: -Vector3F32::DOWN },
        PositionWithNormal { position: Vector3(-w, -h, -d), normal: -Vector3F32::DOWN },

        PositionWithNormal { position: Vector3( w, h,  -d), normal: -Vector3F32::FORWARD },
        PositionWithNormal { position: Vector3( w, -h, -d), normal: -Vector3F32::FORWARD },
        PositionWithNormal { position: Vector3(-w, h,  -d), normal: -Vector3F32::FORWARD },
        PositionWithNormal { position: Vector3(-w, h,  -d), normal: -Vector3F32::FORWARD },
        PositionWithNormal { position: Vector3( w, -h, -d), normal: -Vector3F32::FORWARD },
        PositionWithNormal { position: Vector3(-w, -h, -d), normal: -Vector3F32::FORWARD }
    ]
}
pub struct StaticCube([PositionWithNormal; 6 * 6]);
impl StaticCube {
    pub fn new(&self, size: Vector3F32) -> Self { StaticCube(gen_cube_mesh(size)) }
}
impl ModelData for StaticCube {
    type Vertex = PositionWithNormal;
    type Index = u16;

    fn vertices(&self) -> Cow<[Self::Vertex]> { Cow::Borrowed(&self.0[..]) }
}
pub struct DynamicCube(Vector3F32);
impl DynamicCube {
    pub fn new(&self, size: Vector3F32) -> Self { DynamicCube(size) }
    pub fn set_size(&mut self, size: Vector3F32) { self.0 = size; }
}
impl ModelData for DynamicCube {
    type Vertex = PositionWithNormal;
    type Index = u16;

    fn vertices(&self) -> Cow<[Self::Vertex]> { Cow::Owned(gen_cube_mesh(self.0.clone()).to_vec()) }
}
