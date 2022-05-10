
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum PrimitiveType {
    PointList,
    LineList,
    LineStrip,
    LineListWithAdjacent,
    TriangleList,
    TriangleStrip,
    TriangleListWithAdjacent
}

#[derive(Clone, Copy, PartialEq, PartialOrd)]
pub struct Real(f32);
impl Real {
    pub fn from_valid_f32(v: f32) -> Option<Self> {
        if v.is_nan() { None } else { Some(Self(v)) }
    }
}
impl std::cmp::Eq for Real {}
impl std::hash::Hash for Real {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        unsafe { std::mem::transmute::<_, i32>(self.0).hash(state) }
    }
}

#[derive(Hash, PartialEq, Eq)]
pub struct RenderingConfigure {
    pub primitive_type: PrimitiveType,
    pub is_wire_frame: bool,
    pub line_width: Real
}
impl Default for RenderingConfigure {
    fn default() -> Self {
        RenderingConfigure {
            primitive_type: PrimitiveType::TriangleList,
            is_wire_frame: false,
            line_width: Real::from_valid_f32(1.0).expect("condition failed")
        }
    }
}
pub trait GameObject {
    fn render_info(&self) -> Option<RenderingConfigure> { None }
    fn update(&mut self);
}

pub struct RenderGroup {
    pub camera: peridot_math::Camera,
    pub objects: Vec<Box<dyn GameObject>>
}
impl RenderGroup {
    pub fn update(&mut self) {
        for o in &mut self.objects { o.update(); }
    }
}

pub struct Scene {
    pub groups: Vec<RenderGroup>
}
impl Scene {
    pub fn update(&mut self) {
        for o in &mut self.groups { o.update(); }
    }
}
