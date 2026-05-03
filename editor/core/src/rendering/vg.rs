use std::collections::HashMap;

use bedrock as br;

use crate::{
    rendering::atlas::AtlasRect,
    utils::{Point, SafeF32},
};

#[derive(Clone, Copy)]
pub enum VectorTextureUnit {}
impl crate::utils::Unit for VectorTextureUnit {
    const DBG_NAME: &'static str = "VectorTextureUnit";

    type SignedValueType = f32;
    type UnsignedValueType = f32;
}
impl Point<VectorTextureUnit> {
    pub const fn new_vector_texture(x: f32, y: f32) -> Self {
        Self::new_custom(x, y)
    }

    #[inline(always)]
    pub fn to_lyon(&self) -> lyon_geom::Point<f32> {
        lyon_geom::point(self.x, self.y)
    }
}

pub struct VectorRasterizationState {
    pub fill_tri_points: Vec<[f32; 2]>,
    pub fill_tri_indices: Vec<u16>,
    pub curve_tris: Vec<[f32; 4]>,
    pub updated_rects: Vec<br::Rect2D>,
    pub rounded_fill_rect_radius_requests: HashMap<SafeF32, AtlasRect>,
    pub normalized_2d_mesh_requests: HashMap<usize, (u32, u32)>,
}
impl VectorRasterizationState {
    pub fn new() -> Self {
        Self {
            fill_tri_points: Vec::new(),
            fill_tri_indices: Vec::new(),
            curve_tris: Vec::new(),
            updated_rects: Vec::new(),
            rounded_fill_rect_radius_requests: HashMap::new(),
            normalized_2d_mesh_requests: HashMap::new(),
        }
    }

    pub fn clear(&mut self) {
        self.fill_tri_points.clear();
        self.fill_tri_indices.clear();
        self.curve_tris.clear();
        self.updated_rects.clear();
        self.rounded_fill_rect_radius_requests.clear();
        self.normalized_2d_mesh_requests.clear();
    }

    pub fn is_empty(&self) -> bool {
        // self.fill_tri_points.is_empty() == self.fill_tri_indices.is_empty()
        self.fill_tri_points.is_empty()
            && self.curve_tris.is_empty()
            && self.rounded_fill_rect_radius_requests.is_empty()
            && self.normalized_2d_mesh_requests.is_empty()
    }
}

pub struct VectorVertexRenderer<'a> {
    state: &'a mut VectorRasterizationState,
    pen: Point<VectorTextureUnit>,
    current_figure: Option<(Point<VectorTextureUnit>, usize)>,
}
impl<'a> VectorVertexRenderer<'a> {
    pub fn new(state: &'a mut VectorRasterizationState) -> Self {
        Self {
            state,
            pen: Point::new_vector_texture(0.0, 0.0),
            current_figure: None,
        }
    }

    pub fn move_to(&mut self, p: Point<VectorTextureUnit>) {
        self.state.fill_tri_points.push([p.x, p.y]);

        self.current_figure = Some((p, self.state.fill_tri_points.len() - 1));
        self.pen = p;
    }

    pub fn line_to(&mut self, p: Point<VectorTextureUnit>) {
        let Some((_, filltri_index0)) = self.current_figure else {
            panic!("no figure started?");
        };

        let filltri_index1 = self.state.fill_tri_points.len() - 1;
        self.state.fill_tri_points.push([p.x, p.y]);
        self.state.fill_tri_indices.extend([
            filltri_index0 as u16,
            filltri_index1 as u16,
            self.state.fill_tri_points.len() as u16 - 1,
        ]);

        self.pen = p;
    }

    pub fn quadratic_to(&mut self, ctrl: Point<VectorTextureUnit>, to: Point<VectorTextureUnit>) {
        let Some((_, filltri_index0)) = self.current_figure else {
            panic!("no figure started?");
        };

        let filltri_index1 = self.state.fill_tri_points.len() - 1;
        self.state.fill_tri_points.push([to.x, to.y]);
        self.state.fill_tri_indices.extend([
            filltri_index0 as u16,
            filltri_index1 as u16,
            self.state.fill_tri_points.len() as u16 - 1,
        ]);
        self.state.curve_tris.extend([
            [self.pen.x, self.pen.y, 0.0, 0.0],
            [ctrl.x, ctrl.y, 0.5, 0.0],
            [to.x, to.y, 1.0, 1.0],
        ]);

        self.pen = to;
    }

    pub fn cubic_to(
        &mut self,
        ctrl1: Point<VectorTextureUnit>,
        ctrl2: Point<VectorTextureUnit>,
        to: Point<VectorTextureUnit>,
    ) {
        let Some((_, filltri_index0)) = self.current_figure else {
            panic!("no figure started?");
        };

        lyon_geom::CubicBezierSegment {
            from: self.pen.to_lyon(),
            ctrl1: ctrl1.to_lyon(),
            ctrl2: ctrl2.to_lyon(),
            to: to.to_lyon(),
        }
        .for_each_quadratic_bezier(0.1, &mut |q| {
            let filltri_index1 = self.state.fill_tri_points.len() - 1;
            self.state.fill_tri_points.push([q.to.x, q.to.y]);
            self.state.fill_tri_indices.extend([
                filltri_index0 as u16,
                filltri_index1 as u16,
                self.state.fill_tri_points.len() as u16 - 1,
            ]);
            self.state.curve_tris.extend([
                [q.from.x, q.from.y, 0.0, 0.0],
                [q.ctrl.x, q.ctrl.y, 0.5, 0.0],
                [q.to.x, q.to.y, 1.0, 1.0],
            ]);
        });

        self.pen = to;
    }

    pub fn close(&mut self) {
        // line to figure origin
        let Some((org, filltri_index0)) = self.current_figure.take() else {
            panic!("no figure started?");
        };

        let filltri_index1 = self.state.fill_tri_points.len() - 1;
        self.state.fill_tri_points.push([org.x, org.y]);
        self.state.fill_tri_indices.extend([
            filltri_index0 as u16,
            filltri_index1 as u16,
            self.state.fill_tri_points.len() as u16 - 1,
        ]);

        self.pen = org;
    }

    pub const fn is_figure_opening(&self) -> bool {
        self.current_figure.is_some()
    }
}
