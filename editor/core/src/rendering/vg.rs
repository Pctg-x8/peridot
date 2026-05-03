use std::collections::HashMap;

use bedrock as br;

use crate::{rendering::atlas::AtlasRect, utils::SafeF32};

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
    pen_x: f32,
    pen_y: f32,
    current_figure: Option<(f32, f32, usize)>,
}
impl<'a> VectorVertexRenderer<'a> {
    pub fn new(state: &'a mut VectorRasterizationState) -> Self {
        Self {
            state,
            pen_x: 0.0,
            pen_y: 0.0,
            current_figure: None,
        }
    }

    pub fn move_to(&mut self, x: f32, y: f32) {
        self.state.fill_tri_points.push([x, y]);

        self.current_figure = Some((x, y, self.state.fill_tri_points.len() - 1));
        self.pen_x = x;
        self.pen_y = y;
    }

    pub fn line_to(&mut self, x: f32, y: f32) {
        let Some((_, _, filltri_index0)) = self.current_figure else {
            panic!("no figure started?");
        };

        let filltri_index1 = self.state.fill_tri_points.len() - 1;
        self.state.fill_tri_points.push([x, y]);
        self.state.fill_tri_indices.extend([
            filltri_index0 as u16,
            filltri_index1 as u16,
            self.state.fill_tri_points.len() as u16 - 1,
        ]);

        self.pen_x = x;
        self.pen_y = y;
    }

    pub fn quadratic_to(&mut self, cx: f32, cy: f32, x: f32, y: f32) {
        let Some((_, _, filltri_index0)) = self.current_figure else {
            panic!("no figure started?");
        };

        let filltri_index1 = self.state.fill_tri_points.len() - 1;
        self.state.fill_tri_points.push([x, y]);
        self.state.fill_tri_indices.extend([
            filltri_index0 as u16,
            filltri_index1 as u16,
            self.state.fill_tri_points.len() as u16 - 1,
        ]);
        self.state.curve_tris.extend([
            [self.pen_x, self.pen_y, 0.0, 0.0],
            [cx, cy, 0.5, 0.0],
            [x, y, 1.0, 1.0],
        ]);

        self.pen_x = x;
        self.pen_y = y;
    }

    pub fn cubic_to(&mut self, c1x: f32, c1y: f32, c2x: f32, c2y: f32, x: f32, y: f32) {
        let Some((_, _, filltri_index0)) = self.current_figure else {
            panic!("no figure started?");
        };

        lyon_geom::CubicBezierSegment {
            from: lyon_geom::point(self.pen_x, self.pen_y),
            ctrl1: lyon_geom::point(c1x, c1y),
            ctrl2: lyon_geom::point(c2x, c2y),
            to: lyon_geom::point(x, y),
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

        self.pen_x = x;
        self.pen_y = y;
    }

    pub fn close(&mut self) {
        // line to figure origin
        let Some((ox, oy, filltri_index0)) = self.current_figure.take() else {
            panic!("no figure started?");
        };

        let filltri_index1 = self.state.fill_tri_points.len() - 1;
        self.state.fill_tri_points.push([ox, oy]);
        self.state.fill_tri_indices.extend([
            filltri_index0 as u16,
            filltri_index1 as u16,
            self.state.fill_tri_points.len() as u16 - 1,
        ]);

        self.pen_x = ox;
        self.pen_y = oy;
    }
}
