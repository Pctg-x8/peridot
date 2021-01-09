//! Bezier to SDF Generator: https://astiopin.github.io/2019/01/06/sdf-on-gpu.html

use lyon_path::builder::{FlatPathBuilder, PathBuilder};
use euclid::{Point2D, Vector2D, Angle};

/// https://astiopin.github.io/2019/01/04/qbez-parabola.html
pub struct ParabolaRect {
    pub px_min: peridot::math::Vector2<f32>,
    pub px_max: peridot::math::Vector2<f32>,
    pub x0_lim: f32,
    pub x1_lim: f32,
    pub coord_transform: peridot::math::Matrix2x3F32
}

/// https://www.deepdyve.com/lp/association-for-computing-machinery/resolution-independent-rendering-of-deformable-vector-objects-using-YmvFNsOBWj
pub struct FigureVertices {
    pub triangle_fans: Vec<peridot::math::Vector2<f32>>,
    pub curve_triangles: Vec<peridot::VertexUV2D>,
    pub parabola_rects: Vec<ParabolaRect>
}
pub struct SDFGenerator {
    figure_start: Point2D<f32>,
    current: Point2D<f32>,
    approval_tolerance: f32,
    max_distance: f32,
    figure_vertices: Vec<FigureVertices>
}
impl SDFGenerator {
    pub fn new(approval_tolerance: f32, max_distance: f32) -> Self {
        SDFGenerator {
            figure_start: Point2D::new(0.0, 0.0),
            current: Point2D::new(0.0, 0.0),
            approval_tolerance,
            max_distance,
            figure_vertices: Vec::new()
        }
    }
}
impl FlatPathBuilder for SDFGenerator {
    type PathType = Vec<FigureVertices>;

    fn move_to(&mut self, p: Point2D<f32>) {
        self.figure_start = p;
        self.current = p;
        self.figure_vertices.push(FigureVertices {
            triangle_fans: vec![peridot::math::Vector2(p.x, p.y)],
            curve_triangles: Vec::new(),
            parabola_rects: Vec::new()
        });
    }
    fn line_to(&mut self, to: Point2D<f32>) {
        self.figure_vertices.last_mut().expect("no figure started?").triangle_fans.extend(vec![
            peridot::math::Vector2(self.current.x, self.current.y),
            peridot::math::Vector2(to.x, to.y)
        ]);
        // todo: calc parabola parameters
        self.current = to;
    }
    fn close(&mut self) {
        self.line_to(self.figure_start);
    }
    fn build(self) -> Self::PathType {
        self.figure_vertices
    }
    fn build_and_reset(&mut self) -> Self::PathType {
        self.figure_start = Point2D::new(0.0, 0.0);
        self.current = Point2D::new(0.0, 0.0);
        std::mem::replace(&mut self.figure_vertices, Vec::new())
    }
    fn current_position(&self) -> Point2D<f32> {
        self.current
    }
}
impl PathBuilder for SDFGenerator {
    fn quadratic_bezier_to(&mut self, c: Point2D<f32>, to: Point2D<f32>) {
        let current_figure = self.figure_vertices.last_mut().expect("no figure started?");

        current_figure.triangle_fans.extend(vec![
            peridot::math::Vector2(self.current.x, self.current.y),
            peridot::math::Vector2(to.x, to.y)
        ]);
        current_figure.curve_triangles.extend(vec![
            peridot::VertexUV2D {
                pos: peridot::math::Vector2(self.current.x, self.current.y),
                uv: peridot::math::Vector2(0.0, 0.0)
            },
            peridot::VertexUV2D {
                pos: peridot::math::Vector2(c.x, c.y),
                uv: peridot::math::Vector2(0.5, 0.0)
            },
            peridot::VertexUV2D {
                pos: peridot::math::Vector2(to.x, to.y),
                uv: peridot::math::Vector2(1.0, 1.0)
            }
        ]);
        // todo: calc parabola parameters
        self.current = to;
    }
    fn cubic_bezier_to(&mut self, ctrl1: Point2D<f32>, ctrl2: Point2D<f32>, to: Point2D<f32>) {
        lyon_geom::CubicBezierSegment {
            from: self.current, ctrl1, ctrl2, to
        }.for_each_quadratic_bezier(self.approval_tolerance, &mut |q| self.quadratic_bezier_to(q.ctrl, q.to));
    }
    fn arc(&mut self, center: Point2D<f32>, rad: Vector2D<f32>, sweeping_angle: Angle<f32>, x_rot: Angle<f32>) {
        lyon_geom::Arc {
            center, radii: rad, sweep_angle: sweeping_angle, x_rotation: x_rot, start_angle: Angle::degrees(0.0)
        }.for_each_quadratic_bezier(&mut |q| self.quadratic_bezier_to(q.ctrl, q.to));
    }
}
