//! Bezier to SDF Generator: https://astiopin.github.io/2019/01/06/sdf-on-gpu.html

use euclid::{Angle, Point2D};
use lyon_path::builder::{FlatPathBuilder, PathBuilder};

pub use euclid::{Transform2D, Vector2D};

#[repr(C)]
#[derive(Clone, Debug)]
pub struct ParabolaRectVertex {
    pub pos: peridot::math::Vector2<f32>,
    pub pcoord: peridot::math::Vector2<f32>,
    pub x_limits: [f32; 2],
    pub scale: [f32; 2],
}

/// ref: https://astiopin.github.io/2019/01/04/qbez-parabola.html
pub struct ParabolaRect {
    pub rect_min: peridot::math::Vector2<f32>,
    pub rect_max: peridot::math::Vector2<f32>,
    pub vertex: peridot::math::Vector2<f32>,
    pub xaxis: peridot::math::Vector2<f32>,
    pub yaxis: peridot::math::Vector2<f32>,
    pub x0_lim: f32,
    pub x1_lim: f32,
    pub scale: f32,
}
impl ParabolaRect {
    pub fn from_quadratic_bezier(
        from: peridot::math::Vector2<f32>,
        ctrl: peridot::math::Vector2<f32>,
        to: peridot::math::Vector2<f32>,
        sdf_extra: f32,
    ) -> Self {
        // calc parabola parameters
        let yaxis = ((from + to) * 0.5 - ctrl).normalize();
        let xaxis = peridot::math::Vector2(yaxis.1, -yaxis.0);

        let x0 = (ctrl - from).dot(yaxis) / (2.0 * (ctrl - from).dot(xaxis));
        let x1 = (to - ctrl).dot(yaxis) / (2.0 * (to - ctrl).dot(xaxis));
        let scale = (to - from).dot(xaxis) / (x1 - x0);
        let vertex = from - (yaxis * (x0.powi(2) * scale) + xaxis * (x0 * scale));

        // calc bounding rect
        // Quadratic Bezierの作画方法的にctrlまでカーブはいかない(from-ctrl, ctrl-toの中間点を結んだ線までしかいかない)ので中間地点で判定できる らしい
        let m0 = (from + ctrl) * 0.5;
        let m1 = (ctrl + to) * 0.5;
        let rect_min = from.min(m0).min(m1).min(to) - peridot::math::Vector2(sdf_extra, sdf_extra);
        let rect_max = from.max(m0).max(m1).max(to) + peridot::math::Vector2(sdf_extra, sdf_extra);

        ParabolaRect {
            rect_min,
            rect_max,
            vertex,
            xaxis,
            yaxis,
            x0_lim: x0.min(x1),
            x1_lim: x1.max(x0),
            scale,
        }
    }
    pub fn from_line(
        from: peridot::math::Vector2<f32>,
        to: peridot::math::Vector2<f32>,
        sdf_extra: f32,
    ) -> Self {
        // 直線をxaxisとする
        let xaxis = (to - from).normalize();
        let yaxis = peridot::math::Vector2(-xaxis.1, xaxis.0);

        // 目立たないくらいに少しだけ膨らませるらしい 本当に直線にするとy方向のParabola座標を計算できなくなるので
        let c = (from + to) * 0.5;
        let ll = (from - to).len();
        let xlen = 1.0e-16f32.sqrt();
        let scale = 0.5 * ll / xlen;
        let vertex = c + yaxis * (ll * 1.0e-16);

        let rect_min = from.min(to.clone()) - peridot::math::Vector2(sdf_extra, sdf_extra);
        let rect_max = from.max(to.clone()) + peridot::math::Vector2(sdf_extra, sdf_extra);

        ParabolaRect {
            rect_min,
            rect_max,
            vertex,
            xaxis,
            yaxis,
            x0_lim: -xlen,
            x1_lim: xlen,
            scale,
        }
    }

    /// ワールド座標からParabola Local座標を計算する
    pub fn world_to_par_coord(
        &self,
        p: peridot::math::Vector2<f32>,
    ) -> peridot::math::Vector2<f32> {
        let relative_vertex = p - self.vertex;
        // project to axis
        let xlen = relative_vertex.dot(self.xaxis);
        let ylen = relative_vertex.dot(self.yaxis);

        peridot::math::Vector2(xlen / self.scale, ylen / self.scale)
    }

    pub fn make_vertices(&self) -> [ParabolaRectVertex; 6] {
        let v00 = ParabolaRectVertex {
            pos: peridot::math::Vector2(self.rect_min.0, self.rect_min.1),
            pcoord: self
                .world_to_par_coord(peridot::math::Vector2(self.rect_min.0, self.rect_min.1)),
            x_limits: [self.x0_lim, self.x1_lim],
            scale: [self.scale, 0.0],
        };
        let v10 = ParabolaRectVertex {
            pos: peridot::math::Vector2(self.rect_max.0, self.rect_min.1),
            pcoord: self
                .world_to_par_coord(peridot::math::Vector2(self.rect_max.0, self.rect_min.1)),
            x_limits: [self.x0_lim, self.x1_lim],
            scale: [self.scale, 0.0],
        };
        let v01 = ParabolaRectVertex {
            pos: peridot::math::Vector2(self.rect_min.0, self.rect_max.1),
            pcoord: self
                .world_to_par_coord(peridot::math::Vector2(self.rect_min.0, self.rect_max.1)),
            x_limits: [self.x0_lim, self.x1_lim],
            scale: [self.scale, 0.0],
        };
        let v11 = ParabolaRectVertex {
            pos: peridot::math::Vector2(self.rect_max.0, self.rect_max.1),
            pcoord: self
                .world_to_par_coord(peridot::math::Vector2(self.rect_max.0, self.rect_max.1)),
            x_limits: [self.x0_lim, self.x1_lim],
            scale: [self.scale, 0.0],
        };

        [v00, v01.clone(), v10.clone(), v01, v10, v11]
    }
}

/// https://www.deepdyve.com/lp/association-for-computing-machinery/resolution-independent-rendering-of-deformable-vector-objects-using-YmvFNsOBWj
pub struct FigureVertices {
    pub fill_triangle_points: Vec<peridot::math::Vector2<f32>>,
    pub fill_triangle_indices: Vec<u16>,
    pub curve_triangles: Vec<peridot::VertexUV2D>,
    pub parabola_rects: Vec<ParabolaRect>,
}
pub struct SDFGenerator {
    figure_start: Point2D<f32>,
    current: Point2D<f32>,
    transform: Transform2D<f32>,
    approval_tolerance: f32,
    max_distance: f32,
    figure_vertices: Vec<FigureVertices>,
}
impl SDFGenerator {
    pub fn new(approval_tolerance: f32, max_distance: f32) -> Self {
        SDFGenerator {
            figure_start: Point2D::new(0.0, 0.0),
            current: Point2D::new(0.0, 0.0),
            transform: Transform2D::identity(),
            approval_tolerance,
            max_distance,
            figure_vertices: Vec::new(),
        }
    }

    pub fn set_transform(&mut self, t: Transform2D<f32>) {
        self.transform = t;
    }
}
impl FlatPathBuilder for SDFGenerator {
    type PathType = Vec<FigureVertices>;

    fn move_to(&mut self, p: Point2D<f32>) {
        let p = self.transform.transform_point(&p);

        self.figure_start = p;
        self.current = p;
        self.figure_vertices.push(FigureVertices {
            fill_triangle_points: vec![peridot::math::Vector2(p.x, p.y)],
            fill_triangle_indices: vec![],
            curve_triangles: Vec::new(),
            parabola_rects: Vec::new(),
        });
    }
    fn line_to(&mut self, to: Point2D<f32>) {
        let to = self.transform.transform_point(&to);
        self.pretransformed_line_to(to);
    }
    fn close(&mut self) {
        self.pretransformed_line_to(self.figure_start);
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
impl SDFGenerator {
    fn pretransformed_line_to(&mut self, to: Point2D<f32>) {
        let from = peridot::math::Vector2(self.current.x, self.current.y);
        let tov = peridot::math::Vector2(to.x, to.y);

        let current_figure = self.figure_vertices.last_mut().expect("no figure started?");
        current_figure
            .parabola_rects
            .push(ParabolaRect::from_line(from, tov, self.max_distance));
        let from_point_index = current_figure.fill_triangle_points.len() as u16 - 1;
        current_figure.fill_triangle_points.push(tov);
        current_figure.fill_triangle_indices.extend(vec![
            0,
            from_point_index,
            current_figure.fill_triangle_points.len() as u16 - 1,
        ]);

        self.current = to;
    }
    fn pretransformed_quadratic_bezier_to(&mut self, c: Point2D<f32>, to: Point2D<f32>) {
        let from = peridot::math::Vector2(self.current.x, self.current.y);
        let ctrl = peridot::math::Vector2(c.x, c.y);
        let tov = peridot::math::Vector2(to.x, to.y);

        let current_figure = self.figure_vertices.last_mut().expect("no figure started?");
        current_figure
            .parabola_rects
            .push(ParabolaRect::from_quadratic_bezier(
                from,
                ctrl,
                tov,
                self.max_distance,
            ));
        let from_point_index = current_figure.fill_triangle_points.len() as u16 - 1;
        current_figure.fill_triangle_points.push(tov);
        current_figure.fill_triangle_indices.extend(vec![
            0,
            from_point_index,
            current_figure.fill_triangle_points.len() as u16 - 1,
        ]);
        current_figure.curve_triangles.extend(vec![
            peridot::VertexUV2D {
                pos: from,
                uv: peridot::math::Vector2(0.0, 0.0),
            },
            peridot::VertexUV2D {
                pos: ctrl,
                uv: peridot::math::Vector2(0.5, 0.0),
            },
            peridot::VertexUV2D {
                pos: tov,
                uv: peridot::math::Vector2(1.0, 1.0),
            },
        ]);

        self.current = to;
    }
}
impl PathBuilder for SDFGenerator {
    fn quadratic_bezier_to(&mut self, c: Point2D<f32>, to: Point2D<f32>) {
        let (c, to) = (
            self.transform.transform_point(&c),
            self.transform.transform_point(&to),
        );
        self.pretransformed_quadratic_bezier_to(c, to);
    }
    fn cubic_bezier_to(&mut self, ctrl1: Point2D<f32>, ctrl2: Point2D<f32>, to: Point2D<f32>) {
        lyon_geom::CubicBezierSegment {
            from: self.current,
            ctrl1: self.transform.transform_point(&ctrl1),
            ctrl2: self.transform.transform_point(&ctrl2),
            to: self.transform.transform_point(&to),
        }
        .for_each_quadratic_bezier(self.approval_tolerance, &mut |q| {
            self.pretransformed_quadratic_bezier_to(q.ctrl, q.to)
        });
    }
    fn arc(
        &mut self,
        center: Point2D<f32>,
        rad: Vector2D<f32>,
        sweeping_angle: Angle<f32>,
        x_rot: Angle<f32>,
    ) {
        lyon_geom::Arc {
            center: self.transform.transform_point(&center),
            radii: self.transform.transform_vector(&rad),
            sweep_angle: sweeping_angle,
            x_rotation: x_rot,
            start_angle: Angle::degrees(0.0),
        }
        .for_each_quadratic_bezier(&mut |q| self.pretransformed_quadratic_bezier_to(q.ctrl, q.to));
    }
}
