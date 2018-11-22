//! Peridot Vector Graphics Dept. powered by Pathfinder 2(lyon)

extern crate peridot_math;
extern crate pathfinder_partitioner;
extern crate lyon_path; extern crate euclid;

extern crate font_kit; mod font; pub use font::*;
use font_kit::{error::GlyphLoadingError, hinting::HintingOptions};

use pathfinder_partitioner::{mesh::Mesh, partitioner::Partitioner, FillRule};
use lyon_path::builder::{FlatPathBuilder, PathBuilder};
use lyon_path::geom::euclid::{Transform2D, Vector2D, Angle};
use peridot_math::{Vector2, Vector2F32, Matrix4F32, Matrix4};

pub struct Context {
    meshes: Vec<(Mesh, Matrix4F32)>,
    current_transform: Transform2D<f32>
}
impl Context {
    pub fn new() -> Self {
        Context { meshes: Vec::new(), current_transform: Transform2D::identity() }
    }
}
/// Transforming
impl Context {
    pub fn set_transform(&mut self, tf: Transform2D<f32>) -> &mut Self { self.current_transform = tf; return self; }
    pub fn reset_transform(&mut self) -> &mut Self { self.set_transform(Transform2D::identity()) }

    pub fn translate(&mut self, Vector2(x, y): Vector2F32) -> &mut Self {
        self.current_transform = self.current_transform.post_translate(Vector2D::new(x, y)); return self;
    }
    pub fn rotate(&mut self, rad: f32) -> &mut Self {
        self.current_transform = self.current_transform.post_rotate(Angle::radians(rad)); return self;
    }
    pub fn scale(&mut self, Vector2(x, y): Vector2F32) -> &mut Self {
        self.current_transform = self.current_transform.post_scale(x, y); return self;
    }
}

fn tfconv(v: Transform2D<f32>) -> Matrix4<f32> {
    Matrix4([v.m11, v.m21, 0.0, v.m31], [v.m12, v.m22, 0.0, v.m32], [0.0, 0.0, 1.0, 0.0], [0.0, 0.0, 0.0, 1.0])
}

pub struct FigureContext<'c> {
    ctx: &'c mut Context, partitioner: Partitioner, fill_rule: FillRule
}
impl Context {
    pub fn begin_figure(&mut self, fill_rule: FillRule) -> FigureContext {
        FigureContext { ctx: self, partitioner: Partitioner::new(), fill_rule }
    }

    pub fn text(&mut self, font: &Font, text: &str) -> Result<&mut Self, GlyphLoadingError> {
        let glyphs = text.chars().map(|c| font.glyph_id(c).unwrap_or(0));
        let (mut left_offs, mut max_height) = (0.0, 0.0f32);
        for g in glyphs {
            let (adv, size) = (font.advance(g)?, font.bounds(g)?);
            let mut g0 = Partitioner::new();
            {
                let mut osink = TranslatingPathBuilder(Vector2(left_offs, 0.0), g0.builder_mut());
                font.outline(g, HintingOptions::None, &mut osink)?;
            }
            g0.partition(FillRule::Winding);
            g0.builder_mut().build_and_reset();
            self.meshes.push((g0.into_mesh(), tfconv(self.current_transform)));
            left_offs += adv.x(); max_height = max_height.max(size.size.height);
        }
        return Ok(self);
    }
}
impl<'c> FigureContext<'c> {
    pub fn end(mut self) -> &'c mut Context {
        self.partitioner.partition(self.fill_rule);
        self.partitioner.builder_mut().build_and_reset();
        self.ctx.meshes.push((self.partitioner.into_mesh(), tfconv(self.ctx.current_transform)));

        return self.ctx;
    }
}

use lyon_path::geom::euclid::Point2D;

// OutlineData from FontSystem -> <Apply Translation> -> PathBuilder
pub struct TranslatingPathBuilder<'t, B: 't>(Vector2F32, &'t mut B);
impl<'t, B: FlatPathBuilder + 't> FlatPathBuilder for TranslatingPathBuilder<'t, B> {
    type PathType = B::PathType;
    
    fn move_to(&mut self, mut p: Point2D<f32>) { p.x += self.0.x(); p.y += self.0.y(); self.1.move_to(p) }
    fn line_to(&mut self, mut to: Point2D<f32>) { to.x += self.0.x(); to.y += self.0.y(); self.1.line_to(to) }
    fn close(&mut self) { self.1.close() }
    fn build(self) -> B::PathType { unimplemented!("cannot operate build for translatingpathbuilder"); }
    fn build_and_reset(&mut self) -> B::PathType { self.1.build_and_reset() }
    fn current_position(&self) -> Point2D<f32> { self.1.current_position() }
}
impl<'t, B: PathBuilder + 't> PathBuilder for TranslatingPathBuilder<'t, B> {
    fn quadratic_bezier_to(&mut self, mut ctrl: Point2D<f32>, mut to: Point2D<f32>) {
        ctrl.x += self.0.x(); ctrl.y += self.0.y();
        to.x += self.0.x(); to.y += self.0.y();
        self.1.quadratic_bezier_to(ctrl, to)
    }
    fn cubic_bezier_to(&mut self, mut ctrl1: Point2D<f32>, mut ctrl2: Point2D<f32>, mut to: Point2D<f32>) {
        ctrl1.x += self.0.x(); ctrl1.y += self.0.y();
        ctrl2.x += self.0.x(); ctrl2.y += self.0.y();
        to.x += self.0.x(); to.y += self.0.y();
        self.1.cubic_bezier_to(ctrl1, ctrl2, to)
    }
    fn arc(&mut self, mut center: Point2D<f32>, mut radii: Vector2D<f32>,
            sweep_angle: Angle<f32>, x_rotation: Angle<f32>) {
        center.x += self.0.x(); center.y += self.0.y();
        radii.x += self.0.x(); radii.y += self.0.y();
        self.1.arc(center, radii, sweep_angle, x_rotation)
    }
}

// TODO: あとでModelDataにする
impl Context {
    pub fn vertices(&self) { }
    pub fn indices(&self) {}

    pub fn prealloc(&self) {
        let interior_positions_count = self.meshes.iter().map(|x| x.0.b_quad_vertex_positions.len()).sum();
        let interior_indices_count = self.meshes.iter().map(|x| x.0.b_quad_vertex_interior_indices.len()).sum();
        
    }
}
