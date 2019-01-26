//! Peridot Vector Graphics Dept. powered by Pathfinder 2(lyon)

extern crate peridot_math;
extern crate pathfinder_partitioner;
extern crate lyon_path; extern crate euclid;

extern crate font_kit; mod font; pub use font::*;
use font_kit::{error::GlyphLoadingError, hinting::HintingOptions};
use font_kit::loader::Loader;

use pathfinder_partitioner::{mesh::Mesh, partitioner::Partitioner, FillRule};
use lyon_path::builder::{FlatPathBuilder, PathBuilder};
use lyon_path::geom::euclid::{Transform2D, Vector2D, Angle};
use peridot_math::{Vector2, Vector2F32, Matrix4F32, Matrix4};

pub struct Context {
    meshes: Vec<(Mesh, [f32; 4], [f32; 2])>,
    current_transform: Transform2D<f32>
}
impl Context {
    pub fn new() -> Self {
        Context { meshes: Vec::new(), current_transform: Transform2D::identity() }
    }

    pub fn meshes(&self) -> &[(Mesh, [f32; 4], [f32; 2])] { &self.meshes }
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

fn tfconv_st_ext(v: Transform2D<f32>) -> ([f32; 4], [f32; 2]) {
    ([v.m11, v.m22, v.m31, v.m32], [v.m21, v.m12])
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
            let tf = self.current_transform.post_translate(Vector2D::new(left_offs, 0.0))
                .post_scale(font.scale_value(), font.scale_value());
            font.outline(g, HintingOptions::None, g0.builder_mut())?;
            g0.partition(FillRule::Winding);
            g0.builder_mut().build_and_reset();
            let (st, ext) = tfconv_st_ext(tf);
            self.meshes.push((g0.into_mesh(), st, ext));
            left_offs += adv.x(); max_height = max_height.max(size.size.height);
        }
        return Ok(self);
    }
}
impl<'c> FigureContext<'c> {
    pub fn end(mut self) -> &'c mut Context {
        self.partitioner.partition(self.fill_rule);
        self.partitioner.builder_mut().build_and_reset();
        let (st, ext) = tfconv_st_ext(self.ctx.current_transform);
        self.ctx.meshes.push((self.partitioner.into_mesh(), st, ext));

        return self.ctx;
    }
}

use lyon_path::geom::euclid::Point2D;

// OutlineData from FontSystem -> <Apply Translation> -> PathBuilder
pub struct TranslatingPathBuilder<'t, B: 't>(Vector2F32, &'t mut B);
impl<'t, B: FlatPathBuilder + 't> FlatPathBuilder for TranslatingPathBuilder<'t, B> {
    type PathType = B::PathType;
    
    fn move_to(&mut self, mut p: Point2D<f32>) {
        p.x += self.0.x(); p.y += self.0.y(); self.1.move_to(p * (12.0 / 2048.0))
    }
    fn line_to(&mut self, mut to: Point2D<f32>) {
        to.x += self.0.x(); to.y += self.0.y(); self.1.line_to(to * (12.0 / 2048.0))
    }
    fn close(&mut self) { self.1.close() }
    fn build(self) -> B::PathType { unimplemented!("cannot operate build for translatingpathbuilder"); }
    fn build_and_reset(&mut self) -> B::PathType { self.1.build_and_reset() }
    fn current_position(&self) -> Point2D<f32> { self.1.current_position() }
}
impl<'t, B: PathBuilder + 't> PathBuilder for TranslatingPathBuilder<'t, B> {
    fn quadratic_bezier_to(&mut self, mut ctrl: Point2D<f32>, mut to: Point2D<f32>) {
        ctrl.x += self.0.x(); ctrl.y += self.0.y();
        to.x += self.0.x(); to.y += self.0.y();
        self.1.quadratic_bezier_to(ctrl * (12.0 / 2048.0), to * (12.0 / 2048.0))
    }
    fn cubic_bezier_to(&mut self, mut ctrl1: Point2D<f32>, mut ctrl2: Point2D<f32>, mut to: Point2D<f32>) {
        ctrl1.x += self.0.x(); ctrl1.y += self.0.y();
        ctrl2.x += self.0.x(); ctrl2.y += self.0.y();
        to.x += self.0.x(); to.y += self.0.y();
        self.1.cubic_bezier_to(ctrl1 * (12.0 / 2048.0), ctrl2 * (12.0 / 2048.0), to * (12.0 / 2048.0))
    }
    fn arc(&mut self, mut center: Point2D<f32>, mut radii: Vector2D<f32>,
            sweep_angle: Angle<f32>, x_rotation: Angle<f32>) {
        center.x += self.0.x(); center.y += self.0.y();
        radii.x += self.0.x(); radii.y += self.0.y();
        self.1.arc(center * (12.0 / 2048.0), radii * (12.0 / 2048.0), sweep_angle, x_rotation)
    }
}
