//! Peridot Vector Graphics Dept. powered by Pathfinder 2(lyon)

extern crate peridot_math;
extern crate pathfinder_partitioner;
extern crate lyon_path; extern crate euclid;

extern crate font_kit; mod font; pub use font::*;
use font_kit::{error::GlyphLoadingError, hinting::HintingOptions};

use pathfinder_partitioner::{mesh::Mesh, partitioner::Partitioner, builder::{Builder, Endpoint}};
pub use pathfinder_partitioner::FillRule;
pub use lyon_path::builder::{FlatPathBuilder, PathBuilder};
use lyon_path::geom::euclid::{Transform2D, Vector2D, Angle};
use peridot_math::{Vector2, Vector2F32};

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
            let tf = self.current_transform.post_translate(Vector2D::new(left_offs, -font.ascent()))
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
type V2F32 = euclid::Vector2D<f32>;
type P2F32 = euclid::Point2D<f32>;
/// Compute crosspoint: p0 + a * v0 = p1 + b * v1
/// 
/// Returns Some((a, b)) if succeeded, otherwise None
fn crosspoint_values(p0: P2F32, p1: P2F32, v0: V2F32, v1: V2F32) -> Option<(f32, f32)> {
    // get crosspoint: p0_p + a * dv = last_ep.to - b * dv_pre
    // p0_p.x + a * dv.x = last_ep.to.x - b * dv_pre.x
    // p0_p.y + a * dv.y = last_ep.to.y - b * dv_pre.y
    // : 以下dv.x != 0場合
    // a = (last_ep.to.x - b * dv_pre.x - p0_p.x) / dv.x
    // p0_p.y + a * dv.y = last_ep.to.y - b * dv_pre.y
    // p0_p.y + dv.y * (last_ep.to.x - b * dv_pre.x - p0_p.x) / dv.x = last_ep.to.y - b * dv_pre.y
    // p0_p.y * dv.x + dv.y * last_ep.to.x - dv_y * b * dv_pre.x - dv.y * p0_p.x = last_ep.to.y * dv.x - b * dv_pre.y * dv.x
    // p0_p.y * dv.x - p0_p.x * dv.y + dv.y * (last_ep.to.x - b * dv_pre.x) = dv.x * (last_ep.to.y - b * dv_pre.y)
    // p0_p.y * dv.x - p0_p.x * dv.y = dv.x * (last_ep.to.y - b * dv_pre.y) - dv.y * (last_ep.to.x - b * dv_pre.x)
    // p0_p.y * dv.x - p0_p.x * dv.y = dv.x * last_ep.to.y - b * dv.x * dv_pre.y - dv.y * last_ep.to.x + b * dv.y * dv_pre.x
    // ... = (dv.x * last_ep.to.y - dv.y * last_ep.to.x) - b * (dv.x * dv_pre.y - dv.y * dv_pre.x)
    // b * (dv.x * dv_pre.y - dv.y * dv_pre.x) = (dv.x * last_ep.to.y - dv.y * last_ep.to.x) - (dv.x * p0_p.y - dv.y * p0_p.x)
    // case cross(dv, dv_pre) != 0:
    //    b = cross(dv, last_ep.to - p0_p) / cross(dv, dv_pre)
    //    a = (last_ep.to.x - dv_pre.x * cross(dv, last_ep.to - p0_p) / cross(dv, dv_pre)) / dv.x
    // otherwise: unreachable
    // : dv.x == 0 && dv_pre.x != 0の場合
    // b * dv_pre.x = last_ep.to.x - p0_p.x
    // b = (last_ep.to.x - p0_p.x) / dv_pre.x
    // p0_p.y + a * dv.y = last_ep.to.y - dv_pre.y * (last_ep.to.x - p0_p.x) / dv_pre.x
    // a * dv.y = last_ep.to.y - p0_p.y - dv_pre.y * (last_ep.to.x - p0_p.x) / dv_pre.x
    // a * dv.y * dv_pre.x = last_ep.to.y * dv_pre.x - p0_p.y * dv_pre.x - dv_pre.y * last_ep.to.x + p0_p.x * dv_pre.y
    // a * dv.y * dv_pre.x = cross(dv_pre, last_ep.to) - cross(dv_pre, p0_p)
    // case dv.y * dv_pre.x != 0:
    //    a = cross(dv_pre, last_ep.to - p0_p) / (dv.y * dv_pre.x)
    //    b = (last_ep.to.x - p0_p.x) / dv_pre.x
    // otherwise: unreachable
    // : dv.x == 0 && dv_pre.x == 0の場合
    // undefined
    let (a, b);
    // 上の式はv1が逆になってるので反転
    let v1 = -v1;
    if v0.x != 0.0 {
        let cv = v0.cross(v1);
        if cv == 0.0 { return None; }
        b = v0.cross(p1 - p0) / cv;
        a = (p1.x - v1.x * v0.cross(p1 - p0) / cv) / v0.x;
    }
    else {
        if v1.x == 0.0 { return None; }
        let mdiv = v0.y * v1.x;
        if mdiv == 0.0 { return None; }
        a = v1.cross(p1 - p0) / mdiv;
        b = (p1.x - p0.x) / v1.x;
    }
    return Some((a, b));
}
impl<'c> FigureContext<'c> {
    /// Compute outline of this figure.
    /// NOTE: pathfinder_partitioner::Builderの内部構造を直接書き換えるので、あっちの構造が変わったらこっちも変える必要がある
    pub fn stroke_outline(&mut self, width: f32) {
        let builder_mut = self.partitioner.builder_mut();

        let mut new_subpath_ranges = Vec::new();
        let mut new_endpoints = Vec::new();
        for subpath_range in &builder_mut.subpath_ranges {
            let mut positive_endpoints = Vec::new();
            let mut negative_endpoints = Vec::new();

            let new_subpath_index = new_subpath_ranges.len() as u32;
            
            if subpath_range.start == subpath_range.end { continue; }
            let mut p0 = builder_mut.endpoints[subpath_range.start as usize].to;
            for ep in &mut builder_mut.endpoints[(subpath_range.start + 1) as usize .. subpath_range.end as usize] {
                match ep.ctrl {
                    // Line
                    None => {
                        let dv = (ep.to - p0).normalize();
                        // rotation matrix for 90deg: [c -s, s c] = [0 -1, 1 0]
                        // dv rot 90deg = (-dv.y, dv.x)
                        let norm = Vector2D::new(-dv.y, dv.x);
                        let p0_p = p0 + norm * width * 0.5;
                        let p0_n = p0 - norm * width * 0.5;
                        let p1_p = ep.to + norm * width * 0.5;
                        let p1_n = ep.to - norm * width * 0.5;

                        if positive_endpoints.is_empty() {
                            positive_endpoints.push(Endpoint { ctrl: None, to: p0_p, subpath_index: new_subpath_index });
                        }
                        else {
                            if positive_endpoints.last().expect("No endpoints?").to != p0_p {
                                // curve connection - miter
                                let (last_ep_to, dv_pre);
                                {
                                    // for shortening lifetime
                                    let last_ep = positive_endpoints.last().expect("No endpoints?");
                                    last_ep_to = last_ep.to;
                                    dv_pre = if let Some(cp) = last_ep.ctrl { last_ep_to - cp } else {
                                        let ref last_ep2 = positive_endpoints[positive_endpoints.len() - 2];
                                        last_ep_to - last_ep2.to
                                    }.normalize();
                                }
                                // TODO: 計算できなかったら単純に直線でつなぐ(擬似bevel)
                                let (_, b) = crosspoint_values(p0_p, last_ep_to, dv, dv_pre)
                                    .expect("Unable to compute the crosspoint");
                                let new_last_ep_to = last_ep_to + dv_pre * b;
                                // tweak last endpoint destination
                                positive_endpoints.last_mut().expect("No endpoints?").to = new_last_ep_to;
                            }
                        }
                        positive_endpoints.push(Endpoint { ctrl: None, to: p1_p, subpath_index: new_subpath_index });

                        if negative_endpoints.is_empty() {
                            negative_endpoints.push(Endpoint { ctrl: None, to: p0_n, subpath_index: new_subpath_index });
                        }
                        else {
                            if negative_endpoints.last().expect("No endpoints?").to != p0_p {
                                // connect - miter
                                let (last_ep_to, dv_pre);
                                {
                                    // shortening lifetime
                                    let last_ep = negative_endpoints.last().expect("No endpoints?");
                                    last_ep_to = last_ep.to;
                                    dv_pre = if let Some(cp) = last_ep.ctrl { last_ep_to - cp } else {
                                        let ref last_ep2 = negative_endpoints[negative_endpoints.len() - 2];
                                        last_ep_to - last_ep2.to
                                    }.normalize();
                                }
                                // TODO: 計算できなかったら単純に直線でつなぐ(擬似bevel)
                                let (_, b) = crosspoint_values(p0_p, last_ep_to, dv, dv_pre)
                                    .expect("Unable to compute the crosspoint");
                                let new_last_ep_to = last_ep_to + dv_pre * b;
                                // tweak last endpoint destination
                                negative_endpoints.last_mut().expect("No endpoints?").to = new_last_ep_to;
                            }
                        }
                        negative_endpoints.push(Endpoint { ctrl: None, to: p1_n, subpath_index: new_subpath_index });
                    },
                    // Quadratic Curve
                    Some(c) => {
                        let dv_0 = (c - p0).normalize();
                        let dv_1 = (c - ep.to).normalize();
                        let norm0 = Vector2D::new(-dv_0.y, dv_0.x);
                        let norm1 = Vector2D::new(-dv_1.y, dv_1.x);
                        let p0_p = p0 + norm0 * width * 0.5;
                        let p0_n = p0 - norm0 * width * 0.5;
                        let p1_p = ep.to + norm1 * width * 0.5;
                        let p1_n = ep.to - norm1 * width * 0.5;
                        
                        // for positive endpoints
                        let (a, _) = crosspoint_values(p0_p, p1_p, dv_0, dv_1)
                            .expect("Unable to compute the crosspoint");
                        let ctrl_p = p0_p + dv_0 * a;

                        // for negative endpoints
                        let (a, _) = crosspoint_values(p0_n, p1_n, dv_0, dv_1)
                            .expect("Unable to compute the crosspoint");
                        let ctrl_n = p0_n + dv_0 * a;

                        if positive_endpoints.is_empty() {
                            positive_endpoints.push(Endpoint { ctrl: None, to: p0_p, subpath_index: new_subpath_index });
                        }
                        else {
                            if positive_endpoints.last().expect("No endpoints?").to != p0_p {
                                // curve connection - miter
                                let (last_ep_to, dv_pre);
                                {
                                    // for shortening lifetime
                                    let last_ep = positive_endpoints.last().expect("No endpoints?");
                                    last_ep_to = last_ep.to;
                                    dv_pre = if let Some(cp) = last_ep.ctrl { last_ep_to - cp } else {
                                        let ref last_ep2 = positive_endpoints[positive_endpoints.len() - 2];
                                        last_ep_to - last_ep2.to
                                    }.normalize();
                                }
                                // TODO: 計算できなかったら単純に直線でつなぐ(擬似bevel)
                                let (_, b) = crosspoint_values(p0_p, last_ep_to, dv_0, dv_pre)
                                    .expect("Unable to compute the crosspoint");
                                let new_last_ep_to = last_ep_to + dv_pre * b;
                                // tweak last endpoint destination
                                positive_endpoints.last_mut().expect("No endpoints?").to = new_last_ep_to;
                            }
                        }
                        positive_endpoints.push(Endpoint { ctrl: Some(ctrl_p), to: p1_p, subpath_index: new_subpath_index });

                        if negative_endpoints.is_empty() {
                            negative_endpoints.push(Endpoint { ctrl: None, to: p0_n, subpath_index: new_subpath_index });
                        }
                        else {
                            if negative_endpoints.last().expect("No endpoints?").to != p0_p {
                                // connect - miter
                                let (last_ep_to, dv_pre);
                                {
                                    // shortening lifetime
                                    let last_ep = negative_endpoints.last().expect("No endpoints?");
                                    last_ep_to = last_ep.to;
                                    dv_pre = if let Some(cp) = last_ep.ctrl { last_ep_to - cp } else {
                                        let ref last_ep2 = negative_endpoints[negative_endpoints.len() - 2];
                                        last_ep_to - last_ep2.to
                                    }.normalize();
                                }
                                // TODO: 計算できなかったら単純に直線でつなぐ(擬似bevel)
                                let (_, b) = crosspoint_values(p0_p, last_ep_to, dv_0, dv_pre)
                                    .expect("Unable to compute the crosspoint");
                                let new_last_ep_to = last_ep_to + dv_pre * b;
                                // tweak last endpoint destination
                                negative_endpoints.last_mut().expect("No endpoints?").to = new_last_ep_to;
                            }
                        }
                        negative_endpoints.push(Endpoint { ctrl: Some(ctrl_n), to: p1_n, subpath_index: new_subpath_index });
                    }
                }
                p0 = ep.to;
            }

            let positive_ep_start = positive_endpoints.first().expect("No endpoints?").to;
            // let positive_ep_end = positive_endpoints.last().expect("No endpoints?").to;
            // let negative_ep_start = negative_endpoints.first().expect("No endpoints?").to;
            let negative_ep_end = negative_endpoints.last().expect("No endpoints?").to;

            let all_ep_begin = new_endpoints.len();
            new_endpoints.append(&mut positive_endpoints);
            new_endpoints.push(Endpoint { ctrl: None, to: negative_ep_end, subpath_index: new_subpath_index });
            new_endpoints.append(&mut negative_endpoints);
            new_endpoints.push(Endpoint { ctrl: None, to: positive_ep_start, subpath_index: new_subpath_index });
            new_subpath_ranges.push(all_ep_begin as u32 .. new_endpoints.len() as u32);
        }

        builder_mut.endpoints = new_endpoints;
        builder_mut.subpath_ranges = new_subpath_ranges;
    }
    pub fn end(mut self) -> &'c mut Context {
        self.partitioner.partition(self.fill_rule);
        self.partitioner.builder_mut().build_and_reset();
        let (st, ext) = tfconv_st_ext(self.ctx.current_transform);
        self.ctx.meshes.push((self.partitioner.into_mesh(), st, ext));

        return self.ctx;
    }
}
impl<'c> FlatPathBuilder for FigureContext<'c> {
    type PathType = <Builder as FlatPathBuilder>::PathType;

    fn move_to(&mut self, p: Point2D<f32>) { self.partitioner.builder_mut().move_to(p) }
    fn line_to(&mut self, to: Point2D<f32>) { self.partitioner.builder_mut().line_to(to) }
    fn close(&mut self) { self.partitioner.builder_mut().close() }
    fn build(self) -> Self::PathType { unimplemented!("cannot operate build for FigureContext") }
    fn build_and_reset(&mut self) -> Self::PathType { self.partitioner.builder_mut().build_and_reset() }
    fn current_position(&self) -> Point2D<f32> { self.partitioner.builder().current_position() }
}
impl<'c> PathBuilder for FigureContext<'c> {
    fn quadratic_bezier_to(&mut self, c: Point2D<f32>, to: Point2D<f32>) {
        self.partitioner.builder_mut().quadratic_bezier_to(c, to)
    }
    fn cubic_bezier_to(&mut self, c1: Point2D<f32>, c2: Point2D<f32>, to: Point2D<f32>) {
        self.partitioner.builder_mut().cubic_bezier_to(c1, c2, to)
    }
    fn arc(&mut self, center: Point2D<f32>, rad: Vector2D<f32>, sweeping_angle: Angle<f32>, x_rot: Angle<f32>) {
        self.partitioner.builder_mut().arc(center, rad, sweeping_angle, x_rot)
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

pub mod renderer_pivot {
    pub const LEFT_TOP:      [f32; 2] = [-1.0, -1.0];
    pub const MIDDLE_TOP:    [f32; 2] = [ 0.0, -1.0];
    pub const RIGHT_TOP:     [f32; 2] = [ 1.0, -1.0];
    pub const LEFT_MIDDLE:   [f32; 2] = [-1.0,  0.0];
    pub const MIDDLE_MIDDLE: [f32; 2] = [ 0.0,  0.0];
    pub const RIGHT_MIDDLE:  [f32; 2] = [ 1.0,  0.0];
    pub const LEFT_BOTTOM:   [f32; 2] = [-1.0,  1.0];
    pub const MIDDLE_BOTTOM: [f32; 2] = [ 0.0,  1.0];
    pub const RIGHT_BOTTOM:  [f32; 2] = [ 1.0,  1.0];
}
