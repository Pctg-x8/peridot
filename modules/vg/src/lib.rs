//! Peridot Vector Graphics Dept. powered by Pathfinder 2(lyon)

mod font;
pub use font::*;
pub mod sdf_generator;
pub use self::sdf_generator::SDFGenerator;

use pathfinder_partitioner::{builder::Builder, mesh::Mesh, partitioner::Partitioner};
// use lyon_path::PathEvent;
use lyon_path::geom::euclid::{Angle, Transform2D, Vector2D};
use peridot::math::{Vector2, Vector2F32};

mod rendering;
pub use self::rendering::*;

pub use lyon_path::{
    builder::{FlatPathBuilder, PathBuilder},
    PathEvent,
};
pub use pathfinder_partitioner::FillRule;

pub struct Context {
    meshes: Vec<(Mesh, [f32; 4], [f32; 2])>,
    current_transform: Transform2D<f32>,
    screen_scaling: f32,
}
impl Context {
    pub fn new(screen_scaling: f32) -> Self {
        Context {
            meshes: Vec::new(),
            current_transform: Transform2D::identity(),
            screen_scaling,
        }
    }

    pub fn meshes(&self) -> &[(Mesh, [f32; 4], [f32; 2])] {
        &self.meshes
    }
}
/// Transforming
impl Context {
    pub fn set_transform(&mut self, tf: Transform2D<f32>) -> &mut Self {
        self.current_transform = tf;
        return self;
    }
    pub fn reset_transform(&mut self) -> &mut Self {
        self.set_transform(Transform2D::identity())
    }

    pub fn translate(&mut self, Vector2(x, y): Vector2F32) -> &mut Self {
        self.current_transform = self.current_transform.post_translate(Vector2D::new(x, y));
        return self;
    }
    pub fn rotate(&mut self, rad: f32) -> &mut Self {
        self.current_transform = self.current_transform.post_rotate(Angle::radians(rad));
        return self;
    }
    pub fn scale(&mut self, Vector2(x, y): Vector2F32) -> &mut Self {
        self.current_transform = self.current_transform.post_scale(x, y);
        return self;
    }
}

fn tfconv_st_ext(v: Transform2D<f32>) -> ([f32; 4], [f32; 2]) {
    ([v.m11, v.m22, v.m31, v.m32], [v.m21, v.m12])
}

pub struct FigureContext<'c> {
    ctx: &'c mut Context,
    partitioner: Partitioner,
    fill_rule: FillRule,
}
impl Context {
    pub fn begin_figure(&mut self, fill_rule: FillRule) -> FigureContext {
        FigureContext {
            ctx: self,
            partitioner: Partitioner::new(),
            fill_rule,
        }
    }

    pub fn text<F>(&mut self, font: &F, text: &str) -> Result<&mut Self, GlyphLoadingError>
    where
        F: Font,
        <F as Font>::GlyphID: Default,
    {
        let glyphs = text.chars().map(|c| font.glyph_id(c).unwrap_or_default());
        let (mut left_offs, mut max_height) = (0.0, 0.0f32);
        for g in glyphs {
            let (adv, size) = (font.advance_h(&g)?, font.bounds(&g)?);
            let mut g0 = Partitioner::new();
            let tf = self.current_transform.post_translate(Vector2D::new(
                left_offs * font.scale_value() * self.screen_scaling,
                -font.ascent() * font.scale_value() * self.screen_scaling,
            ));
            if font.outline(&g, g0.builder_mut()).is_ok() {
                g0.partition(FillRule::Winding);
                g0.builder_mut().build_and_reset();
                let (st, ext) = tfconv_st_ext(tf);
                self.meshes.push((g0.into_mesh(), st, ext));
            }
            left_offs += adv;
            max_height = max_height.max(size.size.height);
        }
        return Ok(self);
    }
}
/*type V2F32 = euclid::Vector2D<f32>;
type P2F32 = euclid::Point2D<f32>;
/// Compute crosspoint: p0 + a * v0 = p1 + b * v1
///
/// Returns Some(a) if succeeded, otherwise None
fn crosspoint_value(p0: P2F32, p1: P2F32, v0: V2F32, v1: V2F32) -> Option<f32> {
    // Get Crosspoint! solve: p0 + a * v0 = p1 + b * v1 for (a, b)
    //
    // p0 - p1 = b * v1 - a * v0
    // (p0 - p1).x = b * v1.x - a * v0.x
    // (p0 - p1).y = b * v1.y - a * v0.y

    //    v1.y * (p0 - p1).x = b * v1.x * v1.y - a * v0.x * v1.y
    // -) v1.x * (p0 - p1).y = b * v1.y * v1.x - a * v1.x * v0.y
    // ------------------------------------------------------------
    //    v1.y * (p0 - p1).x - v1.x * (p0 - p1).y = -a * (v0.x * v1.y - v0.y * v1.x)
    //    cross(v1, p0 - p1) = a * cross(v0, v1)

    println!("compute crosspoint: {:?}+a*{:?}={:?}+b*{:?}", p0, v0, p1, v1);
    let cv = v0.cross(v1);
    if cv == 0.0 { None } else { Some(v1.cross(p0 - p1) / cv) }
}*/
/*pub struct StrokePathBuilder { width: f32, traces: Vec<PathEvent>, last: euclid::Point2D<f32> }
impl StrokePathBuilder {
    pub fn new(width: f32) -> Self {
        StrokePathBuilder { width, traces: Vec::new(), last: euclid::Point2D::new(0.0, 0.0) }
    }
}
impl FlatPathBuilder for StrokePathBuilder {
    type PathType = Vec<PathEvent>;

    fn move_to(&mut self, p: euclid::Point2D<f32>) { self.last = p; self.traces.push(PathEvent::MoveTo(p)); }
    fn line_to(&mut self, p: euclid::Point2D<f32>) { self.last = p; self.traces.push(PathEvent::LineTo(p)); }
    fn close(&mut self) { self.traces.push(PathEvent::Close); }
    fn build(self) -> Vec<PathEvent> { unimplemented!("use sink_widened"); }
    fn build_and_reset(&mut self) -> Vec<PathEvent> {
        unimplemented!("use sink_widened");
    }
    fn current_position(&self) -> euclid::Point2D<f32> { self.last }
}
impl PathBuilder for StrokePathBuilder {
    fn quadratic_bezier_to(&mut self, c: euclid::Point2D<f32>, to: euclid::Point2D<f32>) {
        self.last = to; self.traces.push(PathEvent::QuadraticTo(c, to));
    }
    fn cubic_bezier_to(&mut self, c1: euclid::Point2D<f32>, c2: euclid::Point2D<f32>, to: euclid::Point2D<f32>) {
        self.last = to; self.traces.push(PathEvent::CubicTo(c1, c2, to));
    }
    fn arc(&mut self, _center: Point2D<f32>, _rad: Vector2D<f32>, _sweeping_angle: Angle<f32>, _x_rot: Angle<f32>) {
        unimplemented!("unsuppoted arc instruction");
    }
}
fn pathevent_to_point(e: &PathEvent) -> euclid::Point2D<f32> {
    match e {
        &PathEvent::MoveTo(p) | &PathEvent::LineTo(p) |
        &PathEvent::QuadraticTo(_, p) | &PathEvent::CubicTo(_, _, p) => p,
        PathEvent::Close => panic!("The destination point cannot be taken from Close"),
        PathEvent::Arc(_, _, _, _) => panic!("Arc not supported")
    }
}
fn pathevent_to_point_mut(e: &mut PathEvent) -> &mut euclid::Point2D<f32> {
    match e {
        PathEvent::MoveTo(p) | PathEvent::LineTo(p) |
        PathEvent::QuadraticTo(_, p) | PathEvent::CubicTo(_, _, p) => p,
        PathEvent::Close => panic!("The destination point cannot be taken from Close"),
        PathEvent::Arc(_, _, _, _) => panic!("Arc not supported")
    }
}
impl StrokePathBuilder {
    pub fn sink_widened<B: PathBuilder + ?Sized>(self, target_builder: &mut B) {
        let (mut positive_events, mut negative_events) = (Vec::new(), Vec::new());

        let mut cur_point = euclid::Point2D::new(0.0, 0.0);
        println!("Processing Traces: {:?}", self.traces);
        for e in self.traces {
            println!("Processing: {:?}", e);
            match e {
                PathEvent::LineTo(p) => {
                    let dv = (cur_point - p).normalize();
                    let norm = euclid::Vector2D::new(-dv.y, dv.x);
                    let (p0_p, p0_n) = (cur_point + norm * self.width * 0.5, cur_point - norm * self.width * 0.5);
                    let (p1_p, p1_n) = (p + norm * self.width * 0.5, p - norm * self.width * 0.5);

                    if positive_events.is_empty() { positive_events.push(PathEvent::MoveTo(p0_p)); }
                    else {
                        if pathevent_to_point(positive_events.last().expect("No endpoints?")) != p0_p {
                            // curve connection - miter
                            let (last_ep_to, dv_pre);
                            // for shortening lifetime
                            match positive_events.last().expect("No endpoints?") {
                                &PathEvent::MoveTo(p) | &PathEvent::LineTo(p) => {
                                    let ref last_ep2 = positive_events[positive_events.len() - 2];
                                    dv_pre = (p - pathevent_to_point(last_ep2)).normalize();
                                    last_ep_to = p;
                                },
                                &PathEvent::QuadraticTo(c, p) | &PathEvent::CubicTo(_, c, p) => {
                                    dv_pre = (p - c).normalize();
                                    last_ep_to = p;
                                },
                                _ => unreachable!("unable to compute")
                            }
                            // memo: 計算できなかったら単純に直線でつなぐ(擬似bevel)
                            let a = crosspoint_value(p0_p, last_ep_to, dv, dv_pre)
                                .expect("Unable to compute the crosspoint");
                            let new_last_ep_to = p0_p + dv * a;
                            // tweak last endpoint destination
                            *pathevent_to_point_mut(positive_events.last_mut().expect("No endpoints?"))
                                = new_last_ep_to;
                        }
                    }
                    positive_events.push(PathEvent::LineTo(p1_p));

                    if negative_events.is_empty() { negative_events.push(PathEvent::MoveTo(p0_n)); }
                    else {
                        if pathevent_to_point(negative_events.last().expect("No endpoints?")) != p0_n {
                            // connect - miter
                            let (last_ep_to, dv_pre);
                            // for shortening lifetime
                            match negative_events.last().expect("No endpoints?") {
                                &PathEvent::MoveTo(p) | &PathEvent::LineTo(p) => {
                                    let ref last_ep2 = negative_events[negative_events.len() - 2];
                                    dv_pre = (p - pathevent_to_point(last_ep2)).normalize();
                                    last_ep_to = p;
                                },
                                &PathEvent::QuadraticTo(c, p) | &PathEvent::CubicTo(_, c, p) => {
                                    dv_pre = (p - c).normalize();
                                    last_ep_to = p;
                                },
                                _ => unreachable!("unable to compute")
                            }
                            // memo: 計算できなかったら単純に直線でつなぐ(擬似bevel)
                            let a = crosspoint_value(p0_n, last_ep_to, dv, dv_pre)
                                .expect("Unable to compute the crosspoint");
                            let new_last_ep_to = p0_n + dv * a;
                            // tweak last endpoint destination
                            *pathevent_to_point_mut(negative_events.last_mut().expect("No endpoints?"))
                                = new_last_ep_to;
                        }
                    }
                    negative_events.push(PathEvent::LineTo(p1_n));

                    cur_point = p;
                },
                PathEvent::QuadraticTo(c, p) => {
                    let (dv0, dv1) = ((cur_point - c).normalize(), (c - p).normalize());
                    let (norm0, norm1) = (euclid::Vector2D::new(-dv0.y, dv0.x), euclid::Vector2D::new(-dv1.y, dv1.x));
                    let (p0_p, p0_n) = (cur_point + norm0 * self.width * 0.5, cur_point - norm0 * self.width * 0.5);
                    let (p1_p, p1_n) = (p + norm1 * self.width * 0.5, p - norm1 * self.width * 0.5);

                    let ctrl_p = p0_p + dv0 * crosspoint_value(p0_p, p1_p, dv0, -dv1)
                        .expect("Unable to get crosspoint");
                    let ctrl_n = p0_n + dv0 * crosspoint_value(p0_n, p1_n, dv0, -dv1)
                        .expect("unable to get crosspoint");

                    if positive_events.is_empty() { positive_events.push(PathEvent::MoveTo(p0_p)); }
                    else {
                        if pathevent_to_point(positive_events.last().expect("No endpoints?")) != p0_p {
                            // curve connection - miter
                            let (last_ep_to, dv_pre);
                            // for shortening lifetime
                            match positive_events.last().expect("No endpoints?") {
                                &PathEvent::MoveTo(p) | &PathEvent::LineTo(p) => {
                                    let ref last_ep2 = positive_events[positive_events.len() - 2];
                                    dv_pre = (p - pathevent_to_point(last_ep2)).normalize();
                                    last_ep_to = p;
                                },
                                &PathEvent::QuadraticTo(c, p) | &PathEvent::CubicTo(_, c, p) => {
                                    dv_pre = (p - c).normalize();
                                    last_ep_to = p;
                                },
                                _ => unreachable!("unable to compute")
                            }
                            // memo: 計算できなかったら単純に直線でつなぐ(擬似bevel)
                            let new_last_ep_to = p0_p + dv0 * crosspoint_value(p0_p, last_ep_to, dv0, dv_pre)
                                .expect("Unable to compute the crosspoint");
                            // tweak last endpoint destination
                            *pathevent_to_point_mut(positive_events.last_mut().expect("No endpoints?"))
                                = new_last_ep_to;
                        }
                    }
                    positive_events.push(PathEvent::QuadraticTo(ctrl_p, p1_p));

                    if negative_events.is_empty() { negative_events.push(PathEvent::MoveTo(p0_n)); }
                    else {
                        if pathevent_to_point(negative_events.last().expect("No endpoints?")) != p0_n {
                            // connect - miter
                            let (last_ep_to, dv_pre);
                            // for shortening lifetime
                            match negative_events.last().expect("No endpoints?") {
                                &PathEvent::MoveTo(p) | &PathEvent::LineTo(p) => {
                                    let ref last_ep2 = negative_events[negative_events.len() - 2];
                                    dv_pre = (p - pathevent_to_point(last_ep2)).normalize();
                                    last_ep_to = p;
                                },
                                &PathEvent::QuadraticTo(c, p) | &PathEvent::CubicTo(_, c, p) => {
                                    dv_pre = (p - c).normalize();
                                    last_ep_to = p;
                                },
                                _ => unreachable!("unable to compute")
                            }
                            // memo: 計算できなかったら単純に直線でつなぐ(擬似bevel)
                            let new_last_ep_to = p0_n + dv0 * crosspoint_value(p0_n, last_ep_to, dv0, dv_pre)
                                .expect("Unable to compute the crosspoint");
                            // tweak last endpoint destination
                            *pathevent_to_point_mut(negative_events.last_mut().expect("No endpoints?"))
                                = new_last_ep_to;
                        }
                    }
                    negative_events.push(PathEvent::QuadraticTo(ctrl_n, p1_n));

                    cur_point = p;
                },
                PathEvent::CubicTo(c1, c2, p) => {
                    let (dv0, dv1) = ((cur_point - c1).normalize(), (c2 - p).normalize());
                    let (norm0, norm1) = (euclid::Vector2D::new(dv0.y, -dv0.x), euclid::Vector2D::new(dv1.y, -dv1.x));
                    let (p0_p, p0_n) = (cur_point + norm0 * self.width * 0.5, cur_point - norm0 * self.width * 0.5);
                    let (p1_p, p1_n) = (p + norm1 * self.width * 0.5, p - norm1 * self.width * 0.5);

                    // NOTE: S字カーブの時は伸縮が逆転する必要がある 多分この説明ではわからないので図があると嬉しいかも
                    let is_sform = (cur_point - c1).dot(p - c2) < 0.0;

                    let c1_p = p0_p - dv0 * ((cur_point - c1).length() + self.width * 0.5);
                    let c1_n = p0_n - dv0 * ((cur_point - c1).length() - self.width * 0.5);
                    let cv2_long = dv1 * ((c2 - p).length() + self.width * 0.5);
                    let cv2_short = dv1 * ((c2 - p).length() - self.width * 0.5);
                    let (c2_p, c2_n);
                    if is_sform {
                        c2_p = p1_p + cv2_short; c2_n = p1_n + cv2_long;
                    }
                    else {
                        c2_p = p1_p + cv2_long; c2_n = p1_n + cv2_short;
                    }

                    if positive_events.is_empty() { positive_events.push(PathEvent::MoveTo(p0_p)); }
                    else {
                        if pathevent_to_point(positive_events.last().expect("No endpoints?")) != p0_p {
                            // curve connection - miter
                            let (last_ep_to, dv_pre);
                            // for shortening lifetime
                            match positive_events.last().expect("No endpoints?") {
                                &PathEvent::MoveTo(p) | &PathEvent::LineTo(p) => {
                                    let ref last_ep2 = positive_events[positive_events.len() - 2];
                                    dv_pre = (p - pathevent_to_point(last_ep2)).normalize();
                                    last_ep_to = p;
                                },
                                &PathEvent::QuadraticTo(c, p) | &PathEvent::CubicTo(_, c, p) => {
                                    dv_pre = (p - c).normalize();
                                    last_ep_to = p;
                                },
                                _ => unreachable!("unable to compute")
                            }
                            // memo: 計算できなかったら単純に直線でつなぐ(擬似bevel)
                            let new_last_ep_to = p0_p + dv0 * crosspoint_value(p0_p, last_ep_to, dv0, dv_pre)
                                .expect("Unable to compute the crosspoint");
                            // tweak last endpoint destination
                            *pathevent_to_point_mut(positive_events.last_mut().expect("No endpoints?"))
                                = new_last_ep_to;
                        }
                    }
                    positive_events.push(PathEvent::CubicTo(c1_p, c2_p, p1_p));

                    if negative_events.is_empty() { negative_events.push(PathEvent::MoveTo(p0_n)); }
                    else {
                        if pathevent_to_point(negative_events.last().expect("No endpoints?")) != p0_n {
                            // connect - miter
                            let (last_ep_to, dv_pre);
                            // for shortening lifetime
                            match negative_events.last().expect("No endpoints?") {
                                &PathEvent::MoveTo(p) | &PathEvent::LineTo(p) => {
                                    let ref last_ep2 = negative_events[negative_events.len() - 2];
                                    dv_pre = (p - pathevent_to_point(last_ep2)).normalize();
                                    last_ep_to = p;
                                },
                                &PathEvent::QuadraticTo(c, p) | &PathEvent::CubicTo(_, c, p) => {
                                    dv_pre = (p - c).normalize();
                                    last_ep_to = p;
                                },
                                _ => unreachable!("unable to compute")
                            }
                            // memo: 計算できなかったら単純に直線でつなぐ(擬似bevel)
                            let new_last_ep_to = p0_n + dv0 * crosspoint_value(p0_n, last_ep_to, dv0, dv_pre)
                                .expect("Unable to compute the crosspoint");
                            // tweak last endpoint destination
                            *pathevent_to_point_mut(negative_events.last_mut().expect("No endpoints?"))
                                = new_last_ep_to;
                        }
                    }
                    negative_events.push(PathEvent::CubicTo(c1_n, c2_n, p1_n));

                    cur_point = p;
                },
                PathEvent::MoveTo(p) => {
                    if !positive_events.is_empty() {
                        // close existing stroke.
                        for p in positive_events.drain(..) { target_builder.path_event(p); }
                        target_builder.line_to(pathevent_to_point(negative_events.last().expect("No events?")));
                        // append negative events, in reverse order
                        let mut iter = negative_events.drain(..).rev();
                        let mut prev_ctrl = iter.next().expect("No events?");
                        for e in iter {
                            let to = pathevent_to_point(&e);
                            match std::mem::replace(&mut prev_ctrl, e) {
                                PathEvent::LineTo(_) => target_builder.line_to(to),
                                PathEvent::QuadraticTo(c, _) => target_builder.quadratic_bezier_to(c, to),
                                PathEvent::CubicTo(c1, c2, _) => target_builder.cubic_bezier_to(c2, c1, to),
                                PathEvent::MoveTo(_) => { break; }
                                _ => unreachable!()
                            }
                        }
                        target_builder.close();
                    }
                    cur_point = p;
                },
                PathEvent::Close => {
                    if !positive_events.is_empty() {
                        // close existing stroke.

                        // exterior(positive traces)
                        for p in positive_events.drain(..) { target_builder.path_event(p); }
                        target_builder.close();

                        // interior(negative traces)
                        for p in negative_events.drain(..) { target_builder.path_event(p); }
                        target_builder.close();
                    }
                },
                e => unreachable!("unsupported event: {:?}", e)
            }
        }

        println!("Left Traces: {:?} {:?}", positive_events, negative_events);
        if !positive_events.is_empty() {
            // close as unclosed curve.
            for p in positive_events { println!("PositiveEventEmission! {:?}", p); target_builder.path_event(p); }
            target_builder.line_to(pathevent_to_point(negative_events.last().expect("No events?")));
            // append negative events, in reverse order
            let mut iter = negative_events.into_iter().rev();
            let mut prev_ctrl = iter.next().expect("No events?");
            for e in iter {
                println!("NegativeEventIncoming! {:?} -> {:?}", prev_ctrl, e);
                let to = pathevent_to_point(&e);
                match std::mem::replace(&mut prev_ctrl, e) {
                    PathEvent::LineTo(_) => target_builder.line_to(to),
                    PathEvent::QuadraticTo(c, _) => target_builder.quadratic_bezier_to(c, to),
                    PathEvent::CubicTo(c1, c2, _) => target_builder.cubic_bezier_to(c2, c1, to),
                    PathEvent::MoveTo(_) => { break; }
                    _ => unreachable!()
                }
            }
            target_builder.close();
        }
    }
}*/
impl<'c> FigureContext<'c> {
    pub fn end(mut self) -> &'c mut Context {
        self.partitioner.partition(self.fill_rule);
        self.partitioner.builder_mut().build_and_reset();
        let (st, ext) = tfconv_st_ext(self.ctx.current_transform);
        self.ctx
            .meshes
            .push((self.partitioner.into_mesh(), st, ext));

        return self.ctx;
    }
}
impl<'c> FlatPathBuilder for FigureContext<'c> {
    type PathType = <Builder as FlatPathBuilder>::PathType;

    fn move_to(&mut self, p: Point2D<f32>) {
        self.partitioner
            .builder_mut()
            .move_to(p * self.ctx.screen_scaling)
    }
    fn line_to(&mut self, to: Point2D<f32>) {
        self.partitioner
            .builder_mut()
            .line_to(to * self.ctx.screen_scaling)
    }
    fn close(&mut self) {
        self.partitioner.builder_mut().close()
    }
    fn build(self) -> Self::PathType {
        unimplemented!("cannot operate build for FigureContext")
    }
    fn build_and_reset(&mut self) -> Self::PathType {
        self.partitioner.builder_mut().build_and_reset()
    }
    fn current_position(&self) -> Point2D<f32> {
        self.partitioner.builder().current_position()
    }
}
impl<'c> PathBuilder for FigureContext<'c> {
    fn quadratic_bezier_to(&mut self, c: Point2D<f32>, to: Point2D<f32>) {
        self.partitioner
            .builder_mut()
            .quadratic_bezier_to(c * self.ctx.screen_scaling, to * self.ctx.screen_scaling)
    }
    fn cubic_bezier_to(&mut self, c1: Point2D<f32>, c2: Point2D<f32>, to: Point2D<f32>) {
        self.partitioner.builder_mut().cubic_bezier_to(
            c1 * self.ctx.screen_scaling,
            c2 * self.ctx.screen_scaling,
            to * self.ctx.screen_scaling,
        )
    }
    fn arc(
        &mut self,
        center: Point2D<f32>,
        rad: Vector2D<f32>,
        sweeping_angle: Angle<f32>,
        x_rot: Angle<f32>,
    ) {
        self.partitioner.builder_mut().arc(
            center * self.ctx.screen_scaling,
            rad * self.ctx.screen_scaling,
            sweeping_angle,
            x_rot,
        )
    }
}

use lyon_path::geom::euclid::Point2D;

pub mod renderer_pivot {
    pub const LEFT_TOP: [f32; 2] = [-1.0, -1.0];
    pub const MIDDLE_TOP: [f32; 2] = [0.0, -1.0];
    pub const RIGHT_TOP: [f32; 2] = [1.0, -1.0];
    pub const LEFT_MIDDLE: [f32; 2] = [-1.0, 0.0];
    pub const MIDDLE_MIDDLE: [f32; 2] = [0.0, 0.0];
    pub const RIGHT_MIDDLE: [f32; 2] = [1.0, 0.0];
    pub const LEFT_BOTTOM: [f32; 2] = [-1.0, 1.0];
    pub const MIDDLE_BOTTOM: [f32; 2] = [0.0, 1.0];
    pub const RIGHT_BOTTOM: [f32; 2] = [1.0, 1.0];
}
