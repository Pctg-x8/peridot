//! UI Rect Compositioning

use std::{
    collections::{BTreeSet, HashMap},
    sync::Arc,
};

use bedrock::{
    self as br, CommandBufferMut, DescriptorPoolMut, Device, Image, ImageChild, MemoryBound,
    QueueMut, RenderPass, ShaderModule, TypedVulkanStructure, VkHandle,
};
use peridot_math::{Matrix4, Matrix4F32, One, Vector3, Vector4};
#[cfg(windows)]
use windows::Win32::Graphics::{
    Direct2D::Common::{ID2D1SimplifiedGeometrySink, ID2D1SimplifiedGeometrySink_Impl},
    DirectWrite::{IDWritePixelSnapping_Impl, IDWriteTextRenderer, IDWriteTextRenderer_Impl},
};
#[cfg(windows)]
use windows_core::*;

use crate::{
    graphics::{
        BLEND_STATE_SINGLE_NONE, IA_STATE_TRILIST, MS_STATE_EMPTY,
        RASTER_STATE_DEFAULT_FILL_NOCULL, VI_STATE_EMPTY, VulkanDevice,
    },
    rendering::{
        MaskTextureAtlasManager,
        atlas::{AtlasRect, DynamicAtlasManager},
        text::{FontID, GlyphPlacementBox, PerWindowFontSet, TextLayout, TextRun},
    },
    utils::SafeF32,
};

pub const BLUR_SAMPLE_STEPS: usize = 4;

#[repr(C)]
pub struct CompositeInstanceData {
    /// scale_x(width), scale_y(height), translate_x(left), translate_y(top)
    pub pos_st: [f32; 4],
    pub uv_st: [f32; 4],
    pub position_modifier_matrix: Matrix4F32,
    /// left, top, right, bottom (pixels from edge)
    pub slice_borders: [f32; 4],
    // float param1: float4 packed
    pub tex_size_pixels: [f32; 2],
    pub composite_mode: f32,
    pub opacity: f32,
    // float param1 end
    pub color_tint: [f32; 4],
    /// start_sec, end_sec, to_value(fromはpos_stに設定されている値), reserved
    pub pos_x_animation_data: [f32; 4],
    /// x_p1x, x_p1y, x_p2x, x_p2y
    pub pos_x_curve_control_points: [f32; 4],
    /// start_sec, end_sec, to_value(fromはpos_stに設定されている値), reserved
    pub pos_y_animation_data: [f32; 4],
    /// y_p1x, y_p1y, y_p2x, y_p2y
    pub pos_y_curve_control_points: [f32; 4],
    /// start_sec, end_sec, to_value(fromはpos_stに設定されている値), reserved
    pub pos_width_animation_data: [f32; 4],
    /// w_p1x, w_p1y, w_p2x, w_p2y
    pub pos_width_curve_control_points: [f32; 4],
    /// start_sec, end_sec, to_value(fromはpos_stに設定されている値), reserved
    pub pos_height_animation_data: [f32; 4],
    /// h_p1x, h_p1y, h_p2x, h_p2y
    pub pos_height_curve_control_points: [f32; 4],
    /// lt, rt, lb, rb (in pixels)
    pub corner_radius_x: [f32; 4],
    /// lt, rt, lb, rb (in pixels)
    pub corner_radius_y: [f32; 4],
    pub border_color: [f32; 4],
    pub border_thickness: f32,
    pub softedge: f32,
    pub _padding: [f32; 2],
}

#[repr(C)]
struct CompositePushConstants {
    screen_x_pixels: f32,
    screen_y_pixels: f32,
    _padding: [f32; 2],
    rect_mask_left: f32,
    rect_mask_top: f32,
    rect_mask_right: f32,
    rect_mask_bottom: f32,
    rect_mask_left_softness: f32,
    rect_mask_top_softness: f32,
    rect_mask_right_softness: f32,
    rect_mask_bottom_softness: f32,
}

pub const COMPOSITE_PUSH_CONSTANT_RANGES: &'static [br::PushConstantRange] =
    &[br::PushConstantRange::for_type::<CompositePushConstants>(
        br::vk::VK_SHADER_STAGE_ALL_GRAPHICS,
        0,
    )];

#[repr(C)]
pub struct CompositeStreamingData {
    pub current_sec: f32,
}

#[derive(Debug, Clone)]
pub enum CompositeMode<Event> {
    DirectSourceOver,
    ColorTint(AnimatableColor<Event>),
    FillColor(AnimatableColor<Event>),
    ColorTintBackdropBlur(AnimatableColor<Event>, AnimatableFloat<Event>),
    FillColorBackdropBlur(AnimatableColor<Event>, AnimatableFloat<Event>),
}
impl<Event> CompositeMode<Event> {
    const fn shader_mode_value(&self) -> f32 {
        match self {
            Self::DirectSourceOver => 0.0,
            Self::ColorTint(_) => 1.0,
            Self::FillColor(_) => 2.0,
            Self::ColorTintBackdropBlur(_, _) => 3.0,
            Self::FillColorBackdropBlur(_, _) => 4.0,
        }
    }
}

const fn lerp(x: f32, a: f32, b: f32) -> f32 {
    a + (b - a) * x
}

const fn lerp4(x: f32, [a, c, e, g]: [f32; 4], [b, d, f, h]: [f32; 4]) -> [f32; 4] {
    [lerp(x, a, b), lerp(x, c, d), lerp(x, e, f), lerp(x, g, h)]
}

// TODO: このへんうまくまとめたいが......

pub enum FloatParameter<Event> {
    Value(f32),
    Animated {
        start_sec: f32,
        end_sec: f32,
        from_value: f32,
        to_value: f32,
        curve: AnimationCurve,
        event_on_complete: Option<Event>,
    },
}
impl<Event> FloatParameter<Event> {
    pub fn evaluate(&self, current_sec: f32) -> f32 {
        match self {
            &Self::Value(x) => x,
            &Self::Animated {
                from_value,
                to_value,
                start_sec,
                end_sec,
                ref curve,
                ..
            } => lerp(
                curve.interpolate((current_sec - start_sec) / (end_sec - start_sec)),
                from_value,
                to_value,
            ),
        }
    }
}

#[derive(Clone)]
pub enum AnimatableFloat<Event> {
    Value(f32),
    Expression(Arc<dyn Fn(&CompositeTreeParameterStoreRender<Event>) -> f32 + Sync + Send>),
    Animated {
        start_sec: f32,
        end_sec: f32,
        from_value: f32,
        to_value: f32,
        curve: AnimationCurve,
        event_on_complete: Option<Event>,
    },
}
impl<Event> core::fmt::Debug for AnimatableFloat<Event> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Value(x) => f.debug_tuple("AnimatableFloat::Value").field(x).finish(),
            Self::Expression(_) => f
                .debug_tuple("AnimatableFloat::Expression")
                .field(&"<fn>")
                .finish(),
            Self::Animated {
                start_sec,
                end_sec,
                from_value,
                to_value,
                curve,
                ..
            } => f
                .debug_struct("AnimatableFloat::Animated")
                .field("start_sec", start_sec)
                .field("end_sec", end_sec)
                .field("from_value", from_value)
                .field("to_value", to_value)
                .field("curve", curve)
                .field("event_on_complete", &"<event>")
                .finish(),
        }
    }
}
impl<Event> AnimatableFloat<Event> {
    pub fn evaluate(
        &self,
        current_sec: f32,
        parameter_store: &CompositeTreeParameterStoreRender<Event>,
    ) -> f32 {
        match self {
            &Self::Value(x) => x,
            &Self::Expression(ref x) => x(parameter_store),
            &Self::Animated {
                from_value,
                to_value,
                start_sec,
                end_sec,
                ref curve,
                ..
            } => lerp(
                curve.interpolate((current_sec - start_sec) / (end_sec - start_sec)),
                from_value,
                to_value,
            ),
        }
    }

    fn process_on_complete(&mut self, current_sec: f32, cb: impl FnOnce(Event)) {
        if let &mut Self::Animated {
            end_sec,
            ref mut event_on_complete,
            ..
        } = self
            && end_sec <= current_sec
        {
            if let Some(e) = event_on_complete.take() {
                cb(e);
            }
        }
    }
}

#[derive(Clone)]
pub enum AnimatableColor<Event> {
    Value([f32; 4]),
    Expression(Arc<dyn Fn(&CompositeTreeParameterStoreRender<Event>) -> [f32; 4] + Sync + Send>),
    Animated {
        start_sec: f32,
        end_sec: f32,
        from_value: [f32; 4],
        to_value: [f32; 4],
        curve: AnimationCurve,
        event_on_complete: Option<Event>,
    },
}
impl<Event> core::fmt::Debug for AnimatableColor<Event> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Value(x) => f.debug_tuple("AnimatableColor::Value").field(x).finish(),
            Self::Expression(_) => f
                .debug_tuple("AnimatableColor::Expression")
                .field(&"<fn>")
                .finish(),
            Self::Animated {
                start_sec,
                end_sec,
                from_value,
                to_value,
                curve,
                ..
            } => f
                .debug_struct("AnimatableColor::Animated")
                .field("start_sec", start_sec)
                .field("end_sec", end_sec)
                .field("from_value", from_value)
                .field("to_value", to_value)
                .field("curve", curve)
                .field("event_on_complete", &"<event>")
                .finish(),
        }
    }
}
impl<Event> AnimatableColor<Event> {
    pub fn evaluate(
        &self,
        current_sec: f32,
        parameter_store: &CompositeTreeParameterStoreRender<Event>,
    ) -> [f32; 4] {
        match self {
            &Self::Value(x) => x,
            &Self::Expression(ref f) => f(parameter_store),
            &Self::Animated {
                from_value,
                to_value,
                start_sec,
                end_sec,
                ref curve,
                ..
            } => lerp4(
                curve.interpolate((current_sec - start_sec) / (end_sec - start_sec)),
                from_value,
                to_value,
            ),
        }
    }

    fn process_on_complete(&mut self, current_sec: f32, cb: impl FnOnce(Event)) {
        if let &mut Self::Animated {
            end_sec,
            ref mut event_on_complete,
            ..
        } = self
            && end_sec <= current_sec
        {
            if let Some(e) = event_on_complete.take() {
                cb(e);
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum AnimationCurve {
    Linear,
    CubicBezier { p1: (f32, f32), p2: (f32, f32) },
}
impl AnimationCurve {
    #[inline]
    fn interpolate(&self, t: f32) -> f32 {
        match self {
            &AnimationCurve::Linear => t.clamp(0.0, 1.0),
            &AnimationCurve::CubicBezier { p1, p2 } => interpolate_cubic_bezier(t, p1, p2),
        }
    }
}

fn interpolate_cubic_bezier(t: f32, p1: (f32, f32), p2: (f32, f32)) -> f32 {
    // out of range
    if t <= 0.0 {
        return 0.0;
    }
    if t >= 1.0 {
        return 1.0;
    }

    // p01 = mix(vec2(0.0), p1, t) = p1 * t
    // p12 = mix(p1, p2, t) = p1 * (1.0 - t) + p2 * t
    // p23 = mix(p2, vec2(1.0), t) = p2 * (1.0 - t) + vec2(t)
    // p012 = mix(p01, p12, t) = p01 * (1.0 - t) + p12 * t = p1 * t * (1.0 - t) + (p1 * (1.0 -t ) + p2 * t) * t =
    // p1 * t * (1.0 - t) + p1 * t * (1.0 - t) + p2 * t * t = p1 * 2.0 * t * (1.0 - t) + p2 * t * t =
    // p1 * (2.0 * t - 2.0 * t * t) + p2 * t * t
    // p123 = mix(p12, p23, t) = p12 * (1.0 - t) + p23 * t = (p1 * (1.0 - t) + p2 * t) * (1.0 - t) + (p2 * (1.0 - t) + vec2(t)) * t =
    // p1 * (1.0 - t) * (1.0 - t) + p2 * t * (1.0 - t) + p2 * (1.0 - t) * t  + vec2(t * t) =
    // p1 * (1.0 - t) * (1.0 - t) + p2 * 2.0 * t * (1.0 - t) + vec2(t * t) =
    // p1 * (1.0 - t) * (1.0 - t) + p2 * (2.0 * t - 2.0 * t * t) + vec2(t * t)
    // p = mix(p012, p123, t) = p012 * (1.0 - t) + p123 * t =
    // (p1 * (2.0 * t - 2.0 * t * t) + p2 * t * t) * (1.0 - t) + (p1 * (1.0 - t) * (1.0 - t) + p2 * (2.0 * t - 2.0 * t * t) + vec2(t * t)) * t =
    // p1 * (2.0 * t - 2.0 * t * t) * (1.0 - t) + p2 * t * t * (1.0 - t) + p1 * t * (1.0 - t) * (1.0 - t) + p2 * t * (2.0 * t - 2.0 * t * t) + vec2(t * t * t) =
    // p1 * 2.0 * t * (1.0 - t) * (1.0 - t) + p2 * t * t * (1.0 - t) + p1 * t * (1.0 - t) * (1.0 - t) + p2 * 2.0 * t * t * (1.0 - t) + vec2(t * t * t) =
    // p1 * 3.0 * t * (1.0 - t) * (1.0 - t) + p2 * 3.0 * t * t * (1.0 - t) + vec2(t * t * t)
    //
    // (1.0 - t)^2 = 1.0^2 - 2.0 * t + t^2
    //
    // x = (p1.x * 3.0 * t * (1.0 - t) * (1.0 - t) + p2.x * 3.0 * t * t * (1.0 - t) + t * t * t), t = ?
    // x = p1.x * (3.0 * t - 6.0 * t^2 + 3.0 * t^3) + p2.x * (3.0 * t^2 - 3.0 * t^3) + t^3
    // x = (p1.x * 3.0 - p2.x * 3.0 + 1.0) * t^3 + (-p1.x * 6.0 + p2.x * 3.0) * t^2 + p1.x * 3.0 * t
    // 0 = (p1.x * 3.0 - p2.x * 3.0 + 1.0) * t^3 + (-p1.x * 6.0 + p2.x * 3.0) * t^2 + p1.x * 3.0 * t - x

    // x = (p1.x * 3.0 - p2.x * 3.0 + 1.0) * t^3 + (p2.x * 3.0 - p1.x * 6.0) * t^2 + p1.x * 3.0 * t
    // t = ?
    let a = p1.0 * 3.0 - p2.0 * 3.0 + 1.0;
    let b = p2.0 * 3.0 - p1.0 * 6.0;
    let c = p1.0 * 3.0;
    let d = -t;

    let t0 = if a == 0.0 {
        // solve quadratic: (p2.x * 3.0 - p1.x * 6.0) * t^2 + p1.x * 3.0 * t - x = 0
        let dq = c * c - 4.0 * b * d;

        if dq < 0.0 {
            // no value
            return 0.0;
        } else if dq == 0.0 {
            // exactly one
            -c / (2.0 * b)
        } else {
            // select correct value
            let t1 = -c + dq.sqrt() / (2.0 * b);
            let t2 = -c - dq.sqrt() / (2.0 * b);

            if 0.0 <= t2 && t2 <= 1.0 {
                t2
            } else {
                t1.clamp(0.0, 1.0)
            }
        }
    } else {
        // solve cubic: https://peter-shepherd.com/personal_development/mathematics/polynomials/cubicAlgebra.htm
        let a1 = b / a;
        let b1 = c / a;
        let c1 = d / a;
        let p = (3.0 * b1 - a1 * a1) / 3.0;
        let q = (2.0 * a1 * a1 * a1 - 9.0 * a1 * b1 + 27.0 * c1) / 27.0;

        if p == 0.0 {
            if q == 0.0 {
                0.0
            } else {
                let t1 = (-q).cbrt() - a1 / 3.0;
                let t2 = (-q).cbrt() * (-0.5 * 3.0f32.sqrt() / 2.0) - a1 / 3.0;
                let t3 = (-q).cbrt() * (-0.5 - 3.0f32.sqrt() / 2.0) - a1 / 3.0;

                if 0.0 <= t3 && t3 <= 1.0 {
                    t3
                } else if 0.0 <= t2 && t2 <= 1.0 {
                    t2
                } else {
                    t1.clamp(0.0, 1.0)
                }
            }
        } else {
            if q == 0.0 {
                let t1 = -a1 / 3.0;
                let t2 = (-p).sqrt() - a1 / 3.0;
                let t3 = -(-p).sqrt() - a1 / 3.0;

                if 0.0 <= t3 && t3 <= 1.0 {
                    t3
                } else if 0.0 <= t2 && t2 <= 1.0 {
                    t2
                } else {
                    t1.clamp(0.0, 1.0)
                }
            } else {
                let dc = (q * q) / 4.0 + (p * p * p) / 27.0;

                if dc == 0.0 {
                    // two reals
                    let t1 = 2.0 * (-q / 2.0).cbrt() - a1 / 3.0;
                    let t2 = (q / 2.0).cbrt() - a1 / 3.0;

                    if 0.0 <= t2 && t2 <= 1.0 {
                        t2
                    } else {
                        t1.clamp(0.0, 1.0)
                    }
                } else if dc > 0.0 {
                    // one real and two img
                    let u1 = (-(q / 2.0) + dc.sqrt()).cbrt();
                    let v1 = (q / 2.0 + dc.sqrt()).cbrt();

                    let t1 = u1 - v1 - a1 / 3.0;
                    let t2 = -0.5 * (u1 - v1) + (u1 + v1) * 3.0f32.sqrt() / 2.0 - a1 / 3.0;
                    let t3 = -0.5 * (u1 - v1) - (u1 + v1) * 3.0f32.sqrt() / 2.0 - a1 / 3.0;

                    if 0.0 <= t3 && t3 <= 1.0 {
                        t3
                    } else if 0.0 <= t2 && t2 <= 1.0 {
                        t2
                    } else {
                        t1.clamp(0.0, 1.0)
                    }
                } else {
                    // irreducible case
                    let r = (-p / 3.0).powi(3).sqrt();
                    let phi = (-q / (2.0 * r)).acos();

                    let t1 = 2.0 * r.cbrt() * (phi / 3.0).cos() - a1 / 3.0;
                    let t2 =
                        2.0 * r.cbrt() * ((phi + core::f32::consts::TAU) / 3.0).cos() - a1 / 3.0;
                    let t3 = 3.0 * r.cbrt() * ((phi + core::f32::consts::TAU * 2.0) / 3.0).cos()
                        - a1 / 3.0;

                    if 0.0 <= t3 && t3 <= 1.0 {
                        t3
                    } else if 0.0 <= t2 && t2 <= 1.0 {
                        t2
                    } else {
                        t1.clamp(0.0, 1.0)
                    }
                }
            }
        }
    };

    // y = (p1.y * 3.0 - p2.y * 3.0 + 1.0) * t^3 + (p2.y * 3.0 - p1.y * 6.0) * t^2 + p1.y * 3.0 * t
    (p1.1 * 3.0 - p2.1 * 3.0 + 1.0) * t0.powi(3)
        + (p2.1 * 3.0 - p1.1 * 6.0) * t0.powi(2)
        + p1.1 * 3.0 * t0
}

#[derive(Debug, Clone, Copy)]
pub struct ClipConfig {
    pub left_softness: SafeF32,
    pub top_softness: SafeF32,
    pub right_softness: SafeF32,
    pub bottom_softness: SafeF32,
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CustomRenderToken(usize);

#[derive(Debug, Clone, Copy, Default)]
pub enum CompositeRectTextHorizontalAlignment {
    #[default]
    Start,
    Middle,
    End,
}

#[derive(Debug, Clone, Copy, Default)]
pub enum CompositeRectTextVerticalAlignment {
    #[default]
    Start,
    Middle,
    End,
}

#[derive(Debug, Clone)]
pub struct CompositeRectTextRun<Event> {
    pub font_id: FontID,
    pub content: String,
    pub color: AnimatableColor<Event>,
    pub spacing_inline_start: f32,
}
impl<Event> Default for CompositeRectTextRun<Event> {
    fn default() -> Self {
        Self {
            font_id: Default::default(),
            content: Default::default(),
            color: AnimatableColor::Value([0.0, 0.0, 0.0, 1.0]),
            spacing_inline_start: 0.0,
        }
    }
}

#[derive(Debug, Clone)]
pub struct CompositeRectText<Event> {
    pub runs: Vec<CompositeRectTextRun<Event>>,
    pub horizontal_alignment: CompositeRectTextHorizontalAlignment,
    pub vertical_alignment: CompositeRectTextVerticalAlignment,
    pub offset: [f32; 2],
}
impl<Event> Default for CompositeRectText<Event> {
    fn default() -> Self {
        Self {
            runs: Vec::new(),
            horizontal_alignment: Default::default(),
            vertical_alignment: Default::default(),
            offset: [0.0, 0.0],
        }
    }
}

#[derive(Debug, Clone)]
pub struct CornerRadius {
    pub left_top: [f32; 2],
    pub right_top: [f32; 2],
    pub left_bottom: [f32; 2],
    pub right_bottom: [f32; 2],
}
impl Default for CornerRadius {
    #[inline(always)]
    fn default() -> Self {
        Self {
            left_top: [0.0, 0.0],
            right_top: [0.0, 0.0],
            left_bottom: [0.0, 0.0],
            right_bottom: [0.0, 0.0],
        }
    }
}
impl CornerRadius {
    pub const fn all(radius: f32) -> Self {
        Self {
            left_top: [radius, radius],
            right_top: [radius, radius],
            left_bottom: [radius, radius],
            right_bottom: [radius, radius],
        }
    }
}

#[derive(Debug, Clone)]
pub struct Border<Event> {
    pub thickness: f32,
    pub color: AnimatableColor<Event>,
}
impl<Event> Default for Border<Event> {
    fn default() -> Self {
        Self {
            thickness: 0.0,
            color: AnimatableColor::Value([0.0, 0.0, 0.0, 0.0]),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CompositeRect<Event> {
    pub has_bitmap: bool,
    pub base_scale_factor: f32,
    pub corner_radius: CornerRadius,
    pub border: Option<Border<Event>>,
    pub softedge: f32,
    pub offset: [AnimatableFloat<Event>; 2],
    pub size: [AnimatableFloat<Event>; 2],
    pub relative_offset_adjustment: [f32; 2],
    pub relative_size_adjustment: [f32; 2],
    pub clip_child: Option<ClipConfig>,
    pub texatlas_rect_id: Option<usize>,
    pub slice_borders: [f32; 4],
    pub composite_mode: CompositeMode<Event>,
    pub custom_render_token: Option<CustomRenderToken>,
    pub opacity: AnimatableFloat<Event>,
    pub pivot: [f32; 2],
    pub scale_x: AnimatableFloat<Event>,
    pub scale_y: AnimatableFloat<Event>,
    pub text: Option<CompositeRectText<Event>>,
    pub parent: Option<usize>,
    pub children: Vec<usize>,
}
impl<Event> Default for CompositeRect<Event> {
    fn default() -> Self {
        Self {
            has_bitmap: false,
            base_scale_factor: 1.0,
            corner_radius: CornerRadius::default(),
            border: None,
            softedge: 0.0,
            offset: [const { AnimatableFloat::Value(0.0) }; 2],
            size: [const { AnimatableFloat::Value(0.0) }; 2],
            relative_offset_adjustment: [0.0, 0.0],
            relative_size_adjustment: [0.0, 0.0],
            clip_child: None,
            texatlas_rect_id: None,
            slice_borders: [0.0, 0.0, 0.0, 0.0],
            composite_mode: CompositeMode::DirectSourceOver,
            custom_render_token: None,
            opacity: AnimatableFloat::Value(1.0),
            pivot: [0.5; 2],
            scale_x: AnimatableFloat::Value(1.0),
            scale_y: AnimatableFloat::Value(1.0),
            text: None,
            parent: None,
            children: Vec::new(),
        }
    }
}

struct CompositeInstanceManager {
    buffer: br::vk::VkBuffer,
    memory: br::vk::VkDeviceMemory,
    streaming_buffer: br::vk::VkBuffer,
    streaming_memory: br::vk::VkDeviceMemory,
    streaming_memory_requires_flush: bool,
    buffer_stg: br::vk::VkBuffer,
    memory_stg: br::vk::VkDeviceMemory,
    stg_mem_requires_flush: bool,
    capacity: usize,
    count: usize,
    free: BTreeSet<usize>,
}
impl CompositeInstanceManager {
    unsafe fn drop(&mut self, gfx: &VulkanDevice) {
        unsafe {
            br::vkfn_wrapper::destroy_buffer(gfx.native_ptr(), self.buffer_stg, None);
            br::vkfn_wrapper::free_memory(gfx.native_ptr(), self.memory_stg, None);

            br::vkfn_wrapper::destroy_buffer(gfx.native_ptr(), self.streaming_buffer, None);
            br::vkfn_wrapper::free_memory(gfx.native_ptr(), self.streaming_memory, None);

            br::vkfn_wrapper::destroy_buffer(gfx.native_ptr(), self.buffer, None);
            br::vkfn_wrapper::free_memory(gfx.native_ptr(), self.memory, None);
        }
    }

    const INIT_CAP: usize = 1024;

    fn new(gfx: &VulkanDevice) -> Self {
        let mut buffer = br::BufferObject::new(
            gfx,
            &br::BufferCreateInfo::new(
                core::mem::size_of::<CompositeInstanceData>() * Self::INIT_CAP,
                br::BufferUsage::STORAGE_BUFFER | br::BufferUsage::TRANSFER_DEST,
            ),
        )
        .expect("Failed to create composite instance buffer");
        let req = buffer.requirements();
        let Some(memory_index) = gfx.find_device_local_memory_index(req.memoryTypeBits) else {
            tracing::error!(memory_index_mask = req.memoryTypeBits, "no suitable memory");
            std::process::exit(1);
        };
        let memory =
            br::DeviceMemoryObject::new(gfx, &br::MemoryAllocateInfo::new(req.size, memory_index))
                .expect("Failed to allocate composite instance data memory");
        buffer
            .bind(&memory, 0)
            .expect("Failed to bind buffer memory");

        let mut streaming_buffer = br::BufferObject::new(
            gfx,
            &br::BufferCreateInfo::new_for_type::<CompositeStreamingData>(
                br::BufferUsage::UNIFORM_BUFFER,
            ),
        )
        .unwrap();
        let mreq = streaming_buffer.requirements();
        let Some(memory_index) = gfx.find_direct_memory_index(mreq.memoryTypeBits) else {
            tracing::error!(
                memory_index_mask = mreq.memoryTypeBits,
                "no suitable memory for streaming"
            );
            std::process::exit(1);
        };
        let streaming_memory =
            br::DeviceMemoryObject::new(gfx, &br::MemoryAllocateInfo::new(mreq.size, memory_index))
                .unwrap();
        streaming_buffer
            .bind(&streaming_memory, 0)
            .expect("Failed to bind streaming buffer memory");
        let streaming_memory_requires_flush = !gfx.is_coherent_memory(memory_index);

        let mut buffer_stg = br::BufferObject::new(
            gfx,
            &br::BufferCreateInfo::new(
                core::mem::size_of::<CompositeInstanceData>() * Self::INIT_CAP,
                br::BufferUsage::TRANSFER_SRC,
            ),
        )
        .expect("Failed to create composite instance staging buffer");
        let buffer_mreq = buffer.requirements();
        let memory_index = gfx
            .find_host_visible_memory_index(buffer_mreq.memoryTypeBits)
            .expect("no suitable memory");
        let stg_mem_requires_flush = !gfx.is_coherent_memory(memory_index);
        let memory_stg = br::DeviceMemoryObject::new(
            gfx,
            &br::MemoryAllocateInfo::new(buffer_mreq.size, memory_index),
        )
        .expect("Failed to allocate composite instance data staging memory");
        buffer_stg
            .bind(&memory_stg, 0)
            .expect("Failed to bind staging buffer memory");

        let (buffer, _) = buffer.unmanage();
        let (memory, _) = memory.unmanage();
        let (streaming_buffer, _) = streaming_buffer.unmanage();
        let (streaming_memory, _) = streaming_memory.unmanage();
        let (buffer_stg, _) = buffer_stg.unmanage();
        let (memory_stg, _) = memory_stg.unmanage();

        Self {
            buffer,
            memory,
            streaming_buffer,
            streaming_memory,
            streaming_memory_requires_flush,
            buffer_stg,
            memory_stg,
            stg_mem_requires_flush,
            capacity: Self::INIT_CAP,
            count: 0,
            free: BTreeSet::new(),
        }
    }

    fn alloc(&mut self) -> usize {
        if let Some(x) = self.free.pop_first() {
            return x;
        }

        self.count += 1;
        if self.count >= self.capacity {
            todo!("instance buffer overflow!");
        }

        self.count - 1
    }

    fn sync_buffer<'cb>(&self, cr: br::CmdRecord<'cb>) -> br::CmdRecord<'cb> {
        cr.copy_buffer(
            &unsafe { br::VkHandleRef::dangling(self.buffer_stg) },
            &unsafe { br::VkHandleRef::dangling(self.buffer) },
            &[br::BufferCopy::mirror(
                0,
                (core::mem::size_of::<CompositeInstanceData>() * self.capacity) as _,
            )],
        )
    }

    const fn streaming_memory_requires_flush(&self) -> bool {
        self.streaming_memory_requires_flush
    }

    const fn count(&self) -> usize {
        self.count
    }

    const fn memory_stg_requires_explicit_flush(&self) -> bool {
        self.stg_mem_requires_flush
    }

    const fn range_all(&self) -> core::ops::Range<usize> {
        0..core::mem::size_of::<CompositeInstanceData>() * self.count
    }

    const fn buffer_transparent_ref<'x>(&'x self) -> &'x br::VkHandleRef<'x, br::vk::VkBuffer> {
        br::VkHandleRef::from_raw_ref(&self.buffer)
    }

    const fn streaming_buffer_transparent_ref<'x>(
        &'x self,
    ) -> &'x br::VkHandleRef<'x, br::vk::VkBuffer> {
        br::VkHandleRef::from_raw_ref(&self.streaming_buffer)
    }

    const fn staging_memory_raw_handle(&self) -> br::vk::VkDeviceMemory {
        self.memory_stg
    }

    unsafe fn map_staging<'s, 'g>(
        &'s mut self,
        gfx_device: &'g VulkanDevice,
    ) -> br::Result<CompositeInstanceMappedStagingMemory<'s, 'g>> {
        let ptr = unsafe {
            br::vkfn_wrapper::map_memory(
                gfx_device.native_ptr(),
                self.memory_stg,
                0,
                (core::mem::size_of::<CompositeInstanceData>() * self.capacity) as _,
                0,
            )?
        };

        Ok(CompositeInstanceMappedStagingMemory(ptr, self, gfx_device))
    }

    const fn streaming_memory_raw_handle(&self) -> br::vk::VkDeviceMemory {
        self.streaming_memory
    }

    unsafe fn map_streaming<'s, 'g>(
        &'s mut self,
        gfx_device: &'g VulkanDevice,
    ) -> br::Result<CompositeInstanceMappedStreamingMemory<'s, 'g>> {
        let ptr = unsafe {
            br::vkfn_wrapper::map_memory(
                gfx_device.native_ptr(),
                self.streaming_memory,
                0,
                core::mem::size_of::<CompositeStreamingData>() as _,
                0,
            )?
        };

        Ok(CompositeInstanceMappedStreamingMemory(
            ptr, self, gfx_device,
        ))
    }
}

pub struct CompositeInstanceMappedStagingMemory<'m, 'g>(
    *mut core::ffi::c_void,
    &'m mut CompositeInstanceManager,
    &'g VulkanDevice<'g>,
);
impl Drop for CompositeInstanceMappedStagingMemory<'_, '_> {
    fn drop(&mut self) {
        unsafe {
            br::vkfn_wrapper::unmap_memory(self.2.native_ptr(), self.1.memory_stg);
        }
    }
}
impl CompositeInstanceMappedStagingMemory<'_, '_> {
    pub const fn ptr(&self) -> *mut core::ffi::c_void {
        self.0
    }
}

pub struct CompositeInstanceMappedStreamingMemory<'m, 'g>(
    *mut core::ffi::c_void,
    &'m mut CompositeInstanceManager,
    &'g VulkanDevice<'g>,
);
impl Drop for CompositeInstanceMappedStreamingMemory<'_, '_> {
    fn drop(&mut self) {
        unsafe {
            br::vkfn_wrapper::unmap_memory(self.2.native_ptr(), self.1.streaming_memory);
        }
    }
}
impl CompositeInstanceMappedStreamingMemory<'_, '_> {
    pub const fn ptr(&self) -> *mut CompositeStreamingData {
        self.0.cast()
    }
}

#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct CompositeTreeRef(usize);
impl CompositeTreeRef {
    #[inline(always)]
    pub fn entity<'c, Event>(&self, mgr: &'c CompositeTree<Event>) -> &'c CompositeRect<Event> {
        mgr.get(*self)
    }

    #[inline(always)]
    pub fn entity_mut<'c, Event>(
        &self,
        mgr: &'c mut CompositeTree<Event>,
    ) -> &'c mut CompositeRect<Event> {
        mgr.get_mut(*self)
    }

    #[inline(always)]
    pub fn entity_mut_dirtified<'c, Event>(
        &self,
        mgr: &'c mut CompositeTree<Event>,
    ) -> &'c mut CompositeRect<Event> {
        mgr.mark_dirty(*self);
        mgr.get_mut(*self)
    }

    #[inline(always)]
    pub fn mark_dirty<Event>(&self, mgr: &mut CompositeTree<Event>) {
        mgr.mark_dirty(*self);
    }
}

#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct CompositeTreeFloatParameterRef(usize);

enum DirtyFloatParameter<Event> {
    Modified(FloatParameter<Event>),
    Deleted,
}

pub struct CompositeTreeParameterStoreRender<Event> {
    float_parameters: Vec<FloatParameter<Event>>,
    float_values: Vec<f32>,
}
impl<Event> CompositeTreeParameterStoreRender<Event> {
    pub fn evaluate_float(&self, r: CompositeTreeFloatParameterRef, current_sec: f32) -> f32 {
        self.float_parameters[r.0].evaluate(current_sec)
    }

    pub fn float_value(&self, r: CompositeTreeFloatParameterRef) -> f32 {
        self.float_values[r.0]
    }

    fn evaluate_all(&mut self, current_sec: f32) {
        for (v, p) in self
            .float_values
            .iter_mut()
            .zip(self.float_parameters.iter())
        {
            *v = p.evaluate(current_sec);
        }
    }
}

struct CompositeTreeParameterStoreSyncBuffer<Event> {
    dirty_float_parameters: Vec<(usize, DirtyFloatParameter<Event>)>,
    push_float_parameters: Vec<FloatParameter<Event>>,
}
impl<Event> CompositeTreeParameterStoreSyncBuffer<Event> {
    pub fn clean(&mut self, render: &mut CompositeTreeParameterStoreRender<Event>) {
        for x in self.push_float_parameters.drain(..) {
            render.float_parameters.push(x);
            render.float_values.push(0.0);
        }

        for (n, x) in self.dirty_float_parameters.drain(..) {
            match x {
                DirtyFloatParameter::Modified(x) => {
                    render.float_parameters[n] = x;
                    render.float_values[n] = 0.0;
                }
                DirtyFloatParameter::Deleted => {
                    // TODO: 削除が明示的に必要になったら書く
                }
            }
        }
    }
}

pub struct CompositeTreeParameterStore<Event> {
    dirty_float_parameters: HashMap<usize, DirtyFloatParameter<Event>>,
    push_float_parameters: Vec<FloatParameter<Event>>,
    unused_float_parameters: BTreeSet<usize>,
    float_parameter_store_size: usize,
}
impl<Event> CompositeTreeParameterStore<Event> {
    pub fn alloc_float(&mut self, init: FloatParameter<Event>) -> CompositeTreeFloatParameterRef {
        if let Some(x) = self.unused_float_parameters.pop_first() {
            self.dirty_float_parameters
                .insert(x, DirtyFloatParameter::Modified(init));
            return CompositeTreeFloatParameterRef(x);
        }

        let id = CompositeTreeFloatParameterRef(self.float_parameter_store_size);
        self.float_parameter_store_size += 1;
        self.push_float_parameters.push(init);
        id
    }

    pub fn free_float(&mut self, r: CompositeTreeFloatParameterRef) {
        self.unused_float_parameters.insert(r.0);
        self.dirty_float_parameters
            .insert(r.0, DirtyFloatParameter::Deleted);
    }

    pub fn set_float(&mut self, r: CompositeTreeFloatParameterRef, a: FloatParameter<Event>) {
        self.dirty_float_parameters
            .insert(r.0, DirtyFloatParameter::Modified(a));
    }

    fn commit(&mut self, sync: &mut CompositeTreeParameterStoreSyncBuffer<Event>) {
        sync.push_float_parameters
            .extend(self.push_float_parameters.drain(..));
        sync.dirty_float_parameters
            .extend(self.dirty_float_parameters.drain());
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RenderPassAfterOperation {
    None,
    Grab,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RenderPassRequirements {
    pub after_operation: RenderPassAfterOperation,
    pub continued: bool,
}

#[derive(Debug, PartialEq, Eq)]
pub enum CompositeRenderingInstruction {
    DrawInstanceRange {
        index_range: core::ops::Range<usize>,
        backdrop_buffer: usize,
    },
    InsertCustomRenderCommands(CustomRenderToken),
    SetClip {
        shader_parameters: [SafeF32; 8],
    },
    ClearClip,
    GrabBackdrop,
    GenerateBackdropBlur {
        stdev: SafeF32,
        dest_backdrop_buffer: usize,
        rects: Vec<br::Rect2D>,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub struct CompositeRenderingData {
    pub instructions: Vec<CompositeRenderingInstruction>,
    pub render_passes: Vec<RenderPassRequirements>,
    pub required_backdrop_buffer_count: usize,
}
impl CompositeRenderingData {
    pub const EMPTY: Self = Self {
        instructions: Vec::new(),
        render_passes: Vec::new(),
        required_backdrop_buffer_count: 0,
    };
}

const fn rect_overlaps(a: &br::Rect2D, b: &br::Rect2D) -> bool {
    b.offset.x - (a.extent.width as i32) < a.offset.x
        && a.offset.x < b.offset.x + (b.extent.width as i32)
        && b.offset.y - (a.extent.height as i32) < a.offset.y
        && a.offset.y < b.offset.y + (b.extent.height as i32)
}

struct CompositeRenderingInstructionBuilder {
    insts: Vec<CompositeRenderingInstruction>,
    render_passes: Vec<RenderPassRequirements>,
    last_free_backdrop_buffer: usize,
    active_backdrop_blur_index_for_stdev: HashMap<SafeF32, usize>,
    current_backdrop_overlap_rects: Vec<br::Rect2D>,
    backdrop_active: bool,
    max_backdrop_buffer_count: usize,
    screen_rect: br::Rect2D,
    active_clip_parameters: Option<[SafeF32; 8]>,
    clip_invalidated: bool,
}
impl CompositeRenderingInstructionBuilder {
    fn new(screen_size: br::Extent2D) -> Self {
        Self {
            insts: vec![CompositeRenderingInstruction::ClearClip],
            render_passes: Vec::new(),
            last_free_backdrop_buffer: 0,
            active_backdrop_blur_index_for_stdev: HashMap::new(),
            current_backdrop_overlap_rects: Vec::new(),
            backdrop_active: false,
            max_backdrop_buffer_count: 0,
            screen_rect: screen_size.into_rect(br::Offset2D::ZERO),
            active_clip_parameters: None,
            clip_invalidated: true,
        }
    }

    fn build(mut self) -> CompositeRenderingData {
        // process for last backdrop layer
        self.max_backdrop_buffer_count = self
            .max_backdrop_buffer_count
            .max(self.last_free_backdrop_buffer);
        let rpr = RenderPassRequirements {
            after_operation: RenderPassAfterOperation::None,
            continued: !self.render_passes.is_empty(),
        };
        self.render_passes.push(rpr);

        CompositeRenderingData {
            instructions: self.insts,
            render_passes: self.render_passes,
            required_backdrop_buffer_count: self.max_backdrop_buffer_count,
        }
    }

    fn draw_instance(&mut self, index: usize, backdrop_buffer_index: usize) {
        if let Some(&mut CompositeRenderingInstruction::DrawInstanceRange {
            ref mut index_range,
            backdrop_buffer,
        }) = self.insts.last_mut()
        {
            if index_range.end == index && backdrop_buffer == backdrop_buffer_index {
                // optimal pass: fuse
                index_range.end += 1;
                return;
            }
        }

        self.insts
            .push(CompositeRenderingInstruction::DrawInstanceRange {
                index_range: index..index + 1,
                backdrop_buffer: backdrop_buffer_index,
            });
    }

    fn insert_custom_render_commands(&mut self, token: CustomRenderToken) {
        // no dependency check
        self.insts
            .push(CompositeRenderingInstruction::InsertCustomRenderCommands(
                token,
            ));
    }

    fn set_clip(&mut self, rect: &[SafeF32; 4], config: &ClipConfig) {
        let clip_parameters = [
            rect[0],
            rect[1],
            rect[2],
            rect[3],
            config.left_softness,
            config.top_softness,
            config.right_softness,
            config.bottom_softness,
        ];
        if !self.clip_invalidated
            && self
                .active_clip_parameters
                .as_ref()
                .is_some_and(|x| x == &clip_parameters)
        {
            // same clip already active
            return;
        }

        // needs to change clip state...
        match self.insts.last_mut() {
            Some(x @ &mut CompositeRenderingInstruction::ClearClip) => {
                // replace clearclip
                *x = CompositeRenderingInstruction::SetClip {
                    shader_parameters: clip_parameters,
                };
            }
            Some(&mut CompositeRenderingInstruction::SetClip {
                ref shader_parameters,
            }) if &clip_parameters == shader_parameters => {
                // same clip, nop
            }
            Some(&mut CompositeRenderingInstruction::SetClip {
                ref mut shader_parameters,
            }) => {
                // overtake contiguous setclip
                *shader_parameters = clip_parameters;
            }
            _ => {
                // insert new setclip instruction
                self.insts.push(CompositeRenderingInstruction::SetClip {
                    shader_parameters: clip_parameters,
                });
            }
        }

        self.clip_invalidated = false;
        self.active_clip_parameters = Some(clip_parameters);
    }

    fn clear_clip(&mut self) {
        if self.clip_invalidated && self.active_clip_parameters.is_none() {
            // nothing clip activated
            return;
        }

        match self.insts.last_mut() {
            Some(&mut CompositeRenderingInstruction::ClearClip) => {
                // fuse, do nothing
            }
            Some(x @ &mut CompositeRenderingInstruction::SetClip { .. }) => {
                // clip set but no rendering occured, overtake
                *x = CompositeRenderingInstruction::ClearClip;
            }
            _ => {
                self.insts.push(CompositeRenderingInstruction::ClearClip);
            }
        }

        self.clip_invalidated = true;
        self.active_clip_parameters = None;
    }

    /// return: backdrop buffer index
    fn request_backdrop_blur(&mut self, stdev: SafeF32, rect: br::Rect2D) -> usize {
        if !rect_overlaps(&rect, &self.screen_rect) {
            // perfectly culled
            return 0;
        }

        if !self.backdrop_active {
            // first time layer
            self.backdrop_active = true;
            self.insts.extend([
                CompositeRenderingInstruction::GrabBackdrop,
                CompositeRenderingInstruction::GenerateBackdropBlur {
                    stdev,
                    dest_backdrop_buffer: 0,
                    rects: vec![rect],
                },
            ]);
            let rpr = RenderPassRequirements {
                after_operation: RenderPassAfterOperation::Grab,
                continued: !self.render_passes.is_empty(),
            };
            self.render_passes.push(rpr);
            self.clip_invalidated = true;
            self.max_backdrop_buffer_count = self
                .max_backdrop_buffer_count
                .max(self.last_free_backdrop_buffer);
            self.last_free_backdrop_buffer = 1;
            self.current_backdrop_overlap_rects.clear();
            self.active_backdrop_blur_index_for_stdev.clear();
            self.current_backdrop_overlap_rects.push(rect);
            self.active_backdrop_blur_index_for_stdev
                .insert(stdev, self.insts.len() - 1);

            return 0;
        }

        let overlaps = self
            .current_backdrop_overlap_rects
            .iter()
            .any(|x| rect_overlaps(&rect, x));

        if overlaps {
            // non-optimal pass: split to new layer
            self.insts.extend([
                CompositeRenderingInstruction::GrabBackdrop,
                CompositeRenderingInstruction::GenerateBackdropBlur {
                    stdev,
                    dest_backdrop_buffer: 0,
                    rects: vec![rect],
                },
            ]);
            let rpr = RenderPassRequirements {
                after_operation: RenderPassAfterOperation::Grab,
                continued: !self.render_passes.is_empty(),
            };
            self.render_passes.push(rpr);
            self.clip_invalidated = true;
            self.max_backdrop_buffer_count = self
                .max_backdrop_buffer_count
                .max(self.last_free_backdrop_buffer);
            self.last_free_backdrop_buffer = 1;
            self.current_backdrop_overlap_rects.clear();
            self.active_backdrop_blur_index_for_stdev.clear();
            self.current_backdrop_overlap_rects.push(rect);
            self.active_backdrop_blur_index_for_stdev
                .insert(stdev, self.insts.len() - 1);

            return 0;
        }

        // optimal pass: no overlapping layer: fuse or generate
        self.current_backdrop_overlap_rects.push(rect);

        if let Some(&ix) = self.active_backdrop_blur_index_for_stdev.get(&stdev) {
            // fuse
            let &mut CompositeRenderingInstruction::GenerateBackdropBlur {
                ref mut rects,
                dest_backdrop_buffer,
                ..
            } = &mut self.insts[ix]
            else {
                unreachable!();
            };

            rects.push(rect);
            dest_backdrop_buffer
        } else {
            // generate
            self.insts
                .push(CompositeRenderingInstruction::GenerateBackdropBlur {
                    rects: vec![rect],
                    dest_backdrop_buffer: self.last_free_backdrop_buffer,
                    stdev,
                });
            self.last_free_backdrop_buffer += 1;
            self.current_backdrop_overlap_rects.push(rect);
            self.active_backdrop_blur_index_for_stdev
                .insert(stdev, self.insts.len() - 1);

            self.last_free_backdrop_buffer - 1
        }
    }
}

struct CompositeRectCache {
    text_rects: Vec<GlyphPlacementBox>,
    text_width: f32,
    text_height: f32,
}
impl CompositeRectCache {
    fn new() -> Self {
        Self {
            text_rects: Vec::new(),
            text_width: 0.0,
            text_height: 0.0,
        }
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

enum DirtyRect {
    Modified,
    Deleted,
}

enum DirtyRectSync<Event> {
    Modified(CompositeRect<Event>, DirtyFlagSet),
    Deleted,
}

pub struct CompositeTreeRender<Event> {
    rects: Vec<CompositeRect<Event>>,
    dirty_flags: Vec<DirtyFlagSet>,
    caches: Vec<CompositeRectCache>,
    parameter_store: CompositeTreeParameterStoreRender<Event>,
}
impl<Event> CompositeTreeRender<Event> {
    pub fn new() -> Self {
        Self {
            rects: Vec::new(),
            dirty_flags: Vec::new(),
            caches: Vec::new(),
            parameter_store: CompositeTreeParameterStoreRender {
                float_parameters: Vec::new(),
                float_values: Vec::new(),
            },
        }
    }

    pub fn update_shared(&mut self, current_sec: f32) {
        self.parameter_store.evaluate_all(current_sec);
    }

    unsafe fn update(
        &mut self,
        root: CompositeTreeRef,
        inst_builder: &mut CompositeRenderingInstructionBuilder,
        size: br::Extent2D,
        current_sec: f32,
        mapped_head: *mut core::ffi::c_void,
        font_set: &PerWindowFontSet,
        mask_atlas: &mut MaskTextureAtlasManager,
        mask_atlas_rects: &[AtlasRect],
        vector_raster_state: &mut VectorRasterizationState,
        mut on_event: impl FnMut(Event),
    ) {
        // let update_timer = std::time::Instant::now();

        let mut instance_slot_index = 0;
        let mut processes = vec![(
            root.0,
            (
                0.0,
                0.0,
                size.width as f32,
                size.height as f32,
                1.0,
                Matrix4::ONE,
                None::<([SafeF32; 4], ClipConfig)>,
            ),
        )];
        while let Some((
            rn,
            (
                effective_base_left,
                effective_base_top,
                effective_width,
                effective_height,
                parent_opacity,
                parent_matrix,
                active_clip,
            ),
        )) = processes.pop()
        {
            let cache = &mut self.caches[rn];
            let r = &mut self.rects[rn];
            self.dirty_flags[rn].dirty = false;
            let local_left =
                r.offset[0].evaluate(current_sec, &self.parameter_store) * r.base_scale_factor;
            let local_top =
                r.offset[1].evaluate(current_sec, &self.parameter_store) * r.base_scale_factor;
            let local_width =
                r.size[0].evaluate(current_sec, &self.parameter_store) * r.base_scale_factor;
            let local_height =
                r.size[1].evaluate(current_sec, &self.parameter_store) * r.base_scale_factor;

            let left = effective_base_left
                + (effective_width * r.relative_offset_adjustment[0])
                + local_left;
            let top = effective_base_top
                + (effective_height * r.relative_offset_adjustment[1])
                + local_top;
            let w = effective_width * r.relative_size_adjustment[0] + local_width;
            let h = effective_height * r.relative_size_adjustment[1] + local_height;

            let opacity = parent_opacity * r.opacity.evaluate(current_sec, &self.parameter_store);
            let matrix = parent_matrix
                * (Matrix4::translation(Vector3(
                    left - effective_base_left + r.pivot[0] * w,
                    top - effective_base_top + r.pivot[1] * h,
                    0.0,
                )) * Matrix4::scale(Vector4(
                    r.scale_x.evaluate(current_sec, &self.parameter_store),
                    r.scale_y.evaluate(current_sec, &self.parameter_store),
                    1.0,
                    1.0,
                )) * Matrix4::translation(Vector3(-r.pivot[0] * w, -r.pivot[1] * h, 0.0)));

            let border_color = match r.border {
                Some(ref b) => b.color.evaluate(current_sec, &self.parameter_store),
                None => [0.0; 4],
            };

            r.offset[0].process_on_complete(current_sec, &mut on_event);
            r.offset[1].process_on_complete(current_sec, &mut on_event);
            r.size[0].process_on_complete(current_sec, &mut on_event);
            r.size[1].process_on_complete(current_sec, &mut on_event);
            r.opacity.process_on_complete(current_sec, &mut on_event);
            r.scale_x.process_on_complete(current_sec, &mut on_event);
            r.scale_y.process_on_complete(current_sec, &mut on_event);
            match r.composite_mode {
                CompositeMode::DirectSourceOver => (),
                CompositeMode::ColorTint(ref mut t) => {
                    t.process_on_complete(current_sec, &mut on_event)
                }
                CompositeMode::FillColor(ref mut t) => {
                    t.process_on_complete(current_sec, &mut on_event)
                }
                CompositeMode::ColorTintBackdropBlur(ref mut t, ref mut stdev) => {
                    t.process_on_complete(current_sec, &mut on_event);
                    stdev.process_on_complete(current_sec, &mut on_event);
                }
                CompositeMode::FillColorBackdropBlur(ref mut t, ref mut stdev) => {
                    t.process_on_complete(current_sec, &mut on_event);
                    stdev.process_on_complete(current_sec, &mut on_event);
                }
            }
            if let Some(ref mut b) = r.border {
                b.color.process_on_complete(current_sec, &mut on_event);
            }

            if let Some((clip_rect_px, clip_config)) = active_clip {
                inst_builder.set_clip(&clip_rect_px, &clip_config);
            } else {
                inst_builder.clear_clip();
            }

            let texatlas_rect = r.texatlas_rect_id.map_or(
                &const {
                    AtlasRect {
                        left: 0,
                        top: 0,
                        right: 0,
                        bottom: 0,
                    }
                },
                |n| &mask_atlas_rects[n],
            );

            if let Some(t) = r.custom_render_token {
                // Custom Renderがある場合はそっちのみ
                inst_builder.insert_custom_render_commands(t);
            } else if r.has_bitmap {
                unsafe {
                    core::ptr::write(
                        mapped_head
                            .cast::<CompositeInstanceData>()
                            .add(instance_slot_index),
                        CompositeInstanceData {
                            pos_st: [w, h, 0.0, 0.0],
                            uv_st: [
                                ((texatlas_rect.right as f32 - texatlas_rect.left as f32) - 1.0)
                                    / mask_atlas.atlas().size().width as f32,
                                ((texatlas_rect.bottom as f32 - texatlas_rect.top as f32) - 1.0)
                                    / mask_atlas.atlas().size().height as f32,
                                (texatlas_rect.left as f32 + 0.5)
                                    / mask_atlas.atlas().size().width as f32,
                                (texatlas_rect.top as f32 + 0.5)
                                    / mask_atlas.atlas().size().height as f32,
                            ],
                            position_modifier_matrix: matrix.clone().transpose(),
                            slice_borders: r.slice_borders,
                            tex_size_pixels: [
                                mask_atlas.atlas().size().width as _,
                                mask_atlas.atlas().size().height as _,
                            ],
                            composite_mode: r.composite_mode.shader_mode_value(),
                            opacity,
                            color_tint: match r.composite_mode {
                                CompositeMode::DirectSourceOver => [0.0; 4],
                                CompositeMode::ColorTint(ref t) => {
                                    t.evaluate(current_sec, &self.parameter_store)
                                }
                                CompositeMode::FillColor(ref t) => {
                                    t.evaluate(current_sec, &self.parameter_store)
                                }
                                CompositeMode::ColorTintBackdropBlur(ref t, _) => {
                                    t.evaluate(current_sec, &self.parameter_store)
                                }
                                CompositeMode::FillColorBackdropBlur(ref t, _) => {
                                    t.evaluate(current_sec, &self.parameter_store)
                                }
                            },
                            pos_x_animation_data: [0.0; 4],
                            pos_x_curve_control_points: [0.0; 4],
                            pos_y_animation_data: [0.0; 4],
                            pos_y_curve_control_points: [0.0; 4],
                            pos_width_animation_data: [0.0; 4],
                            pos_width_curve_control_points: [0.0; 4],
                            pos_height_animation_data: [0.0; 4],
                            pos_height_curve_control_points: [0.0; 4],
                            corner_radius_x: [
                                r.corner_radius.left_top[0] * r.base_scale_factor,
                                r.corner_radius.right_top[0] * r.base_scale_factor,
                                r.corner_radius.left_bottom[0] * r.base_scale_factor,
                                r.corner_radius.right_bottom[0] * r.base_scale_factor,
                            ],
                            corner_radius_y: [
                                r.corner_radius.left_top[1] * r.base_scale_factor,
                                r.corner_radius.right_top[1] * r.base_scale_factor,
                                r.corner_radius.left_bottom[1] * r.base_scale_factor,
                                r.corner_radius.right_bottom[1] * r.base_scale_factor,
                            ],
                            border_color,
                            border_thickness: r
                                .border
                                .as_ref()
                                .map_or(0.0, |b| b.thickness * r.base_scale_factor),
                            softedge: r.softedge * r.base_scale_factor,
                            _padding: [0.0; 2],
                        },
                    );
                }

                let backdrop_buffer_index = match r.composite_mode {
                    CompositeMode::ColorTintBackdropBlur(_, ref stdev)
                    | CompositeMode::FillColorBackdropBlur(_, ref stdev) => {
                        let stdev = stdev.evaluate(current_sec, &self.parameter_store);

                        if stdev > 0.0 {
                            inst_builder.request_backdrop_blur(
                                unsafe { SafeF32::new_unchecked(stdev) },
                                br::Rect2D {
                                    offset: br::Offset2D {
                                        x: left as _,
                                        y: top as _,
                                    },
                                    extent: br::Extent2D {
                                        width: w as _,
                                        height: h as _,
                                    },
                                },
                            )
                        } else {
                            0
                        }
                    }
                    // とりあえず0
                    _ => 0,
                };

                inst_builder.draw_instance(instance_slot_index, backdrop_buffer_index);
                instance_slot_index += 1;
            }

            if let Some(ref mut t) = r.text {
                if self.dirty_flags[rn].text_layout_dirty {
                    Self::populate_text_layout_cache(
                        cache,
                        t,
                        r.base_scale_factor,
                        font_set,
                        mask_atlas,
                        vector_raster_state,
                    );
                    self.dirty_flags[rn].text_layout_dirty = false;
                }

                let x_offset = match t.horizontal_alignment {
                    CompositeRectTextHorizontalAlignment::Start => 0.0,
                    CompositeRectTextHorizontalAlignment::End => w - cache.text_width,
                    CompositeRectTextHorizontalAlignment::Middle => (w - cache.text_width) * 0.5,
                } + t.offset[0] * r.base_scale_factor;
                let y_offset = match t.vertical_alignment {
                    CompositeRectTextVerticalAlignment::Start => 0.0,
                    CompositeRectTextVerticalAlignment::End => h - cache.text_height,
                    CompositeRectTextVerticalAlignment::Middle => (h - cache.text_height) * 0.5,
                } + t.offset[1] * r.base_scale_factor;
                for b in cache.text_rects.iter() {
                    unsafe {
                        core::ptr::write(
                            mapped_head
                                .cast::<CompositeInstanceData>()
                                .add(instance_slot_index),
                            CompositeInstanceData {
                                pos_st: [
                                    b.width as f32,
                                    b.height as f32,
                                    b.left + x_offset,
                                    b.top + y_offset,
                                ],
                                uv_st: [
                                    b.width as f32 / mask_atlas.atlas().size().width as f32,
                                    b.height as f32 / mask_atlas.atlas().size().height as f32,
                                    b.tex_left as f32 / mask_atlas.atlas().size().width as f32,
                                    b.tex_top as f32 / mask_atlas.atlas().size().height as f32,
                                ],
                                position_modifier_matrix: matrix.clone().transpose(),
                                slice_borders: [0.0; 4],
                                tex_size_pixels: [
                                    mask_atlas.atlas().size().width as _,
                                    mask_atlas.atlas().size().height as _,
                                ],
                                composite_mode: 1.0,
                                opacity,
                                color_tint: t.runs[0]
                                    .color
                                    .evaluate(current_sec, &self.parameter_store),
                                pos_x_animation_data: [0.0; 4],
                                pos_x_curve_control_points: [0.0; 4],
                                pos_y_animation_data: [0.0; 4],
                                pos_y_curve_control_points: [0.0; 4],
                                pos_width_animation_data: [0.0; 4],
                                pos_width_curve_control_points: [0.0; 4],
                                pos_height_animation_data: [0.0; 4],
                                pos_height_curve_control_points: [0.0; 4],
                                corner_radius_x: [0.0; 4],
                                corner_radius_y: [0.0; 4],
                                border_thickness: 0.0,
                                border_color: [0.0; 4],
                                softedge: 0.0,
                                _padding: [0.0; 2],
                            },
                        );
                    }

                    let backdrop_buffer_index = match r.composite_mode {
                        CompositeMode::ColorTintBackdropBlur(_, ref stdev)
                        | CompositeMode::FillColorBackdropBlur(_, ref stdev) => {
                            let stdev = stdev.evaluate(current_sec, &self.parameter_store);

                            if stdev > 0.0 {
                                inst_builder.request_backdrop_blur(
                                    unsafe { SafeF32::new_unchecked(stdev) },
                                    br::Rect2D {
                                        offset: br::Offset2D {
                                            x: left as _,
                                            y: top as _,
                                        },
                                        extent: br::Extent2D {
                                            width: w as _,
                                            height: h as _,
                                        },
                                    },
                                )
                            } else {
                                0
                            }
                        }
                        // とりあえず0
                        _ => 0,
                    };

                    inst_builder.draw_instance(instance_slot_index, backdrop_buffer_index);
                    instance_slot_index += 1;
                }
            }

            processes.extend(r.children.iter().rev().map(|&x| {
                (
                    x,
                    (
                        left,
                        top,
                        w,
                        h,
                        opacity,
                        matrix.clone(),
                        r.clip_child.map(|cc| {
                            (
                                [
                                    SafeF32::new(left).expect("invalid left"),
                                    SafeF32::new(top).expect("invalid top"),
                                    SafeF32::new(left + w).expect("invalid right"),
                                    SafeF32::new(top + h).expect("invalid bottom"),
                                ],
                                cc,
                            )
                        }),
                    ),
                )
            }));
        }

        // let update_time = update_timer.elapsed();
        // println!("instbuild({update_time:?}): {:?}", inst_builder.insts);
    }

    #[tracing::instrument(skip(text_layout, cache, font_set, glyph_atlas, vector_raster_state))]
    fn populate_text_layout_cache(
        cache: &mut CompositeRectCache,
        text_layout: &CompositeRectText<Event>,
        scale_factor: f32,
        font_set: &PerWindowFontSet,
        glyph_atlas: &mut MaskTextureAtlasManager,
        vector_raster_state: &mut VectorRasterizationState,
    ) {
        tracing::trace!("relayout text");

        let text_layout = TextLayout::new(
            text_layout.runs.iter().map(|r| TextRun {
                content: &r.content,
                font: r.font_id,
                spacing_inline_start: r.spacing_inline_start,
            }),
            font_set,
            scale_factor,
        );
        cache.text_rects.clear();
        cache
            .text_rects
            .extend(text_layout.rasterize_and_place_glyphs(
                font_set,
                vector_raster_state,
                glyph_atlas,
                scale_factor,
            ));
        // TODO: LTR前提 RTLサポートもするなら最大値をとる必要がある
        cache.text_width = cache.text_rects.last().map_or(0.0, |r| r.right());
        cache.text_height = text_layout.height();

        /*#[cfg(target_os = "macos")]
        let framesetter = apple_sdk_port::text::Framesetter::from_attributed_string(&str)
            .expect("Framesetter.create");
        #[cfg(target_os = "macos")]
        let frame = framesetter
            .create_frame(
                apple_sdk_port::foundation::Range {
                    location: 0,
                    length: 0,
                },
                &apple_sdk_port::graphics::Path::new_rect(
                    apple_sdk_port::raw::CGRect {
                        origin: apple_sdk_port::raw::CGPoint { x: 0.0, y: 0.0 },
                        size: apple_sdk_port::raw::CGSize {
                            width: f64::MAX,
                            height: f64::MAX,
                        },
                    },
                    None,
                ),
                None,
            )
            .expect("Frame.create");
        #[cfg(target_os = "macos")]
        let lines = frame.lines();
        #[cfg(target_os = "macos")]
        tracing::debug!(line_count = lines.len(), "frameset lines");
        #[cfg(target_os = "macos")]
        for n in 0..lines.len() {
            let runs = lines[n].glyph_runs();
            tracing::debug!(count = runs.len(), "glyph runs");
            let mut baseline_pos: apple_sdk_port::raw::CGFloat = 0.0;
            for m in 0..runs.len() {
                let font = match runs[m]
                    .attributes()
                    .get_untyped_value(apple_sdk_port::foundation::AttributedStringKey::font())
                {
                    Some(x) => unsafe {
                        apple_sdk_port::text::Font::ref_from_untyped_ptr(x.as_ptr())
                    },
                    None => font_set.select(Default::default()),
                };

                baseline_pos = baseline_pos.max(font.ascent());
                // TODO: 複数行になる場合はleadingを行間に足す
                cache.text_height = cache
                    .text_height
                    .max((font.ascent() + font.descent()) as f32 * 2.0);
            }
            let mut x_shift = 0.0;
            for m in 0..runs.len() {
                let run = &runs[m];

                let attributes = run.attributes();
                attributes.apply_untyped_value(|key, value| {
                    tracing::debug!(?key, ?value, "run attribute");
                });
                let font = match attributes
                    .get_untyped_value(apple_sdk_port::foundation::AttributedStringKey::font())
                {
                    Some(x) => unsafe {
                        apple_sdk_port::text::Font::ref_from_untyped_ptr(x.as_ptr())
                    },
                    None => font_set.select(Default::default()),
                };
                let run_index = match attributes.get_untyped_value(&corresponding_run_index) {
                    Some(x) => unsafe {
                        apple_sdk_port::foundation::Number::ref_from_untyped_ptr(x.as_ptr())
                            .i64_value()
                            .expect("invalid attr value") as _
                    },
                    None => 0,
                };
                let font_id = t.runs[run_index].font_id;
                x_shift += t.runs[run_index].spacing_inline_start;

                let glyph_count = run.glyph_count();
                tracing::debug!(count = glyph_count, "run");
                let mut glyph_bounding_rects = Vec::with_capacity(glyph_count as _);
                font.bounding_rects_for_glyphs(
                    apple_sdk_port::text::FontOrientation::Horizontal,
                    unsafe { core::slice::from_raw_parts(run.glyphs_ptr(), glyph_count as _) },
                    glyph_bounding_rects.spare_capacity_mut(),
                );
                unsafe {
                    glyph_bounding_rects.set_len(glyph_count as _);
                }

                for g in 0..glyph_count {
                    let glyph = unsafe { *run.glyphs_ptr().add(g as usize) };
                    let pos = unsafe { &*run.positions().add(g as usize) };
                    let bounding_rect = &glyph_bounding_rects[g as usize];
                    tracing::debug!(glyph, ?font_id, ?pos, ?bounding_rect, "glyph");

                    if bounding_rect.size.width == 0.0 && bounding_rect.size.height == 0.0 {
                        // empty shape(whitespace)
                        continue;
                    }

                    let (r, is_new) = mask_atlas.acquire(
                        (font_id as usize, glyph),
                        (bounding_rect.size.width as f32 * dip_to_pixels_scaling).ceil() as _,
                        (bounding_rect.size.height as f32 * dip_to_pixels_scaling).ceil() as _,
                    );
                    let placement_box = GlyphPlacementBox {
                        left: ((pos.x + bounding_rect.origin.x) as f32 + x_shift)
                            * dip_to_pixels_scaling,
                        top: (baseline_pos + pos.y
                            - (bounding_rect.size.height + bounding_rect.origin.y))
                            as f32
                            * dip_to_pixels_scaling,
                        tex_left: r.left,
                        tex_top: r.top,
                        width: r.width,
                        height: r.height,
                    };
                    cache.text_width = cache.text_width.max(placement_box.right());
                    cache.text_rects.push(placement_box);

                    if is_new {
                        vector_raster_state.updated_rects.push(br::Rect2D {
                            offset: br::Offset2D {
                                x: r.left as _,
                                y: r.top as _,
                            },
                            extent: br::Extent2D {
                                width: r.width,
                                height: r.height,
                            },
                        });

                        let path = font
                            .create_path_for_glyph(glyph, None)
                            .expect("font.create_path_for_glyph");
                        let mut current_figure = None;
                        let mut pen_pos = (0.0, 0.0);
                        let offset_x =
                            r.left as f32 - bounding_rect.origin.x as f32 * dip_to_pixels_scaling;
                        let offset_y = r.top as f32
                            - (bounding_rect.size.height + bounding_rect.origin.y) as f32
                                * dip_to_pixels_scaling;
                        path.apply(|e| match e.r#type {
                            apple_sdk_port::raw::kCGPathElementMoveToPoint => {
                                let to = unsafe { &*e.points };

                                current_figure =
                                    Some((to.clone(), vector_raster_state.fill_tri_points.len()));
                                pen_pos = (to.x, to.y);
                                vector_raster_state.fill_tri_points.push([
                                    to.x as f32 * dip_to_pixels_scaling + offset_x,
                                    to.y as f32 * dip_to_pixels_scaling + offset_y,
                                ]);
                            }
                            apple_sdk_port::raw::kCGPathElementAddLineToPoint => {
                                let to = unsafe { &*e.points };
                                let Some((_, filltri_index0)) = current_figure else {
                                    panic!("no figure started?");
                                };

                                let filltri_index1 = vector_raster_state.fill_tri_points.len() - 1;
                                vector_raster_state.fill_tri_points.push([
                                    to.x as f32 * dip_to_pixels_scaling + offset_x,
                                    to.y as f32 * dip_to_pixels_scaling + offset_y,
                                ]);
                                vector_raster_state.fill_tri_indices.extend([
                                    filltri_index0 as u16,
                                    filltri_index1 as u16,
                                    vector_raster_state.fill_tri_points.len() as u16 - 1,
                                ]);
                                pen_pos = (to.x, to.y);
                            }
                            apple_sdk_port::raw::kCGPathElementAddQuadCurveToPoint => {
                                let points = unsafe { core::slice::from_raw_parts(e.points, 2) };
                                let Some((_, filltri_index0)) = current_figure else {
                                    panic!("no figure started?");
                                };

                                let filltri_index1 = vector_raster_state.fill_tri_points.len() - 1;
                                vector_raster_state.fill_tri_points.push([
                                    points[1].x as f32 * dip_to_pixels_scaling + offset_x,
                                    points[1].y as f32 * dip_to_pixels_scaling + offset_y,
                                ]);
                                vector_raster_state.fill_tri_indices.extend([
                                    filltri_index0 as u16,
                                    filltri_index1 as u16,
                                    vector_raster_state.fill_tri_points.len() as u16 - 1,
                                ]);
                                vector_raster_state.curve_tris.extend([
                                    [
                                        pen_pos.0 as f32 * dip_to_pixels_scaling + offset_x,
                                        pen_pos.1 as f32 * dip_to_pixels_scaling + offset_y,
                                        0.0,
                                        0.0,
                                    ],
                                    [
                                        points[0].x as f32 * dip_to_pixels_scaling + offset_x,
                                        points[0].y as f32 * dip_to_pixels_scaling + offset_y,
                                        0.5,
                                        0.0,
                                    ],
                                    [
                                        points[1].x as f32 * dip_to_pixels_scaling + offset_x,
                                        points[1].y as f32 * dip_to_pixels_scaling + offset_y,
                                        1.0,
                                        1.0,
                                    ],
                                ]);
                                pen_pos = (points[1].x, points[1].y);
                            }
                            apple_sdk_port::raw::kCGPathElementAddCurveToPoint => {
                                let points = unsafe { core::slice::from_raw_parts(e.points, 3) };
                                lyon_geom::CubicBezierSegment {
                                    from: lyon_geom::point(pen_pos.0, pen_pos.1),
                                    ctrl1: lyon_geom::point(points[0].x, points[0].y),
                                    ctrl2: lyon_geom::point(points[1].x, points[1].y),
                                    to: lyon_geom::point(points[2].x, points[2].y),
                                }
                                .for_each_quadratic_bezier(
                                    0.1,
                                    &mut |q| {
                                        let Some((_, filltri_index0)) = current_figure else {
                                            panic!("no figure started?");
                                        };

                                        let filltri_index1 =
                                            vector_raster_state.fill_tri_points.len() - 1;
                                        vector_raster_state.fill_tri_points.push([
                                            q.to.x as f32 * dip_to_pixels_scaling + offset_x,
                                            q.to.y as f32 * dip_to_pixels_scaling + offset_y,
                                        ]);
                                        vector_raster_state.fill_tri_indices.extend([
                                            filltri_index0 as u16,
                                            filltri_index1 as u16,
                                            vector_raster_state.fill_tri_points.len() as u16 - 1,
                                        ]);
                                        vector_raster_state.curve_tris.extend([
                                            [
                                                pen_pos.0 as f32 * dip_to_pixels_scaling + offset_x,
                                                pen_pos.1 as f32 * dip_to_pixels_scaling + offset_y,
                                                0.0,
                                                0.0,
                                            ],
                                            [
                                                q.ctrl.x as f32 * dip_to_pixels_scaling + offset_x,
                                                q.ctrl.y as f32 * dip_to_pixels_scaling + offset_y,
                                                0.5,
                                                0.0,
                                            ],
                                            [
                                                q.to.x as f32 * dip_to_pixels_scaling + offset_x,
                                                q.to.y as f32 * dip_to_pixels_scaling + offset_y,
                                                1.0,
                                                1.0,
                                            ],
                                        ]);
                                        pen_pos = (q.to.x, q.to.y);
                                    },
                                )
                            }
                            apple_sdk_port::raw::kCGPathElementCloseSubpath => {
                                // line to start point
                                let Some((start_point, filltri_index0)) = current_figure.take()
                                else {
                                    panic!("no figure started?");
                                };

                                let filltri_index1 = vector_raster_state.fill_tri_points.len() - 1;
                                vector_raster_state.fill_tri_points.push([
                                    start_point.x as f32 * dip_to_pixels_scaling + offset_x,
                                    start_point.y as f32 * dip_to_pixels_scaling + offset_y,
                                ]);
                                vector_raster_state.fill_tri_indices.extend([
                                    filltri_index0 as u16,
                                    filltri_index1 as u16,
                                    vector_raster_state.fill_tri_points.len() as u16 - 1,
                                ]);
                                pen_pos = (start_point.x, start_point.y);
                            }
                            _ => unreachable!(),
                        })
                    }
                }
            }
        }*/
    }
}

#[derive(Clone)]
struct DirtyFlagSet {
    dirty: bool,
    text_layout_dirty: bool,
}

pub struct CompositeTreeSyncBuffer<Event> {
    pushed_rects: Vec<CompositeRect<Event>>,
    dirty_rects: Vec<(usize, DirtyRectSync<Event>)>,
    parameter_store: CompositeTreeParameterStoreSyncBuffer<Event>,
}
impl<Event> CompositeTreeSyncBuffer<Event> {
    pub fn new() -> Self {
        Self {
            pushed_rects: Vec::new(),
            dirty_rects: Vec::new(),
            parameter_store: CompositeTreeParameterStoreSyncBuffer {
                push_float_parameters: Vec::new(),
                dirty_float_parameters: Vec::new(),
            },
        }
    }

    pub fn clean(&mut self, render: &mut CompositeTreeRender<Event>) {
        let _ = render.rects.try_reserve(self.pushed_rects.len());
        let _ = render.caches.try_reserve(self.pushed_rects.len());
        for x in self.pushed_rects.drain(..) {
            render.rects.push(x);
            render.dirty_flags.push(DirtyFlagSet {
                dirty: true,
                text_layout_dirty: true,
            });
            render.caches.push(CompositeRectCache::new());
        }

        for (n, x) in self.dirty_rects.drain(..) {
            match x {
                DirtyRectSync::Modified(new, df) => {
                    render.rects[n] = new;
                    // Host側でtrueになってるやつだけtrueにする
                    render.dirty_flags[n].dirty = render.dirty_flags[n].dirty || df.dirty;
                    render.dirty_flags[n].text_layout_dirty =
                        render.dirty_flags[n].text_layout_dirty || df.text_layout_dirty;
                }
                DirtyRectSync::Deleted => {
                    // TODO: 今はすることがない そのうちCompositeRectに有効無効のフラグもたせるかも
                }
            }
        }

        self.parameter_store.clean(&mut render.parameter_store);
    }
}

pub struct CompositeTree<Event> {
    rects: Vec<CompositeRect<Event>>,
    dirty_flags: Vec<DirtyFlagSet>,
    pushed_rects: Vec<usize>,
    dirty_rects: HashMap<usize, DirtyRect>,
    unused: BTreeSet<usize>,
    dirty: bool,
    parameter_store: CompositeTreeParameterStore<Event>,
    custom_render_unused: BTreeSet<usize>,
    custom_render_last_id: usize,
}
impl<Event> CompositeTree<Event> {
    pub fn new() -> Self {
        Self {
            rects: Vec::new(),
            dirty_flags: Vec::new(),
            pushed_rects: Vec::new(),
            dirty_rects: HashMap::new(),
            unused: BTreeSet::new(),
            dirty: false,
            parameter_store: CompositeTreeParameterStore {
                push_float_parameters: Vec::new(),
                dirty_float_parameters: HashMap::new(),
                unused_float_parameters: BTreeSet::new(),
                float_parameter_store_size: 0,
            },
            custom_render_unused: BTreeSet::new(),
            custom_render_last_id: 0,
        }
    }

    pub fn create(&mut self, data: CompositeRect<Event>) -> CompositeTreeRef {
        if let Some(x) = self.unused.pop_first() {
            self.rects[x] = data;
            self.dirty_flags[x].dirty = true;
            self.dirty_flags[x].text_layout_dirty = true;
            self.dirty_rects.insert(x, DirtyRect::Modified);
            return CompositeTreeRef(x);
        }

        let id = CompositeTreeRef(self.rects.len());
        self.rects.push(data);
        self.dirty_flags.push(DirtyFlagSet {
            dirty: true,
            text_layout_dirty: true,
        });
        self.pushed_rects.push(id.0);
        id
    }

    pub fn free(&mut self, index: CompositeTreeRef) {
        self.unused.insert(index.0);
        self.dirty_rects.insert(index.0, DirtyRect::Deleted);
    }

    pub fn free_all(&mut self, index: CompositeTreeRef) {
        let mut stack = vec![index.0];
        while let Some(c) = stack.pop() {
            stack.extend(self.rects[c].children.drain(..));
            self.free(CompositeTreeRef(c));
        }
    }

    pub fn acquire_custom_render_token(&mut self) -> CustomRenderToken {
        if let Some(x) = self.custom_render_unused.pop_first() {
            return CustomRenderToken(x);
        }

        let t = CustomRenderToken(self.custom_render_last_id);
        self.custom_render_last_id += 1;
        t
    }

    pub fn release_custom_render_token(&mut self, token: CustomRenderToken) {
        self.custom_render_unused.insert(token.0);
    }

    pub fn get(&self, index: CompositeTreeRef) -> &CompositeRect<Event> {
        &self.rects[index.0]
    }

    pub fn get_mut(&mut self, index: CompositeTreeRef) -> &mut CompositeRect<Event> {
        &mut self.rects[index.0]
    }

    pub fn mark_dirty(&mut self, index: CompositeTreeRef) {
        self.dirty_flags[index.0].dirty = true;
        self.dirty_rects.insert(index.0, DirtyRect::Modified);
        self.dirty = true;
    }

    pub fn mark_text_layout_dirty(&mut self, index: CompositeTreeRef) {
        self.dirty_flags[index.0].text_layout_dirty = true;
        self.dirty_rects.insert(index.0, DirtyRect::Modified);
        self.dirty = true;
    }

    pub fn mark_dirty_all(&mut self, index: CompositeTreeRef) {
        self.dirty_flags[index.0].dirty = true;
        if let Some(ref mut x) = self.rects[index.0].text {
            self.dirty_flags[index.0].text_layout_dirty = true;
        }
        self.dirty_rects.insert(index.0, DirtyRect::Modified);
        self.dirty = true;
    }

    pub fn commit(&mut self, sync_buffer: &mut CompositeTreeSyncBuffer<Event>)
    where
        Event: Clone,
    {
        let _ = sync_buffer
            .pushed_rects
            .try_reserve(self.pushed_rects.len());
        for n in self.pushed_rects.drain(..) {
            sync_buffer.pushed_rects.push(self.rects[n].clone());
            self.dirty_flags[n].dirty = false;
            self.dirty_flags[n].text_layout_dirty = false;
        }

        let _ = sync_buffer.dirty_rects.try_reserve(self.dirty_rects.len());
        for (n, x) in self.dirty_rects.drain() {
            match x {
                DirtyRect::Modified => {
                    sync_buffer.dirty_rects.push((
                        n,
                        DirtyRectSync::Modified(self.rects[n].clone(), self.dirty_flags[n].clone()),
                    ));
                    self.dirty_flags[n].dirty = false;
                    self.dirty_flags[n].text_layout_dirty = false;
                }
                DirtyRect::Deleted => {
                    sync_buffer.dirty_rects.push((n, DirtyRectSync::Deleted));
                }
            }
        }

        self.parameter_store
            .commit(&mut sync_buffer.parameter_store);

        self.dirty = false;
    }

    pub fn add_child(&mut self, parent: CompositeTreeRef, child: CompositeTreeRef) {
        if let Some(p) = self.rects[child.0].parent.replace(parent.0) {
            // unlink from old parent
            self.rects[p].children.retain(|&x| x != child.0);
            self.dirty_rects.insert(p, DirtyRect::Modified);
        }

        self.rects[parent.0].children.push(child.0);
        self.dirty_rects.insert(parent.0, DirtyRect::Modified);
        self.dirty = true;
    }

    pub fn remove_child(&mut self, child: CompositeTreeRef) {
        if let Some(p) = self.rects[child.0].parent.take() {
            self.rects[p].children.retain(|&x| x != child.0);
            self.dirty_rects.insert(p, DirtyRect::Modified);
            self.dirty = true;
        }
    }

    pub const fn parameter_store(&self) -> &CompositeTreeParameterStore<Event> {
        &self.parameter_store
    }

    pub const fn parameter_store_mut(&mut self) -> &mut CompositeTreeParameterStore<Event> {
        &mut self.parameter_store
    }
}

struct CompositeDescriptorSet {}
impl CompositeDescriptorSet {
    pub const BINDINGS: &[br::DescriptorSetLayoutBinding<'static>] = &[
        // instance data
        br::DescriptorType::StorageBuffer
            .make_binding(0, 1)
            .for_shader_stage(
                br::vk::VK_SHADER_STAGE_VERTEX_BIT | br::vk::VK_SHADER_STAGE_FRAGMENT_BIT,
            ),
        // streaming data
        br::DescriptorType::UniformBuffer
            .make_binding(1, 1)
            .only_for_vertex(),
        // texture atlas
        br::DescriptorType::CombinedImageSampler
            .make_binding(2, 1)
            .only_for_fragment(),
    ];

    #[inline(always)]
    pub fn set_instance_buffer<'s>(
        set: br::DescriptorSet,
        info: br::DescriptorBufferInfo<'s>,
    ) -> br::DescriptorSetWriteInfo<'s> {
        set.binding_at(0)
            .write(br::DescriptorContents::StorageBuffer(vec![info]))
    }

    #[inline(always)]
    pub fn set_streaming_buffer<'s>(
        set: br::DescriptorSet,
        info: br::DescriptorBufferInfo<'s>,
    ) -> br::DescriptorSetWriteInfo<'s> {
        set.binding_at(1)
            .write(br::DescriptorContents::UniformBuffer(vec![info]))
    }

    #[inline(always)]
    pub fn set_texture_atlas<'s>(
        set: br::DescriptorSet,
        info: br::DescriptorImageInfo<'s>,
    ) -> br::DescriptorSetWriteInfo<'s> {
        set.binding_at(2)
            .write(br::DescriptorContents::CombinedImageSampler(vec![info]))
    }
}

pub struct CompositeRenderer {
    rp_grabbed: br::vk::VkRenderPass,
    rp_final: br::vk::VkRenderPass,
    rp_continue_grabbed: br::vk::VkRenderPass,
    rp_continue_final: br::vk::VkRenderPass,
    fbs_grabbed: Vec<br::vk::VkFramebuffer>,
    fbs_final: Vec<br::vk::VkFramebuffer>,
    fbs_continue_grabbed: Vec<br::vk::VkFramebuffer>,
    fbs_continue_final: Vec<br::vk::VkFramebuffer>,
    sampler: br::vk::VkSampler,
    dsl_input: br::vk::VkDescriptorSetLayout,
    dsl_input_backdrop: br::vk::VkDescriptorSetLayout,
    pipeline_layout: br::vk::VkPipelineLayout,
    shader: br::vk::VkShaderModule,
    pipeline_grabbed: br::vk::VkPipeline,
    pipeline_final: br::vk::VkPipeline,
    pipeline_continue_grabbed: br::vk::VkPipeline,
    pipeline_continue_final: br::vk::VkPipeline,
    grab_buffer: br::vk::VkImage,
    grab_buffer_view: br::vk::VkImageView,
    grab_buffer_memory: br::vk::VkDeviceMemory,
    backdrop_buffers: Vec<(br::vk::VkImage, br::vk::VkImageView)>,
    backdrop_buffer_memory: br::vk::VkDeviceMemory,
    backdrop_blur_destination_fbs: Vec<br::vk::VkFramebuffer>,
    backdrop_buffers_invalidated: bool,
    input_backdrop_descriptor_pool: br::vk::VkDescriptorPool,
    input_backdrop_descriptor_sets: Vec<br::DescriptorSet>,
    input_backdrop_descriptor_pool_capacity: usize,
    backdrop_fx_blur_processor: BackdropEffectBlurProcessor,
    fixed_descriptor_pool: br::vk::VkDescriptorPool,
    alphamask_group_input_descriptor_set: br::DescriptorSet,
    blur_fixed_descriptor_sets: Vec<br::DescriptorSet>,
    instance_manager: CompositeInstanceManager,
}
impl CompositeRenderer {
    unsafe fn drop(&mut self, gfx: &VulkanDevice) {
        Self::release_all_framebuffers(gfx, &mut self.fbs_grabbed);
        Self::release_all_framebuffers(gfx, &mut self.fbs_final);
        Self::release_all_framebuffers(gfx, &mut self.fbs_continue_grabbed);
        Self::release_all_framebuffers(gfx, &mut self.fbs_continue_final);
        Self::release_all_framebuffers(gfx, &mut self.backdrop_blur_destination_fbs);

        unsafe {
            br::vkfn::destroy_descriptor_pool(
                gfx.native_ptr(),
                self.fixed_descriptor_pool,
                core::ptr::null(),
            );
            br::vkfn::destroy_descriptor_pool(
                gfx.native_ptr(),
                self.input_backdrop_descriptor_pool,
                core::ptr::null(),
            );

            for &(r, v) in self.backdrop_buffers.iter() {
                br::vkfn::destroy_image_view(gfx.native_ptr(), v, core::ptr::null());
                br::vkfn::destroy_image(gfx.native_ptr(), r, core::ptr::null());
            }
            br::vkfn::free_memory(
                gfx.native_ptr(),
                self.backdrop_buffer_memory,
                core::ptr::null(),
            );

            br::vkfn::destroy_image_view(
                gfx.native_ptr(),
                self.grab_buffer_view,
                core::ptr::null(),
            );
            br::vkfn::destroy_image(gfx.native_ptr(), self.grab_buffer, core::ptr::null());
            br::vkfn::free_memory(gfx.native_ptr(), self.grab_buffer_memory, core::ptr::null());

            br::vkfn::destroy_pipeline(
                gfx.native_ptr(),
                self.pipeline_continue_final,
                core::ptr::null(),
            );
            br::vkfn::destroy_pipeline(
                gfx.native_ptr(),
                self.pipeline_continue_grabbed,
                core::ptr::null(),
            );
            br::vkfn::destroy_pipeline(gfx.native_ptr(), self.pipeline_final, core::ptr::null());
            br::vkfn::destroy_pipeline(gfx.native_ptr(), self.pipeline_grabbed, core::ptr::null());
            br::vkfn::destroy_shader_module(gfx.native_ptr(), self.shader, core::ptr::null());
            br::vkfn::destroy_pipeline_layout(
                gfx.native_ptr(),
                self.pipeline_layout,
                core::ptr::null(),
            );
            br::vkfn::destroy_descriptor_set_layout(
                gfx.native_ptr(),
                self.dsl_input_backdrop,
                core::ptr::null(),
            );
            br::vkfn::destroy_descriptor_set_layout(
                gfx.native_ptr(),
                self.dsl_input,
                core::ptr::null(),
            );
            br::vkfn::destroy_sampler(gfx.native_ptr(), self.sampler, core::ptr::null());

            br::vkfn::destroy_render_pass(
                gfx.native_ptr(),
                self.rp_continue_final,
                core::ptr::null(),
            );
            br::vkfn::destroy_render_pass(
                gfx.native_ptr(),
                self.rp_continue_grabbed,
                core::ptr::null(),
            );
            br::vkfn::destroy_render_pass(gfx.native_ptr(), self.rp_final, core::ptr::null());
            br::vkfn::destroy_render_pass(gfx.native_ptr(), self.rp_grabbed, core::ptr::null());

            self.backdrop_fx_blur_processor.drop(gfx);
            self.instance_manager.drop(gfx);
        }
    }

    const INITIAL_BACKDROP_BUFFER_COUNT: usize = 16;
    const PIPELINE_VI_STATE: &'static br::PipelineVertexInputStateCreateInfo<'static> =
        &br::PipelineVertexInputStateCreateInfo::new(&[], &[]);
    const PIPELINE_IA_STATE: &'static br::PipelineInputAssemblyStateCreateInfo =
        &br::PipelineInputAssemblyStateCreateInfo::new(br::PrimitiveTopology::TriangleStrip);
    const PIPELINE_RASTER_STATE: &'static br::PipelineRasterizationStateCreateInfo<'static> =
        &br::PipelineRasterizationStateCreateInfo::new(
            br::PolygonMode::Fill,
            br::CullModeFlags::NONE,
            br::FrontFace::CounterClockwise,
        );
    const PIPELINE_BLEND_STATE: &'static br::PipelineColorBlendStateCreateInfo<'static> =
        &br::PipelineColorBlendStateCreateInfo::new(&[
            br::vk::VkPipelineColorBlendAttachmentState::PREMULTIPLIED,
        ]);

    pub fn new<'b>(
        gfx: &VulkanDevice,
        mask_atlas: br::VkHandleRef<br::vk::VkImageView>,
        rt_format: br::Format,
        rt_buffers: impl Iterator<Item = br::VkHandleRef<'b, br::vk::VkImageView>>,
        rt_size: br::Extent2D,
    ) -> Self {
        let rp_grabbed = gfx
            .create_render_pass(&br::RenderPassCreateInfo2::new(
                &[br::AttachmentDescription2::new(rt_format)
                    .with_layout_to(br::ImageLayout::TransferSrcOpt.from_undefined())
                    .color_memory_op(br::LoadOp::DontCare, br::StoreOp::Store)],
                &[br::SubpassDescription2::new()
                    .colors(&[br::AttachmentReference2::color_attachment_opt(0)])],
                &[br::SubpassDependency2::new(
                    br::SubpassIndex::Internal(0),
                    br::SubpassIndex::External,
                )
                .of_execution(
                    br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
                    br::PipelineStageFlags::TRANSFER,
                )
                .of_memory(
                    br::AccessFlags::COLOR_ATTACHMENT.write,
                    br::AccessFlags::TRANSFER.read,
                )],
            ))
            .unwrap();
        gfx.dbg_set_name(&rp_grabbed, c"CompositeRenderer::rp[grabbed]");
        let rp_final = gfx
            .create_render_pass(&br::RenderPassCreateInfo2::new(
                &[br::AttachmentDescription2::new(rt_format)
                    .with_layout_to(br::ImageLayout::PresentSrc.from_undefined())
                    .color_memory_op(br::LoadOp::DontCare, br::StoreOp::Store)],
                &[br::SubpassDescription2::new()
                    .colors(&[br::AttachmentReference2::color_attachment_opt(0)])],
                &[br::SubpassDependency2::new(
                    br::SubpassIndex::Internal(0),
                    br::SubpassIndex::External,
                )
                .of_execution(
                    br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
                    br::PipelineStageFlags(0),
                )
                .of_memory(
                    br::AccessFlags::COLOR_ATTACHMENT.write,
                    br::AccessFlags::MEMORY.read,
                )
                .by_region()],
            ))
            .unwrap();
        gfx.dbg_set_name(&rp_final, c"CompositeRenderer::rp[final]");
        let rp_continue_grabbed = gfx
            .create_render_pass(&br::RenderPassCreateInfo2::new(
                &[br::AttachmentDescription2::new(rt_format)
                    .with_layout_to(
                        br::ImageLayout::TransferSrcOpt.from(br::ImageLayout::TransferSrcOpt),
                    )
                    .color_memory_op(br::LoadOp::Load, br::StoreOp::Store)],
                &[br::SubpassDescription2::new()
                    .colors(&[br::AttachmentReference2::color_attachment_opt(0)])],
                &[br::SubpassDependency2::new(
                    br::SubpassIndex::Internal(0),
                    br::SubpassIndex::External,
                )
                .of_execution(
                    br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
                    br::PipelineStageFlags::TRANSFER,
                )
                .of_memory(
                    br::AccessFlags::COLOR_ATTACHMENT.write,
                    br::AccessFlags::TRANSFER.read,
                )],
            ))
            .unwrap();
        gfx.dbg_set_name(&rp_continue_grabbed, c"CompositeRenderer::rp[grabbed,cont]");
        let rp_continue_final = gfx
            .create_render_pass(&br::RenderPassCreateInfo2::new(
                &[br::AttachmentDescription2::new(rt_format)
                    .with_layout_to(
                        br::ImageLayout::PresentSrc.from(br::ImageLayout::TransferSrcOpt),
                    )
                    .color_memory_op(br::LoadOp::Load, br::StoreOp::Store)],
                &[br::SubpassDescription2::new()
                    .colors(&[br::AttachmentReference2::color_attachment_opt(0)])],
                &[br::SubpassDependency2::new(
                    br::SubpassIndex::Internal(0),
                    br::SubpassIndex::External,
                )
                .of_execution(
                    br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
                    br::PipelineStageFlags(0),
                )
                .of_memory(
                    br::AccessFlags::COLOR_ATTACHMENT.write,
                    br::AccessFlags::MEMORY.read,
                )
                .by_region()],
            ))
            .unwrap();
        gfx.dbg_set_name(&rp_continue_final, c"CompositeRenderer::rp[final,cont]");

        let buffer_size_hint = rt_buffers.size_hint();
        let buffer_size_hint = buffer_size_hint.1.unwrap_or(buffer_size_hint.0);
        let mut fbs_grabbed = Vec::with_capacity(buffer_size_hint);
        let mut fbs_final = Vec::with_capacity(buffer_size_hint);
        let mut fbs_continue_grabbed = Vec::with_capacity(buffer_size_hint);
        let mut fbs_continue_final = Vec::with_capacity(buffer_size_hint);
        for bb in rt_buffers {
            fbs_grabbed.push(
                br::FramebufferObject::new(
                    gfx,
                    &br::FramebufferCreateInfo::new(
                        &rp_grabbed,
                        &[bb.as_transparent_ref()],
                        rt_size.width,
                        rt_size.height,
                    ),
                )
                .unwrap(),
            );
            fbs_final.push(
                br::FramebufferObject::new(
                    gfx,
                    &br::FramebufferCreateInfo::new(
                        &rp_final,
                        &[bb.as_transparent_ref()],
                        rt_size.width,
                        rt_size.height,
                    ),
                )
                .unwrap(),
            );
            fbs_continue_grabbed.push(
                br::FramebufferObject::new(
                    gfx,
                    &br::FramebufferCreateInfo::new(
                        &rp_continue_grabbed,
                        &[bb.as_transparent_ref()],
                        rt_size.width,
                        rt_size.height,
                    ),
                )
                .unwrap(),
            );
            fbs_continue_final.push(
                br::FramebufferObject::new(
                    gfx,
                    &br::FramebufferCreateInfo::new(
                        &rp_continue_final,
                        &[bb.as_transparent_ref()],
                        rt_size.width,
                        rt_size.height,
                    ),
                )
                .unwrap(),
            );
        }

        let sampler = br::SamplerObject::new(gfx, &br::SamplerCreateInfo::new()).unwrap();

        let dsl_input = br::DescriptorSetLayoutObject::new(
            gfx,
            &br::DescriptorSetLayoutCreateInfo::new(CompositeDescriptorSet::BINDINGS),
        )
        .unwrap();
        let dsl_input_backdrop = br::DescriptorSetLayoutObject::new(
            gfx,
            &br::DescriptorSetLayoutCreateInfo::new(&[br::DescriptorType::CombinedImageSampler
                .make_binding(0, 1)
                .only_for_fragment()]),
        )
        .unwrap();
        let pipeline_layout = br::PipelineLayoutObject::new(
            gfx,
            &br::PipelineLayoutCreateInfo::new(
                &[
                    dsl_input.as_transparent_ref(),
                    dsl_input_backdrop.as_transparent_ref(),
                ],
                COMPOSITE_PUSH_CONSTANT_RANGES,
            ),
        )
        .unwrap();

        let shader = gfx.require_shader("composite.spv");
        let shader_stages = [
            shader.on_stage(br::ShaderStage::Vertex, c"vertMain"),
            shader.on_stage(br::ShaderStage::Fragment, c"fragMain"),
        ];
        let viewports = [rt_size
            .into_rect(br::Offset2D::ZERO)
            .make_viewport(0.0..1.0)];
        let scissors = [rt_size.into_rect(br::Offset2D::ZERO)];
        let vp_state = br::PipelineViewportStateCreateInfo::new_array(&viewports, &scissors);
        let [
            pipeline_grabbed,
            pipeline_final,
            pipeline_continue_grabbed,
            pipeline_continue_final,
        ] = gfx
            .create_graphics_pipelines_array(&[
                br::GraphicsPipelineCreateInfo::new(
                    &pipeline_layout,
                    rp_grabbed.subpass(0),
                    &shader_stages,
                    Self::PIPELINE_VI_STATE,
                    Self::PIPELINE_IA_STATE,
                    &vp_state,
                    Self::PIPELINE_RASTER_STATE,
                    Self::PIPELINE_BLEND_STATE,
                )
                .set_multisample_state(MS_STATE_EMPTY),
                br::GraphicsPipelineCreateInfo::new(
                    &pipeline_layout,
                    rp_final.subpass(0),
                    &shader_stages,
                    Self::PIPELINE_VI_STATE,
                    Self::PIPELINE_IA_STATE,
                    &vp_state,
                    Self::PIPELINE_RASTER_STATE,
                    Self::PIPELINE_BLEND_STATE,
                )
                .set_multisample_state(MS_STATE_EMPTY),
                br::GraphicsPipelineCreateInfo::new(
                    &pipeline_layout,
                    rp_continue_grabbed.subpass(0),
                    &shader_stages,
                    Self::PIPELINE_VI_STATE,
                    Self::PIPELINE_IA_STATE,
                    &vp_state,
                    Self::PIPELINE_RASTER_STATE,
                    Self::PIPELINE_BLEND_STATE,
                )
                .set_multisample_state(MS_STATE_EMPTY),
                br::GraphicsPipelineCreateInfo::new(
                    &pipeline_layout,
                    rp_continue_final.subpass(0),
                    &shader_stages,
                    Self::PIPELINE_VI_STATE,
                    Self::PIPELINE_IA_STATE,
                    &vp_state,
                    Self::PIPELINE_RASTER_STATE,
                    Self::PIPELINE_BLEND_STATE,
                )
                .set_multisample_state(MS_STATE_EMPTY),
            ])
            .unwrap();
        gfx.dbg_set_name(&pipeline_grabbed, c"Composite Pipeline[grabbed]");
        gfx.dbg_set_name(&pipeline_final, c"Composite Pipeline[to final]");
        gfx.dbg_set_name(
            &pipeline_continue_grabbed,
            c"Composite Pipeline[grabbed, continue]",
        );
        gfx.dbg_set_name(
            &pipeline_continue_final,
            c"Composite Pipeline[final, continue]",
        );

        let backdrop_buffers =
            Vec::<br::ImageViewObject<br::ImageObject<&VulkanDevice>>>::with_capacity(
                Self::INITIAL_BACKDROP_BUFFER_COUNT,
            );
        let backdrop_buffer_memory = br::DeviceMemoryObject::new(
            gfx,
            &br::MemoryAllocateInfo::new(10, gfx.find_device_local_memory_index(!0).unwrap()),
        )
        .unwrap();
        let backdrop_blur_destination_fbs = Vec::with_capacity(Self::INITIAL_BACKDROP_BUFFER_COUNT);

        let mut grab_buffer = br::ImageObject::new(
            gfx,
            &br::ImageCreateInfo::new(rt_size, rt_format)
                .with_usage(br::ImageUsageFlags::SAMPLED | br::ImageUsageFlags::TRANSFER_DEST),
        )
        .unwrap();
        let grab_buffer_memory =
            gfx.alloc_device_local_memory_for_requirements(&grab_buffer.requirements());
        grab_buffer.bind(&grab_buffer_memory, 0).unwrap();
        let grab_buffer = br::ImageViewBuilder::new(
            grab_buffer,
            br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
        )
        .create()
        .unwrap();

        let input_backdrop_descriptor_pool = br::DescriptorPoolObject::new(
            gfx,
            &br::DescriptorPoolCreateInfo::new(
                16,
                &[br::DescriptorType::CombinedImageSampler
                    .make_size(Self::INITIAL_BACKDROP_BUFFER_COUNT as _)],
            ),
        )
        .unwrap();
        let input_backdrop_descriptor_sets =
            Vec::<br::DescriptorSet>::with_capacity(Self::INITIAL_BACKDROP_BUFFER_COUNT);

        let instance_manager = CompositeInstanceManager::new(gfx);
        let backdrop_fx_blur_processor = BackdropEffectBlurProcessor::new(gfx, rt_size, rt_format);

        let mut fixed_descriptor_pool = br::DescriptorPoolObject::new(
            gfx,
            &br::DescriptorPoolCreateInfo::new(
                (1 + backdrop_fx_blur_processor.fixed_descriptor_set_count()) as _,
                &[
                    br::DescriptorType::CombinedImageSampler.make_size(
                        (1 + backdrop_fx_blur_processor.fixed_descriptor_set_count()) as _,
                    ),
                    br::DescriptorType::UniformBuffer.make_size(1),
                    br::DescriptorType::StorageBuffer.make_size(1),
                ],
            ),
        )
        .unwrap();
        let [alphamask_group_input_descriptor_set] = fixed_descriptor_pool
            .alloc_array(&[dsl_input.as_transparent_ref()])
            .unwrap();
        let blur_fixed_descriptor_sets =
            backdrop_fx_blur_processor.alloc_fixed_descriptor_sets(&mut fixed_descriptor_pool);

        let mut descriptor_writes = vec![
            CompositeDescriptorSet::set_instance_buffer(
                alphamask_group_input_descriptor_set,
                br::DescriptorBufferInfo::new(
                    instance_manager.buffer_transparent_ref(),
                    0..(core::mem::size_of::<CompositeInstanceData>() * 1024) as _,
                ),
            ),
            CompositeDescriptorSet::set_streaming_buffer(
                alphamask_group_input_descriptor_set,
                br::DescriptorBufferInfo::new(
                    instance_manager.streaming_buffer_transparent_ref(),
                    0..core::mem::size_of::<CompositeStreamingData>() as _,
                ),
            ),
            CompositeDescriptorSet::set_texture_atlas(
                alphamask_group_input_descriptor_set,
                br::DescriptorImageInfo::new(&mask_atlas, br::ImageLayout::ShaderReadOnlyOpt)
                    .with_sampler(&sampler),
            ),
        ];
        backdrop_fx_blur_processor.write_input_descriptor_sets(
            &mut descriptor_writes,
            &grab_buffer,
            &blur_fixed_descriptor_sets,
        );
        gfx.update_descriptor_sets(&descriptor_writes, &[]);

        let (grab_buffer_view, grab_buffer) = grab_buffer.unmanage();

        Self {
            rp_grabbed: rp_grabbed.unmanage().0,
            rp_final: rp_final.unmanage().0,
            rp_continue_grabbed: rp_continue_grabbed.unmanage().0,
            rp_continue_final: rp_continue_final.unmanage().0,
            fbs_grabbed: fbs_grabbed.into_iter().map(|x| x.unmanage().0).collect(),
            fbs_final: fbs_final.into_iter().map(|x| x.unmanage().0).collect(),
            fbs_continue_grabbed: fbs_continue_grabbed
                .into_iter()
                .map(|x| x.unmanage().0)
                .collect(),
            fbs_continue_final: fbs_continue_final
                .into_iter()
                .map(|x| x.unmanage().0)
                .collect(),
            sampler: sampler.unmanage().0,
            dsl_input: dsl_input.unmanage().0,
            dsl_input_backdrop: dsl_input_backdrop.unmanage().0,
            pipeline_layout: pipeline_layout.unmanage().0,
            shader: shader.unmanage().0,
            pipeline_grabbed: pipeline_grabbed.unmanage().0,
            pipeline_final: pipeline_final.unmanage().0,
            pipeline_continue_grabbed: pipeline_continue_grabbed.unmanage().0,
            pipeline_continue_final: pipeline_continue_final.unmanage().0,
            grab_buffer: grab_buffer.unmanage().0,
            grab_buffer_view,
            grab_buffer_memory: grab_buffer_memory.unmanage().0,
            backdrop_buffers: backdrop_buffers
                .into_iter()
                .map(|x| {
                    let (v, r) = x.unmanage();
                    let r = r.unmanage().0;

                    (r, v)
                })
                .collect(),
            backdrop_buffer_memory: backdrop_buffer_memory.unmanage().0,
            backdrop_blur_destination_fbs,
            backdrop_buffers_invalidated: true,
            input_backdrop_descriptor_pool: input_backdrop_descriptor_pool.unmanage().0,
            input_backdrop_descriptor_sets,
            input_backdrop_descriptor_pool_capacity: Self::INITIAL_BACKDROP_BUFFER_COUNT,
            backdrop_fx_blur_processor,
            fixed_descriptor_pool: fixed_descriptor_pool.unmanage().0,
            alphamask_group_input_descriptor_set,
            blur_fixed_descriptor_sets,
            instance_manager,
        }
    }

    pub fn update<Event>(
        &mut self,
        gfx: &VulkanDevice,
        tree: &mut CompositeTreeRender<Event>,
        root: CompositeTreeRef,
        rt_size: br::Extent2D,
        font_set: &PerWindowFontSet,
        mask_atlas: &mut MaskTextureAtlasManager,
        mask_atlas_rects: &[AtlasRect],
        vector_raster_state: &mut VectorRasterizationState,
        on_event: impl FnMut(Event),
        current_sec: f32,
    ) -> CompositeRenderingData {
        let h = self.instance_manager.staging_memory_raw_handle();
        let r = self.instance_manager.range_all();
        let flush_required = self.instance_manager.memory_stg_requires_explicit_flush();
        let ptr = unsafe {
            self.instance_manager
                .map_staging(gfx)
                .expect("composite.instances.stg.map")
        };
        let mut inst_builder = CompositeRenderingInstructionBuilder::new(rt_size);
        unsafe {
            tree.update(
                root,
                &mut inst_builder,
                rt_size,
                current_sec,
                ptr.ptr(),
                font_set,
                mask_atlas,
                mask_atlas_rects,
                vector_raster_state,
                on_event,
            )
        };
        let render_data = inst_builder.build();
        if flush_required {
            unsafe {
                gfx.flush_mapped_memory_ranges(&[br::MappedMemoryRange::new_raw(
                    h,
                    r.start as _,
                    (r.end - r.start) as _,
                )])
                .expect("composite.instance.stg.flush");
            }
        }
        drop(ptr);

        render_data
    }

    pub fn update_streaming_data(&mut self, gfx: &VulkanDevice, data: CompositeStreamingData) {
        let h = self.instance_manager.streaming_memory_raw_handle();
        let flush_required = self.instance_manager.streaming_memory_requires_flush();
        let ptr = unsafe {
            self.instance_manager
                .map_streaming(gfx)
                .expect("composite.streaming,map")
        };
        unsafe {
            core::ptr::write(ptr.ptr().cast::<CompositeStreamingData>(), data);
        }
        if flush_required {
            unsafe {
                gfx.flush_mapped_memory_ranges(&[br::MappedMemoryRange::new_raw(
                    h,
                    0,
                    core::mem::size_of::<CompositeStreamingData>() as _,
                )])
                .expect("composite.streaming.flush");
            }
        }
        drop(ptr);
    }

    pub fn sync_buffer<'r>(&self, r: br::CmdRecord<'r>) -> br::CmdRecord<'r> {
        self.instance_manager.sync_buffer(r)
    }

    #[inline]
    fn release_all_framebuffers(
        gfx_device: &(impl br::VkHandle<Handle = br::vk::VkDevice> + ?Sized),
        fbs: &mut Vec<br::vk::VkFramebuffer>,
    ) {
        for x in fbs.drain(..) {
            unsafe {
                br::vkfn_wrapper::destroy_framebuffer(gfx_device.native_ptr(), x, None);
            }
        }
    }

    pub fn recreate_rt_resources<'s, 'b>(
        &'s mut self,
        gfx: &VulkanDevice,
        rt_format: br::Format,
        rt_buffers: impl Iterator<Item = br::VkHandleRef<'b, br::vk::VkImageView>>,
        rt_size: br::Extent2D,
        descriptor_writes: &mut Vec<br::DescriptorSetWriteInfo<'s>>,
    ) {
        let buffer_size_hint = rt_buffers.size_hint();
        let buffer_size_hint = buffer_size_hint.1.unwrap_or(buffer_size_hint.0);

        Self::release_all_framebuffers(gfx, &mut self.fbs_grabbed);
        Self::release_all_framebuffers(gfx, &mut self.fbs_final);
        Self::release_all_framebuffers(gfx, &mut self.fbs_continue_grabbed);
        Self::release_all_framebuffers(gfx, &mut self.fbs_continue_final);
        let mut fbs_grabbed = Vec::with_capacity(buffer_size_hint);
        let mut fbs_final = Vec::with_capacity(buffer_size_hint);
        let mut fbs_continue_grabbed = Vec::with_capacity(buffer_size_hint);
        let mut fbs_continue_final = Vec::with_capacity(buffer_size_hint);
        for bb in rt_buffers {
            fbs_grabbed.push(
                br::FramebufferObject::new(
                    gfx,
                    &br::FramebufferCreateInfo::new(
                        &unsafe { br::VkHandleRef::dangling(self.rp_grabbed) },
                        &[bb.as_transparent_ref()],
                        rt_size.width,
                        rt_size.height,
                    ),
                )
                .unwrap(),
            );
            fbs_final.push(
                br::FramebufferObject::new(
                    gfx,
                    &br::FramebufferCreateInfo::new(
                        &unsafe { br::VkHandleRef::dangling(self.rp_final) },
                        &[bb.as_transparent_ref()],
                        rt_size.width,
                        rt_size.height,
                    ),
                )
                .unwrap(),
            );
            fbs_continue_grabbed.push(
                br::FramebufferObject::new(
                    gfx,
                    &br::FramebufferCreateInfo::new(
                        &unsafe { br::VkHandleRef::dangling(self.rp_continue_grabbed) },
                        &[bb.as_transparent_ref()],
                        rt_size.width,
                        rt_size.height,
                    ),
                )
                .unwrap(),
            );
            fbs_continue_final.push(
                br::FramebufferObject::new(
                    gfx,
                    &br::FramebufferCreateInfo::new(
                        &unsafe { br::VkHandleRef::dangling(self.rp_continue_final) },
                        &[bb.as_transparent_ref()],
                        rt_size.width,
                        rt_size.height,
                    ),
                )
                .unwrap(),
            );
        }

        self.backdrop_buffers_invalidated = true;

        unsafe {
            // release first
            br::vkfn::destroy_image_view(
                gfx.native_ptr(),
                self.grab_buffer_view,
                core::ptr::null(),
            );
            br::vkfn::destroy_image(gfx.native_ptr(), self.grab_buffer, core::ptr::null());
            br::vkfn::free_memory(gfx.native_ptr(), self.grab_buffer_memory, core::ptr::null());
        }
        let mut grab_buffer = br::ImageObject::new(
            gfx,
            &br::ImageCreateInfo::new(rt_size, rt_format)
                .with_usage(br::ImageUsageFlags::SAMPLED | br::ImageUsageFlags::TRANSFER_DEST),
        )
        .unwrap();
        let grab_buffer_memory =
            gfx.alloc_device_local_memory_for_requirements(&grab_buffer.requirements());
        grab_buffer.bind(&grab_buffer_memory, 0).unwrap();
        let grab_buffer = br::ImageViewBuilder::new(
            grab_buffer,
            br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
        )
        .create()
        .unwrap();
        let (v, r) = grab_buffer.unmanage();
        self.grab_buffer_view = v;
        self.grab_buffer = r.unmanage().0;
        self.grab_buffer_memory = grab_buffer_memory.unmanage().0;

        unsafe {
            br::vkfn::destroy_pipeline(gfx.native_ptr(), self.pipeline_grabbed, core::ptr::null());
            br::vkfn::destroy_pipeline(gfx.native_ptr(), self.pipeline_final, core::ptr::null());
            br::vkfn::destroy_pipeline(
                gfx.native_ptr(),
                self.pipeline_continue_grabbed,
                core::ptr::null(),
            );
            br::vkfn::destroy_pipeline(
                gfx.native_ptr(),
                self.pipeline_continue_final,
                core::ptr::null(),
            );
        }
        let shader_stages = [
            br::PipelineShaderStage::new(
                br::ShaderStage::Vertex,
                br::VkHandleRef::from_raw_ref(&self.shader),
                c"vertMain",
            ),
            br::PipelineShaderStage::new(
                br::ShaderStage::Fragment,
                br::VkHandleRef::from_raw_ref(&self.shader),
                c"fragMain",
            ),
        ];
        let viewports = [rt_size
            .into_rect(br::Offset2D::ZERO)
            .make_viewport(0.0..1.0)];
        let scissors = [rt_size.into_rect(br::Offset2D::ZERO)];
        let vp_state = br::PipelineViewportStateCreateInfo::new_array(&viewports, &scissors);
        let [
            pipeline_grabbed,
            pipeline_final,
            pipeline_continue_grabbed,
            pipeline_continue_final,
        ] = gfx
            .create_graphics_pipelines_array(&[
                br::GraphicsPipelineCreateInfo::new(
                    &unsafe { br::VkHandleRef::dangling(self.pipeline_layout) },
                    br::SubpassRef(br::VkHandleRef::from_raw_ref(&self.rp_grabbed), 0),
                    &shader_stages,
                    Self::PIPELINE_VI_STATE,
                    Self::PIPELINE_IA_STATE,
                    &vp_state,
                    Self::PIPELINE_RASTER_STATE,
                    Self::PIPELINE_BLEND_STATE,
                )
                .set_multisample_state(MS_STATE_EMPTY),
                br::GraphicsPipelineCreateInfo::new(
                    &unsafe { br::VkHandleRef::dangling(self.pipeline_layout) },
                    br::SubpassRef(br::VkHandleRef::from_raw_ref(&self.rp_final), 0),
                    &shader_stages,
                    Self::PIPELINE_VI_STATE,
                    Self::PIPELINE_IA_STATE,
                    &vp_state,
                    Self::PIPELINE_RASTER_STATE,
                    Self::PIPELINE_BLEND_STATE,
                )
                .set_multisample_state(MS_STATE_EMPTY),
                br::GraphicsPipelineCreateInfo::new(
                    &unsafe { br::VkHandleRef::dangling(self.pipeline_layout) },
                    br::SubpassRef(br::VkHandleRef::from_raw_ref(&self.rp_continue_grabbed), 0),
                    &shader_stages,
                    Self::PIPELINE_VI_STATE,
                    Self::PIPELINE_IA_STATE,
                    &vp_state,
                    Self::PIPELINE_RASTER_STATE,
                    Self::PIPELINE_BLEND_STATE,
                )
                .set_multisample_state(MS_STATE_EMPTY),
                br::GraphicsPipelineCreateInfo::new(
                    &unsafe { br::VkHandleRef::dangling(self.pipeline_layout) },
                    br::SubpassRef(br::VkHandleRef::from_raw_ref(&self.rp_continue_final), 0),
                    &shader_stages,
                    Self::PIPELINE_VI_STATE,
                    Self::PIPELINE_IA_STATE,
                    &vp_state,
                    Self::PIPELINE_RASTER_STATE,
                    Self::PIPELINE_BLEND_STATE,
                )
                .set_multisample_state(MS_STATE_EMPTY),
            ])
            .unwrap();
        gfx.dbg_set_name(&pipeline_grabbed, c"Composite Pipeline[grabbed]");
        gfx.dbg_set_name(&pipeline_final, c"Composite Pipeline[to final]");
        gfx.dbg_set_name(
            &pipeline_continue_grabbed,
            c"Composite Pipeline[grabbed, continue]",
        );
        gfx.dbg_set_name(
            &pipeline_continue_final,
            c"Composite Pipeline[final, continue]",
        );
        self.pipeline_grabbed = pipeline_grabbed.unmanage().0;
        self.pipeline_final = pipeline_final.unmanage().0;
        self.pipeline_continue_grabbed = pipeline_continue_grabbed.unmanage().0;
        self.pipeline_continue_final = pipeline_continue_final.unmanage().0;

        self.backdrop_fx_blur_processor
            .recreate_rt_resources(gfx, rt_size, rt_format);
        self.backdrop_fx_blur_processor.write_input_descriptor_sets(
            descriptor_writes,
            br::VkHandleRef::from_raw_ref(&self.grab_buffer_view),
            &self.blur_fixed_descriptor_sets,
        );

        self.fbs_grabbed
            .extend(fbs_grabbed.into_iter().map(|x| x.unmanage().0));
        self.fbs_final
            .extend(fbs_final.into_iter().map(|x| x.unmanage().0));
        self.fbs_continue_grabbed
            .extend(fbs_continue_grabbed.into_iter().map(|x| x.unmanage().0));
        self.fbs_continue_final
            .extend(fbs_continue_final.into_iter().map(|x| x.unmanage().0));
    }

    pub fn prepare_input_backdrop_descriptor_sets(
        &mut self,
        gfx: &VulkanDevice,
        required_count: usize,
    ) {
        let object_count = required_count.max(1);

        if object_count == self.input_backdrop_descriptor_sets.len() {
            // no changes
            return;
        }

        if object_count > self.input_backdrop_descriptor_pool_capacity {
            // resize pool

            self.input_backdrop_descriptor_pool = br::DescriptorPoolObject::new(
                gfx,
                &br::DescriptorPoolCreateInfo::new(
                    object_count as _,
                    &[br::DescriptorType::CombinedImageSampler.make_size(object_count as _)],
                ),
            )
            .unwrap()
            .unmanage()
            .0;
            self.input_backdrop_descriptor_pool_capacity = object_count;
        } else {
            // just reset
            unsafe {
                br::vkfn::reset_descriptor_pool(
                    gfx.native_ptr(),
                    self.input_backdrop_descriptor_pool,
                    0,
                )
                .into_result()
                .unwrap();
            }
        }

        let mut input_backdrop_descriptor_sets =
            Vec::<br::DescriptorSet>::with_capacity(object_count);
        unsafe {
            br::vkfn::allocate_descriptor_sets(
                gfx.native_ptr(),
                &br::vk::VkDescriptorSetAllocateInfo {
                    sType: br::vk::VkDescriptorSetAllocateInfo::TYPE,
                    pNext: core::ptr::null(),
                    descriptorPool: self.input_backdrop_descriptor_pool,
                    descriptorSetCount: object_count as _,
                    pSetLayouts: core::iter::repeat_n(self.dsl_input_backdrop, object_count)
                        .collect::<Vec<_>>()
                        .as_ptr(),
                },
                input_backdrop_descriptor_sets
                    .spare_capacity_mut()
                    .as_mut_ptr()
                    .cast(),
            )
            .into_result()
            .expect("input_backdrop_descriptor_sets.realloc");
            input_backdrop_descriptor_sets.set_len(object_count);
        }

        self.input_backdrop_descriptor_sets.clear();
        self.input_backdrop_descriptor_sets
            .extend(input_backdrop_descriptor_sets);
        self.backdrop_buffers_invalidated = true;
    }

    pub fn update_backdrop_resources(
        &mut self,
        gfx: &VulkanDevice,
        rt_format: br::Format,
        rt_size: br::Extent2D,
        unused: bool,
    ) -> bool {
        if !self.backdrop_buffers_invalidated {
            // no changes
            return false;
        }

        Self::release_all_framebuffers(gfx, &mut self.backdrop_blur_destination_fbs);
        unsafe {
            for (r, v) in self.backdrop_buffers.drain(..) {
                br::vkfn::destroy_image_view(gfx.native_ptr(), v, core::ptr::null());
                br::vkfn::destroy_image(gfx.native_ptr(), r, core::ptr::null());
            }

            br::vkfn::free_memory(
                gfx.native_ptr(),
                self.backdrop_buffer_memory,
                core::ptr::null(),
            );
        }

        let backdrop_count = self.input_backdrop_descriptor_sets.len();
        let mut image_objects = Vec::with_capacity(backdrop_count);
        let mut offsets = Vec::with_capacity(backdrop_count);
        let mut top = 0u64;
        let mut memory_index_mask = !0u32;
        for _ in 0..backdrop_count {
            let image = br::ImageObject::new(
                gfx,
                &br::ImageCreateInfo::new(rt_size, rt_format).with_usage(
                    br::ImageUsageFlags::SAMPLED
                        | br::ImageUsageFlags::COLOR_ATTACHMENT
                        | br::ImageUsageFlags::TRANSFER_DEST,
                ),
            )
            .unwrap();
            let req = image.requirements();
            assert!(req.alignment.is_power_of_two());
            let offset = (top + req.alignment - 1) & !(req.alignment - 1);
            top = offset + req.size;
            memory_index_mask &= req.memoryTypeBits;

            offsets.push(offset);
            image_objects.push(image);
        }
        let Some(memindex) = gfx.find_device_local_memory_index(memory_index_mask) else {
            tracing::error!(
                memory_index_mask,
                "no suitable memory for composition backdrop buffers"
            );
            std::process::exit(1);
        };
        let backdrop_buffer_memory =
            br::DeviceMemoryObject::new(gfx, &br::MemoryAllocateInfo::new(top.max(64), memindex))
                .unwrap();
        for (n, (mut r, o)) in image_objects
            .into_iter()
            .zip(offsets.into_iter())
            .enumerate()
        {
            r.bind(&backdrop_buffer_memory, o as _).unwrap();
            gfx.dbg_set_name(&r, &unsafe {
                std::ffi::CString::from_vec_unchecked(format!("backdrop buffer #{n}").into_bytes())
            });
            let o = br::ImageViewBuilder::new(
                r,
                br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
            )
            .create()
            .unwrap();

            let (v, r) = o.unmanage();
            self.backdrop_buffers.push((r.unmanage().0, v));
        }
        unsafe {
            core::ptr::write(
                &mut self.backdrop_buffer_memory,
                backdrop_buffer_memory.unmanage().0,
            );
        }

        self.backdrop_blur_destination_fbs
            .extend(self.backdrop_buffers.iter().map(|b| {
                br::FramebufferObject::new(
                    gfx,
                    &br::FramebufferCreateInfo::new(
                        &self.backdrop_fx_blur_processor.final_render_pass(),
                        &[unsafe { br::VkHandleRef::dangling(b.1) }],
                        rt_size.width,
                        rt_size.height,
                    ),
                )
                .unwrap()
                .unmanage()
                .0
            }));

        gfx.update_descriptor_sets(
            &self
                .backdrop_buffers
                .iter()
                .zip(self.input_backdrop_descriptor_sets.iter())
                .map(|(v, d)| {
                    d.binding_at(0)
                        .write(br::DescriptorContents::CombinedImageSampler(vec![
                            br::DescriptorImageInfo::new(
                                br::VkHandleRef::from_raw_ref(&v.1),
                                br::ImageLayout::ShaderReadOnlyOpt,
                            )
                            .with_sampler(br::VkHandleRef::from_raw_ref(&self.sampler)),
                        ]))
                })
                .collect::<Vec<_>>(),
            &[],
        );

        if unused {
            // backdrop bufferを結局つかわない場合は0番目のImageLayoutだけ変えておく(Warning対策)
            let mut cp = br::CommandPoolObject::new(
                gfx,
                &br::CommandPoolCreateInfo::new(gfx.present_queue_family_index()).transient(),
            )
            .expect("cp.create");
            let mut cb = br::CommandBufferObject::alloc(
                gfx,
                &br::CommandBufferAllocateInfo::new(&mut cp, 1, br::CommandBufferLevel::Primary),
            )
            .expect("cb.create");
            unsafe {
                cb[0]
                    .begin(&br::CommandBufferBeginInfo::new())
                    .expect("cb.begin")
            }
            .inject(|r| {
                gfx.cmd_pipeline_barrier(
                    r,
                    &br::DependencyInfo::new(
                        &[],
                        &[],
                        &[br::ImageMemoryBarrier2::new(
                            br::VkHandleRef::from_raw_ref(&self.backdrop_buffers[0].0),
                            br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
                        )
                        .transit_to(br::ImageLayout::ShaderReadOnlyOpt.from_undefined())],
                    ),
                )
            })
            .end()
            .expect("cb.end");
            let mut q = gfx.queue(gfx.present_queue_family_index(), 0);
            unsafe {
                q.submit_raw(
                    &[br::SubmitInfo::new(
                        &[],
                        &[],
                        &[cb[0].as_transparent_ref()],
                        &[],
                    )],
                    None,
                )
                .expect("cb.submit");
            }
            q.wait().expect("cb.submit.wait");
        }

        self.backdrop_buffers_invalidated = false;
        true
    }

    pub fn rebind_glyph_atlas<'r>(
        &'r self,
        new_atlas: &'r (impl br::VkHandle<Handle = br::vk::VkImageView> + ?Sized),
        descriptor_writes: &mut Vec<br::DescriptorSetWriteInfo<'r>>,
    ) {
        descriptor_writes.push(CompositeDescriptorSet::set_texture_atlas(
            self.alphamask_group_input_descriptor_set,
            br::DescriptorImageInfo::new(new_atlas, br::ImageLayout::ShaderReadOnlyOpt)
                .with_sampler(br::VkHandleRef::from_raw_ref(&self.sampler)),
        ));
    }

    #[inline]
    pub fn default_backdrop_buffer(
        &self,
    ) -> &(impl br::VkHandle<Handle = br::vk::VkImage> + ?Sized) {
        br::VkHandleRef::from_raw_ref(&self.backdrop_buffers[0].0)
    }

    pub fn select_subpass<'r>(
        &'r self,
        requirements: &RenderPassRequirements,
    ) -> br::SubpassRef<'r, impl br::VkHandle<Handle = br::vk::VkRenderPass> + ?Sized> {
        match requirements {
            RenderPassRequirements {
                after_operation: RenderPassAfterOperation::None,
                continued: false,
            } => br::SubpassRef(br::VkHandleRef::from_raw_ref(&self.rp_final), 0),
            RenderPassRequirements {
                after_operation: RenderPassAfterOperation::None,
                continued: true,
            } => br::SubpassRef(br::VkHandleRef::from_raw_ref(&self.rp_continue_final), 0),
            RenderPassRequirements {
                after_operation: RenderPassAfterOperation::Grab,
                continued: false,
            } => br::SubpassRef(br::VkHandleRef::from_raw_ref(&self.rp_grabbed), 0),
            RenderPassRequirements {
                after_operation: RenderPassAfterOperation::Grab,
                continued: true,
            } => br::SubpassRef(br::VkHandleRef::from_raw_ref(&self.rp_continue_grabbed), 0),
        }
    }

    #[inline]
    pub fn subpass_final<'r>(
        &'r self,
    ) -> br::SubpassRef<'r, impl br::VkHandle<Handle = br::vk::VkRenderPass> + ?Sized> {
        br::SubpassRef(br::VkHandleRef::from_raw_ref(&self.rp_final), 0)
    }

    #[inline]
    pub fn subpass_continue_final<'r>(
        &'r self,
    ) -> br::SubpassRef<'r, impl br::VkHandle<Handle = br::vk::VkRenderPass> + ?Sized> {
        br::SubpassRef(br::VkHandleRef::from_raw_ref(&self.rp_continue_final), 0)
    }

    pub fn populate_commands<'x>(
        &self,
        mut rec: br::CmdRecord<'x>,
        gfx: &VulkanDevice,
        render_data: &CompositeRenderingData,
        rt_size: br::Extent2D,
        rt_image: &(impl br::VkHandle<Handle = br::vk::VkImage> + ?Sized),
        backbuffer_index: usize,
        mut custom_render: impl FnMut(CustomRenderToken, br::CmdRecord<'x>) -> br::CmdRecord<'x>,
    ) -> br::CmdRecord<'x> {
        let render_region = rt_size.into_rect(br::Offset2D::ZERO);

        let mut in_render_pass = false;
        let mut rpt_pointer = 0;
        let mut pipeline_bound = false;

        #[inline]
        fn ensure_in_render_pass<'r>(
            this: &CompositeRenderer,
            gfx: &VulkanDevice,
            in_render_pass: &mut bool,
            render_data: &CompositeRenderingData,
            rpt_pointer: usize,
            backbuffer_index: usize,
            render_region: br::Rect2D,
            rec: br::CmdRecord<'r>,
        ) -> br::CmdRecord<'r> {
            if *in_render_pass {
                return rec;
            }

            *in_render_pass = true;

            let (rp, fb);
            match &render_data.render_passes[rpt_pointer] {
                RenderPassRequirements {
                    continued: false,
                    after_operation: RenderPassAfterOperation::Grab,
                } => {
                    rp = br::VkHandleRef::from_raw_ref(&this.rp_grabbed);
                    fb = this.fbs_grabbed[backbuffer_index];
                }
                RenderPassRequirements {
                    continued: false,
                    after_operation: RenderPassAfterOperation::None,
                } => {
                    rp = br::VkHandleRef::from_raw_ref(&this.rp_final);
                    fb = this.fbs_final[backbuffer_index];
                }
                RenderPassRequirements {
                    continued: true,
                    after_operation: RenderPassAfterOperation::Grab,
                } => {
                    rp = br::VkHandleRef::from_raw_ref(&this.rp_continue_grabbed);
                    fb = this.fbs_continue_grabbed[backbuffer_index];
                }
                RenderPassRequirements {
                    continued: true,
                    after_operation: RenderPassAfterOperation::None,
                } => {
                    rp = br::VkHandleRef::from_raw_ref(&this.rp_continue_final);
                    fb = this.fbs_continue_final[backbuffer_index];
                }
            };

            rec.inject(|r| {
                gfx.cmd_begin_render_pass(
                    r,
                    &br::RenderPassBeginInfo::new(
                        rp,
                        br::VkHandleRef::from_raw_ref(&fb),
                        render_region,
                        &[br::ClearValue::color_f32([0.0, 0.0, 0.0, 1.0])],
                    ),
                )
            })
        }

        #[inline]
        fn ensure_pipeline_bound<'r>(
            this: &CompositeRenderer,
            pipeline_bound: &mut bool,
            render_data: &CompositeRenderingData,
            rpt_pointer: usize,
            rt_size: br::Extent2D,
            rec: br::CmdRecord<'r>,
        ) -> br::CmdRecord<'r> {
            if *pipeline_bound {
                return rec;
            }

            *pipeline_bound = true;

            rec.bind_pipeline(
                br::PipelineBindPoint::Graphics,
                match &render_data.render_passes[rpt_pointer] {
                    RenderPassRequirements {
                        continued: false,
                        after_operation: RenderPassAfterOperation::Grab,
                    } => br::VkHandleRef::from_raw_ref(&this.pipeline_grabbed),
                    RenderPassRequirements {
                        continued: false,
                        after_operation: RenderPassAfterOperation::None,
                    } => br::VkHandleRef::from_raw_ref(&this.pipeline_final),
                    RenderPassRequirements {
                        continued: true,
                        after_operation: RenderPassAfterOperation::Grab,
                    } => br::VkHandleRef::from_raw_ref(&this.pipeline_continue_grabbed),
                    RenderPassRequirements {
                        continued: true,
                        after_operation: RenderPassAfterOperation::None,
                    } => br::VkHandleRef::from_raw_ref(&this.pipeline_continue_final),
                },
            )
            .push_constant(
                br::VkHandleRef::from_raw_ref(&this.pipeline_layout),
                br::vk::VK_SHADER_STAGE_ALL_GRAPHICS,
                0,
                &[rt_size.width as f32, rt_size.height as f32],
            )
            .bind_descriptor_sets(
                br::PipelineBindPoint::Graphics,
                br::VkHandleRef::from_raw_ref(&this.pipeline_layout),
                0,
                &[this.alphamask_group_input_descriptor_set],
                &[],
            )
        }

        for x in render_data.instructions.iter() {
            match x {
                &CompositeRenderingInstruction::DrawInstanceRange {
                    ref index_range,
                    backdrop_buffer,
                } => {
                    rec = ensure_in_render_pass(
                        self,
                        gfx,
                        &mut in_render_pass,
                        render_data,
                        rpt_pointer,
                        backbuffer_index,
                        render_region,
                        rec,
                    )
                    .inject(|rec| {
                        ensure_pipeline_bound(
                            self,
                            &mut pipeline_bound,
                            render_data,
                            rpt_pointer,
                            rt_size,
                            rec,
                        )
                    })
                    .bind_descriptor_sets(
                        br::PipelineBindPoint::Graphics,
                        br::VkHandleRef::from_raw_ref(&self.pipeline_layout),
                        1,
                        &[self.input_backdrop_descriptor_sets[backdrop_buffer]],
                        &[],
                    )
                    .draw(4, index_range.len() as _, 0, index_range.start as _)
                }
                &CompositeRenderingInstruction::InsertCustomRenderCommands(token) => {
                    rec = ensure_in_render_pass(
                        self,
                        gfx,
                        &mut in_render_pass,
                        render_data,
                        rpt_pointer,
                        backbuffer_index,
                        render_region,
                        rec,
                    );

                    rec = custom_render(token, rec);

                    // 別のパイプラインをつかっている可能性があるのでいったん紐づいているのを無効化する
                    pipeline_bound = false;
                }
                &CompositeRenderingInstruction::SetClip {
                    ref shader_parameters,
                } => {
                    rec = ensure_in_render_pass(
                        self,
                        gfx,
                        &mut in_render_pass,
                        render_data,
                        rpt_pointer,
                        backbuffer_index,
                        render_region,
                        rec,
                    )
                    .inject(|rec| {
                        ensure_pipeline_bound(
                            self,
                            &mut pipeline_bound,
                            render_data,
                            rpt_pointer,
                            rt_size,
                            rec,
                        )
                    })
                    .push_constant(
                        br::VkHandleRef::from_raw_ref(&self.pipeline_layout),
                        br::vk::VK_SHADER_STAGE_ALL_GRAPHICS,
                        core::mem::offset_of!(CompositePushConstants, rect_mask_left) as _,
                        &[
                            shader_parameters[0].value() / rt_size.width as f32,
                            shader_parameters[1].value() / rt_size.height as f32,
                            shader_parameters[2].value() / rt_size.width as f32,
                            shader_parameters[3].value() / rt_size.height as f32,
                            shader_parameters[4].value() / rt_size.width as f32,
                            shader_parameters[5].value() / rt_size.height as f32,
                            shader_parameters[6].value() / rt_size.width as f32,
                            shader_parameters[7].value() / rt_size.height as f32,
                        ],
                    );
                }
                &CompositeRenderingInstruction::ClearClip => {
                    rec = ensure_in_render_pass(
                        self,
                        gfx,
                        &mut in_render_pass,
                        render_data,
                        rpt_pointer,
                        backbuffer_index,
                        render_region,
                        rec,
                    )
                    .inject(|rec| {
                        ensure_pipeline_bound(
                            self,
                            &mut pipeline_bound,
                            render_data,
                            rpt_pointer,
                            rt_size,
                            rec,
                        )
                    })
                    .push_constant(
                        br::VkHandleRef::from_raw_ref(&self.pipeline_layout),
                        br::vk::VK_SHADER_STAGE_ALL_GRAPHICS,
                        core::mem::offset_of!(CompositePushConstants, rect_mask_left) as _,
                        &[0.0f32, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0],
                    );
                }
                CompositeRenderingInstruction::GrabBackdrop => {
                    rec = rec
                        .inject(|r| gfx.cmd_end_render_pass(r))
                        .inject(|r| {
                            gfx.cmd_pipeline_barrier(
                                r,
                                &br::DependencyInfo::new(
                                    &[],
                                    &[],
                                    &[br::ImageMemoryBarrier2::new(
                                        br::VkHandleRef::from_raw_ref(&self.grab_buffer),
                                        br::ImageSubresourceRange::new(
                                            br::AspectMask::COLOR,
                                            0..1,
                                            0..1,
                                        ),
                                    )
                                    .transit_to(br::ImageLayout::TransferDestOpt.from_undefined())],
                                ),
                            )
                        })
                        .copy_image(
                            rt_image,
                            br::ImageLayout::TransferSrcOpt,
                            br::VkHandleRef::from_raw_ref(&self.grab_buffer),
                            br::ImageLayout::TransferDestOpt,
                            &[br::ImageCopy {
                                srcSubresource: br::ImageSubresourceLayers::new(
                                    br::AspectMask::COLOR,
                                    0,
                                    0..1,
                                ),
                                dstSubresource: br::ImageSubresourceLayers::new(
                                    br::AspectMask::COLOR,
                                    0,
                                    0..1,
                                ),
                                srcOffset: br::Offset3D::ZERO,
                                dstOffset: br::Offset3D::ZERO,
                                extent: rt_size.with_depth(1),
                            }],
                        )
                        .inject(|r| {
                            gfx.cmd_pipeline_barrier(
                                r,
                                &br::DependencyInfo::new(
                                    &[],
                                    &[],
                                    &[br::ImageMemoryBarrier2::new(
                                        br::VkHandleRef::from_raw_ref(&self.grab_buffer),
                                        br::ImageSubresourceRange::new(
                                            br::AspectMask::COLOR,
                                            0..1,
                                            0..1,
                                        ),
                                    )
                                    .transit_from(
                                        br::ImageLayout::TransferDestOpt
                                            .to(br::ImageLayout::ShaderReadOnlyOpt),
                                    )
                                    .from(
                                        br::PipelineStageFlags2::COPY,
                                        br::AccessFlags2::TRANSFER.write,
                                    )
                                    .to(
                                        br::PipelineStageFlags2::FRAGMENT_SHADER,
                                        br::AccessFlags2::SHADER.read,
                                    )],
                                ),
                            )
                        });
                    rpt_pointer += 1;
                    in_render_pass = false;
                    pipeline_bound = false;
                }
                &CompositeRenderingInstruction::GenerateBackdropBlur {
                    stdev,
                    dest_backdrop_buffer,
                    // 本来は必要な範囲だけ処理できれば効率いいんだけど面倒なので全面処理しちゃう
                    ..
                } => {
                    rec = self.backdrop_fx_blur_processor.populate_commands(
                        rec,
                        stdev,
                        br::VkHandleRef::from_raw_ref(
                            &self.backdrop_blur_destination_fbs[dest_backdrop_buffer],
                        ),
                        gfx,
                        rt_size,
                        &self.blur_fixed_descriptor_sets,
                    );
                }
            };
        }

        rec
    }
}

pub struct BoundCompositeRenderer<'d> {
    core: CompositeRenderer,
    device: &'d VulkanDevice<'d>,
}
impl Drop for BoundCompositeRenderer<'_> {
    fn drop(&mut self) {
        unsafe {
            self.core.drop(self.device);
        }
    }
}
impl core::ops::Deref for BoundCompositeRenderer<'_> {
    type Target = CompositeRenderer;

    fn deref(&self) -> &Self::Target {
        &self.core
    }
}
impl core::ops::DerefMut for BoundCompositeRenderer<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.core
    }
}
impl<'d> BoundCompositeRenderer<'d> {
    pub fn new<'b>(
        device: &'d VulkanDevice,
        mask_atlas: br::VkHandleRef<br::vk::VkImageView>,
        rt_format: br::Format,
        rt_size: br::Extent2D,
        back_buffer_views: impl Iterator<Item = br::VkHandleRef<'b, br::vk::VkImageView>>,
    ) -> Self {
        Self {
            core: CompositeRenderer::new(device, mask_atlas, rt_format, back_buffer_views, rt_size),
            device,
        }
    }
}

struct BackdropEffectBlurProcessor {
    render_pass: br::vk::VkRenderPass,
    temporal_buffers: Vec<(br::vk::VkImage, br::vk::VkImageView)>,
    temporal_buffer_memory: br::vk::VkDeviceMemory,
    downsample_pass_fbs: Vec<br::vk::VkFramebuffer>,
    upsample_pass_fixed_fbs: Vec<br::vk::VkFramebuffer>,
    sampler: br::vk::VkSampler,
    input_dsl: br::vk::VkDescriptorSetLayout,
    pipeline_layout: br::vk::VkPipelineLayout,
    downsample_pipelines: Vec<br::vk::VkPipeline>,
    upsample_pipelines: Vec<br::vk::VkPipeline>,
}
impl BackdropEffectBlurProcessor {
    unsafe fn drop(&mut self, gfx: &VulkanDevice) {
        unsafe {
            self.destroy_framebuffers(gfx);

            for x in self.upsample_pipelines.drain(..) {
                br::vkfn::destroy_pipeline(gfx.native_ptr(), x, core::ptr::null());
            }
            for x in self.downsample_pipelines.drain(..) {
                br::vkfn::destroy_pipeline(gfx.native_ptr(), x, core::ptr::null());
            }
            br::vkfn::destroy_pipeline_layout(
                gfx.native_ptr(),
                self.pipeline_layout,
                core::ptr::null(),
            );
            br::vkfn::destroy_descriptor_set_layout(
                gfx.native_ptr(),
                self.input_dsl,
                core::ptr::null(),
            );
            br::vkfn::destroy_sampler(gfx.native_ptr(), self.sampler, core::ptr::null());

            for (r, v) in self.temporal_buffers.drain(..) {
                br::vkfn::destroy_image_view(gfx.native_ptr(), v, core::ptr::null());
                br::vkfn::destroy_image(gfx.native_ptr(), r, core::ptr::null());
            }
            br::vkfn::free_memory(
                gfx.native_ptr(),
                self.temporal_buffer_memory,
                core::ptr::null(),
            );

            br::vkfn::destroy_render_pass(gfx.native_ptr(), self.render_pass, core::ptr::null());
        }
    }

    #[tracing::instrument(name = "BackdropEffectBlurProcessor::new", skip(gfx))]
    fn new(gfx: &VulkanDevice, rt_size: br::Extent2D, rt_format: br::Format) -> Self {
        let render_pass = gfx
            .create_render_pass(&br::RenderPassCreateInfo2::new(
                &[br::AttachmentDescription2::new(rt_format)
                    .with_layout_to(br::ImageLayout::ShaderReadOnlyOpt.from_undefined())
                    .color_memory_op(br::LoadOp::DontCare, br::StoreOp::Store)],
                &[br::SubpassDescription2::new()
                    .colors(&[br::AttachmentReference2::color_attachment_opt(0)])],
                &[br::SubpassDependency2::new(
                    br::SubpassIndex::Internal(0),
                    br::SubpassIndex::External,
                )
                .of_memory(
                    br::AccessFlags::COLOR_ATTACHMENT.write,
                    br::AccessFlags::SHADER.read,
                )
                .of_execution(
                    br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
                    br::PipelineStageFlags::FRAGMENT_SHADER,
                )],
            ))
            .unwrap();
        gfx.dbg_set_name(
            &render_pass,
            c"Composite BackdropFx(Blur) ProcessRenderPass",
        );

        let mut temporal_buffers = Vec::with_capacity(BLUR_SAMPLE_STEPS);
        let temporal_buffer_memory =
            Self::create_temporal_buffers(gfx, rt_size, rt_format, &mut temporal_buffers);

        let (downsample_pass_fbs, upsample_pass_fixed_fbs) =
            Self::create_framebuffers(gfx, &temporal_buffers, &render_pass, rt_size);

        let sampler = match br::SamplerObject::new(
            gfx,
            &br::SamplerCreateInfo::new()
                .filter(br::FilterMode::Linear, br::FilterMode::Linear)
                .addressing(
                    br::AddressingMode::MirroredRepeat,
                    br::AddressingMode::MirroredRepeat,
                    br::AddressingMode::MirroredRepeat,
                ),
        ) {
            Ok(x) => x,
            Err(e) => {
                tracing::error!(reason = ?e, "blur sampler creation failed");
                std::process::abort();
            }
        };
        let input_dsl = match br::DescriptorSetLayoutObject::new(
            gfx,
            &br::DescriptorSetLayoutCreateInfo::new(&[br::DescriptorType::CombinedImageSampler
                .make_binding(0, 1)
                .only_for_fragment()
                .with_immutable_samplers(&[sampler.as_transparent_ref()])]),
        ) {
            Ok(x) => x,
            Err(e) => {
                tracing::error!(reason = ?e, "blur input dsl creation failed");
                std::process::abort();
            }
        };

        let pipeline_layout = match br::PipelineLayoutObject::new(
            gfx,
            &br::PipelineLayoutCreateInfo::new(
                &[input_dsl.as_transparent_ref()],
                &[br::PushConstantRange::for_type::<[f32; 3]>(
                    br::vk::VK_SHADER_STAGE_VERTEX_BIT,
                    0,
                )],
            ),
        ) {
            Ok(x) => x,
            Err(e) => {
                tracing::error!(reason = ?e, "pipeline layout creation failed");
                std::process::abort();
            }
        };

        let (downsample_pipelines, upsample_pipelines) =
            Self::create_pipelines(gfx, rt_size, &pipeline_layout, &render_pass);

        Self {
            downsample_pass_fbs: downsample_pass_fbs
                .into_iter()
                .map(|x| x.unmanage().0)
                .collect(),
            upsample_pass_fixed_fbs: upsample_pass_fixed_fbs
                .into_iter()
                .map(|x| x.unmanage().0)
                .collect(),
            render_pass: render_pass.unmanage().0,
            temporal_buffers: temporal_buffers
                .into_iter()
                .map(|x| {
                    let (v, r) = x.unmanage();
                    (r.unmanage().0, v)
                })
                .collect(),
            temporal_buffer_memory: temporal_buffer_memory.unmanage().0,
            sampler: sampler.unmanage().0,
            input_dsl: input_dsl.unmanage().0,
            pipeline_layout: pipeline_layout.unmanage().0,
            downsample_pipelines: downsample_pipelines
                .into_iter()
                .map(|x| x.unmanage().0)
                .collect(),
            upsample_pipelines: upsample_pipelines
                .into_iter()
                .map(|x| x.unmanage().0)
                .collect(),
        }
    }

    fn create_pipelines<'x>(
        gfx: &'x VulkanDevice<'x>,
        rt_size: br::Extent2D,
        pipeline_layout: &(impl br::VkHandle<Handle = br::vk::VkPipelineLayout> + ?Sized),
        render_pass: &(impl br::VkHandle<Handle = br::vk::VkRenderPass> + ?Sized),
    ) -> (
        Vec<br::PipelineObject<&'x VulkanDevice<'x>>>,
        Vec<br::PipelineObject<&'x VulkanDevice<'x>>>,
    ) {
        let downsample_shader = gfx.require_shader("dual_kawase_filter/downsample.spv");
        let upsample_shader = gfx.require_shader("dual_kawase_filter/upsample.spv");
        let downsample_stages = [
            downsample_shader.on_stage(br::ShaderStage::Vertex, c"vertMain"),
            downsample_shader.on_stage(br::ShaderStage::Fragment, c"fragMain"),
        ];
        let upsample_stages = [
            upsample_shader.on_stage(br::ShaderStage::Vertex, c"vertMain"),
            upsample_shader.on_stage(br::ShaderStage::Fragment, c"fragMain"),
        ];

        let viewport_scissors = (0..=BLUR_SAMPLE_STEPS)
            .map(|lv| {
                let size = br::Extent2D {
                    width: rt_size.width >> lv,
                    height: rt_size.height >> lv,
                };

                (
                    [size.into_rect(br::Offset2D::ZERO).make_viewport(0.0..1.0)],
                    [size.into_rect(br::Offset2D::ZERO)],
                )
            })
            .collect::<Vec<_>>();
        let viewport_states = viewport_scissors
            .iter()
            .map(|(vp, sc)| br::PipelineViewportStateCreateInfo::new(vp, sc))
            .collect::<Vec<_>>();
        let downsample_pipelines = gfx
            .create_graphics_pipelines(
                &viewport_states[1..]
                    .iter()
                    .map(|vp_state| {
                        br::GraphicsPipelineCreateInfo::new(
                            &pipeline_layout,
                            br::SubpassRef(render_pass, 0),
                            &downsample_stages,
                            VI_STATE_EMPTY,
                            IA_STATE_TRILIST,
                            vp_state,
                            RASTER_STATE_DEFAULT_FILL_NOCULL,
                            BLEND_STATE_SINGLE_NONE,
                        )
                        .set_multisample_state(MS_STATE_EMPTY)
                    })
                    .collect::<Vec<_>>(),
            )
            .unwrap();
        let upsample_pipelines = gfx
            .create_graphics_pipelines(
                &viewport_states[..viewport_states.len() - 1]
                    .iter()
                    .map(|vp_state| {
                        br::GraphicsPipelineCreateInfo::new(
                            &pipeline_layout,
                            br::SubpassRef(render_pass, 0),
                            &upsample_stages,
                            VI_STATE_EMPTY,
                            IA_STATE_TRILIST,
                            vp_state,
                            RASTER_STATE_DEFAULT_FILL_NOCULL,
                            BLEND_STATE_SINGLE_NONE,
                        )
                        .set_multisample_state(MS_STATE_EMPTY)
                    })
                    .collect::<Vec<_>>(),
            )
            .unwrap();

        (downsample_pipelines, upsample_pipelines)
    }

    fn create_temporal_buffers<'x>(
        gfx: &'x VulkanDevice<'x>,
        rt_size: br::Extent2D,
        rt_format: br::Format,
        object_sink: &mut Vec<br::ImageViewObject<br::ImageObject<&'x VulkanDevice<'x>>>>,
    ) -> br::DeviceMemoryObject<&'x VulkanDevice<'x>> {
        let mut resources_offsets = Vec::with_capacity(BLUR_SAMPLE_STEPS);
        let mut top = 0;
        let mut memory_index_mask = !0u32;
        for lv in 1..=BLUR_SAMPLE_STEPS {
            let r = br::ImageObject::new(
                gfx,
                &br::ImageCreateInfo::new(
                    br::Extent2D {
                        width: rt_size.width >> lv,
                        height: rt_size.height >> lv,
                    },
                    rt_format,
                )
                .with_usage(br::ImageUsageFlags::SAMPLED | br::ImageUsageFlags::COLOR_ATTACHMENT),
            )
            .unwrap();
            let req = r.requirements();
            assert!(req.alignment.is_power_of_two());
            let offset = (top + req.alignment - 1) & !(req.alignment - 1);

            top = offset + req.size;
            memory_index_mask &= req.memoryTypeBits;
            resources_offsets.push((r, offset));
        }
        let memory_object = gfx.alloc_device_local_memory(top, memory_index_mask);
        for (mut r, o) in resources_offsets {
            r.bind(&memory_object, o as _).unwrap();

            object_sink.push(
                br::ImageViewBuilder::new(
                    r,
                    br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
                )
                .create()
                .unwrap(),
            );
        }

        memory_object
    }

    /// returns: (downsample, upsample_fixed(only for temporal buffers))
    fn create_framebuffers<'r, 'x>(
        gfx: &'x VulkanDevice<'x>,
        temporal_buffers: &'r [br::ImageViewObject<br::ImageObject<&'x VulkanDevice<'x>>>],
        render_pass: &(impl br::VkHandle<Handle = br::vk::VkRenderPass> + ?Sized),
        rt_size: br::Extent2D,
    ) -> (
        Vec<br::FramebufferObject<'r, &'x VulkanDevice<'x>>>,
        Vec<br::FramebufferObject<'r, &'x VulkanDevice<'x>>>,
    ) {
        let mut downsample_pass_fbs = Vec::with_capacity(temporal_buffers.len());
        let mut upsample_pass_fixed_fbs = Vec::with_capacity(temporal_buffers.len() - 1);
        for (n, b) in temporal_buffers.iter().enumerate() {
            let lv = n + 1;
            let bufsize = br::Extent2D {
                width: rt_size.width >> lv,
                height: rt_size.height >> lv,
            };

            downsample_pass_fbs.push(
                br::FramebufferObject::new(
                    gfx,
                    &br::FramebufferCreateInfo::new(
                        render_pass,
                        &[b.as_transparent_ref()],
                        bufsize.width,
                        bufsize.height,
                    ),
                )
                .unwrap(),
            );
            if lv != temporal_buffers.len() {
                upsample_pass_fixed_fbs.push(
                    br::FramebufferObject::new(
                        gfx,
                        &br::FramebufferCreateInfo::new(
                            render_pass,
                            &[b.as_transparent_ref()],
                            bufsize.width,
                            bufsize.height,
                        ),
                    )
                    .unwrap(),
                );
            }
        }

        (downsample_pass_fbs, upsample_pass_fixed_fbs)
    }

    unsafe fn destroy_framebuffers(&self, gfx: &VulkanDevice) {
        for &x in self.downsample_pass_fbs.iter() {
            unsafe {
                br::vkfn::destroy_framebuffer(gfx.native_ptr(), x, core::ptr::null());
            }
        }
        for &x in self.upsample_pass_fixed_fbs.iter() {
            unsafe {
                br::vkfn::destroy_framebuffer(gfx.native_ptr(), x, core::ptr::null());
            }
        }
    }

    pub const fn fixed_descriptor_set_count(&self) -> usize {
        BLUR_SAMPLE_STEPS + 1
    }

    pub fn alloc_fixed_descriptor_sets(
        &self,
        dp: &mut (impl br::DescriptorPoolMut + ?Sized),
    ) -> Vec<br::DescriptorSet> {
        dp.alloc(
            &core::iter::repeat_n(
                unsafe { br::VkHandleRef::dangling(self.input_dsl) },
                self.fixed_descriptor_set_count(),
            )
            .collect::<Vec<_>>(),
        )
        .unwrap()
    }

    pub fn write_input_descriptor_sets<'s>(
        &'s self,
        writes: &mut Vec<br::DescriptorSetWriteInfo<'s>>,
        first_input: &'s (impl br::VkHandle<Handle = br::vk::VkImageView> + ?Sized),
        descriptor_sets: &[br::DescriptorSet],
    ) {
        writes.reserve(1 + BLUR_SAMPLE_STEPS);
        self.write_first_input_descriptor_set(writes, first_input, descriptor_sets[0]);
        writes.extend((0..BLUR_SAMPLE_STEPS).map(|n| {
            descriptor_sets[n + 1].binding_at(0).write(
                br::DescriptorContents::CombinedImageSampler(vec![br::DescriptorImageInfo::new(
                    br::VkHandleRef::from_raw_ref(&self.temporal_buffers[n].1),
                    br::ImageLayout::ShaderReadOnlyOpt,
                )]),
            )
        }));
    }

    pub fn write_first_input_descriptor_set<'s>(
        &'s self,
        writes: &mut Vec<br::DescriptorSetWriteInfo<'s>>,
        first_input: &'s (impl br::VkHandle<Handle = br::vk::VkImageView> + ?Sized),
        descriptor_set: br::DescriptorSet,
    ) {
        writes.push(descriptor_set.binding_at(0).write(
            br::DescriptorContents::CombinedImageSampler(vec![br::DescriptorImageInfo::new(
                first_input,
                br::ImageLayout::ShaderReadOnlyOpt,
            )]),
        ));
    }

    pub const fn final_render_pass<'a>(&'a self) -> br::VkHandleRef<'a, br::vk::VkRenderPass> {
        unsafe { br::VkHandleRef::dangling(self.render_pass) }
    }

    pub fn populate_commands<'x>(
        &self,
        mut rec: br::CmdRecord<'x>,
        mut stdev: SafeF32,
        dest_fb: &(impl br::VkHandle<Handle = br::vk::VkFramebuffer> + ?Sized),
        gfx: &VulkanDevice,
        rt_size: br::Extent2D,
        input_descriptor_sets: &[br::DescriptorSet],
    ) -> br::CmdRecord<'x> {
        let mut step_count = 0;
        // downsample
        for lv in 1..=BLUR_SAMPLE_STEPS {
            rec = rec
                .inject(|r| {
                    gfx.cmd_begin_render_pass(
                        r,
                        &br::RenderPassBeginInfo::new(
                            br::VkHandleRef::from_raw_ref(&self.render_pass),
                            &unsafe { br::VkHandleRef::dangling(self.downsample_pass_fbs[lv - 1]) },
                            br::Extent2D {
                                width: rt_size.width >> lv,
                                height: rt_size.height >> lv,
                            }
                            .into_rect(br::Offset2D::ZERO),
                            &[br::ClearValue::color_f32([0.0, 0.0, 0.0, 0.0])],
                        ),
                    )
                })
                .bind_pipeline(
                    br::PipelineBindPoint::Graphics,
                    br::VkHandleRef::from_raw_ref(&self.downsample_pipelines[lv - 1]),
                )
                .push_constant(
                    br::VkHandleRef::from_raw_ref(&self.pipeline_layout),
                    br::vk::VK_SHADER_STAGE_VERTEX_BIT,
                    0,
                    &[
                        ((rt_size.width >> (lv - 1)) as f32).recip(),
                        ((rt_size.height >> (lv - 1)) as f32).recip(),
                        stdev.value(),
                    ],
                )
                .bind_descriptor_sets(
                    br::PipelineBindPoint::Graphics,
                    br::VkHandleRef::from_raw_ref(&self.pipeline_layout),
                    0,
                    &[input_descriptor_sets[lv - 1]],
                    &[],
                )
                .draw(3, 1, 0, 0)
                .inject(|r| gfx.cmd_end_render_pass(r));

            step_count += 1;
            stdev = unsafe { SafeF32::new_unchecked(stdev.value() / 2.0) };
            if stdev.value() < 0.5 {
                break;
            }
        }
        // upsample
        for lv in (0..step_count).rev() {
            rec = rec
                .inject(|r| {
                    gfx.cmd_begin_render_pass(
                        r,
                        &br::RenderPassBeginInfo::new(
                            br::VkHandleRef::from_raw_ref(&self.render_pass),
                            &if lv == 0 {
                                // final upsample
                                dest_fb.as_transparent_ref()
                            } else {
                                unsafe {
                                    br::VkHandleRef::dangling(self.upsample_pass_fixed_fbs[lv - 1])
                                }
                            },
                            br::Extent2D {
                                width: rt_size.width >> lv,
                                height: rt_size.height >> lv,
                            }
                            .into_rect(br::Offset2D::ZERO),
                            &[br::ClearValue::color_f32([0.0, 0.0, 0.0, 0.0])],
                        ),
                    )
                })
                .bind_pipeline(
                    br::PipelineBindPoint::Graphics,
                    br::VkHandleRef::from_raw_ref(&self.upsample_pipelines[lv]),
                )
                .push_constant(
                    br::VkHandleRef::from_raw_ref(&self.pipeline_layout),
                    br::vk::VK_SHADER_STAGE_VERTEX_BIT,
                    0,
                    &[
                        ((rt_size.width >> (lv + 1)) as f32).recip(),
                        ((rt_size.height >> (lv + 1)) as f32).recip(),
                        stdev.value(),
                    ],
                )
                .bind_descriptor_sets(
                    br::PipelineBindPoint::Graphics,
                    br::VkHandleRef::from_raw_ref(&self.pipeline_layout),
                    0,
                    &[input_descriptor_sets[lv + 1]],
                    &[],
                )
                .draw(3, 1, 0, 0)
                .inject(|r| gfx.cmd_end_render_pass(r));

            stdev = unsafe { SafeF32::new_unchecked(stdev.value() * 2.0) };
        }

        rec
    }

    #[tracing::instrument(
        name = "BackdropEffectBlurProcessor::recreate_rt_resources",
        skip(self, gfx)
    )]
    pub fn recreate_rt_resources(
        &mut self,
        gfx: &VulkanDevice,
        rt_size: br::Extent2D,
        rt_format: br::Format,
    ) {
        unsafe {
            for x in self.downsample_pipelines.drain(..) {
                br::vkfn::destroy_pipeline(gfx.native_ptr(), x, core::ptr::null());
            }
            for x in self.upsample_pipelines.drain(..) {
                br::vkfn::destroy_pipeline(gfx.native_ptr(), x, core::ptr::null());
            }
        }
        let (downsample_pipelines, upsample_pipelines) = Self::create_pipelines(
            gfx,
            rt_size,
            br::VkHandleRef::from_raw_ref(&self.pipeline_layout),
            br::VkHandleRef::from_raw_ref(&self.render_pass),
        );

        unsafe {
            for x in self.downsample_pass_fbs.drain(..) {
                br::vkfn::destroy_framebuffer(gfx.native_ptr(), x, core::ptr::null());
            }
            for x in self.upsample_pass_fixed_fbs.drain(..) {
                br::vkfn::destroy_framebuffer(gfx.native_ptr(), x, core::ptr::null());
            }

            for (r, v) in self.temporal_buffers.drain(..) {
                br::vkfn::destroy_image_view(gfx.native_ptr(), v, core::ptr::null());
                br::vkfn::destroy_image(gfx.native_ptr(), r, core::ptr::null());
            }
            br::vkfn::free_memory(
                gfx.native_ptr(),
                self.temporal_buffer_memory,
                core::ptr::null(),
            );
        }
        let mut temporal_buffers = Vec::with_capacity(self.temporal_buffers.capacity());
        let temporal_buffer_memory =
            Self::create_temporal_buffers(gfx, rt_size, rt_format, &mut temporal_buffers);
        let (downsample_pass_fbs, upsample_pass_fixed_fbs) = Self::create_framebuffers(
            gfx,
            &temporal_buffers,
            br::VkHandleRef::from_raw_ref(&self.render_pass),
            rt_size,
        );

        self.downsample_pass_fbs
            .extend(downsample_pass_fbs.into_iter().map(|x| x.unmanage().0));
        self.upsample_pass_fixed_fbs
            .extend(upsample_pass_fixed_fbs.into_iter().map(|x| x.unmanage().0));
        self.downsample_pipelines
            .extend(downsample_pipelines.into_iter().map(|x| x.unmanage().0));
        self.upsample_pipelines
            .extend(upsample_pipelines.into_iter().map(|x| x.unmanage().0));
        self.temporal_buffer_memory = temporal_buffer_memory.unmanage().0;
        self.temporal_buffers
            .extend(temporal_buffers.into_iter().map(|x| {
                let (v, r) = x.unmanage();
                (r.unmanage().0, v)
            }));
    }
}

pub struct CompositionSurfaceAtlas {
    resource: br::vk::VkImage,
    resource_view: br::vk::VkImageView,
    memory: br::vk::VkDeviceMemory,
    residency_bitmap: Vec<u8>,
    format: br::Format,
    size: u32,
    region_manager: DynamicAtlasManager,
}
impl CompositionSurfaceAtlas {
    pub unsafe fn drop(&mut self, gfx: &VulkanDevice) {
        unsafe {
            br::vkfn_wrapper::destroy_image_view(gfx.native_ptr(), self.resource_view, None);
            br::vkfn_wrapper::destroy_image(gfx.native_ptr(), self.resource, None);
            br::vkfn_wrapper::free_memory(gfx.native_ptr(), self.memory, None);
        }
    }

    // TODO: できればPhysical Deviceからとれる値をつかったほうがいい
    // 1024なら大抵は問題ないとは思うが...
    const GRANULARITY: u32 = 1024;

    pub fn new(gfx: &VulkanDevice, size: u32, pixel_format: br::Format) -> Self {
        let bpp = match pixel_format {
            br::vk::VK_FORMAT_R8_UNORM => 1,
            _ => unimplemented!("bpp"),
        };

        let image = match br::ImageObject::new(
            gfx,
            &br::ImageCreateInfo::new(br::Extent2D::spread1(size), pixel_format)
                .with_usage(
                    br::ImageUsageFlags::COLOR_ATTACHMENT
                        | br::ImageUsageFlags::SAMPLED
                        | br::ImageUsageFlags::TRANSFER_DEST,
                )
                .flags(br::ImageFlags::SPARSE_BINDING | br::ImageFlags::SPARSE_RESIDENCY),
        ) {
            Ok(x) => x,
            Err(e) => {
                tracing::error!(reason = ?e, "Failed to create image");
                std::process::abort();
            }
        };
        let resource = match br::ImageViewBuilder::new(
            image,
            br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
        )
        .create()
        {
            Ok(x) => x,
            Err(e) => {
                tracing::error!(reason = ?e, "Failed to create image view");
                std::process::abort();
            }
        };

        assert!(size % Self::GRANULARITY == 0);
        let bitmap_div = size / Self::GRANULARITY;
        let mut residency_bitmap = vec![0; (bitmap_div * bitmap_div) as usize];
        tracing::debug!(
            size,
            granularity = Self::GRANULARITY,
            block_count = bitmap_div * bitmap_div,
            "ComositionSurfaceAtlas management parameters",
        );

        let image_memory_requirements = resource.image().sparse_requirements_alloc();
        for x in image_memory_requirements.iter() {
            tracing::debug!(?x, "image memory requirements");
        }

        let image_memory_requirements = resource.image().requirements();
        tracing::debug!(?image_memory_requirements, "image memory requirements");

        let memory_index =
            match gfx.find_device_local_memory_index(image_memory_requirements.memoryTypeBits) {
                Some(x) => x,
                None => {
                    tracing::error!(
                        memory_type_mask =
                            format!("0x{:08x}", image_memory_requirements.memoryTypeBits),
                        "No suitable memory for surface atlas"
                    );
                    std::process::abort();
                }
            };
        let memory = match br::DeviceMemoryObject::new(
            gfx,
            &br::MemoryAllocateInfo::new(
                (Self::GRANULARITY * Self::GRANULARITY * bpp) as _,
                memory_index,
            ),
        ) {
            Ok(x) => x,
            Err(e) => {
                tracing::error!(
                    size = Self::GRANULARITY * Self::GRANULARITY * bpp,
                    memory_index,
                    reason = ?e,
                    "Failed to allocate first memory block"
                );
                std::process::abort();
            }
        };

        if let Err(e) = unsafe {
            gfx.bind_sparse_raw(
                &[br::vk::VkBindSparseInfo {
                    sType: br::vk::VkBindSparseInfo::TYPE,
                    pNext: core::ptr::null(),
                    waitSemaphoreCount: 0,
                    pWaitSemaphores: core::ptr::null(),
                    signalSemaphoreCount: 0,
                    pSignalSemaphores: core::ptr::null(),
                    bufferBindCount: 0,
                    pBufferBinds: core::ptr::null(),
                    imageBindCount: 1,
                    pImageBinds: [br::vk::VkSparseImageMemoryBindInfo {
                        image: resource.image().native_ptr(),
                        bindCount: 1,
                        pBinds: [br::vk::VkSparseImageMemoryBind {
                            subresource: br::ImageSubresource::new(br::AspectMask::COLOR, 0, 0),
                            offset: br::Offset3D::ZERO,
                            extent: br::Extent2D::spread1(Self::GRANULARITY).with_depth(1),
                            memory: memory.native_ptr(),
                            memoryOffset: 0,
                            flags: 0,
                        }]
                        .as_ptr(),
                    }]
                    .as_ptr(),
                    imageOpaqueBindCount: 0,
                    pImageOpaqueBinds: core::ptr::null(),
                }],
                None,
            )
        } {
            tracing::warn!(reason = ?e, "Failed to bind initial block");
        }
        residency_bitmap[0] = 0x01;

        let mut region_manager = DynamicAtlasManager::new();
        // free entire region
        region_manager.free(AtlasRect {
            left: 0,
            top: 0,
            right: Self::GRANULARITY,
            bottom: Self::GRANULARITY,
        });

        let (memory, _) = memory.unmanage();
        let (resource_view, resource) = resource.unmanage();
        let (resource, _, _, _, _) = resource.unmanage();

        Self {
            resource_view,
            resource,
            memory,
            residency_bitmap,
            size,
            format: pixel_format,
            region_manager,
        }
    }

    pub const fn resource_view_transparent_ref<'x>(
        &'x self,
    ) -> &'x br::VkHandleRef<'x, br::vk::VkImageView> {
        br::VkHandleRef::from_raw_ref(&self.resource_view)
    }

    pub const fn image_transparent_ref<'x>(&'x self) -> &'x br::VkHandleRef<'x, br::vk::VkImage> {
        br::VkHandleRef::from_raw_ref(&self.resource)
    }

    pub const fn size(&self) -> u32 {
        self.size
    }

    pub const fn format(&self) -> br::Format {
        self.format
    }

    pub const fn vk_extent(&self) -> br::Extent2D {
        br::Extent2D::spread1(self.size)
    }

    pub const fn uv_from_pixels(&self, pixels: f32) -> f32 {
        pixels / self.size as f32
    }

    #[tracing::instrument(skip(self), ret(level = tracing::Level::TRACE))]
    pub fn alloc(&mut self, required_width: u32, required_height: u32) -> AtlasRect {
        match self.region_manager.alloc(required_width, required_height) {
            Some(x) => x,
            None => {
                todo!("alloc new tile");
            }
        }
    }

    pub fn free(&mut self, rect: AtlasRect) {
        self.region_manager.free(rect);
    }
}

pub struct BoundCompositionSurfaceAtlas<'d> {
    gfx: &'d VulkanDevice<'d>,
    raw: CompositionSurfaceAtlas,
}
impl Drop for BoundCompositionSurfaceAtlas<'_> {
    fn drop(&mut self) {
        unsafe {
            self.raw.drop(self.gfx);
        }
    }
}
impl<'d> BoundCompositionSurfaceAtlas<'d> {
    #[tracing::instrument(skip(gfx))]
    pub fn new(gfx: &'d VulkanDevice, size: u32, pixel_format: br::vk::VkFormat) -> Self {
        Self {
            raw: CompositionSurfaceAtlas::new(gfx, size, pixel_format),
            gfx,
        }
    }

    pub const fn unbound(self) -> CompositionSurfaceAtlas {
        let raw = unsafe { core::ptr::read(&self.raw) };
        core::mem::forget(self);

        raw
    }
}
