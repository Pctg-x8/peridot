
struct PushConstants {
  float2 screenSize;
  float4 rectMaskInScreenUV;
  float4 rectMaskSoftnessInScreenUV;
}

[vk::push_constant]
PushConstants pushConstants;

/// composite mode constant(mapped with composite.rs)
enum CompositeMode : uint {
  DirectSourceOver,
  ColorTint,
  FillColor,
  ColorTintBackdropBlur,
  FillColorBackdropBlur,
  FillLinearGradient,
  FillRadialGradient,
  ColorPickerGradientBox,
}
extension CompositeMode {
  property requiresBackdropComposition : bool {
    inline get {
      return this == CompositeMode.ColorTintBackdropBlur ||
             this == CompositeMode.FillColorBackdropBlur;
    }
  }
}

enum TextureMappingMode : uint { Stretch, Repeat }

struct CompositeInstanceData {
  /// scale_x(width), scale_y(height), translate_x(left), translate_y(top)
  float4 posSTStatic;
  float4 uvSTStatic;
  float4x4 positionModifierMatrix;
  /// left, top, right, bottom (pixels from edge)
  float4 sliceBorders;
  /// tex_size_w_px, tex_size_h_px, composite_mode, opacity
  float4 miscValues;
  float4 colorTint;
  /// lt, rt, lb, rb (in pixels)
  float4 cornerRadiusX;
  /// lt, rt, lb, rb (in pixels)
  float4 cornerRadiusY;
  float4 borderColor;
  /// border_thickness, softedge, gradientDataIndex, sourceTextureIndex
  float4 miscValues2;
  /// border_break_pattern_x, border_break_pattern_y, textureMappingMode, ..
  float4 miscValues3;

  property texSlicedSizedPixels : float2 {
    inline get { return this.uvSTStatic.xy * this.miscValues.xy; }
  }

  property borderThickness : float {
    inline get { return this.miscValues2.x; }
  }

  property softedge : float {
    inline get { return this.miscValues2.y; }
  }

  inline func globalUV(p01: float2)->float2 {
    return p01 * this.uvSTStatic.xy + this.uvSTStatic.zw;
  }

  private inline func computeAnimatedValue(currentSec: float, fromValue: float,
                                           toValue: float, curvePos1: float2,
                                           curvePos2: float2, startSec: float,
                                           endSec: float)
      ->float {
    return lerp(fromValue, toValue,
                interpolateCurve(curvePos1, curvePos2,
                                 timeRemap(startSec, endSec, currentSec)));
  }
}

struct GradientData {
  float4 startColor;
  float4 endColor;
  float4 params;
}

struct StreamingData {
  float currentSec;
}

[vk::binding(0, 0)]
StructuredBuffer<CompositeInstanceData> instanceDataArray;
[vk::binding(1, 0)]
StructuredBuffer<GradientData> gradientDataArray;
[vk::binding(2, 0)]
ConstantBuffer<StreamingData> streamingData;
[vk::binding(3, 0)]
Sampler2D<float4> texture[2];
[vk::binding(0, 1)]
Sampler2D<float4> backdropTexture;

struct VertexOutput {
  float4 pos : SV_Position;
  FragmentInput fragmentInput : Varyings;
}

[shader("vertex")]
func vertMain(uint vindex: SV_VertexID, uint iindexOffset: SV_InstanceID,
              uint iindexBase: SV_StartInstanceLocation)
    ->VertexOutput {
  let iindex = iindexBase + iindexOffset;
  let p = float2((vindex & 0x01) == 0 ? 0.0 : 1.0,
                 (vindex & 0x02) == 0.0 ? 0.0 : 1.0);
  let posST = instanceDataArray[iindex].posSTStatic;
  let pos4 = mul(instanceDataArray[iindex].positionModifierMatrix,
                 float4(p * posST.xy + posST.zw, 0.0, 1.0));

  VertexOutput vo;

  vo.pos = float4(pos4.xy * 2.0 / pushConstants.screenSize - 1.0, 0.0, 1.0);
  vo.fragmentInput.packedValues3.xy = p * posST.xy;
  vo.fragmentInput.packedValues3.zw = posST.xy;
  vo.fragmentInput.screenUV = pos4.xy / pushConstants.screenSize;
  vo.fragmentInput.packedValues2.xy = instanceDataArray[iindex].uvSTStatic.zw;
  vo.fragmentInput.packedValues2.zw = instanceDataArray[iindex].miscValues.xy;
  vo.fragmentInput.packedValues.xy = instanceDataArray[iindex].globalUV(p);
  vo.fragmentInput.packedValues.zw = instanceDataArray[iindex].miscValues.zw;
  vo.fragmentInput.sliceBordersLTRB = instanceDataArray[iindex].sliceBorders;
  vo.fragmentInput.colorTint = instanceDataArray[iindex].colorTint;
  vo.fragmentInput.texSlicedSizePixels.xy =
      instanceDataArray[iindex].texSlicedSizedPixels;
  vo.fragmentInput.texSlicedSizePixels.zw = 0.0;
  vo.fragmentInput.cornerRadiusX = instanceDataArray[iindex].cornerRadiusX;
  vo.fragmentInput.cornerRadiusY = instanceDataArray[iindex].cornerRadiusY;
  vo.fragmentInput.borderColor =
      premultiplyAlpha(instanceDataArray[iindex].borderColor);
  vo.fragmentInput.packedValues4.x = instanceDataArray[iindex].borderThickness;
  vo.fragmentInput.packedValues4.y = instanceDataArray[iindex].softedge;
  vo.fragmentInput.packedValues4.z = instanceDataArray[iindex].miscValues2.z;
  vo.fragmentInput.packedValues4.w = instanceDataArray[iindex].miscValues2.w;
  vo.fragmentInput.packedValues5.xy = instanceDataArray[iindex].miscValues3.xy;
  vo.fragmentInput.packedValues5.z = instanceDataArray[iindex].miscValues3.z;

  return vo;
}

struct FragmentInput {
  /// uv.xy, compositeMode, opacity
  float4 packedValues;
  /// uvOffset.xy, texSizePixels.xy
  float4 packedValues2;
  /// relativePixelCoord.xy, renderSizePixels.xy
  float4 packedValues3;
  /// borderThickness, softedge, gradientDataIndex, sourceTextureIndex
  float4 packedValues4;
  /// borderBreakPatternX, borderBreakPatternY, textureMappingMode, ..
  float4 packedValues5;
  float4 sliceBordersLTRB;
  float4 colorTint;
  float4 texSlicedSizePixels;
  float2 screenUV;
  /// lt, rt, lb, rb (in pixels)
  float4 cornerRadiusX;
  /// lt, rt, lb, rb (in pixels)
  float4 cornerRadiusY;
  float4 borderColor;

  property relativePixelCoord : float2 {
    inline get { return this.packedValues3.xy; }
  }

  property texSizePixels : float2 {
    inline get { return this.packedValues2.zw; }
  }

  property renderSizePixels : float2 {
    inline get { return this.packedValues3.zw; }
  }

  property localUV : float2 {
    inline get { return this.relativePixelCoord / this.renderSizePixels; }
  }

  property renderingPixelCoordLeft : float {
    inline get { return this.relativePixelCoord.x; }
  }

  property renderingPixelCoordRight : float {
    inline get { return this.renderSizePixels.x - this.relativePixelCoord.x; }
  }

  property renderingPixelCoordTop : float {
    inline get { return this.relativePixelCoord.y; }
  }

  property renderingPixelCoordBottom : float {
    inline get { return this.renderSizePixels.y - this.relativePixelCoord.y; }
  }

  property cornerRadiusLT : float2 {
    inline get { return float2(cornerRadiusX.x, cornerRadiusY.x); }
  }

  property cornerRadiusRT : float2 {
    inline get { return float2(cornerRadiusX.y, cornerRadiusY.y); }
  }

  property cornerRadiusLB : float2 {
    inline get { return float2(cornerRadiusX.z, cornerRadiusY.z); }
  }

  property cornerRadiusRB : float2 {
    inline get { return float2(cornerRadiusX.w, cornerRadiusY.w); }
  }

  property sourceTextureIndex : uint {
    inline get { return uint(this.packedValues4.w); }
  }

  property textureMappingMode : TextureMappingMode {
    inline get { return TextureMappingMode(this.packedValues5.z); }
  }

  property textureUV : float2 {
    inline get {
      if (all(this.sliceBordersLTRB == float4(0.0))) {
        // no 9slices
        switch (this.textureMappingMode) {
        case TextureMappingMode.Stretch:
          return this.packedValues.xy;
        case TextureMappingMode.Repeat:
          return this.packedValues2.xy +
                 ((this.relativePixelCoord % this.texSlicedSizePixels.xy) /
                  this.texSizePixels);
        }
      }

      let uvOffset = this.packedValues2.xy;
      let relativePixelCoord = this.packedValues3.xy;
      let renderSizePixels = this.packedValues3.zw;
      let texSizePixels = this.packedValues2.zw;

      return uvOffset +
             float2(computeSlicedRelativeUVForAxis(
                        relativePixelCoord.x, renderSizePixels.x,
                        texSizePixels.x, this.sliceBordersLTRB.x,
                        this.sliceBordersLTRB.z, this.texSlicedSizePixels.x),
                    computeSlicedRelativeUVForAxis(
                        relativePixelCoord.y, renderSizePixels.y,
                        texSizePixels.y, this.sliceBordersLTRB.y,
                        this.sliceBordersLTRB.w, this.texSlicedSizePixels.y));
    }
  }

  property compositeMode : CompositeMode {
    inline get { return CompositeMode(this.packedValues.z); }
  }

  property opacity : float {
    inline get { return this.packedValues.w; }
  }

  property borderThickness : float {
    inline get { return this.packedValues4.x; }
  }

  property borderBreakPattern : float2 {
    inline get { return this.packedValues5.xy; }
  }

  property softedge : float {
    inline get { return this.packedValues4.y; }
  }

  property gradientDataIndex : uint {
    inline get { return uint(this.packedValues4.z); }
  }
}

[shader("fragment")]
float4 fragMain(FragmentInput input: Varyings) : SV_Target {
  let rectMaskInUV : RectMask = { {
                                   pushConstants.rectMaskInScreenUV.x,
                                   pushConstants.rectMaskInScreenUV.y,
                                   pushConstants.rectMaskInScreenUV.z,
                                   pushConstants.rectMaskInScreenUV.w,
                                 },
                                  {
                                    pushConstants.rectMaskSoftnessInScreenUV.x,
                                    pushConstants.rectMaskSoftnessInScreenUV.y,
                                    pushConstants.rectMaskSoftnessInScreenUV.z,
                                    pushConstants.rectMaskSoftnessInScreenUV.w,
                                  } };

  if (rectMaskInUV.isClipped(input.screenUV)) {
    // out of mask
    discard;
  }

  var col : float4;
  // determine base color
  switch (input.compositeMode) {
  case CompositeMode.ColorTint:
  case CompositeMode.ColorTintBackdropBlur:
    // assuming texture is r8 format
    col = input.colorTint *
          float4(1.0, 1.0, 1.0,
                 texture[input.sourceTextureIndex].Sample(input.textureUV).x);
    break;
  case CompositeMode.FillColor:
  case CompositeMode.FillColorBackdropBlur:
    col = input.colorTint;
    break;
  case CompositeMode.FillLinearGradient: {
    // nearest point on gradient line
    let a = saturate(dot(
                normalize(gradientDataArray[input.gradientDataIndex].params.zw -
                          gradientDataArray[input.gradientDataIndex].params.xy),
                input.relativePixelCoord / input.renderSizePixels -
                    gradientDataArray[input.gradientDataIndex].params.xy)) /
            length(gradientDataArray[input.gradientDataIndex].params.zw -
                   gradientDataArray[input.gradientDataIndex].params.xy);
    col = lerp(gradientDataArray[input.gradientDataIndex].startColor,
               gradientDataArray[input.gradientDataIndex].endColor, a);
    break;
  }
  case CompositeMode.FillRadialGradient: {
    // starting from center
    let a =
        saturate(length((input.relativePixelCoord / input.renderSizePixels -
                         gradientDataArray[input.gradientDataIndex].params.xy) /
                        gradientDataArray[input.gradientDataIndex].params.zw));
    col = lerp(gradientDataArray[input.gradientDataIndex].startColor,
               gradientDataArray[input.gradientDataIndex].endColor, a);
    break;
  }
  case CompositeMode.ColorPickerGradientBox: {
    let l = 1.0 - input.localUV.y;
    let s = input.localUV.x;
    col = float4(
        pow(lerp(float3(1.0, 1.0, 1.0), input.colorTint.xyz, s) * l, 2.2), 1.0);
    break;
  }
  case CompositeMode.DirectSourceOver:
  default:
    // fallback: direct source-over
    col = texture[input.sourceTextureIndex].Sample(input.textureUV);
  }

  // compute distance from edge
  // TODO: GPU負荷大きそうならテクスチャにするのも考える
  var distanceFromEdge =
      min(min(input.renderingPixelCoordLeft, input.renderingPixelCoordRight),
          min(input.renderingPixelCoordTop, input.renderingPixelCoordBottom));
  if (input.renderingPixelCoordLeft < input.cornerRadiusLT.x &&
      input.renderingPixelCoordTop < input.cornerRadiusLT.y) {
    let roundedDistance = (1.0 - length((float2(input.renderingPixelCoordLeft,
                                                input.renderingPixelCoordTop) -
                                         input.cornerRadiusLT) /
                                        input.cornerRadiusLT)) *
                          maxElement(input.cornerRadiusLT.x);
    distanceFromEdge = min(roundedDistance, distanceFromEdge);
  }
  if (input.renderingPixelCoordRight < input.cornerRadiusRT.x &&
      input.renderingPixelCoordTop < input.cornerRadiusRT.y) {
    let roundedDistance = (1.0 - length((float2(input.renderingPixelCoordRight,
                                                input.renderingPixelCoordTop) -
                                         input.cornerRadiusRT) /
                                        input.cornerRadiusRT)) *
                          maxElement(input.cornerRadiusRT.x);
    distanceFromEdge = min(roundedDistance, distanceFromEdge);
  }
  if (input.renderingPixelCoordLeft < input.cornerRadiusLB.x &&
      input.renderingPixelCoordBottom < input.cornerRadiusLB.y) {
    let roundedDistance =
        (1.0 - length((float2(input.renderingPixelCoordLeft,
                              input.renderingPixelCoordBottom) -
                       input.cornerRadiusLB) /
                      input.cornerRadiusLB)) *
        maxElement(input.cornerRadiusLB.x);
    distanceFromEdge = min(roundedDistance, distanceFromEdge);
  }
  if (input.renderingPixelCoordRight < input.cornerRadiusRB.x &&
      input.renderingPixelCoordBottom < input.cornerRadiusRB.y) {
    let roundedDistance =
        (1.0 - length((float2(input.renderingPixelCoordRight,
                              input.renderingPixelCoordBottom) -
                       input.cornerRadiusRB) /
                      input.cornerRadiusRB)) *
        maxElement(input.cornerRadiusRB.x);
    distanceFromEdge = min(roundedDistance, distanceFromEdge);
  }
  // let x = 1.0 - abs(distanceFromEdge) / 128.0;
  // return float4(x, x, x, 1.0);

  let cornerAlpha = smoothstep(0.0, 1.0, distanceFromEdge);
  let softEdgeAlpha = smoothstep(0.0, input.softedge, distanceFromEdge);

  // apply softmask, opacity and premultiply
  col.w *= rectMaskInUV.softmaskRate(input.screenUV) * input.opacity *
           cornerAlpha * softEdgeAlpha;
  col.xyz *= col.w;

  // blend with borderline
  let borderPatternAlpha =
      select(input.borderBreakPattern.x == 0.0, 1.0,
             select((int(min(input.renderingPixelCoordLeft,
                             input.renderingPixelCoordRight) /
                         input.borderBreakPattern.x) &
                     0x01) == 0,
                    1.0, 0.0)) *
      select(input.borderBreakPattern.y == 0.0, 1.0,
             select((int(min(input.renderingPixelCoordTop,
                             input.renderingPixelCoordBottom) /
                         input.borderBreakPattern.y) &
                     0x01) == 0,
                    1.0, 0.0));
  let borderAlpha =
      smoothstep(0.0, 1.0, distanceFromEdge) *
      (1.0 - smoothstep(0.0 + input.borderThickness,
                        1.0 + input.borderThickness, distanceFromEdge)) *
      borderPatternAlpha;
  col = blendAlphaPremultipliedColor(
      input.borderColor * borderAlpha * input.opacity, col);

  if (col.w > 0.0 && input.compositeMode.requiresBackdropComposition) {
    // blend with backdrop(forced alpha = 1)
    let backdrop = backdropTexture.Sample(input.screenUV);
    col = blendAlphaPremultipliedColor(col, backdrop);
    col.w = 1.0;
  }

  return col;
}

// Helper functions

inline func maxElement(float2 v)->float { return max(v.x, v.y); }

inline func premultiplyAlpha(float4 c)->float4 {
  return float4(c.xyz * c.w, c.w);
}
inline func blendAlphaPremultipliedColor(source: float4, dest: float4)->float4 {
  return float4(source.xyz + dest.xyz * (1.0 - source.w),
                source.w + dest.w * (1.0 - source.w));
}

func interpolateCurve(p1: float2, p2: float2, x: float)->float {
  // p01 = mix(vec2(0.0), p1, t) = p1 * t
  // p12 = mix(p1, p2, t) = p1 * (1.0 - t) + p2 * t
  // p23 = mix(p2, vec2(1.0), t) = p2 * (1.0 - t) + vec2(t)
  // p012 = mix(p01, p12, t) = p01 * (1.0 - t) + p12 * t = p1 * t * (1.0 - t) +
  // (p1 * (1.0 -t ) + p2 * t) * t = p1 * t * (1.0 - t) + p1 * t * (1.0 - t) +
  // p2 * t * t = p1 * 2.0 * t * (1.0 - t) + p2 * t * t = p1 * (2.0 * t - 2.0 *
  // t * t) + p2 * t * t p123 = mix(p12, p23, t) = p12 * (1.0 - t) + p23 * t =
  // (p1 * (1.0 - t) + p2 * t) * (1.0 - t) + (p2 * (1.0 - t) + vec2(t)) * t = p1
  // * (1.0 - t) * (1.0 - t) + p2 * t * (1.0 - t) + p2 * (1.0 - t) * t  + vec2(t
  // * t) = p1 * (1.0 - t) * (1.0 - t) + p2 * 2.0 * t * (1.0 - t) + vec2(t * t)
  // = p1 * (1.0 - t) * (1.0 - t) + p2 * (2.0 * t - 2.0 * t * t) + vec2(t * t)
  // p = mix(p012, p123, t) = p012 * (1.0 - t) + p123 * t =
  // (p1 * (2.0 * t - 2.0 * t * t) + p2 * t * t) * (1.0 - t) + (p1 * (1.0 - t) *
  // (1.0 - t) + p2 * (2.0 * t - 2.0 * t * t) + vec2(t * t)) * t = p1 * (2.0 * t
  // - 2.0 * t * t) * (1.0 - t) + p2 * t * t * (1.0 - t) + p1 * t * (1.0 - t) *
  // (1.0 - t) + p2 * t * (2.0 * t - 2.0 * t * t) + vec2(t * t * t) = p1 * 2.0 *
  // t * (1.0 - t) * (1.0 - t) + p2 * t * t * (1.0 - t) + p1 * t * (1.0 - t) *
  // (1.0 - t) + p2 * 2.0 * t * t * (1.0 - t) + vec2(t * t * t) = p1 * 3.0 * t *
  // (1.0 - t) * (1.0 - t) + p2 * 3.0 * t * t * (1.0 - t) + vec2(t * t * t)
  //
  // (1.0 - t)^2 = 1.0^2 - 2.0 * t + t^2
  //
  // x = (p1.x * 3.0 * t * (1.0 - t) * (1.0 - t) + p2.x * 3.0 * t * t * (1.0 -
  // t) + t * t * t), t = ? x = p1.x * (3.0 * t - 6.0 * t^2 + 3.0 * t^3) + p2.x
  // * (3.0 * t^2 - 3.0 * t^3) + t^3 x = (p1.x * 3.0 - p2.x * 3.0 + 1.0) * t^3 +
  // (-p1.x * 6.0 + p2.x * 3.0) * t^2 + p1.x * 3.0 * t 0 = (p1.x * 3.0 - p2.x
  // * 3.0 + 1.0) * t^3 + (-p1.x * 6.0 + p2.x * 3.0) * t^2 + p1.x * 3.0 * t - x

  // x = (p1.x * 3.0 - p2.x * 3.0 + 1.0) * t^3 + (p2.x * 3.0 - p1.x * 6.0) * t^2
  // + p1.x * 3.0 * t t = ?
  let a = p1.x * 3.0 - p2.x * 3.0 + 1.0;
  let b = p2.x * 3.0 - p1.x * 6.0;
  let c = p1.x * 3.0;
  let d = -x;

  float t;
  if (a == 0) {
    // solve quadratic: (p2.x * 3.0 - p1.x * 6.0) * t^2 + p1.x * 3.0 * t - x = 0
    if (let val = solveQuadraticClamped(b, c, d)) {
      t = val;
    } else {
      // no value
      return 0.0;
    }
  } else {
    t = solveCubicClamped(a, b, c, d);
  }

  // y = (p1.y * 3.0 - p2.y * 3.0 + 1.0) * t^3 + (p2.y * 3.0 - p1.y * 6.0) * t^2
  // + p1.y * 3.0 * t
  return (p1.y * 3.0 - p2.y * 3.0 + 1.0) * t * t * t +
         (p2.y * 3.0 - p1.y * 6.0) * t * t + p1.y * 3.0 * t;
}

func solveQuadraticClamped(a: float, b: float, c: float)->Optional<float> {
  let dq = b * b - 4.0 * a * c;
  if (dq < 0.0) {
    // no value
    return none;
  }

  if (dq == 0.0) {
    // exactly one
    return -c * 0.5 * b;
  }

  // select correct value
  let t1 = -c + sqrt(dq) * 0.5 * b;
  let t2 = -c - sqrt(dq) * 0.5 * b;

  if (0.0 <= t1 && t1 <= 1.0) {
    return t1;
  }

  if (0.0 <= t2 && t2 <= 1.0) {
    return t2;
  }

  // outside all
  return saturate(t1);
}

// solve cubic:
// https://peter-shepherd.com/personal_development/mathematics/polynomials/cubicAlgebra.htm
func solveCubicClamped(a: float, b: float, c: float, d: float)->float {
  let a1 = b / a;
  let b1 = c / a;
  let c1 = d / a;
  let p = (3.0 * b1 - a1 * a1) / 3.0;
  let q = (2.0 * a1 * a1 * a1 - 9.0 * a1 * b1 + 27.0 * c1) / 27.0;

  if (p == 0.0) {
    // simple form

    if (q == 0.0f) {
      return 0.0;
    }

    let t1 = cbrt(-q) - a1 / 3.0;
    let t2 = cbrt(-q) * (-0.5 + sqrt(3.0) * 0.5) - a1 / 3.0;
    let t3 = cbrt(-q) * (-0.5 - sqrt(3.0) * 0.5) - a1 / 3.0;

    if (0.0 <= t1 && t1 <= 1.0) {
      return t1;
    }

    if (0.0 <= t2 && t2 <= 1.0) {
      return t2;
    }

    if (0.0 <= t3 && t3 <= 1.0) {
      return t3;
    }

    // outside all
    return saturate(t1);
  }

  if (q == 0.0) {
    let t1 = -a1 / 3.0;
    let t2 = sqrt(-p) - a1 / 3.0;
    let t3 = -sqrt(-p) - a1 / 3.0;

    if (0.0 <= t1 && t1 <= 1.0) {
      return t1;
    }

    if (0.0 <= t2 && t2 <= 1.0) {
      return t2;
    }

    if (0.0 <= t3 && t3 <= 1.0) {
      return t3;
    }

    // outside all
    return saturate(t1);
  }

  let dc = (q * q) / 4.0 + (p * p * p) / 27.0;
  if (dc == 0.0) {
    // two reals
    let t1 = 2.0f * cbrt(-q * 0.5) - a1 / 3.0;
    let t2 = cbrt(q * 0.5) - a1 / 3.0;

    if (0.0 <= t1 && t1 <= 1.0) {
      return t1;
    }

    if (0.0 <= t2 && t2 <= 1.0) {
      return t2;
    }

    // outside all
    return saturate(t1);
  }

  if (dc > 0.0f) {
    // one real and two img
    let u1 = cbrt(-q * 0.5 + sqrt(dc));
    let v1 = cbrt(q * 0.5 + sqrt(dc));

    let t1 = u1 - v1 - a1 / 3.0;
    let t2 = -0.5 * (u1 - v1) + (u1 + v1) * sqrt(3.0) * 0.5 - a1 / 3.0;
    let t3 = -0.5 * (u1 - v1) - (u1 + v1) * sqrt(3.0) * 0.5 - a1 / 3.0;

    if (0.0 <= t1 && t1 <= 1.0) {
      return t1;
    }

    if (0.0 <= t2 && t2 <= 1.0) {
      return t2;
    }

    if (0.0 <= t3 && t3 <= 1.0) {
      return t3;
    }

    // outside all
    return saturate(t1);
  }

  // irreducible case
  let r = sqrt(pow(-p / 3.0, 3.0));
  let phi = acos(-q / (2.0 * r));

  let t1 = 2.0 * cbrt(r) * cos(phi / 3.0) - a1 / 3.0;
  let t2 = 2.0 * cbrt(r) * cos((phi + 2.0 * 3.1415926) / 3.0) - a1 / 3.0;
  let t3 = 2.0 * cbrt(r) * cos((phi + 4.0 * 3.1415926) / 3.0) - a1 / 3.0;

  if (0.0 <= t1 && t1 <= 1.0) {
    return t1;
  }

  if (0.0 <= t2 && t2 <= 1.0) {
    return t2;
  }

  if (0.0 <= t3 && t3 <= 1.0) {
    return t3;
  }

  // outside all
  return saturate(t1);
}

inline func cbrt(v: float)->float { return pow(v, 1.0 / 3.0); }

inline func timeRemap(start: float, end: float, t: float)->float {
  return saturate((t - start) / (end - start));
}

func computeSlicedRelativeUVForAxis(relativePixelCoord: float,
                                    renderSizePixels: float,
                                    texSizePixels: float, sliceBorderMin: float,
                                    sliceBorderMax: float,
                                    texSlicedSizePixels: float)
    ->float {
  if (relativePixelCoord < sliceBorderMin) {
    // min
    let r = relativePixelCoord / sliceBorderMin;
    let uvSizeInTex = sliceBorderMin / texSizePixels;

    return r * uvSizeInTex;
  }

  if (renderSizePixels - relativePixelCoord < sliceBorderMax) {
    // max
    let r = (renderSizePixels - relativePixelCoord) / sliceBorderMax;
    let uvSizeInTex = sliceBorderMax / texSizePixels;

    return r * uvSizeInTex;
  }

  // inner
  let r = (relativePixelCoord - sliceBorderMin) /
          (renderSizePixels - sliceBorderMax - sliceBorderMin);
  let uvSizeInTex =
      (texSlicedSizePixels - sliceBorderMax - sliceBorderMin) / texSizePixels;
  let uvOffset = sliceBorderMin / texSizePixels;

  return uvOffset + r * uvSizeInTex;
}

struct Rect {
  float left, top, right, bottom;

  inline func isIn(p: float2)->bool {
    return this.left <= p.x && p.x <= this.right && this.top <= p.y &&
           p.y <= this.bottom;
  }
}

struct RectMask {
  Rect clip, softness;

  inline func isClipped(p: float2)->bool { return !this.clip.isIn(p); }

  inline func softmaskRate(p: float2)->float {
    let x0 = saturate(p.x / this.softness.left);
    let x1 = saturate((this.clip.right - p.x) / this.softness.right);
    let y0 = saturate(p.y / this.softness.top);
    let y1 = saturate((this.clip.bottom - p.y) / this.softness.bottom);

    return min(min(x0, x1), min(y0, y1));
  }
}
