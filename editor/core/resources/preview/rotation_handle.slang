#include "common-bindings.slang"

[vk::push_constant]
ConstantBuffer<float4[4]> colorPalette;

struct VshInput {
  [vk::location(0)]
  float4 pos : POSITION0;
  [vk::location(1)]
  uint colorIndex : TEXCOORD0;
}

struct VshOutput {
  float4 pos : SV_Position;
  FshInput toFragment;
}

[shader("vertex")]
func vertMain(data: VshInput)->VshOutput {
  var p2 = data.pos;
  p2.xyz *= length(cameraData.cameraPos - float4(0.0, 0.0, 0.0, 1.0)) * 0.25;

  VshOutput vo;
  vo.pos = mul(cameraData.worldToClipSpace, p2);
  vo.toFragment.col = colorPalette[data.colorIndex];
  vo.toFragment.d = dot(normalize(data.pos.xyz), (cameraData.cameraPos - float4(0.0, 0.0, 0.0, 1.0)).xyz);
  return vo;
}

struct FshInput {
  float4 col : TEXCOORD0;
  float d: TEXCOORD1;
}

struct FshOutput {
  float4 col : SV_Target;
}

[shader("fragment")]
func fragMain(data: FshInput)->FshOutput {
    if (data.d < 0.0) {
        discard;
    }

  FshOutput fo;
  fo.col = data.col;
  return fo;
}
