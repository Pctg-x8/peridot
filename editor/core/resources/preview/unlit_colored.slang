#include "common-bindings.slang"

struct VshInput {
  [vk::location(0)]
  float4 pos : POSITION0;
  [vk::location(1)]
  float4 col : TEXCOORD0;
}

struct VshOutput {
  float4 pos : SV_Position;
  FshInput toFragment;
}

[shader("vertex")]
func vertMain(data: VshInput)->VshOutput {
  var p2 = data.pos;
  p2.xyz *= length(cameraData.cameraPos - float4(0.0, 0.0, 0.0, 1.0));

  VshOutput vo;
  vo.pos = mul(cameraData.worldToClipSpace, p2);
  vo.toFragment.col = data.col;
  return vo;
}

struct FshInput {
  float4 col : TEXCOORD0;
}

struct FshOutput {
  float4 col : SV_Target;
}

[shader("fragment")]
func fragMain(data: FshInput)->FshOutput {
  FshOutput fo;
  fo.col = data.col;
  return fo;
}
