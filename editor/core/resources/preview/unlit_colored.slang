#include "common-bindings.slang"

[vk::push_constant]
ConstantBuffer<float4[5]> colorPalette;

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
  let objectWorldPos = float3(objectData.objectToWorldSpace._14, objectData.objectToWorldSpace._24, objectData.objectToWorldSpace._34);
  p2.xyz *= length(cameraData.cameraPos.xyz - objectWorldPos);

  VshOutput vo;
  vo.pos = mul(cameraData.worldToClipSpace, mul(objectData.objectToWorldSpace, p2));
  vo.toFragment.col = colorPalette[data.colorIndex];
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
