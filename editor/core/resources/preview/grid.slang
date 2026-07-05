#include "common-bindings.slang"

struct PushConstants {
  float4 dir;
  float4 start;
  float4 altdir;
  float scale;
}

[vk::push_constant]
PushConstants pushConstants;

static const float4 COLOR = float4(0.25, 0.25, 0.25, 0.25);

struct VshInput {
  uint vindex : SV_VertexID;
  uint iindex : SV_InstanceID;
}

struct VshOutput {
  float4 pos : SV_Position;
  FshInput toFragment;
}

[shader("vertex")]
func vertMain(data: VshInput)->VshOutput {
  let baseLevel = floor(select(cameraData.cameraPos.y == 0.0, 0.0,
                               log10(abs(cameraData.cameraPos.y))));
  let baseScale = pow(10.0, max(-1.0, baseLevel));
  let actualScale = baseScale * pushConstants.scale;
  let basePos = floor(cameraData.cameraPos / actualScale) * actualScale;
  let p =
      basePos * pushConstants.altdir +
      cameraData.cameraPos * pushConstants.dir +
      (pushConstants.start + pushConstants.altdir * data.iindex) * actualScale +
      select(data.vindex == 0, -250.0, 250.0) * pushConstants.dir * actualScale;

  let opacityRising = saturate((abs(cameraData.cameraPos.y) - actualScale) /
                               (actualScale * 0.5));
  let opacityFall =
      saturate(1.0 - (abs(cameraData.cameraPos.y) - actualScale * 10.0) /
                         (actualScale * 5.0));

  VshOutput vo;
  vo.pos = mul(cameraData.worldToClipSpace, float4(p.xyz, 1.0));
  vo.toFragment.p = float4(p.xyz, 1.0);
  vo.toFragment.opacity = opacityRising * opacityFall;
  vo.toFragment.actualScale = actualScale;
  return vo;
}

struct FshInput {
  float4 p : TEXCOORD0;
  float opacity : TEXCOORD1;
  float actualScale : TEXCOORD2;
}

struct FshOutput {
  float4 col : SV_Target;
}

[shader("fragment")]
func fragMain(data: FshInput)->FshOutput {
  // affects only xz
  let fadeout = 1.0 - smoothstep(FADEOUT_START_DIST, FADEOUT_END_DIST,
                                 length(data.p.xz - cameraData.cameraPos.xz));

  FshOutput fo;
  fo.col = COLOR * fadeout * data.opacity;
  return fo;
}
