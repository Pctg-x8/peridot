#include "common-bindings.slang"

struct Vertex {
  [vk::location(0)]
  float4 dir : POSITION;
  [vk::location(1)]
  float4 offset : TEXCOORD0;
}

struct VshOutput {
  float4 pos : SV_Position;
  float4 col : COLOR;
  float4 p : TEXCOORD0;
}

[shader("vertex")]
func vertMain(v: Vertex)->VshOutput {
  let p = cameraData.cameraPos * v.dir + v.offset;
  // let p = float4(0.0, 0.0, 0.0, 1.0) + v.offset;

  VshOutput vo;
  vo.pos = mul(cameraData.worldToClipSpace, p);
  vo.col = v.dir;
  vo.p = p;
  return vo;
}

struct FragmentData {
  float4 col : COLOR;
  float4 p : TEXCOORD0;
}

struct FshOutput {
  float4 col : SV_Target;
}

[shader("fragment")]
func fragMain(data: FragmentData)->FshOutput {
  let fadeout = 1.0 - smoothstep(FADEOUT_START_DIST, FADEOUT_END_DIST,
                                 length(data.p - cameraData.cameraPos));

  FshOutput o;
  o.col = data.col * fadeout;
  return o;
}
