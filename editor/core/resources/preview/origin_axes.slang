struct CameraData {
  float4x4 worldToClipSpace;
  float4 cameraPos;
}

[vk::binding(0, 0)]
ConstantBuffer<CameraData> cameraData;

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
  let a = 1.0 - smoothstep(50.0, 100.0, length(data.p - cameraData.cameraPos));

  FshOutput o;
  o.col = data.col * a;
  return o;
}
