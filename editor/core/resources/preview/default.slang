struct CameraData {
  float4x4 worldToClipSpace;
  float4x4 worldToCameraSpace;
  float4x4 cameraToClipSpace;
  float4 cameraPos;
}

[vk::binding(0, 0)]
ConstantBuffer<CameraData> cameraData;

struct ObjectData {
  float4x4 objectToWorldSpace;
}

[vk::binding(0, 1)]
ConstantBuffer<ObjectData> objectData;

struct VshInput {
  [vk::location(0)]
  float4 pos;
  [vk::location(1)]
  float4 normal;
}

struct VshOutput {
  float4 pos : SV_Position;
  FshInput toFragment;
}

[shader("vertex")]
func vertMain(data: VshInput)->VshOutput {
  VshOutput vo;
  vo.pos = mul(cameraData.worldToClipSpace,
               mul(objectData.objectToWorldSpace, data.pos));
  vo.toFragment.norm = mul(objectData.objectToWorldSpace, data.normal);
  return vo;
}

struct FshInput {
  float4 norm : TEXCOORD0;
}

struct FshOutput {
  float4 col : SV_Target;
}

[shader("fragment")]
func fragMain(data: FshInput)->FshOutput {
  let lightIncidentDir = normalize(float4(0.5, 1.0, 1.0, 0.0));
  // half-lambert: https://developer.valvesoftware.com/wiki/Half_Lambert
  let lightingIntensity = sqrt(dot(data.norm, lightIncidentDir) * 0.5 + 0.5);

  FshOutput fo;
  fo.col = float4(lightingIntensity, lightingIntensity, lightingIntensity, 1.0);
  return fo;
}
