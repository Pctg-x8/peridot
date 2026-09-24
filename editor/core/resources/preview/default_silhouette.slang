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
}

struct VshOutput {
  float4 pos : SV_Position;
}

[shader("vertex")]
func vertMain(data: VshInput)->VshOutput {
  VshOutput vo;
  vo.pos = mul(cameraData.worldToClipSpace,
               mul(objectData.objectToWorldSpace, data.pos));
  return vo;
}

struct FshOutput {
  float4 col : SV_Target;
}

[shader("fragment")]
func fragMain()->FshOutput {
  FshOutput fo;
  fo.col = float4(1.0, 1.0, 1.0, 1.0);
  return fo;
}
