struct CameraData {
  float4x4 worldToClipSpace;
  float4x4 worldToCameraSpace;
  float4x4 cameraToClipSpace;
  float4 cameraPos;
}

struct ObjectData {
    float4x4 objectToWorldSpace;
}

[vk::binding(0, 0)]
ConstantBuffer<CameraData> cameraData;

[vk::binding(0, 1)]
ConstantBuffer<ObjectData> objectData;

static const float FADEOUT_START_DIST = 500.0;
static const float FADEOUT_END_DIST = 1000.0;
