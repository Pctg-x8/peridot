struct CameraData {
  float4x4 worldToClipSpace;
  float4 cameraPos;
}

[vk::binding(0, 0)]
ConstantBuffer<CameraData> cameraData;

static const float FADEOUT_START_DIST = 500.0;
static const float FADEOUT_END_DIST = 1000.0;
