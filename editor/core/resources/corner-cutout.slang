struct PushConstants {
  float2 screenSize;
}

[vk::push_constant]
PushConstants pushConstants;

static const float RADIUS = 8.0;

struct VertexOutput {
  float4 pos : SV_Position;
  float2 uv : TEXCOORD0;
}

[shader("vertex")]
VertexOutput vertMain(uint vindex: SV_VertexID, uint iindex: SV_InstanceID) {
  let p = float2((vindex & 0x01) == 0 ? 0.0 : 1.0,
                 (vindex & 0x02) == 0 ? 0.0 : 1.0);
  let o = float2((iindex & 0x01) == 0 ? -1.0 : 1.0,
                 (iindex & 0x02) == 0 ? -1.0 : 1.0);
  let s = float2((iindex & 0x01) == 0 ? 1.0 : -1.0,
                 (iindex & 0x02) == 0 ? 1.0 : -1.0);

  VertexOutput vo;
  vo.pos =
      float4(2.0 * p * s * RADIUS / pushConstants.screenSize + o, 0.0, 1.0);
  vo.uv = p;
  return vo;
}

[shader("fragment")]
float4 fragMain(float2 uv: TEXCOORD0) : SV_Target {
  let d = 1.0 - smoothstep(1.0, 1.0 + 1.0 / RADIUS, length(1.0 - uv));
  return float4(d, d, d, d);
}
