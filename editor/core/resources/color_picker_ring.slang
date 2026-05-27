struct VertexOutput {
  float4 pos : SV_Position;
  float2 uv : TEXCOORD0;
}

static const float RING_THICKNESS = 16.0;

[shader("vertex")]
VertexOutput vertMain(uint vid: SV_VertexID) {
  let pos01 = float2(select((vid & 0x01) == 0, 0.0, 1.0),
                     select((vid & 0x02) == 0, 0.0, 1.0));

  VertexOutput vo;
  vo.uv = pos01 * 2.0;
  vo.pos = float4(pos01 * 4.0 - 1.0, 0.0, 1.0);
  return vo;
}

[shader("fragment")]
float4 fragMain(float2 uv: TEXCOORD0) : SV_Target {
  let dc = length(uv - 0.5);
  let a = (1.0 - smoothstep(0.5, 0.5 + 1.0 / 225.0, dc)) *
          smoothstep(0.5 - RING_THICKNESS / 225.0 - 1.0 / 225.0,
                     0.5 - RING_THICKNESS / 225.0, dc);
  return float4(uv, 0.0, 1.0) * a;
}
