struct VertexOutput {
  float4 pos : SV_Position;
  float2 uv : TEXCOORD0;
}

[shader("vertex")]
VertexOutput vertMain(uint vid: SV_VertexID) {
  let pos01 = float2(select((vid & 0x01) == 0, 0.0, 1.0),
                     select((vid & 0x02) == 0, 0.0, 1.0));

  VertexOutput vo;
  vo.uv = pos01 * 2.0;
  vo.pos = float4(pos01 * 2.0 - 1.0, 0.0, 1.0);
  return vo;
  
}

[shader("fragment")]
float4 fragMain(float2 uv: TEXCOORD0) : SV_Target {
  return float4(1.0 - uv, 0.0, 1.0);
}
