struct VertexOutput {
  float4 pos : SV_Position;
  float2 uv : TEXCOORD0;
}

[vk::constant_id(0)]
const float RENDER_WIDTH_PX = 128.0;
[vk::constant_id(1)]
const float RENDER_HEIGHT_PX = 128.0;

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
  return select((uv.x < 0.5) == (uv.y < 0.5), float4(1.0, 1.0, 1.0, 1.0),
                float4(0.5, 0.5, 0.5, 1.0));
}
