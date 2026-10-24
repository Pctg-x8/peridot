struct VertexOutput {
  float4 pos : SV_Position;
  float2 uv : TEXCOORD0;
}

static const float PI = 3.14159265358979323846;

static const float RING_THICKNESS = 12.0;
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
  let hue =
      360.0 * (atan2(uv.y - 0.5, uv.x - 0.5) / (2.0 * PI) + 0.5) + 360.0 - 90.0;
  let col = float3(hueToRGBWave(hue + 120.0), hueToRGBWave(hue),
                   hueToRGBWave(hue - 120.0));

  // needs gamma collection?
  return float4(col, 1.0) * ringOpacity(uv);
}

inline func ringOpacity(float2 uv)->float {
  let d = length(uv - 0.5);
  let out = smoothstep(0.5, 0.5 + 1.0 / RENDER_WIDTH_PX, d);
  let in = smoothstep(0.5 - RING_THICKNESS / 128.0 - 1.0 / RENDER_WIDTH_PX,
                      0.5 - RING_THICKNESS / 128.0, d);

  return (1.0 - out) * in;
}

inline func hueToRGBWave(float hue)->float {
  // generate ／￣￣＼＿＿ wave
  // triangle wave(asin(sin(x)), adjusted for x=0..6, y=-2..2) + saturating
  return saturate(asin(sin(fmod(hue / 60.0, 6.0) * PI / 4.0)) * 4.0 / PI);
}
