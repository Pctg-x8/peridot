struct StreamingBufferContent {
  float currentSec;
}

[vk::binding(0, 0)]
ConstantBuffer<StreamingBufferContent> streamingBufferContent;

struct VertexOutput {
  float4 pos : SV_Position;
  float4 col : COLOR;
}

[shader("vertex")]
func vertMain(uint vindex: SV_VertexID)->VertexOutput {
  VertexOutput vo;

  switch (vindex) {
  case 0:
    vo.pos = float4(0.0, -0.5, 0.0, 1.0);
    vo.col = float4(1.0, 1.0, 1.0, 1.0);
    break;
  case 1:
    vo.pos = float4(-0.5, 0.5, 0.0, 1.0);
    vo.col = float4(0.0, 1.0, 1.0, 0.5);
    break;
  default:
    vo.pos = float4(0.5, 0.5, 0.0, 1.0);
    vo.col = float4(1.0, 0.0, 1.0, 0.75);
    break;
  }

  let th = streamingBufferContent.currentSec * 2.0 * 3.1415926 / 3.0;
  vo.pos = mul(float4x4(float4(cos(th), -sin(th), 0.0, 0.0),
                        float4(sin(th), cos(th), 0.0, 0.0),
                        float4(0.0, 0.0, 1.0, 0.0), float4(0.0, 0.0, 0.0, 1.0)),
               vo.pos);

  return vo;
}

[shader("fragment")]
float4 fragMain(float4 col: COLOR) : SV_Target {
  return float4(col.xyz * col.a, col.a);
}
