struct PushConstants {
  float4x4 positionModifierMatrix;
  float2 size;
  float2 screenSize;
}

[vk::push_constant]
PushConstants pushConstants;
[vk::binding(0, 0)]
Sampler2D<float4> tex;

struct VertexOutput {
  FragmentInput finput : Varyings;
  float4 pos : SV_Position;
}

[shader("vertex")]
func vertMain(uint vindex: SV_VertexID)->VertexOutput {
  let p = float2(select((vindex & 0x01) == 0, 0.0, 1.0),
                 select((vindex & 0x02) == 0, 0.0, 1.0));
  let p4 = mul(pushConstants.positionModifierMatrix,
               float4(p * pushConstants.size, 0.0, 1.0));

  VertexOutput vo;
  vo.pos = float4(p4.xy * 2.0 / pushConstants.screenSize - 1.0, 0.0, 1.0);
  vo.finput.uv = p;
  return vo;
}

struct FragmentInput {
  float2 uv;
}

[shader("fragment")]
float4 fragMain(FragmentInput input) : SV_Target {
  return tex.Sample(input.uv);
}
