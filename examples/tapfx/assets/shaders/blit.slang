import peridot;

struct VertexOutput {
  float4 pos : SV_Position;
  FragmentInput fragInput : Varyings;
}

struct Parameters {
  float4x4 projection;
  float time;
  float2 offset;
  Sampler2D tex;
}
[peridot::material_property]
ParameterBlock<Parameters> params;

[shader("vertex")]
VertexOutput vertMain([peridot::vertex] float2 pos: POSITION,
                      [peridot::vertex] float2 uv: TEXCOORD0) {
  let scaling =
      (params.time * 2.0) >= 1.0 ? 0.0 : (1.0 - pow(1.0 - params.time, 8.0));

  VertexOutput vo;

  vo.pos =
      mul(params.projection, float4(params.offset + pos * scaling, 0.5, 1.0));
  vo.fragInput.uv = uv;
  vo.fragInput.alpha = pow(1.0 - params.time, 1.8);

  return vo;
}

struct FragmentInput {
  float2 uv;
  float alpha;
}

[shader("fragment")]
float4 fragMain(FragmentInput input: Varyings) : SV_Target {
  let c = params.tex.Sample(input.uv);

  return float4(c.xyz, 1.0) * c.a * input.alpha;
}
