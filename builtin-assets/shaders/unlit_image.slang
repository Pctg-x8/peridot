import peridot;

struct MaterialProperties {
  [peridot::property_name("Main Texture")]
  Sampler2D tex;
  float4 colorTint;
}
[peridot::material_property]
ParameterBlock<MaterialProperties> materialProperties;

struct FragmentInput {
  float2 uv;
}

struct VertexOutput {
  float4 pos : SV_Position;
  FragmentInput fragInput : Varyings;
}

[shader("vertex")]
VertexOutput vertMain([peridot::vertex] float4 pos: POSITION0,
                      [peridot::vertex] float4 uv: TEXCOORD0,
                      peridot::VertexShaderContext ctx) {
  VertexOutput vo;

  vo.pos = ctx.objectToClipSpace(pos);
  vo.fragInput.uv = uv.xy;

  return vo;
}

[shader("fragment")]
float4 fragMain(FragmentInput input: Varyings) : SV_Target {
  return materialProperties.tex.Sample(input.uv) * materialProperties.colorTint;
}
