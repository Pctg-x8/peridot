Pass "Unlit"
  RenderOption NoCulling

    Shader
[Peridot::VertexInput]
struct Vertex {
  float3 pos : POSITION0;
  float3 normal : NORMAL0;
}

struct VertexOutput {
  FragmentInput fragmentInput : Varyings;
  float4 pos : SV_Position;
}

struct FragmentInput {
  float3 normal : NORMAL0;
}

[shader("vertex")]
VertexOutput vertMain(Vertex v, Peridot::VertexShaderContext ctx) {
  VertexOutput vo;

  vo.pos = ctx.objectToClipSpace(float4(v.pos, 1.0));
  vo.fragmentInput.normal = normalize(v.normal);

  return vo;
}

[shader("fragment")]
float4 fragMain(FragmentInput input : Varyings, Peridot::FragmentShaderContext ctx) {
  // fixed lightdir
  let lightDir = normalize(float3(-1.0, -0.3, 0.2));
  let ambientLight = float4(0.1, 0.1, 0.1, 0.0);
  // half-lambert
  let lighting = pow(dot(input.normal, -lightDir) * 0.5 + 0.5, 2.0);

  return float4(lighting, lighting, lighting, 1.0) + ambientLight;
}
  End
End
