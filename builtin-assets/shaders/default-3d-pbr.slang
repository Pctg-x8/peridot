import peridot;

struct MaterialProperties {
    float roughness;
}
[peridot::material_property]
ParameterBlock<MaterialProperties> materialProperties;

struct VertexOutput {
  float4 pos : SV_Position;
  FragmentInput fragmentInput : FragInput;
}

[shader("vertex")]
VertexOutput vertMain([peridot::vertex] float4 pos : POSITION0, [peridot::vertex] float3 normal : NORMAL0, [peridot::vertex] float2 uv : TEXCOORD0, peridot::VertexShaderContext ctx) {
    VertexOutput vo;

    vo.pos = ctx.objectToClipSpace(pos);
    vo.fragmentInput.normal = normalize(ctx.objectToClipSpace(float4(normal, 0.0)));
    vo.fragmentInput.uv = uv;

    return vo;
}

struct FragmentInput {
    float4 normal : TEXCOORD0;
    float2 uv : TEXCOORD1;
}

[shader("fragment")]
float4 fragMain(FragmentInput input : FragInput) : SV_Target0 {
    return float4(input.normal.xyz * 0.5 + 0.5, 1.0);
}
