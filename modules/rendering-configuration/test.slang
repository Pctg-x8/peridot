struct Vertex {
    float4 pos : POSITION;
    float2 uv : TEXCOORD0;
}

struct VertexOutput {
    FragmentInput fragmentInput : Varying;
    float4 pos : SV_Position;
}

struct FragmentInput {
    float2 uv;
}

[shader("vertex")]
VertexOutput vertMain(Vertex v) {
    VertexOutput vo;

    vo.fragmentInput.uv = v.uv;
    vo.pos = v.pos;

    return vo;
}

struct MaterialParameter {
    Sampler2D mainTex;
}
ParameterBlock<MaterialParameter> material;

[shader("fragment")]
float4 fragMain(FragmentInput f : Varying) : SV_Target {
    return material.mainTex.Sample(f.uv);
}
