Properties
  [Instanceable]
  maintex: Texture2D = Use "Texture2D.white"
End

Pass "Unlit"
    RenderOption NoCulling, InstancedOnly

    Shader
[Peridot::VertexInput]
struct Vertex {
    float4 pos : POSITION0;
    float2 uv : TEXCOORD0;
}

struct VertexOutput {
    FragmentInput fragmentInput : Varyings;
    float4 pos : SV_Position;
}

struct FragmentInput {
    float2 uv : TEXCOORD0;
}

[shader("vertex")]
VertexOutput vertMain(Vertex v, Peridot::VertexShaderContext ctx) {
    VertexOutput vo;

    vo.pos = ctx.objectToClipSpace(v.pos);
    vo.fragmentInput.uv = v.uv.xy * ctx.properties.maintex_uvst.xy + ctx.properties.maintex_uvst.zw;

    return vo;
}

[shader("fragment")]
float4 fragMain(FragmentInput input : Varyings, Peridot::FragmentShaderContext ctx) : SV_Target {
    // return float4(input.uv, 1.0, 1.0);
    var col = ctx.properties.maintex.Sample(input.uv);
    col.rgb *= col.a;
    return col;
}
    End
End
