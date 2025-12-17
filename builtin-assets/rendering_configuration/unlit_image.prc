Properties
  tex: Texture2D = Use "Texture2D.white"
End

Pass "Unlit"
  RenderOption NoCulling

  Shader
struct VertexOutput {
    FragmentInput fragmentInput : Varyings;
    float4 pos : SV_Position;
}

struct FragmentInput {
    float2 uv : TEXCOORD0;
}

[shader("vertex")]
VertexOutput vertMain(
    [Peridot::VertexInput] float4 pos : POSITION0,
    [Peridot::VertexInput] float4 uv : TEXCOORD0,
    Peridot::VertexShaderContext ctx
) {
    VertexOutput vo;

    vo.pos = ctx.worldToClipSpace(pos);
    vo.fragmentInput.uv = uv.xy;

    return vo;
}

[shader("fragment")]
float4 fragMain(FragmentInput input : Varyings, Peridot::FragmentShaderContext ctx) : SV_Target {
    return ctx.properties.tex.Sample(input.uv);
}
  End
End
