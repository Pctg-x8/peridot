Properties
  tex: Texture2D = Use "Texture2D.white"
End

Pass "Unlit"
  VertexBindings
    pos: Float4 [POSITION0]
    uv: Float4 [TEXCOORD0]
  End

  Shader
struct VertexOutput {
    FragmentInput fragmentInput : Varyings;
    float4 pos : SV_Position;
}

struct FragmentInput {
    float2 uv : TEXCOORD0;
}

[shader("vertex")]
VertexOutput vertMain(Vertex v) {
    VertexOutput vo;

    vo.pos = mul(mul(v.pos, PeridotObjectParameters::transformMatrix()), PeridotCameraParameters::viewProjectionMatrix());
    vo.fragmentInput.uv = v.uv.xy;

    return vo;
}

[shader("fragment")]
float4 fragMain(FragmentInput input : Varyings) : SV_Target {
    return PeridotMaterialParameters::tex.Sample(input.uv);
}
  End
End
