# Peridot Rendering Configuration example

Properties
  mainTexture: Texture2D = Use "Texture2D.white"
  [Immutable]
  colorTint: RGB = (1, 1, 1, 1)
  [PerDrawCall]
  colorTintOverride: RGB = (1, 1, 1, 1)
  [PerDrawCall]
  mode: UInt = 0
End

Pass "Visibility" = Use "Visibility"
Pass "Visibility.Lighting"
  VertexBindings
    pos: Float4 [POSITION0]
    uv: Float2 [TEXCOORD0]
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
VertexOutput vert(Vertex v) {
    VertexOutput vo;

    vo.fragmentInput.uv = v.uv;
    vo.pos = v.pos;

    return vo;
}

[shader("fragment")]
float4 frag(FragmentInput input : Varyings) : SV_Target {
    return PeridotMaterialParameters::mainTexture.Sample(input.uv) * PeridotMaterialParameters::colorTint * PeridotMaterialParameters::perDrawCall.colorTintOverride;
}
  End
End
