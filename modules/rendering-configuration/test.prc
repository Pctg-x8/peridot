# Peridot Rendering Configuration example

Properties
  mainTexture: Texture2D = Use "Texture2D.white"
  [Immutable]
  colorTint: RGB = (1, 1, 1, 1)
End

Pass "Visibility" = Use "Visibility"
Pass "Visibility.Lighting"
  VertexBindings
    pos : POSITION0
    uv : TEXCOORD0
  End

  Shader
struct Vertex {
    float4 pos : POSITION;
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
VertexOutput vert(Vertex v) {
    VertexOutput vo;

    vo.fragmentInput.uv = v.uv;
    vo.pos = v.pos;

    return vo;
}

[shader("fragment")]
float4 frag(FragmentInput input : Varyings) : SV_Target {
    return mainTexture.Sample(input.uv) * colorTint;
}
  End
End
