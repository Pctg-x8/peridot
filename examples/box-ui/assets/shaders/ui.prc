Properties
    mainTex: Texture2D = Use "Texture2D.white"
End

Pass "Unlit"
    RenderOption NoCulling
    VertexBindings
        pos: Float2 [POSITION0]
        pos_st: Float4 [POSITION1]
        uv_st: Float4 [TEXCOORD0]
        col: Float4 [COLOR0]
    End

    Shader
struct VertexOutput {
    FragmentInput fragmentInput : Varyings;
    float4 pos : SV_Position;
}

struct FragmentInput {
    float2 uv;
    float4 col;
}

[shader("vertex")]
VertexOutput vert(Vertex v) {
    VertexOutput o;

    o.pos = float4((v.pos * v.pos_st.xy + v.pos_st.zw) * 2.0 / PeridotCameraParameters::targetPixelSize() - 1.0, 0.0, 1.0);
    o.fragmentInput.uv = v.pos * v.uv_st.xy + v.uv_st.zw;
    o.fragmentInput.col = v.col;

    return o;
}

[shader("fragment")]
float4 frag(FragmentInput input : Varyings) : SV_Target {
    float d = PeridotMaterialParameters::mainTex.Sample(input.uv).r;
    float a = smoothstep(0.5 - 1.0 / 32.0, 0.5, d);
    float4 col = float4(1.0, 1.0, 1.0, a) * input.col;
    col.rgb *= col.a;
    return col;
}
    End
End
