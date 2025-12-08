
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
    o.fragmentInput.col.rgb *= o.fragmentInput.col.a;

    return o;
}

[shader("fragment")]
float4 frag(FragmentInput input : Varyings) : SV_Target {
    float4 x = input.col;
    x.x += input.uv.x;
    x.y += input.uv.y;

    return x;
}
    End
End
