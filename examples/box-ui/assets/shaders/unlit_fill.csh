VertexInput {
    Binding 0 [PerVertex] { pos: vec2; }
    Binding 1 [PerInstance] { pos_st: vec4; col: vec4; }
}
PushConstant[VertexShader] ScreenParams { vec2 pixelSize; }

VertexShader {
    RasterPosition = vec4((pos * pos_st.xy + pos_st.zw) * 2.0f / pixelSize - 1.0f, 0.0f, 1.0f);
    col_o = col;
    col_o.rgb *= col_o.a;
}
Varyings VertexShader -> FragmentShader {
    col_o: vec4;
}
FragmentShader {
    Target[0] = col_o;
}
