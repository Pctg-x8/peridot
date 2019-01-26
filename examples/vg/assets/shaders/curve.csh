VertexInput {
    Binding 0 [PerVertex] { ipos: vec2; }
    Binding 1 [PerVertex] { curve_data: i8vec4; }
}
SamplerBuffer[VertexShader](0, 0) glyphTransformSTExt
Header[VertexShader] {
    void fetchGlyphTransformFor(int id, out vec4 st, out vec2 ext) {
        st  = texelFetch(glyphTransformSTExt, id * 2 + 0);
        ext = texelFetch(glyphTransformSTExt, id * 2 + 1).xy;
    }
    vec2 transformAffine(vec2 vin, vec4 st, vec2 ext) { return vin * st.xy + vin.yx * ext + st.zw; }
    vec2 applyHints(vec2 pos) {
        return pos;
    }
}
PushConstant[VertexShader] ScreenInfo {
    int glyph_index;
}
VertexShader {
    vec4 gst; vec2 gext; fetchGlyphTransformFor(glyph_index, gst, gext);
    RasterPosition = vec4((2.0 * transformAffine(applyHints(ipos), gst, gext) / (64.0 * 96.0)) * vec2(1.0, -1.0) * vec2(3.0 / 4.0, 1.0), 0.0, 1.0);
    helper_coord = vec2(curve_data.xy * 0.5);
    lb_dir = curve_data.z;
}
Varyings VertexShader -> FragmentShader { helper_coord: vec2; lb_dir: int; }
FragmentShader {
    float sd = pow(helper_coord.x, 2) - helper_coord.y;
    if(sd * lb_dir < 0) discard;
    Target[0] = vec4(1.0, 1.0, 1.0, 1.0);
}
