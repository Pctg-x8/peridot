VertexInput {
    Binding 0 [PerVertex] { ipos: vec2; }
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
    RasterPosition = vec4((2.0 * transformAffine(applyHints(ipos), gst, gext) / vec2(640.0, 480.0)) * vec2(1.0, -1.0), 0.0, 1.0);
}
FragmentShader {
    Target[0] = vec4(1.0);
}
