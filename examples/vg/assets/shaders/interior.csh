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
    layout(offset=0) vec2 target_pixels;
    layout(offset=8) int glyph_index;
    layout(offset=12) int unused;
}
SpecConstant[VertexShader](0) RasterOffsetX: float = 0.0;
SpecConstant[VertexShader](1) RasterOffsetY: float = 0.0;
VertexShader {
    vec4 gst; vec2 gext; fetchGlyphTransformFor(glyph_index, gst, gext);
    RasterPosition = vec4((2.0 * transformAffine(applyHints(ipos), gst, gext) / target_pixels) * vec2(1.0, -1.0), 0.0, 1.0);
    RasterPosition.xy += vec2(RasterOffsetX, RasterOffsetY);
}
FragmentShader {
    Target[0] = vec4(1.0);
}
