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
    vec2 target_pixels; int glyph_index; int dummy;
}
SpecConstant[VertexShader](0) RasterOffsetX: float = 0.0;
SpecConstant[VertexShader](1) RasterOffsetY: float = 0.0;
VertexShader {
    vec4 gst; vec2 gext; fetchGlyphTransformFor(glyph_index, gst, gext);
    RasterPosition = vec4((2.0 * transformAffine(applyHints(ipos), gst, gext) / target_pixels) * vec2(1.0, -1.0), 0.0, 1.0);
    RasterPosition.xy += vec2(RasterOffsetX, RasterOffsetY);
    helper_coord = vec2(curve_data.xy * 0.5);
    lb_dir = curve_data.z;
}
Varyings VertexShader -> FragmentShader { helper_coord: vec2; lb_dir: int; }
FragmentShader {
    /*// gradients
    vec2 px = dFdx(helper_coord), py = dFdy(helper_coord);
    // chain rule
    float fx = (2 * helper_coord.x) * px.x - px.y, fy = (2 * helper_coord.x) * py.x - py.y;
    // signed distance
    float sd = (pow(helper_coord.x, 2) - helper_coord.y) / sqrt(pow(fx, 2) + pow(fy, 2));
    // linear alpha 1..inside, 0..outside
    float alpha = min(0.5 - lb_dir*sd, 1);
    if(alpha < 0) discard;
    Target[0] = vec4(1.0, 1.0, 1.0, 1.0) * alpha;*/

    float sd = pow(helper_coord.x, 2) - helper_coord.y;
    if(sd * lb_dir < 0) discard;
    Target[0] = vec4(1.0, 1.0, 1.0, 1.0);
}
