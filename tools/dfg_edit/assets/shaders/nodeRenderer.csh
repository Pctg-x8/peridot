VertexInput {
    Binding 0 [PerVertex] { ipos: vec2; }
}
SpecConstant[FragmentShader](1) CornerRoundingRadius: float = 0.0;
Uniform[VertexShader](0, 0) ScreenProps {
    float scaling; vec2 offset_g;
}
Header[VertexShader]
{
    struct NodeRenderParams
    {
        vec2 offset; vec2 size;
        vec4 tint_color;
    }
}
Uniform[VertexShader](1, 0) NodePlacements {
    NodeRenderParams render_params[32];
}
VertexShader {
    NodeRenderParams& idata = render_params[InstanceIndex];
    vec2 p = ipos * 0.5f * idata.size + offset_g + idata.offset;
    RasterPosition = vec4(p, 0.0f, 1.0f);
    uv = ipos * 0.5f + 0.5f;
    tint_color = idata.tint_color;
}
Varyings VertexShader -> FragmentShader {
    uv: vec2;
    tint_color: vec4;
}
FragmentShader {
    vec4 brightness = mix(vec4(0.3f, 0.3f, 0.3f, 1.0f), vec4(0.6f, 0.6f, 0.6f, 1.0f), uv.y);
    vec4 final_color = mix(brightness, tint_color, 0.3f);
    final_color *= 0.8f;
    Target[0] = final_color;
}