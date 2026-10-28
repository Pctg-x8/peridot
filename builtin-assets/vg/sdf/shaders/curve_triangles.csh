VertexInput {
    Binding 0 [PerVertex] { ipos: vec2; uv: vec2; }
}
SpecConstant[VertexShader](0) TargetTextureWidth: float = 640.0;
SpecConstant[VertexShader](1) TargetTextureHeight: float = 480.0;
VertexShader {
    RasterPosition = vec4((ipos / vec2(TargetTextureWidth, -TargetTextureHeight)) * 2.0 - 1.0, 0.0, 1.0);
    uv_out = uv;
}
Varyings VertexShader -> FragmentShader { uv_out: vec2; }
FragmentShader {
    float x = uv_out.x * uv_out.x - uv_out.y;
    if (x >= 0) discard;
    Target[0] = vec4(1.0, 1.0, 1.0, 1.0);
}
