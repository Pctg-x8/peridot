VertexInput {
    Binding 0 [PerVertex] { ipos: vec2; }
}
PushConstant[VertexShader] pushConstant {
    vec2 viewportSize;
}
VertexShader {
    RasterPosition = vec4((ipos / vec2(viewportSize.x, -viewportSize.y)) * 2.0 - 1.0, 0.0, 1.0);
}
SpecConstant[FragmentShader](0) EnableColorOutput: bool = false;
FragmentShader {
    if (EnableColorOutput) Target[0] = vec4(1.0, 1.0, 1.0, 1.0);
}
