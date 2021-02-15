VertexInput {
    Binding 0 [PerVertex] { ipos: vec2; }
}
SpecConstant[VertexShader](0) TargetTextureWidth: float = 0.5;
SpecConstant[VertexShader](1) TargetTextureHeight: float = 0.5;
VertexShader {
    RasterPosition = vec4((ipos / vec2(TargetTextureWidth, -TargetTextureHeight)) - 1.0, 0.0, 1.0);
}
SpecConstant[FragmentShader](0) EnableColorOutput: bool = false;
FragmentShader {
    if (EnableColorOutput) Target[0] = vec4(1.0, 1.0, 1.0, 1.0);
}
