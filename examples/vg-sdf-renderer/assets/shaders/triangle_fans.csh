VertexInput {
    Binding 0 [PerVertex] { ipos: vec2; }
}
SpecConstant[VertexShader](0) TargetTextureWidth: float = 640.0;
SpecConstant[VertexShader](1) TargetTextureHeight: float = 480.0;
VertexShader {
    RasterPosition = vec4(ipos / vec2(TargetTextureWidth, -TargetTextureHeight), 0.0, 1.0);
}
SpecConstant[FragmentShader](0) EnableColorOutput: bool = false;
FragmentShader {
    if (EnableColorOutput) Target[0] = vec4(1.0, 1.0, 1.0, 1.0);
}
