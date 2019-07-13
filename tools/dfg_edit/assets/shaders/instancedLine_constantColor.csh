VertexInput {
    Binding 0 [PerVertex] { pos: vec2; }
}
PushConstant[FragmentShader] Color { vec4 color; }
Uniform[VertexShader](0, 0) ScreenProps { float scaling; vec2 offset; }
SpecConstant[VertexShader](0) Spacing: float = 10.0;
SpecConstant[VertexShader](1) CloneDirection: int = 0;
SpecConstant[VertexShader](2) AspectWH: float = 1.0;

Header[VertexShader] {
    vec2 vdir() { return CloneDirection == 0 ? vec2(1.0, 0.0) : vec2(0.0, 1.0); }
    float aspectCorrect() { return CloneDirection == 0 ? 1.0 : AspectWH; }
}

VertexShader {
    vec2 offset_l = offset - floor(offset / Spacing) * Spacing;
    vec2 p = pos + vdir() * (offset_l + vec2(Spacing * InstanceIndex)) * scaling * aspectCorrect() - vdir();
    RasterPosition = vec4(p, 0.0, 1.0);
}
FragmentShader {
    Target[0] = color;
    Target[0].rgb *= Target[0].a;
}
