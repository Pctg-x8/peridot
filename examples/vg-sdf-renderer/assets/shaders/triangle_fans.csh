VertexInput {
    Binding 0 [PerVertex] { ipos: vec2; }
}
VertexShader {
    RasterPosition = vec4(ipos / vec2(640.0, -480.0), 0.0, 1.0);
}
FragmentShader {
    Target[0] = vec4(1.0, 1.0, 1.0, 1.0);
}
