VertexInput {
    Binding 0 [PerVertex] { pos: vec2; }
}
VertexShader {
    RasterPosition = vec4(pos, 0.0, 1.0);
    vpos = pos;
}
Varyings VertexShader -> FragmentShader {
    vpos: vec2;
}
FragmentShader {
    vec3 eyepos = vec3(0.0, 0.0, -FOCAL_LENGTH);
    vec3 raydir = vec3(vpos, 0.0) - eyepos;
    Target[0] = vec4(raydir, 1.0);
}

SpecConstant[FragmentShader](0) FOCAL_LENGTH: float = 25.0;
