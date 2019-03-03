VertexInput {
    Binding 0 [PerVertex] { ipos: vec4; }
    Binding 1 [PerVertex] { normal: vec4; }
    Binding 2 [PerVertex] { uv: vec2; }
}
Uniform[VertexShader](0, 0) WorldSettings { mat4 vp; vec4 light_dir; }
Uniform[VertexShader](1, 0) ObjectSettings { mat4 tf; }

VertexShader {
    RasterPosition = transpose(vp) * transpose(tf) * ipos;
    uv_v = uv;
    lambert_term = dot(light_dir.xyz, normal.xyz) * 0.5 + 0.5;
}
Varyings VertexShader -> FragmentShader {
    uv_v: vec2;
    lambert_term: float;
}
FragmentShader {
    Target[0] = vec4(lambert_term, lambert_term, lambert_term, 1.0);
}