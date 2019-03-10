VertexInput {
    Binding 0 [PerVertex] { ipos: vec4; }
    Binding 1 [PerVertex] { normal: vec4; }
    Binding 2 [PerVertex] { uv: vec2; }
}
Uniform[VertexShader](0, 0) WorldSettings { mat4 vp; vec4 light_dir; }
Uniform[VertexShader](1, 0) ObjectSettings { mat4 tf; }
PushConstant[FragmentShader] MaterialData { vec4 diffuse_color; }
Sampler2D[FragmentShader](2, 0) tex

VertexShader {
    RasterPosition = transpose(vp) * transpose(tf) * ipos;
    RasterPosition.y = -RasterPosition.y;
    uv_v = uv;
    lambert_term = dot(light_dir.xyz, -normal.xyz) * 0.25 + 0.75;
}
Varyings VertexShader -> FragmentShader {
    uv_v: vec2;
    lambert_term: float;
}
FragmentShader {
    Target[0] = texture(tex, uv_v) * diffuse_color;
    Target[0].rgb *= lambert_term * Target[0].a;
}