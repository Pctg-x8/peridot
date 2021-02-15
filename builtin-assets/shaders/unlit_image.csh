VertexInput { Binding 0 [PerVertex] { pos: vec3; uv: vec2; } }
VertexShader {
    RasterPosition = transpose(vp) * transpose(obj) * vec4(pos, 1.0);
    uv_v = uv;
}
FragmentShader {
    Target[0] = texture(tex, uv_v);
}

Varyings VertexShader -> FragmentShader {
    uv_v: vec2;
}

Uniform[VertexShader](0, 0) Camera { mat4 vp, obj; }
Sampler2D[FragmentShader](0, 1) tex

