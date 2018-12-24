VertexInput { Binding 0 [PerVertex] { pos: vec3; uv: vec2; } }
VertexShader {
    RasterPosition = transpose(vp) * transpose(obj) * vec4(pos, 1.0);
    uv_v = uv;
}
FragmentShader {
    Target[0] = vec4(uv_v, 1.0, 1.0);
}

Varyings VertexShader -> FragmentShader {
    uv_v: vec2;
}

Uniform[VertexShader](0, 0) Camera { mat4 vp, obj; }
