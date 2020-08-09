VertexInput { Binding 0 [PerVertex] { pos: vec2; uv: vec2; } }
VertexShader {
    const float scaling = (time * 2.0) >= 1.0 ? 0.0 : (1.0 - pow(1.0 - time, 8.0));
    RasterPosition = transpose(projection) * vec4(offset + pos * scaling, 0.5, 1.0);
    uv_v = uv;
}
Varyings VertexShader -> FragmentShader {
    uv_v: vec2;
}
FragmentShader {
    Target[0] = vec4(uv_v, 0.0, 1.0);
}

Uniform[VertexShader](0, 0) UICamera {
    mat4 projection;
    float time;
    vec2 offset;
}
