// Skybox Shader from http://publications.lib.chalmers.se/records/fulltext/203057/203057.pdf

VertexInput {
    Binding 0 [PerVertex] { pos: vec2; uvin: vec2; }
}

VertexShader {
    uv = uvin;
    RasterPosition = vec4(pos, 0.0f, 1.0f);
}

Varyings VertexShader -> FragmentShader {
    uv: vec2;
}

FragmentShader {
    Target[0] = vec4(uv, 1.0f, 1.0f);
}
