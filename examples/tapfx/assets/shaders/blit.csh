VertexInput {
    [Position] pos: vec2;
    [Texcoord0] uv: vec2;
}
VertexShader {
    const float scaling = (time * 2.0) >= 1.0 ? 0.0 : (1.0 - pow(1.0 - time, 8.0));
    gl_Position = transpose(projection) * vec4(offset + pos * scaling, 0.5, 1.0);
    uv_v = uv;
    alpha = pow(1.0 - time, 1.8);
}
Varyings Vertex -> Fragment {
    uv_v: vec2;
    alpha: float;
}
FragmentShader {
    const vec4 c = texture(tex, uv_v);
    color = vec4(c.xyz, 1.0) * c.a * alpha;
}
FragmentOutput {
    color: vec4;
}

Uniform[Vertex](0, 0) UICamera {
    projection: mat4;
    time: float;
    offset: vec2;
}
Sampler2D[Fragment](1, 0) tex;
