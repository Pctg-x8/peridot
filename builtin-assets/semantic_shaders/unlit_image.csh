VertexInput {
    [Position] pos: vec4;
    [Texcoord0] uv: vec4;
}

Uniform[Vertex](0, 0) CameraParameters {
    viewProjectionMatrix: mat4;
}
Uniform[Vertex](1, 0) ObjectParameters {
    transformMatrix: mat4;
}
Sampler2D[Fragment](1, 1) tex;

VertexShader {
    gl_Position = transpose(viewProjectionMatrix) * transpose(transformMatrix) * pos;
    uv_v = uv.xy;
}

Varyings Vertex -> Fragment {
    uv_v: vec2;
}

FragmentShader {
    color = texture(tex, uv_v);
}

FragmentOutput {
    color: vec4;
}
