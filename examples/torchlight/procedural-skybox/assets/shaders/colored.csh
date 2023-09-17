VertexInput {
    Binding 0 [PerVertex] { pos: vec4; color: vec4; }
}

Uniform[VertexShader](0, 0) ViewUniform {
    mat4 main_view_projection;
    mat4 main_view;
    float persp_fov_rad;
}

VertexShader {
    vc = color;
    RasterPosition = transpose(main_view_projection) * pos;
}

Varyings VertexShader -> FragmentShader {
    vc: vec4;
}

FragmentShader {
    vec4 c = vc;
    c.rgb *= c.a;
    Target[0] = c;
}