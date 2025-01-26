VertexInput {
    [Position] pos: vec4;
    [Texcoord0] uvw: vec4;
    [Normal] normal: vec4;
}

Uniform[Vertex](0, 0) CameraParameter {
    viewProjectionMatrix: mat4;
    incidentLightDir: vec4;
}
Uniform[Vertex](1, 0) ObjectParameter {
    transformMatrix: mat4;
}

VertexShader {
    gl_Position = transpose(viewProjectionMatrix) * (transpose(transformMatrix) * pos);
    uvw_v = uvw;
    lightingIntensity = dot(-incidentLightDir.xyz, normal.xyz) * 0.5f + 0.5f;
}

Varyings Vertex -> Fragment {
    uvw_v: vec4;
    lightingIntensity: float;
}

FragmentShader {
    color = vec4(vec3(1.0f, 1.0f, 1.0f) * lightingIntensity, 1.0f);
}

FragmentOutput {
    color: vec4;
}
