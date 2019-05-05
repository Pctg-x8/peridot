VertexInput {
    Binding 0 [PerVertex] { ipos: vec3; }
    Binding 1 [PerVertex] { normal: vec3; }
}
Uniform[VertexShader](0, 0) WorldSettings { mat4 vp; vec4 light_dir; }
Storage[VertexShader](1, 0) ObjectSettings { mat4 tf[INSTANCES]; }
PushConstant[FragmentShader] MaterialData { vec4 albedo; float metallic, roughness; }
SpecConstant[VertexShader](0) INSTANCES: uint = 1;

VertexShader {
    RasterPosition = transpose(vp) * transpose(tf[gl_InstanceIndex]) * vec4(ipos, 1.0);
    RasterPosition.y = -RasterPosition.y;
    lambert_term = dot(light_dir.xyz, mat3(transpose(tf[gl_InstanceIndex])) * -normal.xyz);
}
Varyings VertexShader -> FragmentShader {
    lambert_term: float;
}
FragmentShader {
    Target[0] = albedo;
    Target[0].rgb *= lambert_term * Target[0].a;
}