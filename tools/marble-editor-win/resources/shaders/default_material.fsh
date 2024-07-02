#version 450

layout(location = 0) in vec4 normal;
layout(location = 1) in vec4 uv;

layout(location = 0) out vec4 color;

layout(set = 1, binding = 1) uniform ForwardRenderLightData {
    vec3 incidentLightDir;
    float lightIntensity;
};

void main()
{
    const float diffuse = pow(dot(normal.xyz, -incidentLightDir) * 0.5 + 0.5, 2.0) * lightIntensity;

    color = vec4(vec3(1.0, 1.0, 1.0) * diffuse + vec3(0.3, 0.3, 0.3), 1.0);
    color = vec4(normal.xyz, 1.0f);
}
