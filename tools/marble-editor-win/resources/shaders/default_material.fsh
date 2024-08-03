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
    const float diffuse = pow(dot(normal.xyz, -incidentLightDir) * 0.5f + 0.5f, 2.0f) * lightIntensity;

    color = vec4(vec3(0.8f, 0.8f, 0.8f) * diffuse + vec3(0.3f, 0.3f, 0.3f), 1.0f);
}
