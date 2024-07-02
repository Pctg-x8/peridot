#version 450

layout(location = 0) in vec4 pos;
layout(location = 1) in vec4 normal;
layout(location = 2) in vec4 uv;

out gl_PerVertex { out vec4 gl_Position; };
layout(location = 0) out vec4 normal_v;
layout(location = 1) out vec4 uv_v;

layout(set = 0, binding = 0) uniform RenderCameraData
{
    mat4 cameraViewProjectionMatrix;
};
layout(set = 1, binding = 0) uniform PerObjectData
{
    mat4 objectTransformMatrix;
};

void main()
{
    gl_Position = pos * objectTransformMatrix * cameraViewProjectionMatrix;
    normal_v = normalize(normal * objectTransformMatrix);
    uv_v = uv;
}
