#version 450

layout(location = 0) in vec4 pos;
layout(location = 1) in vec4 color;
out gl_PerVertex { out vec4 gl_Position; };
layout(location = 0) out vec4 colorOut;

layout(std140, set = 0, binding = 0) uniform RenderCameraMatrix
{
    mat4 cameraViewProjectionMatrix;
};
layout(push_constant) uniform PushedProperties
{
    layout(offset = 0) mat4 objectTransformMatrix;
};

void main()
{
    gl_Position = pos * objectTransformMatrix * cameraViewProjectionMatrix;
    colorOut = color;
}
