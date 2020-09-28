#version 450

#define MAX_RENDERED_INSTANCE_SIZE 65536

layout(location = 0) in vec4 pos;
layout(location = 1) in vec2 iuv;

layout(location = 0) out vec2 ouv;
out gl_PerVertex { out vec4 gl_Position; };

layout(set = 0, binding = 0, std140) uniform Camera {
    mat4 mvp;
};
layout(set = 1, binding = 0, std140) readonly buffer InstanceMatrices {
    mat4 inst[MAX_RENDERED_INSTANCE_SIZE];
};

void main() {
    gl_Position = inst[gl_InstanceIndex] * mvp * pos;
    ouv = iuv;
}
