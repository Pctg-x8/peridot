#version 450

layout(location = 0) in vec4 pos;
layout(location = 1) in vec4 col;

layout(location = 0) out vec4 ocol;
out gl_PerVertex { out vec4 gl_Position; };

layout(set = 0, binding = 0) uniform Camera {
    mat4 vp;
};

void main() {
    ocol = col;
    gl_Position = transpose(vp) * pos;
}
