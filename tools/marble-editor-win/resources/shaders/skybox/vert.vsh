// Skybox Shader from http://publications.lib.chalmers.se/records/fulltext/203057/203057.pdf

#version 450

layout(location = 0) in vec2 pos;
layout(location = 1) in vec2 uvin;

layout(location = 0) out vec2 uv;
out gl_PerVertex { out vec4 gl_Position; };

void main() {
    uv = uvin;
    gl_Position = vec4(pos, 1.0f, 1.0f);
}
