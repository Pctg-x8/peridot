#version 450

layout(location = 0) in vec2 pos;
layout(location = 1) in vec2 uv;

layout(location = 0) out vec2 uvout;
out gl_PerVertex { out vec4 gl_Position; };

void main()
{
    gl_Position = vec4(pos, 0.0f, 1.0f);
    uvout = uv;
}
