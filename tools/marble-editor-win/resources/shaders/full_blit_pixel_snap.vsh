#version 450

layout(location = 0) out vec2 uv;
out gl_PerVertex { out vec4 gl_Position; };
layout(push_constant) uniform PushConstants
{
    vec2 texelSize;
};

void main()
{
    uv = vec2(float((uint(gl_VertexIndex) & 0x01) == 0), float((uint(gl_VertexIndex) & 0x02) == 0));
    gl_Position = vec4((uv * 2.0f - 1.0f) * (1.0f - 2.0f * texelSize) + texelSize, 0.0f, 1.0f);
}
