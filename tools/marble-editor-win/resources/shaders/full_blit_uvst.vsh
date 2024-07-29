#version 450

layout(location = 0) out vec2 uv;
out gl_PerVertex { out vec4 gl_Position; };

layout(push_constant) uniform PushConstants
{
    vec4 uvst;
};

void main()
{
    const vec2 base = vec2(float((uint(gl_VertexIndex) & 0x01) == 0), float((uint(gl_VertexIndex) & 0x02) == 0));
    uv = base * uvst.zw + uvst.xy;
    gl_Position = vec4(base * 2.0f - 1.0f, 0.0f, 1.0f);
}
