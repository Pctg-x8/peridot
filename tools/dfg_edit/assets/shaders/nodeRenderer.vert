#version 450

layout(location = 0) in vec2 ipos;
layout(location = 0) out vec2 uv;
layout(location = 1) out vec4 tint_color;
layout(location = 2) out vec2 uv_scale;
out gl_PerVertex { out vec4 gl_Position; };

#define MaxRenderableInstances 32

layout(set = 0, binding = 0) uniform ScreenProps
{
    float scaling; vec2 offset_g;
};
struct RenderParam
{
    vec2 offset; vec2 size;
    vec4 tint_color;
};
layout(set = 1, binding = 0) buffer NodeRenderingParams
{
    RenderParam render_params[MaxRenderableInstances];
};

void main()
{
    const RenderParam idata = render_params[gl_InstanceIndex];
    vec2 p = ipos * 0.5f * idata.size + offset_g + idata.offset;
    gl_Position = vec4(p * scaling, 0.0f, 1.0f);
    uv = ipos * 0.5f + 0.5f;
    tint_color = idata.tint_color;
    uv_scale = idata.size;
}
