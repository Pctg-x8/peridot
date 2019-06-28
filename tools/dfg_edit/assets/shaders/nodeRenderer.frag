#version 450

layout(location = 0) in vec2 uv;
layout(location = 1) in vec4 tint_color;
layout(location = 2) in vec2 uv_scale;
layout(location = 0) out vec4 target;

void main()
{
    vec4 brightness = mix(vec4(0.3f, 0.3f, 0.3f, 1.0f), vec4(0.6f, 0.6f, 0.6f, 1.0f), uv.y);
    target = mix(brightness, tint_color, 0.3f);
    target *= 0.8f;
}
