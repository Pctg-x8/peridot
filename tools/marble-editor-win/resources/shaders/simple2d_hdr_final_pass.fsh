#version 450

#include "hdr_final_pass/ACES.glsl"

layout(location = 0) in vec2 uv;

layout(location = 0) out vec4 color;

layout(set = 0, binding = 0, input_attachment_index = 0) uniform subpassInput hdrColor;

void main() {
    vec4 hdr = subpassLoad(hdrColor);

    vec4 ldr = vec4(ACESFitted(hdr.rgb), 1.0f);

    color = ldr;
}
