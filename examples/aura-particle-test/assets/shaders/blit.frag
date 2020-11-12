#version 450

layout(location = 0) in vec2 uv;
layout(location = 1) in float alpha;
layout(location = 0) out vec4 target;

void main() {
    target = vec4(uv.xy, 1.0f, 1.0f) * alpha;
}
