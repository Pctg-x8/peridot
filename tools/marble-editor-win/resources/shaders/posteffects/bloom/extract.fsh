#version 450

layout(set = 0, binding = 0) uniform sampler2D inputTex;
layout(location = 0) in vec2 uv;
layout(location = 0) out vec4 target;

layout(push_constant) uniform PushConstants {
    float threshold;
};

void main() {
    const vec4 col = texture(inputTex, uv);

    target = vec4(max(vec3(0.0f), col.rgb - threshold), 1.0);
}
