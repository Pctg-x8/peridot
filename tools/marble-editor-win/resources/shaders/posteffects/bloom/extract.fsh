#version 450

layout(set = 0, binding = 0) uniform sampler2D inputTex;
layout(set = 0, binding = 1) uniform PostEffectGlobalWorkBuffer {
    float exposure_base_lum;
};
layout(location = 0) in vec2 uv;
layout(location = 0) out vec4 target;

layout(push_constant) uniform PushConstants {
    float threshold;
    float sensor_sensitivity, reflected_light_calibration_const, lens_vignette_attenuation;
};

float lum(vec4 col) {
    return dot(vec3(0.2125, 0.7154, 0.0721), col.rgb);
}

vec3 apply_auto_exposure(vec3 hdr) {
    return hdr * exposure_base_lum;
}

void main() {
    const vec4 col = texture(inputTex, uv);

    target = vec4(apply_auto_exposure(col.rgb * max(0.0f, lum(col) - threshold)), 1.0);
}
