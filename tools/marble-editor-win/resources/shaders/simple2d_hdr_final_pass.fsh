#version 450

#include "hdr_final_pass/ACES.glsl"

layout(location = 0) in vec2 uv;

layout(location = 0) out vec4 color;

layout(set = 0, binding = 0, input_attachment_index = 0) uniform subpassInput hdrColor;

layout(set = 0, binding = 1) uniform PostEffectGlobalWorkBuffer {
    float exposure_base_lum;
};

layout(push_constant) uniform PushConstants {
    float sensor_sensitivity, reflected_light_calibration_const, lens_vignette_attenuation;
};

vec3 apply_auto_exposure(vec3 hdr) {
    return hdr * exposure_base_lum;
}

void main() {
    vec4 hdr = subpassLoad(hdrColor);

    hdr.rgb = apply_auto_exposure(hdr.rgb);

    vec4 ldr = vec4(ACESFitted(hdr.rgb), 1.0f);

    ldr.rgb = 1.0 - pow(1.0 - ldr.rgb, vec3(2.2));

    color = ldr;
}
