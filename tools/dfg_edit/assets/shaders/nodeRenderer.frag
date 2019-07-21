#version 450

layout(location = 0) in vec2 uv;
layout(location = 1) in vec4 tint_color;
layout(location = 2) in vec2 uv_scale;
layout(location = 0) out vec4 target;

const float CornerRadius = 12.0f;
const float CornerEdgeThresold = CornerRadius * 0.5f;
const float BorderThinkness = 0.5f;

void main()
{
    const vec2 uv_dist_from_edge = uv_scale - abs(uv * 2.0f - 1.0f) * uv_scale;

    // AA用に追加で考慮するサイズ uv_scale座標空間で
    const float aa_ext = 1.0f;

    const float re_outer = length(max(vec2(0.0f), vec2(CornerRadius) - uv_dist_from_edge * vec2(1.0f, 1.0f)));
    const float a = 1.0f - smoothstep(CornerEdgeThresold - aa_ext, CornerEdgeThresold, re_outer);
    const float shadow_alpha = max(1.0f - re_outer / CornerRadius, 0.0f) * 0.75f;
    const float border_alpha =
        smoothstep(CornerEdgeThresold - BorderThinkness * 0.5f - aa_ext, CornerEdgeThresold - BorderThinkness * 0.5f, re_outer) *
        (1.0f - smoothstep(CornerEdgeThresold + BorderThinkness * 0.5f, CornerEdgeThresold + BorderThinkness * 0.5f + aa_ext, re_outer));
    const vec4 brightness = mix(vec4(0.1f, 0.1f, 0.1f, 1.0f), vec4(0.25f, 0.25f, 0.25f, 1.0f), pow(uv.y, 1.8f));
    const vec4 face_color = mix(brightness, tint_color, 0.2f);
    const vec4 border_color = vec4(0.8f, 0.8f, 0.8f, 1.0f);

    target = mix(vec4(0.1f, 0.1f, 0.1f, 1.0f) * shadow_alpha, face_color * 0.8f, floor(a));
    target = mix(target, border_color, border_alpha);
}
