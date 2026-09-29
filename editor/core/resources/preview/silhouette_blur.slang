[vk::constant_id(0)]
const float offsetX = 0.0;
[vk::constant_id(1)]
const float offsetY = 0.0;
static const float2 offset = float2(offsetX, offsetY);

[vk::constant_id(2)]
const float gaussianBlurFactor0;
[vk::constant_id(3)]
const float gaussianBlurFactor1;
[vk::constant_id(4)]
const float gaussianBlurFactor2;
[vk::constant_id(5)]
const float gaussianBlurFactor3;
static const float[] gaussianBlurFactors = { gaussianBlurFactor3, gaussianBlurFactor2, gaussianBlurFactor1, gaussianBlurFactor0, gaussianBlurFactor1, gaussianBlurFactor2, gaussianBlurFactor3 };
static const uint gaussianBlurTaps = 7;
static const float gaussianBlurOffset = float(gaussianBlurTaps - 1) * 0.5;

[vk::binding(0, 0)]
Sampler2D input;

struct VshOutput {
    float4 pos : SV_Position;
    float2 uv : TEXCOORD0;
}

[shader("vertex")]
VshOutput vertMain(uint vindex: SV_VertexID) {
    VshOutput vo;
    vo.uv = float2(select((vindex & 0x01) == 0x00, 0.0, 2.0), select((vindex & 0x02) == 0x00, 0.0, 2.0));
    vo.pos = float4(vo.uv * 2.0 - 1.0, 0.0, 1.0);
    return vo;
}

[shader("fragment")]
float4 fragMain(float2 uv : TEXCOORD0) : SV_Target {
    var x = float4(0.0);
    for (var i = 0; i < gaussianBlurTaps; i++) {
        x += input.Sample(uv + offset * (float(i) - gaussianBlurOffset)) * gaussianBlurFactors[i];
    }

    return x;
}
