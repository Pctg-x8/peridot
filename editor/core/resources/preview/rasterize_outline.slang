[vk::binding(0, 0)]
Sampler2D blurredSilhouette;

struct VshOutput {
    float4 pos : SV_Position;
    float2 uv : TEXCOORD0;
}

[shader("vertex")]
VshOutput vertMain(uint vindex : SV_VertexID) {
    VshOutput vo;
    vo.uv = float2(select((vindex & 0x01) == 0x00, 0.0, 1.0), select((vindex & 0x02) == 0x00, 0.0, 1.0));
    vo.pos = float4(vo.uv * 4.0 - 1.0, 0.0, 1.0);
    return vo;
}

[shader("fragment")]
float4 fragMain(float2 uv : TEXCOORD0) : SV_Target {
    return float4(1.0, 0.5, 0.0, 1.0) * step(0.001, blurredSilhouette.Sample(uv).r);
}
