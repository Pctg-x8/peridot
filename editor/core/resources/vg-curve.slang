[vk::constant_id(0)]
const float TargetTextureWidth = 640.0;
[vk::constant_id(1)]
const float TargetTextureHeight = 480.0;

struct VertexOutput
{
    float4 pos : SV_Position;
    float2 uv : TEXCOORD0;
}

[shader("vertex")]
VertexOutput vertMain([vk::location(0)] float2 pos: POSITION0, [vk::location(1)] float2 uv: TEXCOORD0)
{
    VertexOutput vo;
    vo.pos = float4((pos / float2(TargetTextureWidth, -TargetTextureHeight)) * 2.0 - 1.0, 0.0, 1.0);
    vo.uv = uv;
    return vo;
}

[shader("fragment")]
void fragMain(float2 uv: TEXCOORD0)
{
    let x = uv.x * uv.x - uv.y;
    if (x >= 0)
    {
        discard;
    }
}
