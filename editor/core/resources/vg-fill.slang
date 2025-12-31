[vk::constant_id(0)]
const float TargetTextureWidth = 0.5;
[vk::constant_id(1)]
const float TargetTextureHeight = 0.5;

[shader("vertex")]
float4 vertMain([vk::location(0)] float2 pos: POSITION0) : SV_Position
{
    return float4((pos / float2(TargetTextureWidth, -TargetTextureHeight)) * 2.0 - 1.0, 0.0, 1.0);
}

[shader("fragment")]
void fragMain()
{
}
