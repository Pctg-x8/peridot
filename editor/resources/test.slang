struct Vertex
{
    [vk::location(0)]
    float4 pos01 : POSITION0;
    [vk::location(1)]
    float4 posst : POSITION1;
    [vk::location(2)]
    float4 uvst : TEXCOORD0;
}

struct FragmentInput
{
    float2 uv;
}

struct VertexOutput
{
    float4 pos : SV_Position;
    FragmentInput fragmentInput : Varyings;
}

struct PushConstant
{
    float2 screenSize;
}

[vk::push_constant]
PushConstant pushConstant;

[vk::binding(0, 0)]
Sampler2D tex;

[shader("vertex")]
VertexOutput vertMain(Vertex v)
{
    let p2 = (v.pos01.xy * v.posst.xy + v.posst.zw) * 2.0 / pushConstant.screenSize - 1.0;

    VertexOutput vo;
    vo.pos = float4(p2, 0.0, 1.0);
    vo.fragmentInput.uv = v.pos01.xy * v.uvst.xy + v.uvst.zw;
    return vo;
}

[shader("fragment")]
float4 fragMain(FragmentInput input: Varyings) : SV_Target
{
    return float4(1.0, 1.0, 1.0, 1.0);

    var col = tex.Sample(input.uv);
    col.rgb *= col.a;
    return col;
}
