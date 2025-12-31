// https://community.arm.com/cfs-file/__key/communityserver-blogs-components-weblogfiles/00-00-00-20-66/siggraph2015_2D00_mmg_2D00_marius_2D00_notes.pdf

struct Params {
    float2 texelSize;
    float offsetScale;
}

[vk::push_constant]
Params params;

[vk::binding(0, 0)]
Sampler2D<float4> tex;

struct VertexOutput {
    float4 pos : SV_Position;
    FragmentInput fragmentInput : Varyings;
}

[shader("vertex")]
VertexOutput vertMain(uint vindex : SV_VulkanVertexID) {
    let p = float2(vindex != 1 ? 0.0 : 2.0, vindex != 2 ? 0.0 : 2.0);
    let halfPixel = params.texelSize * 0.5;

    VertexOutput vo;
    vo.pos = float4(p * 2.0 - 1.0, 0.0, 1.0);
    vo.fragmentInput.uvCenter = p;
    vo.fragmentInput.uv_lb_rt.xy = p + halfPixel * params.offsetScale;
    vo.fragmentInput.uv_lb_rt.zw = p - halfPixel * params.offsetScale;
    vo.fragmentInput.uv_lt_rb.xy = p + float2(halfPixel.x, -halfPixel.y) * params.offsetScale;
    vo.fragmentInput.uv_lt_rb.zw = p - float2(halfPixel.x, -halfPixel.y) * params.offsetScale;
    return vo;
}

struct FragmentInput {
    float2 uvCenter;
    float4 uv_lb_rt;
    float4 uv_lt_rb;
}

[shader("fragment")]
float4 fragMain(FragmentInput input : Varyings) : SV_Target0 {
    var a = tex.Sample(input.uvCenter) * 4.0;
    a += tex.Sample(input.uv_lb_rt.xy);
    a += tex.Sample(input.uv_lb_rt.zw);
    a += tex.Sample(input.uv_lt_rb.xy);
    a += tex.Sample(input.uv_lt_rb.zw);

    return a * 0.125;
}
