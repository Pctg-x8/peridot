Properties
  [Instanceable]
  maintex: Texture2D = "white"
End

Pass "Unlit"
    RenderOption NoCulling, InstancedOnly
    VertexBindings
        pos: Float4 [POSITION0]
        uv: Float4 [TEXCOORD0]
    End

    Shader
struct VertexOutput {
    FragmentInput fragmentInput : Varyings;
    float4 pos : SV_Position;
}

struct FragmentInput {
    float2 uv : TEXCOORD0;
}

[shader("vertex")]
VertexOutput vertMain(Vertex v) {
    VertexOutput vo;

    vo.pos = mul(PeridotCameraParameters::viewProjectionMatrix(), mul(PeridotObjectParameters::transformMatrix(PERIDOT_OBJECT_PARAMETERS_ARGS(v)), v.pos));
    vo.fragmentInput.uv = v.uv.xy * PeridotMaterialParameters::instancedProperty[v.__peridot_instanceVars.instanceIndex].maintex_uvst.xy + PeridotMaterialParameters::instancedProperty[v.__peridot_instanceVars.instanceIndex].maintex_uvst.zw;

    return vo;
}

[shader("fragment")]
float4 fragMain(FragmentInput input : Varyings) : SV_Target {
    return float4(input.uv, 1.0, 1.0);
}
    End
End
