[shader("vertex")]
float4 vertMain(uint vindex: SV_VertexID) : SV_Position
{
    switch (vindex)
    {
    case 0:
        return float4(-1.0, -1.0, 0.0, 1.0);
    case 1:
        return float4(3.0, -1.0, 0.0, 1.0);
    default:
        return float4(-1.0, 3.0, 0.0, 1.0);
    }
}

[shader("fragment")]
float fragMain() : SV_Target
{
    return 1.0;
}
