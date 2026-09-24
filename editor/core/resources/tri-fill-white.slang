[shader("vertex")]
float4 vertMain(float2 p: POSITION0) : SV_Position {
  return float4(p, 0.0, 1.0);
}

[shader("fragment")]
float4 fragMain() : SV_Target { return float4(1.0, 1.0, 1.0, 1.0); }
