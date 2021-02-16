VertexInput {
    Binding 0 [PerVertex] { ipos: vec2; uv: vec2; lim: vec2; sc: vec2; }
}
SpecConstant[VertexShader](0) TargetTextureWidth: float = 640.0;
SpecConstant[VertexShader](1) TargetTextureHeight: float = 480.0;
SpecConstant[VertexShader](2) LineWidth: float = 1.0;
VertexShader {
    RasterPosition = vec4((ipos / vec2(TargetTextureWidth, -TargetTextureHeight)) - 1.0, 0.0, 1.0);
    pcoord = uv;
    limits = lim;
    scale = sc.x / LineWidth;
}
Varyings VertexShader -> FragmentShader { pcoord: vec2; limits: vec2; scale: float; }
Header[FragmentShader] {
#define ALGORITHM_ITERATIONS 3
    // solve x^3+(0.5-Sy)*x-0.5*Sx = 0 for nearest point of parabola
    // Then calc nearest distance
    float distance(vec2 coord) {
        // The Low-Rank LDL^T Quartic Solver, Section 3

        // solve for positive region in parabola coord, then apply sign
        float sig = coord.x > 0.0 ? 1.0 : -1.0;
        float px = abs(coord.x);

        float g = 0.5 - coord.y;
        float h = 0.5 * px;         // always h < 0
        float xr = sqrt(h);
        float x0 = g < -h ? sqrt(-g)
            : g > xr ? h / g
            : xr;

        for (int _ = 0; _ < ALGORITHM_ITERATIONS; _++) {
            // linearized reciprocalなのでsecant methodで求める(であってる？)
            // x0^2 * x^2 - h * x + (x0^2 * g + 2 * x0 * h) = 0
            // h = -0.5 * px なので 2 * h = -px, -h = 0.5 * px
            // x0^2 * x^2 + 0.5 * px * x + (x0^2 * g - px * x0)
            float c = x0 * x0 * g - x0 * px;
            float b = 0.5 * px;
            float xmin = (2.0 * c) / (-b - sqrt(b*b - 4.0 * x0*x0 * c));
            x0 = xmin;
        }
        x0 = x0 * sig;

        // (x-x0) is a factor of x^3+(0.5-Sy)*x-0.5*Sx
        // (x-x0)(x^2+x*x0+(0.5-Sy+x0^2)) = 0 // まじめに計算すると0にはならないんだけど、x-x0が0になるのはわかってるので潰す
        // x^2+x*x0+(0.5-Sy+x0^2) = 0
        // x = (-x0 +- sqrt(x0^2 - 4(0.5-Sy+x0^2)))/2 = (-x0*0.5) +- sqrt((x0^2 - 4(0.5-Sy+x0^2)*(1/4)))
        // = (-x0*0.5) +- sqrt((x0^2 - 2+4Sy-4*x0^2)/4) = (-x0*0.5) +- sqrt(-(3/4)*x0^2 - 0.5+Sy)
        // = (-x0*0.5) +- sqrt(-0.75*x0^2 - (0.5-Sy))
        float d = -0.75 * x0 * x0 - g;
        float x1 = -x0 * 0.5 - sqrt(d);
        float x2 = -x0 * 0.5 + sqrt(d);

        float x0_a = clamp(x0, limits.x, limits.y);
        float x1_a = clamp(x1, limits.x, limits.y);
        float x2_a = clamp(x2, limits.x, limits.y);
        float d0sq = dot(vec2(x0_a, x0_a * x0_a) - coord, vec2(x0_a, x0_a * x0_a) - coord);
        float d1sq = dot(vec2(x1_a, x1_a * x1_a) - coord, vec2(x1_a, x1_a * x1_a) - coord);
        float d2sq = dot(vec2(x2_a, x2_a * x2_a) - coord, vec2(x2_a, x2_a * x2_a) - coord);
        float dsq = min(d0sq, min(d1sq, d2sq));

        return sqrt(dsq) * scale;
    }
}
FragmentShader {
    float d = distance(pcoord);
    float lum = max(0.0, 0.5 - 0.5 * d);

    Target[0] = vec4(lum, lum, lum, lum);
}
