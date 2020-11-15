// Skybox Shader from http://publications.lib.chalmers.se/records/fulltext/203057/203057.pdf

VertexInput {
    Binding 0 [PerVertex] { pos: vec2; uvin: vec2; }
}

VertexShader {
    uv = uvin;
    RasterPosition = vec4(pos, 0.0f, 1.0f);
}

Varyings VertexShader -> FragmentShader {
    uv: vec2;
}

Uniform[FragmentShader](0, 2) ViewUniform { float eyeHeight, viewZenithAngleIn, viewAzimuthAngleIn; }

Header[FragmentShader] {
    layout(set = 0, binding = 0) uniform sampler3D scatter;
    layout(set = 0, binding = 1) uniform sampler2D transmittance;

    // From precompute_common.comp
    const float H_ATM = 80000;
    const float R_EARTH = 6371000;
    const vec3 RayleighCoeffs = vec3(6.55e-6, 1.73e-5, 2.30e-5);
    const vec3 MieCoeffs = vec3(2e-6, 2e-6, 2e-6);
    const vec3 TransmittanceMieCoeffs = MieCoeffs / 0.9;

    // Parameterizations //
    float parameterizeHeight(float h) { return sqrt(h / H_ATM); }
    float parameterizeViewZenithCos(float cv, float h)
    {
        const float ch = -sqrt(h * (2.0 * R_EARTH + h)) / (R_EARTH + h);
        return cv > ch ? (0.5 + 0.5 * pow((cv - ch) / (1.0 - ch), 0.2)) : (0.5 * pow((ch - cv) / (ch + 1.0), 0.2));
    }
    float parameterizeSunZenithCos(float cs)
    {
        return 0.5 * ((1.0 - 0.26) + (atan(max(cs, -0.1975) * tan(1.26 * 0.75)) / 0.75));
    }

    const float MieAsymmetryFactor = 0.93;
    float phaseRayleigh(float cv) { return 8.0 * (7.0 / 5.0 + 0.5 * cv) / 10.0; }
    float phaseMie(float cv)
    {
        const float v = (3.0 * (1.0 - pow(MieAsymmetryFactor, 2.0))) / (2.0 * (2.0 + pow(MieAsymmetryFactor, 2.0)));
        return v * (1.0 + pow(cv, 2.0)) / pow(1.0 + pow(MieAsymmetryFactor, 2.0) - 2.0 * MieAsymmetryFactor * pow(cv, 2.0), 3.0 / 2.0);
    }

    vec4 getScatterLight(float height, float cv, float cs)
    {
        return texture(scatter, vec3(parameterizeHeight(height), parameterizeViewZenithCos(cv, height), parameterizeSunZenithCos(cs)));
    }
    vec3 estimateMieRgb(vec4 scatterLight)
    {
        return scatterLight.xyz * (scatterLight.w / scatterLight.x) * (RayleighCoeffs.x / MieCoeffs.x) * (MieCoeffs / RayleighCoeffs);
    }
    vec4 lookupTransmittance(float height, float cvs)
    {
        return texture(transmittance, vec2(cvs, parameterizeHeight(height)));
    }

    // ACES tonemapper //
    // https://github.com/TheRealMJP/BakingLab/blob/master/BakingLab/ACES.hlsl
    const mat3 INPUT_MATRIX = mat3(
        vec3(0.59719, 0.35458, 0.04823),
        vec3(0.07600, 0.90834, 0.01566),
        vec3(0.02840, 0.13383, 0.83777)
    );
    const mat3 OUTPUT_MATRIX = mat3(
        vec3( 1.60475, -0.53108, -0.07367),
        vec3(-0.10208,  1.10813, -0.00605),
        vec3(-0.00327, -0.07276,  1.07602)
    );
    vec3 fit(vec3 v) {
        const vec3 a = v * (v + 0.0245786) - 0.000090537;
        const vec3 b = v * (0.983729 * v + 0.4329510) + 0.238081;
        
        return a / b;
    }

    vec4 tonemap(vec4 i) {
        vec3 c = fit(i.xyz * INPUT_MATRIX) * OUTPUT_MATRIX;
        return vec4(clamp(c, 0.0, 1.0), i.w);
    }

    float asRadians(float deg) { return deg * 3.1415926f / 180.0f; }
}

FragmentShader {
    const float aspect = 4.0 / 3.0;
    const float zd = 1.0 / tan(35.0 * 3.1415926 / 180.0);
    const mat3 viewVecRotX = mat3(
        1.0, 0.0, 0.0,
        0.0, cos(asRadians(90.0 - viewZenithAngleIn)), -sin(asRadians(90.0 - viewZenithAngleIn)),
        0.0, sin(asRadians(90.0 - viewZenithAngleIn)), cos(asRadians(90.0 - viewZenithAngleIn))
    );
    const mat3 viewVecRotY = mat3(
        cos(asRadians(viewAzimuthAngleIn)), 0.0, sin(asRadians(viewAzimuthAngleIn)),
        0.0, 1.0, 0.0,
        -sin(asRadians(viewAzimuthAngleIn)), 0.0, cos(asRadians(viewAzimuthAngleIn))
    );
    const vec3 viewvec = viewVecRotY * viewVecRotX * normalize(vec3((2.0 * uv.x - 1.0) * aspect, -(2.0 * uv.y - 1.0), zd));
    const float cv = dot(viewvec, vec3(0.0, 1.0, 0.0));
    const float camHeight = eyeHeight;
    const vec3 incidentLightDir = normalize(vec3(0.0, -0.4, -0.8));
    const float cs = dot(-incidentLightDir, vec3(0.0, 1.0, 0.0));
    const float vs_cos = dot(incidentLightDir, -viewvec);

    const vec4 scatter = getScatterLight(camHeight, cv, cs);
    const vec3 mieRgb = phaseMie(vs_cos) * estimateMieRgb(scatter);
    Target[0] = tonemap(vec4(10.0 * (/*lookupTransmittance(eyeHeight, cs).xyz + */phaseRayleigh(vs_cos) * scatter.xyz + mieRgb), 1.0));
}
