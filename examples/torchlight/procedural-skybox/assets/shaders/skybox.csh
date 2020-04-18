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

Uniform[FragmentShader](0, 2) ViewUniform { float eyeHeight, viewZenithAngleIn; }

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
        return 0.5 * ((1.0 - 0.26) + (atan(max(cs, -0.1975) * tan(1.26 * 1.1)) / 1.1));
    }

    const float MieAsymmetryFactor = 0.3;
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
    vec4 lookupTransmittance(float height, vec2 dir)
    {
        return texture(transmittance, vec2(parameterizeSunZenithCos(dir.y), parameterizeHeight(height)));
    }
}

FragmentShader {
    const float thView = (viewZenithAngleIn + (2.0 * uv.y - 1.0) * 35.0) * 3.1415926 / 180.0;
    const float cv = cos(thView);
    const float camHeight = eyeHeight;
    const vec2 incidentLightDir = normalize(vec2(0.5, -1.0));
    const float vs_cos = dot(incidentLightDir, -vec2(sqrt(1.0 - pow(cv, 2.0)), cv));

    const vec4 scatter = getScatterLight(camHeight, cv, -incidentLightDir.y);
    const vec3 mieRgb = phaseMie(vs_cos) * estimateMieRgb(scatter);

    Target[0] = vec4(20.0 * (phaseRayleigh(vs_cos) * scatter.xyz + mieRgb), 1.0);
    // Target[0] = vec4(parameterizeHeight(camHeight), parameterizeViewZenithCos(cv, camHeight), parameterizeSunZenithCos(incidentLightDir.y), 1.0);
}
