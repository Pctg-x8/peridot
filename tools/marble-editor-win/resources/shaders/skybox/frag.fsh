// Skybox Shader from http://publications.lib.chalmers.se/records/fulltext/203057/203057.pdf

#version 450

layout(location = 0) in vec2 uv;

layout(location = 0) out vec4 target;

layout(set = 0, binding = 0, std140) uniform RenderCameraData {
    mat4 cameraViewProjectionMatrix;
    mat4 cameraInverseViewMatrix;
    float perspFovInRadians, aspectWH;
};
layout(set = 1, binding = 0, std140) uniform PrimaryDirectionalLightData {
    vec3 incidentLightDir;
    float lightIntensity;
};
layout(set = 1, binding = 1) uniform sampler3D scatter;
layout(set = 1, binding = 2) uniform sampler2D transmittance;

// From precompute_common.comp
const float H_ATM = 80000;
const float R_EARTH = 6371000;
const vec3 RayleighCoeffs = vec3(6.55e-6, 1.23e-5, 2.30e-5);
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

void main() {
    const float zd = 1.0 / tan(perspFovInRadians * 0.5);
    const vec3 viewvec = normalize((vec4((2.0 * uv.x - 1.0) * aspectWH, -(2.0 * uv.y - 1.0), zd, 0.0) * cameraInverseViewMatrix).xyz);
    const float cv = dot(viewvec, vec3(0.0, 1.0, 0.0));
    const float cs = dot(-incidentLightDir.xyz, vec3(0.0, 1.0, 0.0));
    const float camHeight = (vec4(0.0, 0.0, 0.0, 1.0) * cameraInverseViewMatrix).y;
    const float vs_cos = dot(incidentLightDir.xyz, -viewvec);

    const float ph_rayleigh = (1.0f + pow(vs_cos, 2.0f)) * 3.0f / 4.0f;

    const float fade = clamp(viewvec.y / 0.02f, 0.0f, 1.0f);

    const vec4 scatterLight = getScatterLight(camHeight, cv, cs);
    const vec3 mieRgb = phaseMie(vs_cos) * estimateMieRgb(scatterLight);
    target = mix(vec4(0.1, 0.1, 0.13, 1.0), vec4(ph_rayleigh * scatterLight.xyz * lightIntensity + mieRgb * lightIntensity, 1.0), fade);
}
