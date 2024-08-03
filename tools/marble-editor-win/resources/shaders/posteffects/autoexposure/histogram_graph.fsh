#version 450

layout(location = 0) in vec2 uv;
layout(location = 0) out vec4 col;
layout(set = 0, binding = 0) uniform PostEffectGlobalWorkBufferView {
    float _exposureBaseLum;
    uint histogramMaxValue;
};
layout(set = 0, binding = 1) buffer HistogramComputationOutputBuffer {
    uint histogram[256];
};

void main() {
    float targetHistogramIndexF = clamp(uv.x * 255.0, 0.0, 255.0);
    uint targetHistogramIndexLow = uint(targetHistogramIndexF);
    uint targetHistogramIndexHigh = clamp(uint(targetHistogramIndexF) + 1, 0, 255);

    uint histogramLow = histogram[targetHistogramIndexLow];
    uint histogramHigh = histogram[targetHistogramIndexHigh];
    float histogramValue = mix(float(histogramLow), float(histogramHigh), fract(targetHistogramIndexF));

    float histogramBarRate = histogramValue / float(histogramMaxValue);

    col = (1.0 - histogramBarRate) <= uv.y ? vec4(1.0) : vec4(0.0);
}
