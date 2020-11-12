#version 450

#define MAX_RENDERED_INSTANCE_SIZE 65536

struct ParticleInstance {
    mat4 model_trans;
    float alpha;
};

layout(location = 0) in vec4 pos;
layout(location = 1) in vec2 iuv;

layout(location = 0) out vec2 ouv;
layout(location = 1) out float alpha;
out gl_PerVertex { out vec4 gl_Position; };

layout(set = 0, binding = 0, std140) uniform Camera {
    mat4 mvp;
};
layout(set = 1, binding = 0, std140) readonly buffer InstanceMatrices {
    ParticleInstance inst[MAX_RENDERED_INSTANCE_SIZE];
};

void main() {
    gl_Position = transpose(inst[gl_InstanceIndex].model_trans) * transpose(mvp) * pos;
    ouv = iuv;
    alpha = inst[gl_InstanceIndex].alpha;
}
