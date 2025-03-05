#version 430 core
// #extension GL_NV_gpu_shader5 : enable

layout(std430, binding = 0) buffer Vertices {
    vec2 vertices[];
};

layout(std430, binding = 1) buffer Indices {
    uint indices[];
};

struct UVZ {
    vec2 uv;
    bool z;
};

layout(std430, binding = 2) buffer Uvs {
    UVZ uvs[];
};

uniform int selected_vert;

uniform int vertex_offset;
uniform int triangle_offset;

uniform vec2 scale;
uniform vec2 translation_before_scaling; 
uniform vec2 translation_after_scaling; 

out vec2 uv;
out float inner;

void main() {
    int offset_idx = gl_VertexID + triangle_offset * 3;
    uint index = indices[offset_idx] + vertex_offset;

    uv = uvs[offset_idx].uv;
    inner = uvs[offset_idx].z ? 1.0f : -1.0f;

    vec2 vertex = vertices[index];
    vertex += translation_before_scaling;
    vertex *= scale; 
    vertex += translation_after_scaling;
    // bool on_curve = on_curves[index];

    // uvz = vec3(0, 1, 1);

    uint triangle = index / 3;

    // color = vec3(
    //         step(2, triangle++ % 3),
    //         step(2, triangle++ % 3),
    //         step(2, triangle % 3)
    //     );

    gl_Position = vec4(vertex, -0.2, 1);
    gl_PointSize = 3;
    if (selected_vert == indices[offset_idx]) {
        gl_PointSize = 8;
    }
}
